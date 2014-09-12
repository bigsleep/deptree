{-# LANGUAGE TypeOperators, OverloadedStrings, FlexibleContexts, FlexibleInstances, DeriveDataTypeable, TemplateHaskell #-}
import Control.Eff ((:>), Eff, Member)
import Control.Eff.Exception (runExc)
import Control.Eff.Lift (Lift, lift, runLift)
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (replicateM_, foldM, void, when, unless)
import qualified Control.Exception (Exception, SomeException(..))

import Distribution.Package (PackageName(..), Dependency(..))
import Distribution.PackageDescription (CondTree(..), GenericPackageDescription(..))
import Distribution.PackageDescription.Parse (ParseResult(..), parsePackageDescription)

import Data.GraphViz (DotGraph(..), DotStatements(..), DotNode(..), DotEdge(..))
import Data.GraphViz.Commands (GraphvizCommand(Dot), GraphvizOutput(Svg), graphvizWithHandle)
import Data.GraphViz.Commands.IO (hGetStrict)

import qualified Data.List as L (lookup, elem)
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable)
import qualified Data.ByteString.Char8 as B (pack, unpack)
import qualified Data.ByteString.Lazy as L (ByteString, length, append, concat, readFile)
import qualified Data.ByteString.Lazy.Char8 as L (pack, unpack)
import qualified Data.Text.Lazy.Encoding as T (encodeUtf8)
import qualified Data.Aeson as DA (decode, encode)
import qualified Data.Aeson.TH as DA (deriveJSON, defaultOptions)
import Data.Reflection (Given, given)
import qualified Database.Redis as Redis (ConnectInfo(..))

import qualified Network.HTTP.Types as HTTP (methodGet, hAccept, status404, status500)
import qualified Network.HTTP.Client as N (Request(..), Response(..), Manager, parseUrl, newManager, defaultManagerSettings)
import qualified Network.Wai.Handler.Warp as Warp (run)

import Wf.Control.Eff.HttpClient (HttpClient, httpClient, runHttpClient)
import Wf.Application.Exception (Exception, throwException)
import Wf.Application.Logger (Logger, runLoggerStdIO, LogLevel(..))
import Wf.Application.Kvs (Kvs)
import qualified Wf.Application.Kvs as Kvs (get, set, exists)
import Wf.Network.Http.Types (Request, Response, defaultResponse, requestMethod, requestRawPath)
import Wf.Network.Http.Response (setStatus, setContentType, setContentLength, html)
import Wf.Web.Api (apiRoutes, getApi, ApiInfo(..))
import Wf.Network.Wai (toWaiApplication)
import Wf.Control.Eff.Run.Kvs.Redis (runKvsRedis)
import Wf.Data.Serializable (Serializable(..))
import Settings (Settings(..))



main :: IO ()
main = do
    manager <- N.newManager N.defaultManagerSettings
    settings <- loadSettings
    let interval = settingsIntervalMinutes settings
        port = settingsPort settings
        redis = settingsRedis settings
    _ <- forkIO $ worker interval manager redis
    Warp.run port . toWaiApplication . run manager redis $ routes

    where
    loadSettings :: IO Settings
    loadSettings = do
        a <- fmap DA.decode . L.readFile $ "example2/config/settings.json"
        case a of
             Just settings -> return settings
             Nothing -> error "load failure"



worker :: Int -> N.Manager -> Redis.ConnectInfo -> IO ()
worker sleepMinutes manager redis = do
    update
    routine
    where
    routine = do
        replicateM_ sleepMinutes $ threadDelay 60000000
        _ <- forkIO routine
        update
        return ()
    update = void . runLift . runExc . runLoggerStdIO WARN . runKvsRedis redis . runHttpClient manager $ (updatePackageLibraryDependencies :: Eff (HttpClient :> Kvs :> Logger :> Exception :> Lift IO :> ()) ())



type M = Eff
    (  HttpClient
    :> Kvs
    :> Logger
    :> Exception
    :> Lift IO
    :> ())

routes :: Request () -> M (Response L.ByteString)
routes request = apiRoutes notFound rs request method path
    where
    rs = [ getApi "/" (const rootApp)
         , getApi "/:package" (const depTreeApp)
         ]
    method = requestMethod request
    path = requestRawPath request

notFound :: (Monad m) => m (Response L.ByteString)
notFound = return . setStatus HTTP.status404 . defaultResponse $ ""


rootApp :: M (Response L.ByteString)
rootApp = do
    packages <- getPackageNames
    let links = L.concat . map (makeLink . L.pack) $ packages
        body = "<!DOCTYPE html>\n<meta charset=\"utf-8\">\n<title>DepTree</title>\n<h1>DepTree</h1>\n" `L.append` links
    return . html body $ defaultResponse ()

    where
    makeLink name = "<a href=\"" `L.append` name `L.append` "\">" `L.append` name `L.append` "</a>&nbsp;"


depTreeApp :: (Given ApiInfo) => M (Response L.ByteString)
depTreeApp = do
    package <- getUrlParam "package"
    (n, e) <- depTree package
    body <- lift $ toSvg n e
    return . setContentType "image/svg+xml" . setContentLength (fromIntegral $ L.length body) $ defaultResponse body

    where
    depTree name = do
        let name' = B.unpack name
        Kvs.exists name >>= flip unless (throwError "package not found")
        cache <- Kvs.get . B.pack $ "cache:" ++ name'
        case cache of
            Just a -> return a
            _ -> do
                (n, e) <- pickNodesAndEdges True ([], [])  name'
                when (length n > 10) $ cacheNodesAndEdges name' (n, e)
                return (n, e)
    throwError s = throwException . Error $ s
    params = apiInfoParameters given
    getUrlParam a = case L.lookup a params of
        Just x -> return x
        Nothing -> throwError "no url parameter"



run :: N.Manager
    -> Redis.ConnectInfo
    -> (Request () -> M (Response L.ByteString))
    -> Request ()
    -> IO (Response L.ByteString)
run manager redis app = run'
    where
    run' = runLift
        . (>>= handleError)
        . runExc
        . runLoggerStdIO WARN
        . runKvsRedis redis
        . runHttpClient manager
        . app
    internalError = setStatus HTTP.status500 . defaultResponse $ ""
    handleError (Left (Control.Exception.SomeException e)) = do
        lift $ print e
        return internalError
    handleError (Right r) = return r



getPackageNames :: (Member HttpClient r, Member Exception r) => Eff r [String]
getPackageNames = do
    reqInit <- case N.parseUrl "http://hackage.haskell.org/packages/" of
                    Right r -> return r
                    Left e -> throwException e

    let req = reqInit { N.method = HTTP.methodGet
                      , N.requestHeaders = [(HTTP.hAccept, "application/json")]
                      }

    res <- httpClient req

    return . map packageName . fromMaybe [] . DA.decode . N.responseBody $ res



getLibraryDependencies :: (Member HttpClient r, Member Exception r) => String -> Eff r [String]
getLibraryDependencies package = do
    req <- case N.parseUrl $ "http://hackage.haskell.org/package/" ++ package ++ '/' : package ++ ".cabal" of
                    Right r -> return r
                    Left e -> throwException e

    res <- httpClient req

    return . handleResult . parsePackageDescription . L.unpack . N.responseBody $ res
    where
    handleResult (ParseOk _ a) = map pkName . fromMaybe [] . fmap condTreeConstraints . condLibrary $ a
    handleResult _ = []
    pkName (Dependency (PackageName name) _) = name


updatePackageLibraryDependencies :: M ()
updatePackageLibraryDependencies = do
    names <- getPackageNames
    mapM_ nameAndDeps names
    mapM_ updateCache names
    where
    nameAndDeps name = do
        ds <- getLibraryDependencies name
        lift . putStrLn $ name
        Kvs.set (B.pack name) ds
        lift $ threadDelay 500000
    updateCache name = do
        cacheNodesAndEdges name =<< pickNodesAndEdges False ([], []) name

pickNodesAndEdges :: Bool -> ([String], [(String, String)]) -> String -> M ([String], [(String, String)])
pickNodesAndEdges useCache (nodes, edges) cur
    | L.elem cur nodes = return (nodes, edges)
    | otherwise = do
        cache <- if useCache then Kvs.get (B.pack $ "cache:" ++ cur) else return Nothing
        case cache of
            Just a -> return a
            _ -> do
                ds <- fmap (fromMaybe []) $ Kvs.get (B.pack cur)
                let nodes' = cur : nodes
                    edges' = map ((,) cur) ds ++ edges
                foldM (pickNodesAndEdges useCache) (nodes', edges') ds

cacheNodesAndEdges :: String -> ([String], [(String, String)]) -> M ()
cacheNodesAndEdges name a = void $ Kvs.set (B.pack $ "cache:" ++ name) a

toGraphviz :: [String] -> [(String, String)] -> DotGraph String
toGraphviz nodes edges = DotGraph
    { strictGraph = True
    , directedGraph = True
    , graphID = Nothing
    , graphStatements =
        DotStmts
        { attrStmts = []
        , subGraphs = []
        , nodeStmts = map toDotNode nodes
        , edgeStmts = map toDotEdge edges
        }
    }
    where
    toDotNode a = DotNode a []
    toDotEdge (a, b) = DotEdge a b []

toSvg :: [String] -> [(String, String)] -> IO L.ByteString
toSvg nodes edges = fmap T.encodeUtf8 $ graphvizWithHandle Dot g Svg hGetStrict
    where
    g = toGraphviz nodes edges

data Error = Error String deriving (Eq, Show, Typeable)

instance Control.Exception.Exception Error

instance Serializable [String] where
    serialize = DA.encode
    deserialize = DA.decode

instance Serializable ([String], [(String, String)]) where
    serialize = DA.encode
    deserialize = DA.decode

data PackageName' = PackageName'
    { packageName :: String
    } deriving (Show)
DA.deriveJSON DA.defaultOptions ''PackageName'
