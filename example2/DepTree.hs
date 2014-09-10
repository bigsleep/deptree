{-# LANGUAGE TypeOperators, OverloadedStrings, FlexibleContexts, DeriveDataTypeable, TemplateHaskell #-}
import Control.Eff ((:>), Eff, Member, SetMember)
import Control.Eff.Exception (runExc)
import Control.Eff.Lift (Lift, lift, runLift)
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (forever, replicateM_, mapM, mapM_)
import qualified Control.Exception (Exception, SomeException(..))

import Distribution.Package (PackageName(..), Dependency(..))
import Distribution.PackageDescription (CondTree(..), GenericPackageDescription(..))
import Distribution.PackageDescription.Parse (ParseResult(..), parsePackageDescription)

import Data.GraphViz (DotGraph(..), DotStatements(..), DotNode(..), DotEdge(..))
import Data.GraphViz.Commands (GraphvizCommand(Dot), GraphvizOutput(Svg), graphvizWithHandle)
import Data.GraphViz.Commands.IO (hGetStrict)

import qualified Data.List as L (lookup)
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable)
import qualified Data.ByteString as B (ByteString)
import qualified Data.ByteString.Char8 as B (pack, unpack)
import qualified Data.ByteString.Lazy as L (ByteString, length, putStr, append, concat, readFile, fromStrict)
import qualified Data.ByteString.Lazy.Char8 as L (pack, unpack)
import qualified Data.Text.Lazy.Encoding as T (encodeUtf8)
import qualified Data.HashMap.Strict as HM (HashMap, fromList, lookup, empty, member)
import qualified Data.Aeson as DA (decode, encode)
import qualified Data.Aeson.TH as DA (deriveJSON, defaultOptions)
import Data.Reflection (Given, give, given)
import qualified Database.Redis as Redis (ConnectInfo(..))

import qualified Network.HTTP.Types as HTTP (methodGet, hAccept, status404, status500)
import qualified Network.HTTP.Client as N (Request(..), Response(..), Manager, parseUrl, newManager, defaultManagerSettings)
import qualified Network.Wai.Handler.Warp as Warp (run)

import Wf.Control.Eff.HttpClient (HttpClient, httpClient, runHttpClient)
import Wf.Application.Exception (Exception, throwException)
import Wf.Application.Logger (Logger, logDebug, runLoggerStdIO, LogLevel(..))
import Wf.Application.Kvs (Kvs)
import qualified Wf.Application.Kvs as Kvs (get, set, exists, keys)
import Wf.Network.Http.Types (Request, Response, defaultResponse, requestMethod, requestRawPath, requestHeaders, requestQuery)
import Wf.Network.Http.Response (setStatus, setContentType, setContentLength, html)
import Wf.Web.Api (apiRoutes, getApi, postApi, ApiInfo(..))
import Wf.Network.Wai (toWaiResponse, toWaiApplication)
import Wf.Control.Eff.Run.Kvs.Redis (runKvsRedis)
import Settings (Settings(..))



main :: IO ()
main = do
    manager <- N.newManager N.defaultManagerSettings
    settings <- loadSettings
    let interval = settingsIntervalMinutes settings
        port = settingsPort settings
        redis = settingsRedis settings
    forkIO $ worker interval manager redis
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
        forkIO routine
        update
        return ()
    update = runLift . runExc . runLoggerStdIO DEBUG . runKvsRedis redis . runHttpClient manager $ (updatePackageLibraryDependencies :: Eff (HttpClient :> Kvs :> Logger :> Exception :> Lift IO :> ()) ())



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
    lift $ print package
    maybeTree <- depTree package
    case maybeTree of
        Just tree -> do
            body <- lift . toSvg $ tree
            return . setContentType "image/svg+xml" . setContentLength (fromIntegral $ L.length body) $ defaultResponse body
        Nothing -> notFound
    where
    depTree name = do
        exists <- Kvs.exists name
        if exists
            then return . Just =<< createDepTree [] (B.unpack name)
            else return Nothing
    throwError s = throwException . Error $ s
    params = apiInfoParameters given
    getUrlParam a = case L.lookup a params of
        Just x -> return x
        Nothing -> throwError $ "no url parameter"



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
        . runLoggerStdIO DEBUG
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


updatePackageLibraryDependencies
    :: (Member HttpClient r, Member Kvs r, Member Exception r, SetMember Lift (Lift IO) r)
    => Eff r ()
updatePackageLibraryDependencies =
    mapM_ nameAndDeps =<< getPackageNames
    where
    nameAndDeps name = do
        ds <- getLibraryDependencies name
        lift . putStrLn $ name
        Kvs.set (B.pack name) (DA.encode ds)
        lift $ threadDelay 500000

createDepTree :: (Member Kvs r) => [String] -> String -> Eff r (Tree String)
createDepTree ancestors name
    | elem name ancestors = return $ Tree name []
    | otherwise = return . Tree name =<< mapM (createDepTree (name : ancestors)) =<< childNames
    where childNames = fmap (maybe [] (fromMaybe [] . DA.decode)) . Kvs.get . B.pack $ name

toGraphviz :: Tree String -> DotGraph String
toGraphviz tree = DotGraph
    { strictGraph = True
    , directedGraph = True
    , graphID = Nothing
    , graphStatements =
        DotStmts
        { attrStmts = []
        , subGraphs = []
        , nodeStmts = nodes
        , edgeStmts = edges
        }
    }
    where
    nodes = listNodes tree
    edges = listEdges tree
    listNodes (Tree a as) = DotNode a [] : concatMap listNodes as
    listEdges (Tree a as) = map (edge a) as ++ concatMap listEdges as
    edge a (Tree b _) = DotEdge a b []

toSvg :: Tree String -> IO L.ByteString
toSvg tree = fmap T.encodeUtf8 $ graphvizWithHandle Dot g Svg hGetStrict
    where
    g = toGraphviz tree

data Error = Error String deriving (Eq, Show, Typeable)

instance Control.Exception.Exception Error

data Tree a = Tree a [Tree a] deriving (Eq, Show, Read)

data PackageName' = PackageName'
    { packageName :: String
    } deriving (Show)
DA.deriveJSON DA.defaultOptions ''PackageName'
