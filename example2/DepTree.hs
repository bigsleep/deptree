{-# LANGUAGE TypeOperators, OverloadedStrings, FlexibleContexts, DeriveDataTypeable, TemplateHaskell #-}
import Control.Eff ((:>), Eff, Member, SetMember)
import Control.Eff.Exception (runExc)
import Control.Eff.Lift (Lift, lift, runLift)
import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.STM (TVar, newTVar, readTVar, writeTVar, atomically)
import Control.Monad (forever, replicateM_)
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
import qualified Data.ByteString.Char8 as B (unpack)
import qualified Data.ByteString.Lazy as L (ByteString, length, putStr)
import qualified Data.ByteString.Lazy.Char8 as L (pack, unpack)
import qualified Data.Text.Lazy.Encoding as T (encodeUtf8)
import qualified Data.HashMap.Strict as HM (HashMap, fromList, lookup, empty, member)
import qualified Data.Aeson as DA (decode)
import qualified Data.Aeson.TH as DA (deriveJSON, defaultOptions)
import Data.Reflection (Given, give, given)

import qualified Network.HTTP.Types as HTTP (methodGet, hAccept, status404, status500)
import qualified Network.HTTP.Client as N (Request(..), Response(..), Manager, parseUrl, newManager, defaultManagerSettings)
import qualified Network.Wai.Handler.Warp as Warp (run)

import Wf.Control.Eff.HttpClient (HttpClient, httpClient, runHttpClient)
import Wf.Application.Exception (Exception, throwException)
import Wf.Application.Logger (Logger, logDebug, runLoggerStdIO, LogLevel(..))
import Wf.Network.Http.Types (Request, Response, defaultResponse, requestMethod, requestRawPath, requestHeaders, requestQuery)
import Wf.Network.Http.Response (setStatus, setContentType, setContentLength)
import Wf.Web.Api (apiRoutes, getApi, postApi, ApiInfo(..))
import Wf.Network.Wai (toWaiResponse, toWaiApplication)



main :: IO ()
main = do
    tv <- atomically $ newTVar HM.empty
    manager <- N.newManager N.defaultManagerSettings
    forkIO $ worker 1440 manager tv
    Warp.run 3000 . toWaiApplication . run manager $ routes tv




worker :: Int -> N.Manager -> TVar (HM.HashMap String [String]) -> IO ()
worker sleepMinutes manager tv = forever $ routine
    where
    routine = do
        updatePackageLibraryDependencies manager tv
        replicateM_ sleepMinutes $ threadDelay 60000000



type M = Eff
    (  HttpClient
    :> Logger
    :> Exception
    :> Lift IO
    :> ())

routes :: TVar (HM.HashMap String [String]) -> Request () -> M (Response L.ByteString)
routes tv request = apiRoutes notFound rs request method path
    where
    rs = [ getApi "/:package" (const $ depTreeApp tv)
         ]
    method = requestMethod request
    path = requestRawPath request

notFound :: (Monad m) => m (Response L.ByteString)
notFound = return . setStatus HTTP.status404 . defaultResponse $ ""

depTreeApp :: (Given ApiInfo) => TVar (HM.HashMap String [String]) -> M (Response L.ByteString)
depTreeApp tv = do
    package <- getUrlParam "package"
    lift $ print package
    maybeTree <- depTree . B.unpack $ package
    case maybeTree of
        Just tree -> do
            body <- lift . toSvg $ tree
            return . setContentType "image/svg+xml" . setContentLength (fromIntegral $ L.length body) $ defaultResponse body
        Nothing -> notFound
    where
    depTree name = lift . atomically $ do
        hs <- readTVar tv
        if HM.member name hs
            then return . Just $ createDepTree hs [] name
            else return Nothing
    throwError s = throwException . Error $ s
    params = apiInfoParameters given
    getUrlParam a = case L.lookup a params of
        Just x -> return x
        Nothing -> throwError $ "no url parameter"



run :: N.Manager
    -> (Request () -> M (Response L.ByteString))
    -> Request ()
    -> IO (Response L.ByteString)
run manager app = run'
    where
    run' = runLift
        . (>>= handleError)
        . runExc
        . runLoggerStdIO DEBUG
        . runHttpClient manager
        . app
    internalError = setStatus HTTP.status500 . defaultResponse $ ""
    handleError (Left (Control.Exception.SomeException e)) = do
        lift $ print e
        return internalError
    handleError (Right r) = return r



updatePackageLibraryDependencies :: N.Manager -> TVar (HM.HashMap String [String]) -> IO ()
updatePackageLibraryDependencies manager tv = do
    r <- runLift . runExc . runHttpClient manager $ (getPackageLibraryDependencies :: Eff (HttpClient :> Exception :> Lift IO :> ()) (HM.HashMap String [String]))
    handleResult r
    where
    handleResult (Right a) = atomically . writeTVar tv $ a
    handleResult _ = threadDelay 100000000 >> updatePackageLibraryDependencies manager tv



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


getPackageLibraryDependencies
    :: (Member HttpClient r, Member Exception r, SetMember Lift (Lift IO) r)
    => Eff r (HM.HashMap String [String])
getPackageLibraryDependencies =
    fmap HM.fromList $ mapM (nameAndDeps) =<< getPackageNames
    where
    nameAndDeps name = do
        ds <- getLibraryDependencies name
        lift . putStrLn $ name
        lift $ threadDelay 100000
        return (name, ds)

createDepTree :: HM.HashMap String [String] -> [String] -> String -> Tree String
createDepTree deps ancestors name
    | elem name ancestors = Tree name []
    | otherwise = Tree name $ map (createDepTree  deps (name : ancestors)) childNames
    where childNames = fromMaybe [] $ HM.lookup name deps

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
