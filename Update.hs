{-# LANGUAGE OverloadedStrings #-}
module Update
( runUpdateWorker
, keepAlive
, DepTree
) where

import Control.Monad (filterM, replicateM_, forever)
import Control.Monad.Trans.Resource (runResourceT)
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (TVar, writeTVar, atomically)
import Control.Exception (catch, SomeException(..), throw)

import qualified Data.Conduit.Binary as C (sinkFile, sinkLbs, sourceFile)
import qualified Network.HTTP.Conduit as N
import qualified Data.Conduit as C

import System.Process (system)
import System.Exit (ExitCode(..))
import System.Directory (getDirectoryContents, doesDirectoryExist)

import qualified Data.ByteString as B (ByteString)
import qualified Data.ByteString.Char8 as B (pack)
import qualified Data.ByteString.Lazy as L (ByteString, append, concat)
import qualified Data.ByteString.Lazy.Char8 as L (pack, unpack)

import Data.List ((\\), intercalate, sort)
import Data.Maybe (fromMaybe)
import qualified Data.HashMap.Strict as HM (HashMap, fromList)

import qualified Distribution.Package as Dist (PackageName(..), Dependency(..))
import qualified Distribution.PackageDescription as Dist (CondTree(..), GenericPackageDescription(..))
import qualified Distribution.PackageDescription.Parse as Dist (ParseResult(..), parsePackageDescription)


type DepTree = HM.HashMap B.ByteString [B.ByteString]

hackageUrl :: String
hackageUrl = "http://hackage.haskell.org/00-index.tar.gz"

tb :: String
tb = "00-index.tar.gz"

indexPath :: String
indexPath = "index"


runUpdateWorker :: Int -> TVar L.ByteString -> TVar DepTree -> IO ()
runUpdateWorker sleepMinutes root dtree = forever (update' >> sleep)
    where
    sleep = replicateM_ sleepMinutes $ threadDelay 6000000
    update' = update root dtree
              `catch` \(SomeException e) -> putStrLn ("update failure: " ++ show e)


update :: TVar L.ByteString -> TVar DepTree -> IO ()
update root dtree = do
    putStrLn "update started"
    download hackageUrl tb
    _ <- system ("rm -r " ++ indexPath)
    throwIfFailure =<< system ("mkdir " ++ indexPath)
    throwIfFailure =<< system ("tar xzf " ++ tb ++ " -C " ++ indexPath)

    packageNames <- filterM (doesDirectoryExist . ((indexPath ++ "/") ++)) =<< fmap (\\ [".", "..", "preferred-versions"]) (getDirectoryContents indexPath)
    atomically . writeTVar dtree =<< return . HM.fromList =<< mapM f packageNames

    updateRootPage root packageNames
    where
    f n = do
        deps <- dependencies n
        return (B.pack n, deps)
    throwIfFailure e @ (ExitFailure _) = throw e
    throwIfFailure _ = return ()


download :: String -> FilePath -> IO ()
download url dest = do
    request <- N.parseUrl url
    N.withManager $ \manager -> do
        response <- N.http request manager
        N.responseBody response C.$$+- C.sinkFile dest


dependencies :: String -> IO [B.ByteString]
dependencies pname = do
    vs <- getDirectoryContents (indexPath ++ '/' : pname)
    let maximumVersion = showVersion . maximum . map parseVersion $ (vs \\ [".", ".."])
    let path = indexPath ++ "/" ++ pname ++ "/" ++ maximumVersion ++ "/" ++ pname ++ ".cabal"
    runResourceT $
        C.sourceFile path C.$$
        C.sinkLbs >>= return . map B.pack . f . Dist.parsePackageDescription . L.unpack
    where
    f (Dist.ParseOk _ a) = map pkName . fromMaybe [] . fmap Dist.condTreeConstraints . Dist.condLibrary $ a
    f _ = []
    pkName (Dist.Dependency (Dist.PackageName name) _) = name


updateRootPage :: TVar L.ByteString -> [String] -> IO ()
updateRootPage tv packageNames = atomically $ writeTVar tv body
    where
    links = L.concat . map (makeLink . L.pack) . sort $ packageNames
    body = "<!DOCTYPE html>\n<meta charset=\"utf-8\">\n<title>DepTree</title>\n<h1>DepTree</h1>\n" `L.append` links
    makeLink name = "<a href=\"" `L.append` name `L.append` "\">" `L.append` name `L.append` "</a>&nbsp;"


parseVersion :: String -> [Int]
parseVersion = parse . break (== '.')
    where
    parse (a, []) = [read a]
    parse (a, _:b) = read a : parseVersion b


showVersion :: [Int] -> String
showVersion = intercalate "." . map show


keepAlive :: Int -> String -> IO ()
keepAlive sleepMinutes url = forever (wakeup >> sleep)
    where
    sleep = replicateM_ sleepMinutes $ threadDelay 6000000
    wakeup = download url "tmp"
              `catch` \(SomeException e) -> putStrLn ("wakeup failure: " ++ show e)


