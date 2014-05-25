{-# LANGUAGE OverloadedStrings, TypeOperators, FlexibleContexts #-}
module Application.Run.SessionSpec
( sessionSpec
) where

import Control.Eff (Eff, VE(..), (:>), Member, SetMember, admin, handleRelay)
import Control.Eff.Session (Session(..), startSession, readSession, deleteSession)
import qualified Control.Eff.Kvs as Kvs (Kvs(..), get)
import Control.Eff.Lift (Lift, runLift)
import Control.Eff.Exception (Exc, runExc, throwExc)
import Control.Eff.State.Strict (State, evalState)
import Control.Eff.Reader.Strict (Reader, runReader)
import Control.Eff.Logger (Logger, LogLevel(..), runLoggerStdIO)
import qualified Data.Map as M (Map, empty)
import qualified Data.ByteString as B (ByteString)
import qualified Data.ByteString.Char8 as B (pack)
import qualified Data.ByteString.Lazy as L (ByteString, fromStrict, toStrict)
import qualified Data.ByteString.Lazy.Char8 as L (pack)
import Data.UnixTime (UnixTime,  getUnixTime)

import Application.Run.Kvs.Map (runKvsMap)
import Application.Run.Session (runSession, SessionKvs(..), SessionError(..))

import Test.Hspec (Spec, describe, it, shouldBe)
import qualified Test.Hspec.QuickCheck as Q
import qualified Test.QuickCheck.Property as Q


sessionSpec :: Spec
sessionSpec = describe "session" $ do
    sessionStartSpec
    sessionReadSpec
    sessionDeleteSpec


sessionStartSpec :: Spec
sessionStartSpec = it "start session" $ do
    let val = "hello" :: L.ByteString
    let code = do {
        k <- startSession val;
        x <- Kvs.get SessionKvs k;
        case x of
             Just v -> return v;
             Nothing -> throwExc (SessionExpired "error");
        }
    r <- runTest code
    r `shouldBe` Right val


sessionReadSpec :: Spec
sessionReadSpec = it "read session" $ do
    let val = "abcdefg" :: L.ByteString
    let code = startSession val >>= readSession
    r <- runTest code
    r `shouldBe` Right val


sessionDeleteSpec :: Spec
sessionDeleteSpec = it "delete session" $ do
    let val = "hello" :: L.ByteString
    let code = do {
        k <- startSession val;
        deleteSession k;
        readSession k;
        }
    r <- runTest code :: IO (Either SessionError L.ByteString)
    r `shouldBe` Left (SessionExpired "session expired.")


runTest :: Eff ( Session
              :> Reader UnixTime
              :> Exc SessionError
              :> Kvs.Kvs SessionKvs
              :> State (M.Map B.ByteString L.ByteString)
              :> Logger String
              :> Lift IO
              :> ()) a
              -> IO (Either SessionError a)
runTest a = do
    t <- getUnixTime
    runLift . runLoggerStdIO DEBUG . evalState M.empty . runKvsMap . runExc . flip runReader t . runSession 0 $ a
