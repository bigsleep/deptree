{-# LANGUAGE OverloadedStrings, TypeOperators, FlexibleContexts #-}
module Application.Run.SessionSpec
( sessionSpec
) where

import Control.Eff (Eff, VE(..), (:>), Member, SetMember, admin, handleRelay)
import Control.Eff.Session (Session(..), sget, sput, sdestroy, getSessionId)
import qualified Control.Eff.Kvs as Kvs (Kvs(..), get)
import Control.Eff.Lift (Lift, runLift)
import Control.Eff.Exception (Exc, runExc, throwExc)
import Control.Eff.State.Strict (State, get, put, evalState, runState)
import Control.Eff.Reader.Strict (Reader, runReader)
import Control.Eff.Logger (LogLevel(..), runLoggerStdIO)

import Control.Exception (SomeException)

import qualified Data.Map as M (Map, fromList, empty, member, null)
import qualified Data.HashMap.Strict as HM (fromList)
import qualified Data.ByteString as B (ByteString)
import qualified Data.ByteString.Char8 as B (pack)
import qualified Data.ByteString.Lazy as L (ByteString, fromStrict, toStrict)
import qualified Data.ByteString.Lazy.Char8 as L (pack)
import Data.UnixTime (UnixTime,  getUnixTime)
import Data.Serializable (serialize)

import qualified Network.Wai as Wai (Request, defaultRequest, requestHeaders)
import Web.Cookie (SetCookie, renderCookies)
import Blaze.ByteString.Builder (toByteString)

import Application.Logger (Logger, logDebug)
import Application.Run.Kvs.Map (runKvsMap)
import Application.Exception (Exception(..))
import Application.Session (SessionKvs(..), SessionError(..), SessionData(..), defaultSessionData)
import Application.Run.Session (runSession)

import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import qualified Test.Hspec.QuickCheck as Q
import qualified Test.QuickCheck.Property as Q

sessionSpec :: Spec
sessionSpec = describe "session" $ do

    it "start automatically" $ do
        let key = "state" :: B.ByteString
        let val = "hello" :: B.ByteString
        let code = do
                    sput key val
                    getSessionId
        Right (s, sid) <- runTest "SID" M.empty Wai.defaultRequest code
        shouldSatisfy s (M.member sid)

    it "restore automatically" $ do
        let name = "SID"
        let sid = "testSessionId000"
        let cookies = [(name, sid)]
        let headers = [("Cookie", toByteString . renderCookies $ cookies)]
        let request = Wai.defaultRequest { Wai.requestHeaders = headers }
        let key = "hello"
        let val = "world"
        let sval = HM.fromList [(key, val)]
        let sd = serialize defaultSessionData { sessionId = sid, sessionValue = sval }
        let sessionState = M.fromList [(sid, sd)]
        let code = do
                    v <- sget key
                    sid' <- getSessionId
                    return (v, sid')
        Right (_, result) <- runTest name sessionState request code
        result `shouldBe` (Just val, sid)

    it "destroy" $ do
        let key = "state" :: B.ByteString
        let val = "hello" :: B.ByteString
        let code = do
                    sput key val
                    before <- getSessionId
                    sdestroy
                    after <- getSessionId
                    return (before, after)
        Right (s, (_, afterId)) <- runTest "SID" M.empty Wai.defaultRequest code
        afterId `shouldBe` ""
        shouldSatisfy s M.null



runTest :: B.ByteString ->
           M.Map B.ByteString L.ByteString ->
           Wai.Request ->
           Eff ( Session
              :> Kvs.Kvs SessionKvs
              :> State (M.Map B.ByteString L.ByteString)
              :> State SessionData
              :> Exception
              :> Reader Wai.Request
              :> Reader UnixTime
              :> Logger
              :> Lift IO
              :> ()) a ->
           IO (Either SomeException (M.Map B.ByteString L.ByteString, a))
runTest name s request a = do
    t <- getUnixTime
    runLift
        . runLoggerStdIO DEBUG
        . flip runReader t
        . flip runReader request
        . runExc
        . evalState defaultSessionData
        . runState s
        . runKvsMap
        . runSession name 0 $ a
