{-# LANGUAGE OverloadedStrings, TypeOperators, FlexibleContexts #-}
module Wf.Control.Eff.Run.SessionSpec
( sessionSpec
) where

import Control.Eff (Eff, VE(..), (:>), Member, SetMember, admin, handleRelay)
import qualified Wf.Control.Eff.Kvs as Kvs (Kvs(..), get)
import Control.Eff.Lift (Lift, runLift)
import Control.Eff.Exception (Exc, runExc, throwExc)
import Control.Eff.State.Strict (State, get, put, evalState, runState)
import Control.Eff.Reader.Strict (Reader, runReader)
import Wf.Control.Eff.Logger (LogLevel(..), runLoggerStdIO)

import Control.Exception (SomeException)

import qualified Data.Map as M (Map, fromList, empty, member, null)
import qualified Data.HashMap.Strict as HM (fromList)
import qualified Data.ByteString as B (ByteString)
import qualified Data.ByteString.Char8 as B (pack)
import qualified Data.ByteString.Lazy as L (ByteString, fromStrict, toStrict)
import qualified Data.ByteString.Lazy.Char8 as L (pack)
import qualified Data.Aeson as DA (encode)
import Wf.Data.Serializable (serialize)

import qualified Network.Wai as Wai (Request, defaultRequest, requestHeaders)
import Web.Cookie (SetCookie, renderCookies)
import Blaze.ByteString.Builder (toByteString)

import Wf.Application.Logger (Logger, logDebug)
import Wf.Control.Eff.Run.Kvs.Map (runKvsMap)
import Wf.Application.Exception (Exception(..))
import Wf.Web.Session (Session(..), sget, sput, sttl, sdestroy, getSessionId, runSession, SessionKvs(..), SessionError(..), SessionState(..), SessionData(..), defaultSessionState, defaultSessionData)
import Wf.Application.Time (Time, getCurrentTime, addSeconds)

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
                    sttl 10
                    getSessionId
        Right (s, sid) <- runTest "SID" False M.empty Wai.defaultRequest code
        shouldSatisfy s (M.member sid)

    it "restore automatically" $ do
        let name = "SID"
        let sid = "testSessionId000"
        let cookies = [(name, sid)]
        let headers = [("Cookie", toByteString . renderCookies $ cookies)]
        let request = Wai.defaultRequest { Wai.requestHeaders = headers }
        let key = "hello"
        let val = ("world", 1, [3]) :: (String, Integer, [Integer])
        let sval = HM.fromList [(key, DA.encode $ [val])]
        t <- getCurrentTime
        let expireDate = addSeconds t 10
        let sd = serialize SessionData { sessionValue = sval, sessionStartDate = t, sessionExpireDate = expireDate }
        let sessionState = M.fromList [(sid, sd)]
        let code = do
                    v <- sget key
                    sid' <- getSessionId
                    return (v, sid')
        Right (_, result) <- runTest name False sessionState request code
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
        Right (s, (_, afterId)) <- runTest "SID" False M.empty Wai.defaultRequest code
        afterId `shouldBe` ""
        shouldSatisfy s M.null



runTest :: B.ByteString ->
           Bool ->
           M.Map B.ByteString L.ByteString ->
           Wai.Request ->
           Eff ( Session
              :> Kvs.Kvs SessionKvs
              :> State (M.Map B.ByteString L.ByteString)
              :> State SessionState
              :> Exception
              :> Reader Wai.Request
              :> Reader Time
              :> Logger
              :> Lift IO
              :> ()) a ->
           IO (Either SomeException (M.Map B.ByteString L.ByteString, a))
runTest name isSecure s request a = do
    t <- getCurrentTime
    runLift
        . runLoggerStdIO DEBUG
        . flip runReader t
        . flip runReader request
        . runExc
        . evalState defaultSessionState
        . runState s
        . runKvsMap
        . runSession name isSecure 0 $ a
