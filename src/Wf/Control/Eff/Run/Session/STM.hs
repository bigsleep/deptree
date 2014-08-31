{-# LANGUAGE TypeOperators, OverloadedStrings, FlexibleContexts, QuasiQuotes #-}
module Wf.Control.Eff.Run.Session.STM
( --runSessionSTM
) where

import qualified Data.ByteString as B (ByteString, append)
import qualified Data.ByteString.Char8 as B (pack)
import qualified Data.HashMap.Strict as HM (HashMap, lookup, insert, delete, empty, member)
import qualified Data.Aeson as DA (decode, encode)
import qualified Blaze.ByteString.Builder as Blaze (toByteString)
import Data.Maybe (listToMaybe)

import Control.Concurrent.STM (TVar, newTVar, readTVar, modifyTVar', atomically)
import Control.Eff (Eff, VE(..), (:>), Member, SetMember, admin, handleRelay)
import qualified Control.Eff.State.Strict as State (State, get, put, modify)
import Control.Eff.Lift (Lift, lift)
import Control.Monad (when)
import Control.Applicative ((<$>), (<*>))

import Wf.Control.Eff.Session (Session(..))
import Wf.Control.Eff.Run.Session (genSessionId)
import Wf.Network.Http.Types (RequestHeader)
import Wf.Web.Session.Types (SessionState(..), SessionData(..), SessionSettings(..), SessionKvs(..), defaultSessionState)
import Wf.Application.Exception (Exception)
import Wf.Application.Logger (Logger, logDebug, logInfo, logError)
import qualified Wf.Application.Time as T (Time, formatTime, addSeconds, diffTime)

import qualified Web.Cookie as Cookie (parseCookies, renderSetCookie, def, setCookieName, setCookieValue, setCookieExpires, setCookieSecure)
import Text.Printf.TH (s)

data SessionStore = SessionStore (TVar (HM.HashMap B.ByteString SessionData))

createSessionStore
    ::
    ( Member Logger r
    , SetMember Lift (Lift IO) r
    )
    => Eff r SessionStore
createSessionStore = lift . fmap SessionStore . atomically . newTVar $ HM.empty

runSessionSTM
    ::
    ( Member Exception r
    , Member Logger r
    , Member (State.State SessionState) r
    , SetMember Lift (Lift IO) r
    )
    => SessionStore
    -> SessionSettings
    -> T.Time
    -> Maybe B.ByteString
    -> Eff (Session :> r) a
    -> Eff r a

runSessionSTM (SessionStore tv) sessionSettings current requestSessionId eff = do
    loadSession
    r <- loop . admin $ eff
    saveSession
    return r

    where
    sname = sessionName sessionSettings

    ttl = sessionTtl sessionSettings

    isSecure = sessionIsSecure sessionSettings

    loadSession = do
        sd <- maybe (return Nothing) readSession requestSessionId
        maybe (return ()) putLoadedSession ((,) <$> requestSessionId <*> sd)
        logDebug $ [s|load session. session=%s|] (show sd)

    readSession sid = lift . atomically $ readTVar tv >>= return . HM.lookup sid

    putLoadedSession (sid, sd) = do
        if current < sessionExpireDate sd
            then State.put SessionState { sessionId = sid, sessionData = sd, isNew = False }
            else (lift . atomically . modifyTVar' tv $ (HM.delete sid)) >> State.put defaultSessionState

    saveSession = do
        sd <- State.get
        let sid = sessionId sd
        when (sid /= "") $ do
            lift . atomically $ modifyTVar' tv (HM.insert sid (sessionData $ sd))

    newSession = do
        let len = 100
        sid <- lift $ genSessionId sname current len
        duplicate <- lift . atomically . fmap (HM.member sid) $ readTVar tv
        if duplicate
            then newSession
            else do
                 let start = current
                 let end = T.addSeconds start ttl
                 let sd = SessionData HM.empty start end
                 State.put SessionState { sessionId = sid, sessionData = sd, isNew = True }
                 logInfo $ [s|new session. sessionId=%s|] sid

    loop (Val a) = return a

    loop (E u) = handleRelay u loop handle

    handle (SessionGet k c) = do
        m <- return . HM.lookup k . sessionValue . sessionData =<< State.get
        loop . c $ listToMaybe =<< DA.decode =<< m

    handle (SessionPut k v c) = do
        sd <- State.get
        when (sessionId sd == "") newSession
        State.modify f
        loop c

        where
        f ses @ (SessionState _ d @ (SessionData m _ _)  _) = ses { sessionData = d { sessionValue = HM.insert k encoded m } }
        encoded = DA.encode $ [v]

    handle (SessionTtl ttl' c) = do
        let expire = T.addSeconds current ttl'
        State.modify (f expire)
        loop c

        where
        f expire ses @ (SessionState _ d _) = ses { sessionData = d { sessionExpireDate = expire } }

    handle (SessionDestroy c) = do
        sid <- fmap sessionId State.get
        when (sid /= "") (lift . atomically $ modifyTVar' tv (HM.delete sid))
        State.put defaultSessionState
        loop c

    handle (GetSessionId c) =
        loop . c . sessionId =<< State.get

    handle (RenderSetCookie c) = do
        session <- State.get
        let sid = sessionId session
            expire = sessionExpireDate . sessionData $ session
            setCookie = Cookie.def
                      { Cookie.setCookieName = sname
                      , Cookie.setCookieValue = sid
                      , Cookie.setCookieExpires = Just expire
                      , Cookie.setCookieSecure = isSecure
                      }
        loop . c $ ("Set-Cookie", Blaze.toByteString . Cookie.renderSetCookie $ setCookie)
