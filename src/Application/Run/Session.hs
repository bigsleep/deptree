{-# LANGUAGE TypeOperators, OverloadedStrings, FlexibleContexts, QuasiQuotes, DeriveDataTypeable, StandaloneDeriving, QuasiQuotes, TypeFamilies #-}
module Application.Run.Session
( runSession
) where

import Control.Eff (Eff, VE(..), (:>), Member, SetMember, admin, handleRelay)
import Control.Eff.Reader.Strict (Reader, ask)
import qualified Control.Eff.State.Strict as State (State, get, put, modify)
import Control.Eff.Lift (Lift, lift)
import Control.Eff.Session (Session(..))
import qualified Control.Eff.Kvs as Kvs (Kvs, get, setWithTtl, delete, exists, ttl)

import Control.Monad (when)

import Data.Typeable (Typeable)
import qualified Data.ByteString as B (ByteString, append)
import qualified Data.ByteString.Char8 as B (pack)
import qualified Data.UnixTime as T (UnixTime, formatUnixTime)
import qualified Data.List as L (lookup)
import qualified Data.HashMap.Strict as HM (lookup, insert)
import Data.Maybe (fromMaybe)

import qualified Network.Wai as Wai (Request, requestHeaders)
import qualified Web.Cookie as Cookie (parseCookies)

import System.Random (getStdGen, randomRs)
import Text.Printf.TH (s)

import Application.Session (SessionData(..), SessionKvs(..), defaultSessionData)
import Application.Exception (Exception)
import Application.Logger (Logger, logDebug, logInfo, logError)

deriving instance Typeable T.UnixTime

runSession :: ( Member Exception r
              , Member Logger r
              , Member (Kvs.Kvs SessionKvs) r
              , Member (Reader T.UnixTime) r
              , Member (Reader Wai.Request) r
              , Member (State.State SessionData) r
              , SetMember Lift (Lift IO) r
              )
           => B.ByteString -> Integer -> Eff (Session :> r) a -> Eff r a
runSession sessionName ttl eff = do
    loadSession
    r <- loop . admin $ eff
    saveSession
    return r
    where loop (Val a) = return a

          loop (E u) = handleRelay u loop handle

          loadSession = do
            request <- ask
            sid <- return . getRequestSessionId sessionName $ request
            sd <- maybe (return Nothing) (Kvs.get SessionKvs) sid
            maybe (return ()) State.put (sd :: Maybe SessionData)
            logDebug $ [s|load session. session=%s|] (show sd)

          saveSession = do
            sd <- State.get
            let sid = sessionId sd
            when (sid /= "") $ do
                currentTtl <- fmap (fromMaybe ttl) $ Kvs.ttl SessionKvs sid
                Kvs.setWithTtl SessionKvs sid sd currentTtl
                logDebug $ [s|save session. session=%s|] (show sd)

          newSession = do
            t <- ask
            let len = 100
            sid <- lift $ genSessionId t len
            duplicate <- Kvs.exists SessionKvs sid
            if duplicate
                then newSession
                else do
                     State.put (defaultSessionData { sessionId = sid })
                     logInfo $ [s|new session. sessionId=%s|] sid

          handle (SessionGet k c) =
            loop . c . HM.lookup k . sessionValue =<< State.get

          handle (SessionPut k v c) = do
            sd <- State.get
            when (sessionId sd == "") newSession
            State.modify (\x -> x { sessionValue = HM.insert k v (sessionValue x) })
            loop c

          handle (SessionDestroy c) = do
            sid <- fmap sessionId State.get
            when (sid /= "") (Kvs.delete SessionKvs sid >> return ())
            State.put defaultSessionData
            loop c

          handle (GetSessionId c) =
            loop . c . sessionId =<< State.get


getRequestSessionId :: B.ByteString -> Wai.Request -> Maybe B.ByteString
getRequestSessionId name = (L.lookup name =<<) . fmap Cookie.parseCookies . L.lookup "Cookie" . Wai.requestHeaders

genRandomByteString :: Int -> IO B.ByteString
genRandomByteString len = return . B.pack . take len . map (chars !!) . randomRs (0, length chars - 1) =<< getStdGen
    where chars = ['0' .. '9'] ++ ['a' .. 'z'] ++ ['A' .. 'Z']

genSessionId :: T.UnixTime -> Int -> IO B.ByteString
genSessionId t len = do
    dateStr <- T.formatUnixTime ":%Y%m%d:" t
    randomStr <- genRandomByteString len
    return $ "SID" `B.append` dateStr `B.append` randomStr

