{-# LANGUAGE TypeOperators, OverloadedStrings, FlexibleContexts, QuasiQuotes, DeriveDataTypeable, StandaloneDeriving, TypeFamilies #-}
module Application.Run.Session
( runSession
, SessionKvs(..)
, SessionError(..)
) where

import Control.Eff (Eff, VE(..), (:>), Member, SetMember, admin, handleRelay)
import Control.Eff.Reader.Strict (Reader, ask)
import Control.Eff.Lift (Lift, lift)
import Control.Eff.Exception (Exc, throwExc)
import Control.Eff.Session (Session(..))
import qualified Control.Eff.Kvs as Kvs (Kvs, get, setWithTtl, delete, KeyType)
import Data.Serializable (serialize)
import Data.Typeable (Typeable)
import qualified Data.ByteString as B (ByteString, append)
import qualified Data.ByteString.Char8 as B (pack)
import qualified Data.UnixTime as T (UnixTime, formatUnixTime)
import System.Random (getStdGen, randomRs)
import Text.Printf.TH (s)

import Application.Logger (Logger, logDebug, logInfo, logError)

deriving instance Typeable T.UnixTime

data SessionKvs = SessionKvs deriving (Typeable)

type instance Kvs.KeyType SessionKvs = B.ByteString

data SessionError =
    SessionExpired String |
    SessionStartFailure String |
    SessionDeleteFailure String
    deriving (Show, Read, Eq, Typeable)

runSession :: ( Member (Exc SessionError) r
              , Member Logger r
              , Member (Kvs.Kvs SessionKvs) r
              , Member (Reader T.UnixTime) r
              , SetMember Lift (Lift IO) r
              )
           => Integer -> Eff (Session :> r) a -> Eff r a
runSession ttl = loop . admin
    where loop (Val a) = return a

          loop (E u) = handleRelay u loop handle

          handle (StartSession v c) = do
            t <- ask
            dateStr <- lift $ T.formatUnixTime ":%Y%m%d:" t
            randomStr <- lift $ genRandomByteString 40
            let ssid = "SSID" `B.append` dateStr `B.append` randomStr
            r <- Kvs.setWithTtl SessionKvs ssid v ttl
            if r then do
                    logDebug $ [s|startSession success. ssid=%s info=%s|] ssid (serialize v)
                    loop (c ssid)
                 else do
                    logError $ [s|startSession failure. ssid=%s info=%s|] ssid (serialize v)
                    throwExc $ SessionStartFailure "kvs error."

          handle (ReadSession k c) = do
            r <- Kvs.get SessionKvs k
            case r of
                 Just v -> do
                    logDebug $ [s|readSession success. ssid=%s info=%s|] k (serialize v)
                    loop (c v)
                 Nothing -> do
                    logInfo $ [s|readSession failure. ssid=%s|] k
                    throwExc $ SessionExpired "session expired."

          handle (DeleteSession k c) = do
            r <- Kvs.delete SessionKvs k
            if r then do
                    logInfo $ [s|deleteSession success. ssid=%s|] k
                    loop c
                 else do
                    logError $ [s|deleteSession failure. ssid=%s|] k
                    throwExc $ SessionDeleteFailure "kvs error."


genRandomByteString :: Int -> IO B.ByteString
genRandomByteString len = return . B.pack . take len . map (chars !!) . randomRs (0, length chars - 1) =<< getStdGen
    where chars = ['0' .. '9'] ++ ['a' .. 'z'] ++ ['A' .. 'Z']

