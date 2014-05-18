{-# LANGUAGE TypeOperators, OverloadedStrings, FlexibleContexts, QuasiQuotes #-}
module Application.Run.Session
( runSession
, genSessionId
) where

import Control.Eff (Eff, VE(..), (:>), Member, SetMember, admin, handleRelay)
import Control.Eff.Lift (Lift, lift)
import Control.Eff.Exception (Exc, throwExc)
import Control.Eff.Session (Session(..))
import Control.Eff.Logger (Logger, logDebug, logInfo, logError)
import qualified Control.Eff.Kvs as Kvs (Kvs, get, setWithTtl, delete)
import Data.Serializable (serialize)
import qualified Data.ByteString as B (ByteString, append)
import qualified Data.ByteString.Char8 as B (pack)
import System.Random (getStdGen, randomRs)
import Text.Printf.TH (s)


runSession :: (Member (Exc String) r, Member (Logger String) r, Member (Kvs.Kvs B.ByteString) r, Member (Lift IO) r, SetMember Lift (Lift IO) r)
           => Integer -> Eff (Session :> r) a -> Eff r a
runSession ttl = loop . admin
    where loop (Val a) = return a

          loop (E u) = handleRelay u loop handle

          handle (StartSession v c) = do
            randomStr <- lift $ genSessionId 40
            let ssid = "SSID" `B.append` randomStr
            r <- Kvs.setWithTtl ssid v ttl
            if r then do
                    logDebug $ [s|startSession success. ssid=%s info=%s|] ssid (serialize v)
                    loop (c ssid)
                 else do
                    logError $ [s|startSession failure. ssid=%s info=%s|] ssid (serialize v)
                    throwExc ("error" :: String)

          handle (ReadSession k c) = do
            r <- Kvs.get k
            case r of
                 Just v -> do
                    logDebug $ [s|readSession success. ssid=%s info=%s|] k (serialize v)
                    loop (c v)
                 Nothing -> do
                    logInfo $ [s|readSession failure. ssid=%s|] k
                    throwExc ("error" :: String)

          handle (DeleteSession k c) = do
            r <- Kvs.delete k
            if r then do
                    logInfo $ [s|deleteSession success. ssid=%s|] k
                    loop c
                 else do
                    logError $ [s|deleteSession failure. ssid=%s|] k
                    throwExc ("error" :: String)


genSessionId :: Int -> IO B.ByteString
genSessionId len = return . B.pack . take len . map (chars !!) . randomRs (0, length chars - 1) =<< getStdGen
    where chars = ['0' .. '9'] ++ ['a' .. 'z'] ++ ['A' .. 'Z']

