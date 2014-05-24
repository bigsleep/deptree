{-# LANGUAGE TypeOperators, FlexibleContexts, QuasiQuotes #-}
module Application.Run.Kvs.Redis
( runKvsRedis
) where

import Control.Eff (Eff, VE(..), (:>), Member, SetMember, admin, handleRelay)
import Control.Eff.Lift (Lift, lift)
import qualified Control.Eff.Kvs as Kvs (Kvs(..))
import Control.Eff.Logger (Logger, logDebug, logError)
import qualified Database.Redis as Redis (get, set, setex, del, ConnectInfo, connect, runRedis, Status(..))
import qualified Data.ByteString as B (ByteString)
import qualified Data.ByteString.Lazy as L (toStrict, fromStrict)
import Data.Serializable (serialize, deserialize)
import Text.Printf.TH (s)


runKvsRedis :: (Member (Lift IO) r, SetMember Lift (Lift IO) r, Member (Logger String) r)
            => Redis.ConnectInfo -> Eff (Kvs.Kvs B.ByteString :> r) a -> Eff r a
runKvsRedis cinfo eff = do
    cn <- lift (Redis.connect cinfo)
    loop cn . admin $ eff

    where loop _ (Val a) = return a

          loop cn (E u) = handleRelay u (loop cn) (handle cn)

          handleRedis cn redis handleResult' = (lift . Redis.runRedis cn $ redis) >>= handleResult'

          handle cn (Kvs.Get k c) = handleRedis cn (Redis.get k) handleResult
            where handleResult (Right x) = loop cn . c $ (x >>= deserialize . L.fromStrict)
                  handleResult (Left x) = logError ("redis get failure. reply=" ++ show x) >> loop cn (c Nothing)

          handle cn (Kvs.Set k v c) = handleRedis cn (Redis.set k . L.toStrict . serialize $ v) handleResult
            where handleResult (Right x) = do
                    logDebug $ [s|redis set success. key=%s value=%s status=%s|] k (serialize v) (show x)
                    loop cn . c $ (x == Redis.Ok)

                  handleResult (Left x) = do
                    logError $ [s|redis set failure. key=%s value=%s reply=%s|] k (serialize v) (show x)
                    loop cn . c $ False

          handle cn (Kvs.SetWithTtl k v ttl c) = handleRedis cn (Redis.setex k ttl . L.toStrict . serialize $ v) handleResult
            where handleResult (Right Redis.Ok) = do
                    logDebug $ [s|redis setex success. key=%s ttl=%d value=%s status=Ok|] k ttl (serialize v)
                    loop cn . c $ True

                  handleResult x = do
                    logError $ [s|redis setex failure. key=%s ttl=%d value=%s reply=%s|] k ttl (serialize v) (show x)
                    loop cn . c $ False

          handle cn (Kvs.Delete k c) = handleRedis cn (Redis.del [k]) handleResult
            where handleResult (Right x) = do
                    logDebug $ [s|redis del success. key=%s deleted=%d|] k x
                    loop cn . c $ True

                  handleResult (Left x) = do
                    logError $ [s|redis del failure. key=%s reply=%s|] k (show x)
                    loop cn . c $ False
