{-# LANGUAGE OverloadedStrings, TypeOperators, FlexibleContexts #-}
module Application.Run.Kvs.RedisSpec
( kvsRedisSpec
) where

import Control.Eff (Eff, VE(..), (:>), Member, SetMember, admin, handleRelay)
import Control.Eff.Lift (Lift, runLift)
import Control.Eff.Logger (Logger, LogLevel(..), runLoggerStdIO)
import qualified Control.Eff.Kvs as Kvs (Kvs(..), get, set, setWithTtl, delete)
import Control.Concurrent (threadDelay)
import qualified Database.Redis as Redis (get, set, setex, del, ConnectInfo(..), defaultConnectInfo, PortID(..), connect, runRedis, Status(..))
import qualified Data.ByteString as B (ByteString)
import qualified Data.ByteString.Char8 as B (pack)
import qualified Data.ByteString.Lazy as L (ByteString, fromStrict, toStrict)
import qualified Data.ByteString.Lazy.Char8 as L (pack)

import Application.Run.Kvs.Redis (runKvsRedis)

import Test.Hspec (Spec, describe, it, shouldBe)
import qualified Test.Hspec.QuickCheck as Q
import qualified Test.QuickCheck.Property as Q


kvsRedisSpec :: Spec
kvsRedisSpec = do
    setSpec
    setWithTtlSpec
    getSpec
    deleteSpec


testConnectInfo :: Redis.ConnectInfo
testConnectInfo = Redis.defaultConnectInfo { Redis.connectPort = Redis.PortNumber 6380 }


setSpec :: Spec
setSpec = do
    Q.prop "kvs-redis set" $
        \val -> Q.ioProperty $ do
            let key = "key1"
            let code = Kvs.set key (L.pack val)
            r <- runTest code
            con <- Redis.connect testConnectInfo
            x <- Redis.runRedis con $ Redis.get key
            let message = "expected: " ++ show val ++ " result: " ++ show x ++ " return: " ++ show r
            return $ if r && ((Right . Just . B.pack $ val) == x)
                        then Q.succeeded
                        else Q.failed { Q.reason = message}


setWithTtlSpec :: Spec
setWithTtlSpec = describe "kvs-redis setWithTtl" $
    it "should store value within a time to live" $ do
        let key = "key2"
        let val = "hello world"
        let ttl = 1
        let code = Kvs.setWithTtl key (L.pack val) ttl
        r <- runTest code
        r `shouldBe` True
        con <- Redis.connect testConnectInfo
        x <- Redis.runRedis con $ Redis.get key
        x `shouldBe` (Right . Just . B.pack $ val)
        threadDelay 1000100
        y <- Redis.runRedis con $ Redis.get key
        y `shouldBe` Right Nothing


getSpec :: Spec
getSpec = do
    Q.prop "kvs-redis get" $
        \val -> Q.ioProperty $ do
            let key = "key3"
            con <- Redis.connect testConnectInfo
            Redis.runRedis con $ Redis.set key (B.pack val)
            let code = Kvs.get key
            x <- runTest code
            let message = "expected: " ++ show val ++ " result: " ++ show x
            let result = if (Just . L.pack $ val) == x
                            then Q.succeeded
                            else Q.failed { Q.reason = message }
            return result

    Q.prop "kvs-redis get nothing" $
        Q.ioProperty $ do
            let key = "key4" :: B.ByteString
            con <- Redis.connect testConnectInfo
            Redis.runRedis con $ Redis.del [key]
            let code = Kvs.get key
            x <- runTest code
            let message = "expected: Nothing result: " ++ show x
            let result = if Nothing == (x :: Maybe L.ByteString)
                            then Q.succeeded
                            else Q.failed { Q.reason = message }
            return result


deleteSpec :: Spec
deleteSpec = describe "kvs-redis delete" $
    it "should delete" $ do
        let key = "key5"
        let val = "hoge"
        con <- Redis.connect testConnectInfo
        Redis.runRedis con $ Redis.set key (B.pack val)
        let code = Kvs.delete key
        r <- runTest code
        r `shouldBe` True
        x <- Redis.runRedis con $ Redis.get key
        x `shouldBe` Right Nothing


runTest :: Eff (Kvs.Kvs B.ByteString :> Logger String :> Lift IO :> ()) a -> IO a
runTest = runLift . runLoggerStdIO DEBUG . runKvsRedis testConnectInfo

