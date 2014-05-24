module Main where

import Application.Run.Kvs.RedisSpec (kvsRedisSpec)

import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
    kvsRedisSpec
