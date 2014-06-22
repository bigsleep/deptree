module Main where

import Application.Run.Kvs.RedisSpec (kvsRedisSpec)
import Application.Run.SessionSpec (sessionSpec)
import Web.RoutingSpec (routingSpec)
import Web.Authenticate.OAuth2Spec (oauth2Spec)

import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
    kvsRedisSpec
    sessionSpec
    routingSpec
    oauth2Spec
