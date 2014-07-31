module Main where

import Wf.Control.Eff.Run.Kvs.RedisSpec (kvsRedisSpec)
import Wf.Control.Eff.Run.SessionSpec (sessionSpec)
import Wf.Web.RoutingSpec (routingSpec)
import Wf.Web.JsonApiSpec (jsonApiSpec)
import Wf.Web.Authenticate.OAuth2Spec (oauth2Spec)
import Wf.Web.Authenticate.OAuth2GoogleSpec (oauth2GoogleSpec)

import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
    kvsRedisSpec
    sessionSpec
    routingSpec
    jsonApiSpec
    oauth2Spec
    oauth2GoogleSpec
