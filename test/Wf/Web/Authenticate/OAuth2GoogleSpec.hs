{-# LANGUAGE DeriveDataTypeable #-}
module Wf.Web.Authenticate.OAuth2GoogleSpec
( oauth2GoogleSpec
) where

import qualified Data.ByteString.Lazy as L (readFile)
import qualified Data.Aeson as DA (Value(..), encode, decode)
import Data.Typeable (Typeable)

import Wf.Web.Authenticate.OAuth2 (OAuth2Config)

import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

data User = User
    { userId :: String
    , userName :: String
    , userEmail :: String
    } deriving (Show, Typeable, Eq)

oauth2GoogleSpec :: Spec
oauth2GoogleSpec = describe "oauth2 google" $
    it "authenticate using google oauth2" $ do
        config <- fmap DA.decode $ L.readFile "test/config/oauth2google.json"
        print (config :: Maybe OAuth2Config)
        return () :: IO ()
