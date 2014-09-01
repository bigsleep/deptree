{-# LANGUAGE OverloadedStrings #-}
module Settings
( Settings(..)
) where

import Control.Monad (mzero)
import qualified Data.Aeson as DA (Value(..), FromJSON(..), (.:))

import Wf.Web.Authenticate.OAuth2 (OAuth2Config)
import Wf.Web.Session (SessionSettings)

data Settings = Settings
    { settingsPort :: Int
    , settingsOAuth2 :: OAuth2Config
    , settingsSession :: SessionSettings
    } deriving (Show)

instance DA.FromJSON Settings where
    parseJSON (DA.Object o) = do
        port <- o DA..: "port"
        oauth2 <- o DA..: "oauth2"
        session <- o DA..: "session"
        return (Settings port oauth2 session)

    parseJSON _ = mzero
