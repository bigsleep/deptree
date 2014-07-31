{-# LANGUAGE OverloadedStrings, StandaloneDeriving #-}
module Settings
( Settings(..)
) where

import Control.Monad (mzero)
import qualified Data.Aeson as DA (Value(..), FromJSON(..), (.:))
import qualified Database.Redis as Redis (ConnectInfo(..), PortID(PortNumber), defaultConnectInfo)

import Wf.Web.Authenticate.OAuth2 (OAuth2Config)
import Wf.Web.Session (SessionSettings)

data Settings = Settings
    { settingsPort :: Int
    , settingsOAuth2 :: OAuth2Config
    , settingsRedis :: Redis.ConnectInfo
    , settingsSession :: SessionSettings
    } deriving (Show)

deriving instance Show Redis.ConnectInfo

instance DA.FromJSON Settings where
    parseJSON (DA.Object o) = do
        port <- o DA..: "port"
        oauth2 <- o DA..: "oauth2"
        r <- o DA..: "redis"
        redisHost <- r DA..: "host"
        redisPort <- r DA..: "port"
        redisDatabase <- r DA..: "database"
        session <- o DA..: "session"
        let redis = Redis.defaultConnectInfo { Redis.connectHost = redisHost, Redis.connectPort = Redis.PortNumber . toEnum $ redisPort, Redis.connectDatabase = redisDatabase }
        return (Settings port oauth2 redis session)

    parseJSON _ = mzero
