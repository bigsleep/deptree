{-# LANGUAGE OverloadedStrings, TemplateHaskell, StandaloneDeriving #-}
module Settings
( Settings(..)
) where

import Control.Monad (mzero)
import qualified Data.Aeson as DA (Value(..), FromJSON(..), (.:))
import qualified Database.Redis as Redis (ConnectInfo(..), PortID(PortNumber), defaultConnectInfo)

data Settings = Settings
    { settingsPort :: Int
    , settingsIntervalMinutes :: Int
    , settingsRedis :: Redis.ConnectInfo
    } deriving (Show)

deriving instance Show Redis.ConnectInfo

instance DA.FromJSON Settings where
    parseJSON (DA.Object o) = do
        port <- o DA..: "port"
        interval <- o DA..: "interval"
        r <- o DA..: "redis"
        redisHost <- r DA..: "host"
        redisPort <- r DA..: "port"
        redisDatabase <- r DA..: "database"
        let redis = Redis.defaultConnectInfo { Redis.connectHost = redisHost, Redis.connectPort = Redis.PortNumber . toEnum $ redisPort, Redis.connectDatabase = redisDatabase }
        return (Settings port interval redis)
    parseJSON _ = mzero
