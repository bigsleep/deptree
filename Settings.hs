{-# LANGUAGE OverloadedStrings #-}
module Settings
( Settings(..)
) where

import Control.Monad (mzero)
import qualified Data.Aeson as DA (Value(..), FromJSON(..), (.:))

data Settings = Settings
    { settingsPort :: Int
    , settingsUrl :: String
    , settingsIntervalMinutes :: Int
    } deriving (Show)

instance DA.FromJSON Settings where
    parseJSON (DA.Object o) = do
        port <- o DA..: "port"
        url <- o DA..: "url"
        interval <- o DA..: "interval"
        return (Settings port url interval)
    parseJSON _ = mzero
