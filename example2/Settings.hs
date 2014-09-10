{-# LANGUAGE TemplateHaskell #-}
module Settings
( Settings(..)
) where

import qualified Data.Aeson.TH as DA (deriveJSON, defaultOptions)

data Settings = Settings
    { settingsPort :: Int
    , settingsIntervalMinutes :: Int
    } deriving (Show)

DA.deriveJSON DA.defaultOptions ''Settings

