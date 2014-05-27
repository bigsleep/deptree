{-# LANGUAGE DeriveDataTypeable, TypeFamilies, FlexibleContexts #-}
module Application.Logger
( Logger
, DefaultLogger(..)
, log
, logDebug
, logNotice
, logInfo
, logWarn
, logError
) where

import Control.Eff (Eff, Member)
import qualified Control.Eff.Logger (Logger, LogOutputType, LogLevel(..), log)
import Data.Typeable (Typeable)
import Prelude hiding (log)

data DefaultLogger = DefaultLogger deriving (Show, Typeable)

type instance Control.Eff.Logger.LogOutputType DefaultLogger = String

type Logger = Control.Eff.Logger.Logger DefaultLogger

log :: (Member Logger r) => Control.Eff.Logger.LogLevel -> String -> Eff r ()
log = Control.Eff.Logger.log DefaultLogger

logDebug, logNotice, logInfo, logWarn, logError :: (Member Logger r) => String -> Eff r ()
logDebug = log Control.Eff.Logger.DEBUG
logNotice = log Control.Eff.Logger.NOTICE
logInfo = log Control.Eff.Logger.INFO
logWarn = log Control.Eff.Logger.WARN
logError = log Control.Eff.Logger.ERROR
