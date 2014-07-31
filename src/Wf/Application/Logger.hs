{-# LANGUAGE DeriveDataTypeable, TypeFamilies, FlexibleContexts #-}
module Wf.Application.Logger
( Logger
, DefaultLogger(..)
, Wf.Control.Eff.Logger.LogLevel(..)
, log
, logDebug
, logNotice
, logInfo
, logWarn
, logError
, Wf.Control.Eff.Logger.runLoggerStdIO
) where

import Control.Eff (Eff, Member)
import qualified Wf.Control.Eff.Logger (Logger, LogOutputType, LogLevel(..), log, runLoggerStdIO)
import Data.Typeable (Typeable)
import Prelude hiding (log)

data DefaultLogger = DefaultLogger deriving (Show, Typeable)

type instance Wf.Control.Eff.Logger.LogOutputType DefaultLogger = String

type Logger = Wf.Control.Eff.Logger.Logger DefaultLogger

log :: (Member Logger r) => Wf.Control.Eff.Logger.LogLevel -> String -> Eff r ()
log = Wf.Control.Eff.Logger.log DefaultLogger

logDebug, logNotice, logInfo, logWarn, logError :: (Member Logger r) => String -> Eff r ()
logDebug = log Wf.Control.Eff.Logger.DEBUG
logNotice = log Wf.Control.Eff.Logger.NOTICE
logInfo = log Wf.Control.Eff.Logger.INFO
logWarn = log Wf.Control.Eff.Logger.WARN
logError = log Wf.Control.Eff.Logger.ERROR
