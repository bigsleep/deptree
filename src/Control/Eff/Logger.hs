{-# LANGUAGE TypeOperators, FlexibleContexts, DeriveDataTypeable, DeriveFunctor, TypeFamilies #-}
module Control.Eff.Logger
( log
, logDebug
, logNotice
, logInfo
, logWarn
, logError
, runLoggerStdIO
, Logger(..)
, LogLevel(..)
, LogOutputType
) where

import Control.Eff ((:>), VE(..), Eff, Member, SetMember, admin, handleRelay, inj, send)
import Control.Eff.Lift (Lift, lift)
import Control.Monad (when)
import Data.Typeable (Typeable)
import Prelude hiding (log)

data LogLevel = DEBUG | NOTICE | INFO | WARN | ERROR deriving (Show, Read, Eq, Enum, Ord, Typeable)

type family LogOutputType logger :: *

data Logger logger a = Logger logger LogLevel (LogOutputType logger) a deriving (Typeable, Functor)

log :: (Typeable logger, Member (Logger logger) r) => logger -> LogLevel -> LogOutputType logger -> Eff r ()
log logger lv s = send $ \f -> inj $ Logger logger lv s $ f ()

logDebug, logNotice, logInfo, logWarn, logError :: (Typeable logger, Member (Logger logger) r) => logger -> LogOutputType logger -> Eff r ()
logDebug = flip log DEBUG
logNotice = flip log NOTICE
logInfo = flip log INFO
logWarn = flip log WARN
logError = flip log ERROR

runLoggerStdIO :: (Show (LogOutputType logger), Typeable logger, Member (Lift IO) r, SetMember Lift (Lift IO) r)
               => LogLevel -> Eff (Logger logger :> r) a -> Eff r a
runLoggerStdIO minL = loop minL . admin
    where loop _ (Val a) = return a
          loop mlv (E u) = handleRelay u (loop mlv) $
                            \(Logger _ lv s f) -> when (lv >= mlv) (lift $ p lv s) >> loop mlv f
          p lv s = putStrLn $ "[" ++ show lv ++ "] " ++ show s
