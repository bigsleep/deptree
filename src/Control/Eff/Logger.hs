{-# LANGUAGE TypeOperators, FlexibleContexts, DeriveDataTypeable, DeriveFunctor #-}
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
) where

import Control.Eff ((:>), VE(..), Eff, Member, SetMember, admin, handleRelay, inj, send)
import Control.Eff.Lift (Lift, lift)
import Control.Monad (when)
import Data.Typeable (Typeable)
import Prelude hiding (log)

data LogLevel = DEBUG | NOTICE | INFO | WARN | ERROR deriving (Show, Read, Eq, Enum, Ord, Typeable)

data Logger s a = Logger LogLevel s a deriving (Typeable, Functor)

log :: (Typeable s, Member (Logger s) r) => LogLevel -> s -> Eff r ()
log l s = send $ \f -> inj $ Logger l s $ f ()

logDebug, logNotice, logInfo, logWarn, logError :: (Member (Logger String) r) => String -> Eff r ()
logDebug = log DEBUG
logNotice = log NOTICE
logInfo = log INFO
logWarn = log WARN
logError = log ERROR

runLoggerStdIO :: (Show s, Typeable s, Member (Lift IO) r, SetMember Lift (Lift IO) r)
               => LogLevel -> Eff (Logger s :> r) a -> Eff r a
runLoggerStdIO minL = loop minL . admin
    where loop _ (Val a) = return a
          loop mlv (E u) = handleRelay u (loop mlv) $
                            \(Logger lv s f) -> when (lv >= mlv) (lift $ p lv s) >> loop mlv f
          p lv s = putStrLn $ "[" ++ show lv ++ "] " ++ show s
