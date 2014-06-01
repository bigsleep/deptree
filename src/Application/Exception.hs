{-# LANGUAGE FlexibleContexts #-}
module Application.Exception
( Exception
, throwException
) where

import qualified Control.Exception (Exception, SomeException(..))
import Control.Eff (Eff, Member)
import Control.Eff.Exception (Exc, throwExc)

type Exception = Exc Control.Exception.SomeException

throwException :: (Control.Exception.Exception e, Member (Exc Control.Exception.SomeException) r) => e -> Eff r a
throwException = throwExc . Control.Exception.SomeException
