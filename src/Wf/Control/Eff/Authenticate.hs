{-# LANGUAGE FlexibleContexts, DeriveDataTypeable, DeriveFunctor, TypeFamilies #-}
module Wf.Control.Eff.Authenticate
( Authenticate
, authenticate
) where

import Control.Eff (Eff, Member, send, inj)
import Data.Typeable (Typeable)

data Authenticate auth a =
    Authenticate (auth -> a)
    deriving (Typeable, Functor)

authenticate :: (Typeable auth, Member (Authenticate auth) r)
             => Eff r auth
authenticate = send $ inj . Authenticate
