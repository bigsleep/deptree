{-# LANGUAGE FlexibleContexts, DeriveDataTypeable, DeriveFunctor, TypeFamilies #-}
module Wf.Control.Eff.Authenticate
( Authenticate(..)
, AuthenticationType(..)
, authenticate
, authenticationTransfer
) where

import Control.Eff (Eff, Member, send, inj)
import Data.Typeable (Typeable)

class AuthenticationType auth where
    type AuthenticationUserType auth :: *
    type AuthenticationKeyType auth :: *

data Authenticate auth a =
    Authenticate auth (AuthenticationKeyType auth) (AuthenticationUserType auth -> a) |
    AuthenticationTransfer auth a
    deriving (Typeable, Functor)

authenticate :: (Typeable auth, Member (Authenticate auth) r)
             => auth -> AuthenticationKeyType auth -> Eff r (AuthenticationUserType auth)
authenticate auth key = send $ inj . Authenticate auth key

authenticationTransfer :: (Typeable auth, Member (Authenticate auth) r)
             => auth -> Eff r ()
authenticationTransfer auth = send $ \f -> inj . AuthenticationTransfer auth $ f ()
