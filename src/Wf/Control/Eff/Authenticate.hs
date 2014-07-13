{-# LANGUAGE FlexibleContexts, DeriveDataTypeable, DeriveFunctor, TypeFamilies #-}
module Wf.Control.Eff.Authenticate
( Authenticate(..)
, AuthenticationType(..)
, authenticate
, authenticationTransfer
) where

import Control.Eff (Eff, Member, send, inj)
import Data.Typeable (Typeable)
import Wf.Network.Http.Response (Response)

class AuthenticationType auth where
    type AuthenticationUserType auth :: *
    type AuthenticationKeyType auth :: *
    type AuthenticationResponseBodyType auth :: *

data Authenticate auth a =
    Authenticate auth (AuthenticationKeyType auth) (AuthenticationUserType auth -> a) |
    AuthenticationTransfer auth (Response (AuthenticationResponseBodyType auth)) (Response (AuthenticationResponseBodyType auth) -> a)
    deriving (Typeable, Functor)

authenticate :: (Typeable auth, Member (Authenticate auth) r)
             => auth -> AuthenticationKeyType auth -> Eff r (AuthenticationUserType auth)
authenticate auth key = send $ inj . Authenticate auth key

authenticationTransfer :: (Typeable auth, Member (Authenticate auth) r)
             => auth -> Response (AuthenticationResponseBodyType auth) -> Eff r (Response (AuthenticationResponseBodyType auth))
authenticationTransfer auth response = send $ inj . AuthenticationTransfer auth response
