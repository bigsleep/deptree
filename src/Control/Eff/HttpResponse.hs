{-# LANGUAGE FlexibleContexts, DeriveFunctor, DeriveDataTypeable #-}
module Control.Eff.HttpResponse
( putResponse
, HttpResponse(..)
) where

import Control.Eff (Eff, Member, send, inj)
import Data.Typeable (Typeable)

data HttpResponse response a =
    HttpResponse response
    deriving (Functor, Typeable)

putResponse :: (Typeable response, Member (HttpResponse response) r)
            => response -> Eff r ()
putResponse response = send $ \_ -> inj $ HttpResponse response


