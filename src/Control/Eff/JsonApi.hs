{-# LANGUAGE FlexibleContexts, DeriveDataTypeable, ExistentialQuantification #-}
module Control.Eff.JsonApi
( JsonApi(..)
, jsonInput
, jsonOutput
) where

import Control.Eff (Eff, Member, send, inj)

import Data.Typeable (Typeable)
import qualified Data.Aeson as DA (FromJSON, ToJSON)

data JsonApi a =
    forall i. DA.FromJSON i => JsonInput (i -> a) |
    forall o. DA.ToJSON o => JsonOutput o a
    deriving (Typeable)

instance Functor JsonApi where
    fmap f (JsonInput g) = JsonInput (f . g)
    fmap f (JsonOutput o a) = JsonOutput o (f a)

jsonInput :: (DA.FromJSON i, Member JsonApi r) => Eff r i
jsonInput = send $ inj . JsonInput

jsonOutput :: (DA.ToJSON o, Member JsonApi r) => o -> Eff r ()
jsonOutput o = send $ \f -> inj . JsonOutput o $ f ()
