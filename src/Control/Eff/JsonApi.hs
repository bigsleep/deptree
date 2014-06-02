{-# LANGUAGE OverloadedStrings, TypeOperators, FlexibleContexts, DeriveDataTypeable, DeriveFunctor, ExistentialQuantification #-}
module Control.Eff.JsonApi
( JsonApi(..)
, jsonInput
, jsonOutput
, JsonRequest(..)
, jsonRequest
, JsonApiException(..)
, runJsonApi
) where

import Control.Eff ((:>), Eff, VE(..), Member, send, inj, admin, handleRelay)
import Control.Eff.Exception (Exc, throwExc)
import Control.Exception (Exception(..), SomeException, toException)
import Control.Eff.State.Strict (State, put)

import Data.Typeable (Typeable)
import qualified Data.Aeson as DA (FromJSON, ToJSON, Value(..), Result(..), fromJSON, toJSON)
import qualified Data.HashMap.Strict as HM (lookup)


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



data JsonRequest a =
    JsonRequest (DA.Value -> a)
    deriving (Functor, Typeable)

jsonRequest :: (Member JsonRequest r) => Eff r DA.Value
jsonRequest = send $ inj . JsonRequest



data JsonResponse =
    JsonResponse DA.Value
    deriving (Show, Eq, Typeable)



data JsonApiException =
    JsonParseError String
    deriving (Show, Typeable)

instance Exception JsonApiException



runJsonApi :: (Member JsonRequest r, Member (Exc SomeException) r, Member (State JsonResponse) r)
           => Eff (JsonApi :> r) a -> Eff r a
runJsonApi = loop . admin

    where loop (Val a) = return a

          loop (E u) = handleRelay u loop handle

          handle (JsonInput c) = do
            val <- jsonRequest
            case DA.fromJSON =<< val ^? "request" of
                 DA.Success input -> loop . c $ input
                 DA.Error errMsg -> throwExc . toException $ JsonParseError errMsg

          handle (JsonOutput o c) =
            put (JsonResponse . DA.toJSON $ o) >> loop c

          (^?) (DA.Object obj) key = case HM.lookup key obj of
                                          Just a -> DA.Success a
                                          Nothing -> DA.Error "no requst field."
          (^?) _ _ = DA.Error "request is not a json object."
