{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, TypeFamilies, TemplateHaskell, StandaloneDeriving #-}
module Application.Session
( SessionData(..)
, SessionKvs(..)
, SessionError(..)
, defaultSessionData
) where

import qualified Control.Exception (Exception(..))
import qualified Control.Eff.Kvs as Kvs (KeyType)

import Data.Typeable (Typeable)
import Data.HashMap.Strict (HashMap, empty)
import Data.Word (Word32)
import qualified Data.ByteString as B (ByteString)
import Data.Serializable (Serializable(..))
import qualified Data.Aeson as DA (encode, decode)
import qualified Data.Aeson.TH as DA (deriveJSON, defaultOptions)

data SessionData = SessionData
    { sessionId :: B.ByteString
    , sessionValue :: HashMap B.ByteString B.ByteString
    , sessionHostAddress :: Word32
    } deriving (Show, Eq, Typeable)

defaultSessionData :: SessionData
defaultSessionData = SessionData "" empty 0

DA.deriveJSON DA.defaultOptions ''SessionData

data SessionKvs = SessionKvs deriving (Typeable)

type instance Kvs.KeyType SessionKvs = B.ByteString

data SessionError =
    SessionError String
    deriving (Show, Eq, Typeable)

instance Control.Exception.Exception SessionError

instance Serializable SessionData where
    serialize = DA.encode
    deserialize = DA.decode
