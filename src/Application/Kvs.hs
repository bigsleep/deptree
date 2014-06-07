{-# LANGUAGE DeriveDataTypeable, TypeFamilies, FlexibleContexts #-}
module Application.Kvs
( Kvs
, DefaultKvs(..)
, KvsError(..)
, get
, set
, setWithTtl
, delete
, exists
, ttl
) where

import Control.Eff (Eff, Member)
import qualified Control.Eff.Kvs (get, set, setWithTtl, delete, exists, ttl, Kvs, KeyType)

import Control.Exception (Exception)

import Data.Typeable (Typeable)
import qualified Data.ByteString as B (ByteString)
import Data.Serializable (Serializable)

data DefaultKvs = DefaultKvs deriving (Show, Typeable)

type instance Control.Eff.Kvs.KeyType DefaultKvs = B.ByteString

type Kvs = Control.Eff.Kvs.Kvs DefaultKvs

data KvsError =
    KvsConnectionError String |
    KvsError String
    deriving (Show, Typeable)

instance Exception KvsError

get :: (Typeable v, Serializable v, Member Kvs r) => B.ByteString -> Eff r (Maybe v)
get = Control.Eff.Kvs.get DefaultKvs

set :: (Typeable v, Serializable v, Member Kvs r) => B.ByteString -> v -> Eff r ()
set = Control.Eff.Kvs.set DefaultKvs

setWithTtl :: (Typeable v, Serializable v, Member Kvs r) => B.ByteString -> v -> Integer -> Eff r ()
setWithTtl = Control.Eff.Kvs.setWithTtl DefaultKvs

delete :: (Member Kvs r) => B.ByteString -> Eff r Bool
delete = Control.Eff.Kvs.delete DefaultKvs

exists :: (Member Kvs r) => B.ByteString -> Eff r Bool
exists = Control.Eff.Kvs.exists DefaultKvs

ttl :: (Member Kvs r) => B.ByteString -> Eff r (Maybe Integer)
ttl = Control.Eff.Kvs.ttl DefaultKvs
