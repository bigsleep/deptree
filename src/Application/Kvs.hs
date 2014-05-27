{-# LANGUAGE DeriveDataTypeable, TypeFamilies, FlexibleContexts #-}
module Application.Kvs
( Kvs
, DefaultKvs(..)
, get
, set
, setWithTtl
, delete
) where

import Control.Eff (Eff, Member)
import qualified Control.Eff.Kvs (get, set, setWithTtl, delete, Kvs, KeyType)
import Data.Typeable (Typeable)
import qualified Data.ByteString as B (ByteString)
import Data.Serializable (Serializable)

data DefaultKvs = DefaultKvs deriving (Show, Typeable)

type instance Control.Eff.Kvs.KeyType DefaultKvs = B.ByteString

type Kvs = Control.Eff.Kvs.Kvs DefaultKvs

get :: (Typeable v, Serializable v, Member Kvs r) => B.ByteString -> Eff r (Maybe v)
get = Control.Eff.Kvs.get DefaultKvs

set :: (Typeable v, Serializable v, Member Kvs r) => B.ByteString -> v -> Eff r Bool
set = Control.Eff.Kvs.set DefaultKvs

setWithTtl :: (Typeable v, Serializable v, Member Kvs r) => B.ByteString -> v -> Integer -> Eff r Bool
setWithTtl = Control.Eff.Kvs.setWithTtl DefaultKvs

delete :: (Member Kvs r) => B.ByteString -> Eff r Bool
delete = Control.Eff.Kvs.delete DefaultKvs
