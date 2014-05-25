{-# LANGUAGE TypeOperators, FlexibleContexts, DeriveDataTypeable, ExistentialQuantification, TypeFamilies #-}
module Control.Eff.Kvs
( get
, set
, setWithTtl
, delete
, Kvs(..)
, KeyType
) where

import Control.Eff (Eff, Member, inj, send)
import Data.Serializable (Serializable)
import Data.Typeable (Typeable)

type family KeyType kvs :: *

data Kvs kvs a =
    forall v. (Typeable v, Serializable v) => Get kvs (KeyType kvs) (Maybe v -> a) |
    forall v. (Typeable v, Serializable v) => Set kvs (KeyType kvs) v (Bool -> a) |
    forall v. (Typeable v, Serializable v) => SetWithTtl kvs (KeyType kvs) v Integer (Bool -> a) |
    Delete kvs (KeyType kvs) (Bool -> a)
    deriving (Typeable)

instance Functor (Kvs kvs) where
    fmap f (Get kvs k c) = Get kvs k (f . c)
    fmap f (Set kvs k v c) = Set kvs k v (f . c)
    fmap f (SetWithTtl kvs k v ttl c) = SetWithTtl kvs k v ttl (f . c)
    fmap f (Delete kvs k c) = Delete kvs k (f . c)


get :: (Typeable kvs, Typeable v, Serializable v, Member (Kvs kvs) r) => kvs -> KeyType kvs -> Eff r (Maybe v)
get kvs k = send $ inj . Get kvs k

set :: (Typeable kvs, Typeable v, Serializable v, Member (Kvs kvs) r) => kvs -> KeyType kvs -> v -> Eff r Bool
set kvs k v = send $ inj . Set kvs k v

setWithTtl :: (Typeable kvs, Typeable v, Serializable v, Member (Kvs kvs) r) => kvs -> KeyType kvs -> v -> Integer -> Eff r Bool
setWithTtl kvs k v ttl = send $ inj . SetWithTtl kvs k v ttl

delete :: (Typeable kvs, Member (Kvs kvs) r) => kvs -> KeyType kvs -> Eff r Bool
delete kvs k = send $ inj . Delete kvs k
