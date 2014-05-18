{-# LANGUAGE TypeOperators, FlexibleContexts, DeriveDataTypeable, ExistentialQuantification #-}
module Control.Eff.Kvs
( get
, set
, setWithTtl
, delete
, Kvs(..)
) where

import Control.Eff (Eff, Member, inj, send)
import Data.Serializable (Serializable)
import Data.Typeable (Typeable)

data Kvs k a =
    forall v. Serializable v => Get k (Maybe v -> a) |
    forall v. Serializable v => Set k v (Bool -> a) |
    forall v. Serializable v => SetWithTtl k v Integer (Bool -> a) |
    Delete k (Bool -> a)
    deriving (Typeable)

instance Functor (Kvs k) where
    fmap f (Get k c) = Get k (f . c)
    fmap f (Set k v c) = Set k v (f . c)
    fmap f (SetWithTtl k v ttl c) = SetWithTtl k v ttl (f . c)
    fmap f (Delete k c) = Delete k (f . c)


get :: (Typeable k, Typeable v, Serializable v, Member (Kvs k) r) => k -> Eff r (Maybe v)
get k = send $ inj . Get k

set :: (Typeable k, Typeable v, Serializable v, Member (Kvs k) r) => k -> v -> Eff r Bool
set k v = send $ inj . Set k v

setWithTtl :: (Typeable k, Typeable v, Serializable v, Member (Kvs k) r) => k -> v -> Integer -> Eff r Bool
setWithTtl k v ttl = send $ inj . SetWithTtl k v ttl

delete :: (Typeable k, Member (Kvs k) r) => k -> Eff r Bool
delete k = send $ inj . Delete k
