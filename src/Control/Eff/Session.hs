{-# LANGUAGE FlexibleContexts, DeriveDataTypeable, ExistentialQuantification #-}
module Control.Eff.Session
( startSession
, readSession
, deleteSession
, Session(..)
) where

import Control.Eff (Eff, Member, inj, send)
import Data.Serializable (Serializable)
import Data.Typeable (Typeable)
import qualified Data.ByteString as B (ByteString)

data Session a =
    forall v. (Typeable v, Serializable v) => StartSession v (B.ByteString -> a) |
    forall v. (Typeable v, Serializable v) => ReadSession B.ByteString (v -> a) |
    DeleteSession B.ByteString a
    deriving (Typeable)

instance Functor Session where
    fmap f (StartSession v c) = StartSession v (f . c)
    fmap f (ReadSession k c) = ReadSession k (f . c)
    fmap f (DeleteSession k c) = DeleteSession k (f c)

startSession :: (Typeable v, Serializable v, Member Session r) => v -> Eff r B.ByteString
startSession v = send $ inj . StartSession v

readSession :: (Typeable v, Serializable v, Member Session r) => B.ByteString -> Eff r v
readSession k = send $ inj . ReadSession k

deleteSession :: (Member Session r) => B.ByteString -> Eff r ()
deleteSession k = send $ \f -> inj $ DeleteSession k $ f ()
