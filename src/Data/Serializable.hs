module Data.Serializable
( Serializable
, serialize
, deserialize
) where

import qualified Data.ByteString.Lazy as L (ByteString)

class Serializable a where
    serialize :: a -> L.ByteString
    deserialize :: L.ByteString -> Maybe a

instance Serializable L.ByteString where
    serialize = id
    deserialize = return
