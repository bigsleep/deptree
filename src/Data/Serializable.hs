module Data.Serializable
( Serializable
, serialize
, deserialize
) where

import qualified Data.ByteString as B (ByteString)

class Serializable a where
    serialize :: a -> B.ByteString
    deserialize :: B.ByteString -> Maybe a
