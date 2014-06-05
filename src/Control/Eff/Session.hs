{-# LANGUAGE FlexibleContexts, DeriveDataTypeable, DeriveFunctor #-}
module Control.Eff.Session
( sget
, sput
, sdestroy
, getSessionId
, Session(..)
) where

import Control.Eff (Eff, Member, inj, send)
import Data.Typeable (Typeable)
import qualified Data.ByteString as B (ByteString)

data Session a =
    SessionGet B.ByteString (Maybe B.ByteString -> a) |
    SessionPut B.ByteString B.ByteString a |
    SessionDestroy a |
    GetSessionId (B.ByteString -> a)
    deriving (Functor, Typeable)

sget :: (Member Session r) => B.ByteString -> Eff r (Maybe B.ByteString)
sget k = send $ inj . SessionGet k

sput :: (Member Session r) => B.ByteString -> B.ByteString -> Eff r ()
sput k v = send $ \f -> inj . SessionPut k v $ f ()

sdestroy :: (Member Session r) => Eff r ()
sdestroy = send $ \f -> inj . SessionDestroy $ f ()

getSessionId :: (Member Session r) => Eff r B.ByteString
getSessionId = send $ inj . GetSessionId


