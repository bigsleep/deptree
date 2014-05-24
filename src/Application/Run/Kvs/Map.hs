{-# LANGUAGE TypeOperators, FlexibleContexts #-}
module Application.Run.Kvs.Map
( runKvsMap
) where

import Control.Eff (Eff, VE(..), (:>), Member, admin, handleRelay)
import Control.Eff.State.Strict (State, get, modify)
import qualified Control.Eff.Kvs as Kvs (Kvs(..))

import qualified Data.Map as M (Map, lookup, insert, delete)
import qualified Data.ByteString as B (ByteString)
import qualified Data.ByteString.Lazy as L (ByteString)
import Data.Serializable (serialize, deserialize)

runKvsMap :: (Member (State (M.Map B.ByteString L.ByteString)) r) => Eff (Kvs.Kvs B.ByteString :> r) a -> Eff r a
runKvsMap = loop . admin
    where loop (Val a) = return a
          loop (E u) = handleRelay u loop handle

          handle (Kvs.Get k c) = do
            m <- get
            let r = deserialize =<< M.lookup k m
            loop (c r)

          handle (Kvs.Set k v c) = modify (M.insert k (serialize v)) >> loop (c True)

          handle (Kvs.Delete k c) = modify f >> loop (c True)
            where f :: M.Map B.ByteString L.ByteString -> M.Map B.ByteString L.ByteString
                  f = M.delete k

          handle (Kvs.SetWithTtl k v _ c)  = handle (Kvs.Set k v c)
