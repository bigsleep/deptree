{-# LANGUAGE FlexibleContexts #-}
module Root
( rootApp
) where

import Control.Eff (Eff, SetMember)
import Control.Eff.Lift (Lift, lift)
import Control.Concurrent.STM (TVar, readTVar, atomically)
import qualified Data.ByteString.Lazy as L (ByteString)
import qualified Network.Wai as Wai (Request)
import Wf.Network.Http.Response (Response, defaultResponse, html)

rootApp
    :: SetMember Lift (Lift IO) r
    => TVar L.ByteString
    -> Wai.Request
    -> Eff r (Response L.ByteString)
rootApp tv _ = do
    body <- lift . atomically . readTVar $ tv
    return . html body $ defaultResponse ()
