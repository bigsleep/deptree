{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, TypeFamilies #-}
module Wf.Network.Http.Response.Lbs
( Response
, putResponse
, setStatus
, addHeader
, setHeaders
, setBody
, redirect
) where

import Control.Eff (Eff, Member)
import Data.Typeable (Typeable)
import qualified Data.ByteString as B (ByteString)
import qualified Data.ByteString.Lazy as L (ByteString)
import qualified Wf.Network.Http.Types as Internal (Response, ResponseStatus, ResponseHeader)
import qualified Wf.Control.Eff.HttpResponse as Internal

data ResponseBodyLbs = ResponseBodyLbs deriving (Show, Typeable)

type instance Internal.HttpResponseBodyType ResponseBodyLbs = L.ByteString

type Response = Internal.HttpResponse ResponseBodyLbs

putResponse :: Member (Internal.HttpResponse ResponseBodyLbs) r => Internal.Response L.ByteString -> Eff r ()
putResponse = Internal.putResponse ResponseBodyLbs

setStatus :: Member (Internal.HttpResponse ResponseBodyLbs) r => Internal.ResponseStatus -> Eff r ()
setStatus = Internal.setStatus ResponseBodyLbs

addHeader :: Member (Internal.HttpResponse ResponseBodyLbs) r => Internal.ResponseHeader -> Eff r ()
addHeader = Internal.addHeader ResponseBodyLbs

setHeaders :: Member (Internal.HttpResponse ResponseBodyLbs) r => [Internal.ResponseHeader] -> Eff r ()
setHeaders = Internal.setHeaders ResponseBodyLbs

setBody :: Member (Internal.HttpResponse ResponseBodyLbs) r => L.ByteString -> Eff r ()
setBody = Internal.setBody ResponseBodyLbs

redirect :: Member (Internal.HttpResponse ResponseBodyLbs) r => B.ByteString -> Eff r ()
redirect = Internal.redirect ResponseBodyLbs
