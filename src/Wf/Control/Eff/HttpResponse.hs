{-# LANGUAGE FlexibleContexts, DeriveFunctor, DeriveDataTypeable, TypeFamilies #-}
module Wf.Control.Eff.HttpResponse
( setStatus
, addHeader
, setHeader
, setBody
, redirect
, HttpResponse(..)
, ResponseBody(..)
, Response(..)
) where

import Control.Eff (Eff, Member, send, inj)
import Data.Typeable (Typeable)
import qualified Data.ByteString as B (ByteString)
import qualified Blaze.ByteString.Builder as Blaze (Builder)
import qualified Network.HTTP.Types as HTTP (Header, Status)
import qualified Network.Wai as Wai (StreamingBody)

type ResponseStatus = HTTP.Status

type ResponseHeader = HTTP.Header

data ResponseBody =
    ResponseBodyBuilder Blaze.Builder |
    ResponseBodyFile FilePath |
    ResponseBodyStream Wai.StreamingBody

data Response = Response
    { responseStatus :: ResponseStatus
    , responseHeaders :: [ResponseHeader]
    , responseBody :: ResponseBody
    } deriving (Typeable)

data HttpResponse a =
    HttpResponseSetStatus ResponseStatus a |
    HttpResponseAddHeader ResponseHeader a |
    HttpResponseSetHeaders [ResponseHeader] a |
    HttpResponseSetBody ResponseBody a |
    HttpResponseRedirect B.ByteString a
    deriving (Functor, Typeable)

setStatus :: (Member HttpResponse r) => ResponseStatus -> Eff r ()
setStatus s = send $ \f -> inj $ HttpResponseSetStatus s $ f ()

addHeader :: (Member HttpResponse r) => ResponseHeader -> Eff r ()
addHeader h = send $ \f -> inj $ HttpResponseAddHeader h $ f ()

setHeader :: (Member HttpResponse r) => [ResponseHeader] -> Eff r ()
setHeader hs = send $ \f -> inj $ HttpResponseSetHeaders hs $ f ()

setBody :: (Member HttpResponse r) => ResponseBody -> Eff r ()
setBody body = send $ \f -> inj $ HttpResponseSetBody body $ f ()

redirect :: (Member HttpResponse r) => B.ByteString -> Eff r ()
redirect uri = send $ \f -> inj $ HttpResponseRedirect uri $ f ()
