{-# LANGUAGE FlexibleContexts, DeriveFunctor, DeriveDataTypeable #-}
module Wf.Control.Eff.HttpResponse
( putResponse
, setStatus
, addHeader
, setHeaders
, setBody
, redirect
, HttpResponse(..)
) where

import Control.Eff (Eff, Member, send, inj)
import Data.Typeable (Typeable)
import qualified Data.ByteString as B (ByteString)
import Wf.Network.Http.Types (Response(..), ResponseStatus, ResponseHeader, ResponseBodyType)

data HttpResponse tag a =
    HttpResponsePutResponse tag (Response tag) |
    HttpResponseSetStatus tag ResponseStatus a |
    HttpResponseAddHeader tag ResponseHeader a |
    HttpResponseSetHeaders tag [ResponseHeader] a |
    HttpResponseSetBody tag (ResponseBodyType tag) a |
    HttpResponseRedirect tag B.ByteString
    deriving (Functor, Typeable)

putResponse :: (Typeable tag, Member (HttpResponse tag) r) => tag -> Response tag -> Eff r ()
putResponse tag response = send $ const . inj $ HttpResponsePutResponse tag response

setStatus :: (Typeable tag, Member (HttpResponse tag) r) => tag -> ResponseStatus -> Eff r ()
setStatus tag s = send $ \f -> inj $ HttpResponseSetStatus tag s $ f ()

addHeader :: (Typeable tag, Member (HttpResponse tag) r) => tag -> ResponseHeader -> Eff r ()
addHeader tag h = send $ \f -> inj $ HttpResponseAddHeader tag h $ f ()

setHeaders :: (Typeable tag, Member (HttpResponse tag) r) => tag -> [ResponseHeader] -> Eff r ()
setHeaders tag hs = send $ \f -> inj $ HttpResponseSetHeaders tag hs $ f ()

setBody :: (Typeable tag, Member (HttpResponse tag) r) => tag -> ResponseBodyType tag -> Eff r ()
setBody tag body = send $ \f -> inj $ HttpResponseSetBody tag body $ f ()

redirect :: (Typeable tag, Member (HttpResponse tag) r) => tag -> B.ByteString -> Eff r ()
redirect tag uri = send $ const . inj $ HttpResponseRedirect tag uri
