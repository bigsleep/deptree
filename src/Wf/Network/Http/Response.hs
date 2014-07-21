{-# LANGUAGE OverloadedStrings #-}
module Wf.Network.Http.Response
( Response
, setStatus
, addHeader
, setHeaders
, setBody
, redirect
) where

import qualified Data.ByteString as B (ByteString)
import qualified Network.HTTP.Types as HTTP (status302)
import Wf.Network.Http.Types (Response(..), ResponseStatus, ResponseHeader)

setStatus :: ResponseStatus -> Response body -> Response body
setStatus s res = res { responseStatus = s }

addHeader :: ResponseHeader -> Response body -> Response body
addHeader h res = res { responseHeaders = h : responseHeaders res }

setHeaders :: [ResponseHeader] -> Response body -> Response body
setHeaders hs res = res { responseHeaders = hs }

setBody :: body -> Response a -> Response body
setBody body res = res { responseBody = body }

redirect :: B.ByteString -> Response body -> Response body
redirect url res =
    let status = HTTP.status302
        header = ("Location", url)
    in res { responseStatus = status, responseHeaders = header : responseHeaders res }

