{-# LANGUAGE DeriveDataTypeable, TypeFamilies #-}
module Wf.Network.Http.Types
( Request(..)
, HttpVersion
, RequestMethod
, RequestHeader
, RequestQuery
, Response(..)
, ResponseStatus
, ResponseHeader
, defaultRequest
, defaultResponse
) where

import Data.Typeable (Typeable)
import qualified Data.ByteString as B (ByteString, empty)
import qualified Network.HTTP.Types as HTTP (HttpVersion, Header, Method, Status, Query, http10, methodGet, status200)

type HttpVersion = HTTP.HttpVersion

type RequestMethod = HTTP.Method

type RequestHeader = HTTP.Header

type RequestQuery = HTTP.Query

data Request body = Request
    { requestHttpVersion :: HttpVersion
    , requestMethod :: RequestMethod
    , requestHeaders :: [RequestHeader]
    , requestPath :: [B.ByteString]
    , requestRawPath :: B.ByteString
    , requestQuery :: [RequestQuery]
    , requestRawQuery :: B.ByteString
    , requestBody :: body
    , isSecure :: Bool
    } deriving (Show, Typeable, Eq)

type ResponseStatus = HTTP.Status

type ResponseHeader = HTTP.Header

data Response body = Response
    { responseStatus :: ResponseStatus
    , responseHeaders :: [ResponseHeader]
    , responseBody :: body
    } deriving (Show, Typeable, Eq)

defaultRequest :: Request ()
defaultRequest = Request
    { requestHttpVersion = HTTP.http10
    , requestMethod = HTTP.methodGet
    , requestHeaders = []
    , requestPath = []
    , requestRawPath = B.empty
    , requestQuery = []
    , requestRawQuery = B.empty
    , isSecure = False
    , requestBody = ()
    }

defaultResponse :: Response ()
defaultResponse = Response
    { responseStatus = HTTP.status200
    , responseHeaders = []
    , responseBody = ()
    }
