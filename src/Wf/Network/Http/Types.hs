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
) where

import Data.Typeable (Typeable)
import qualified Data.ByteString as B (ByteString)
import qualified Network.HTTP.Types as HTTP (HttpVersion, Header, Method, Status, Query)

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
    } deriving (Show, Typeable)

type ResponseStatus = HTTP.Status

type ResponseHeader = HTTP.Header

data Response body = Response
    { responseStatus :: ResponseStatus
    , responseHeaders :: [ResponseHeader]
    , responseBody :: body
    } deriving (Show, Typeable)

