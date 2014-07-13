{-# LANGUAGE DeriveDataTypeable, TypeFamilies #-}
module Wf.Network.Http.Types
( Response(..)
, ResponseStatus
, ResponseHeader
) where

import Data.Typeable (Typeable)
import qualified Network.HTTP.Types as HTTP (Header, Status)

type ResponseStatus = HTTP.Status

type ResponseHeader = HTTP.Header

data Response body = Response
    { responseStatus :: ResponseStatus
    , responseHeaders :: [ResponseHeader]
    , responseBody :: body
    } deriving (Show, Typeable)

