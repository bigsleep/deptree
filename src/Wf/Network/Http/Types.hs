{-# LANGUAGE DeriveDataTypeable, TypeFamilies #-}
module Wf.Network.Http.Types
( Response(..)
, ResponseStatus
, ResponseHeader
, ResponseBodyType
) where

import Data.Typeable (Typeable)
import qualified Network.HTTP.Types as HTTP (Header, Status)

type ResponseStatus = HTTP.Status

type ResponseHeader = HTTP.Header

data Response tag = Response
    { responseTypeTag :: tag
    , responseStatus :: ResponseStatus
    , responseHeaders :: [ResponseHeader]
    , responseBody :: ResponseBodyType tag
    } deriving (Typeable)

instance (Show tag) => Show (Response tag) where
    show res = "Response { tag: " ++ show (responseTypeTag res) ++
               ", status: " ++ show (responseStatus res) ++
               ", headers: " ++ show (responseHeaders res) ++
               ", body: ... }"

type family ResponseBodyType tag :: *


