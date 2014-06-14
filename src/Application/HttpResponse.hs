{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Application.HttpResponse
( putResponse
, redirect
) where

import Control.Eff (Eff, Member)
import qualified Control.Eff.HttpResponse as R (HttpResponse, putResponse)

import qualified Network.Wai as Wai (Response, responseLBS)
import qualified Network.HTTP.Types as HTTP (status302, Query, renderQueryBuilder)

import qualified Data.ByteString as B (ByteString)
import qualified Blaze.ByteString.Builder as Blaze (toByteString, fromByteString)
import Data.Monoid ((<>))

putResponse :: (Member (R.HttpResponse Wai.Response) r)
            => Wai.Response -> Eff r ()
putResponse = R.putResponse

redirect :: B.ByteString -> HTTP.Query -> Wai.Response
redirect uri params =
    let builder = Blaze.fromByteString uri <> HTTP.renderQueryBuilder True params
        headers = [("Location", Blaze.toByteString builder)]
    in Wai.responseLBS HTTP.status302 headers ""
