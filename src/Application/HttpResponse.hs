{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Application.HttpResponse
( putResponse
, redirect
) where

import Control.Eff (Eff, Member)
import qualified Control.Eff.HttpResponse as R (HttpResponse, putResponse)
import qualified Network.Wai as Wai (Response, responseLBS)
import qualified Network.HTTP.Types as HTTP (status302)
import qualified Data.ByteString as B (ByteString)

putResponse :: (Member (R.HttpResponse Wai.Response) r)
            => Wai.Response -> Eff r ()
putResponse = R.putResponse

redirect :: (Member (R.HttpResponse Wai.Response) r)
         => B.ByteString -> Eff r ()
redirect uri = do
    let headers = [("Location", uri)]
    putResponse $ Wai.responseLBS HTTP.status302 headers ""

