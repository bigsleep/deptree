{-# LANGUAGE OverloadedStrings, TypeOperators, FlexibleContexts #-}
module Wf.Control.Eff.Run.HttpResponse
( runHttpResponse
) where

import Control.Eff ((:>), Eff, VE(..), Member, handleRelay, admin)
import Control.Eff.State.Strict (State, modify)
import Wf.Control.Eff.HttpResponse (HttpResponse(..), Response(..))
import qualified Network.HTTP.Types as HTTP (status302)

runHttpResponse :: (Member (State Response) r) => Eff (HttpResponse :> r) a -> Eff r a
runHttpResponse = loop . admin

    where loop (Val a) = return a

          loop (E u) = handleRelay u loop handle

          handle (HttpResponseSetStatus s c) = do
            modify $ \res -> res { responseStatus = s }
            loop c

          handle (HttpResponseAddHeader h c) = do
            modify $ \res -> res { responseHeaders = h : responseHeaders res }
            loop c

          handle (HttpResponseSetHeaders hs c) = do
            modify $ \res -> res { responseHeaders = hs }
            loop c

          handle (HttpResponseSetBody b c) = do
            modify $ \res -> res { responseBody = b }
            loop c

          handle (HttpResponseRedirect url c) = do
            let status = HTTP.status302
                header = ("Location", url)
            modify $ \res -> res { responseStatus = status, responseHeaders = header : responseHeaders res }
            loop c

