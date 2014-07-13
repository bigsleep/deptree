{-# LANGUAGE OverloadedStrings, TypeOperators, FlexibleContexts #-}
module Wf.Control.Eff.Run.HttpResponse
( runHttpResponse
) where

import Control.Eff ((:>), Eff, VE(..), Member, handleRelay, admin)
import Control.Eff.State.Strict (State, modify, get)
import Data.Typeable (Typeable)
import Wf.Network.Http.Types (Response(..))
import Wf.Control.Eff.HttpResponse (HttpResponse(..))
import qualified Network.HTTP.Types as HTTP (status302)

runHttpResponse :: (Typeable tag, Member (State (Response tag)) r) => Eff (HttpResponse tag :> r) a -> Eff r (Maybe a, Response tag)
runHttpResponse = loop . admin

    where loop (Val a) = do
            response <- get
            return (Just a, response)

          loop (E u) = handleRelay u loop handle

          handle (HttpResponsePutResponse _ res) = return (Nothing, res)

          handle (HttpResponseSetStatus tag s c) = do
            modify $ \res -> res { responseTypeTag = tag, responseStatus = s }
            loop c

          handle (HttpResponseAddHeader tag h c) = do
            modify $ \res -> res { responseTypeTag = tag, responseHeaders = h : responseHeaders res }
            loop c

          handle (HttpResponseSetHeaders tag hs c) = do
            modify $ \res -> res { responseTypeTag = tag, responseHeaders = hs }
            loop c

          handle (HttpResponseSetBody tag b c) = do
            modify $ \res -> res { responseTypeTag = tag, responseBody = b }
            loop c

          handle (HttpResponseRedirect tag url) = do
            res <- get
            let status = HTTP.status302
                header = ("Location", url)
                response = res { responseTypeTag = tag, responseStatus = status, responseHeaders = header : responseHeaders res }
            return (Nothing, response)

