{-# LANGUAGE TypeOperators #-}
module Wf.Control.Eff.Run.HttpResponse
( runHttpResponse
) where

import Control.Eff ((:>), Eff, VE(..), handleRelay, admin)
import Wf.Control.Eff.HttpResponse (HttpResponse(..))
import Data.Typeable (Typeable)

runHttpResponse :: (Typeable response)
                => response -> Eff (HttpResponse response :> r) a -> Eff r response
runHttpResponse def = loop . admin

    where loop (Val _) = return def

          loop (E u) = handleRelay u loop handle

          handle (HttpResponse response) = return response

