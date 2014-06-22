{-# LANGUAGE TypeOperators, DeriveDataTypeable, RankNTypes, ExistentialQuantification #-}
module Wf.Web.Api
( apiRoutes
, ApiDefinition(..)
, ApiT(..)
) where

import qualified Data.ByteString as B (ByteString)
import Data.Typeable (Typeable)
import qualified Wf.Web.Routing as R (RouteDefinition, route, routes)
import qualified Network.HTTP.Types as HTTP (Method)

data ApiDefinition request response i o m = ApiDefinition
    { apiRouteDefinition :: R.RouteDefinition
    , apiRequestParser :: request -> m i
    , apiResponseRenderer :: o -> m response
    , apiImplement :: i -> m o
    , apiBefore :: m ()
    , apiAfter :: m ()
    } deriving (Typeable)

newtype ApiT request response m = ApiT { unApiT :: forall i o. ApiDefinition request response i o m }

apiRoutes :: (Monad m) => m response -> [ApiT request response m] -> request -> HTTP.Method -> B.ByteString -> m response
apiRoutes defaultApp apis request = R.routes defaultApp (map entry apis)
    where
    entry (ApiT api) = R.route (apiRouteDefinition api) (exec api)
    exec api = do
        apiBefore api
        i <- apiRequestParser api request
        o <- apiImplement api i
        r <- apiResponseRenderer api o
        apiAfter api
        return r
