{-# LANGUAGE TypeOperators, DeriveDataTypeable, FlexibleContexts, RankNTypes, ExistentialQuantification #-}
module Wf.Web.Api
( apiRoutes
, ApiDefinition(..)
, ApiInfo(..)
, getParameter
) where

import qualified Data.List as L (lookup)
import qualified Data.ByteString as B (ByteString)
import Data.Typeable (Typeable)
import Data.Reflection (Given, give, given)
import qualified Wf.Web.Routing as R (RouteDefinition, Parameter, route, routes)
import qualified Network.HTTP.Types as HTTP (Method)

data ApiDefinition request response m = ApiDefinition
    { apiName :: String
    , apiRouteDefinition :: R.RouteDefinition
    , apiImplement :: (Given ApiInfo) => request -> m response
    , apiBefore :: m ()
    , apiAfter :: m ()
    } deriving (Typeable)

data ApiInfo = ApiInfo
    { apiInfoApiName :: String
    , apiInfoRouteDefinition :: R.RouteDefinition
    , apiInfoParameters :: [R.Parameter]
    } deriving (Typeable)

apiRoutes :: (Monad m) => m response -> [ApiDefinition request response m] -> request -> HTTP.Method -> B.ByteString -> m response
apiRoutes defaultApp apis request = R.routes defaultApp (map entry apis)
    where
    entry api = R.route (apiRouteDefinition api) (exec api)
    exec api parameters = do
        apiBefore api
        let apiInfo = ApiInfo { apiInfoApiName = apiName api
                              , apiInfoRouteDefinition = apiRouteDefinition api
                              , apiInfoParameters = parameters
                              }
        r <- give apiInfo $ apiImplement api request
        apiAfter api
        return r

getParameter :: Given ApiInfo => B.ByteString -> Maybe B.ByteString
getParameter name = L.lookup name $ apiInfoParameters given
