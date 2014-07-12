{-# LANGUAGE TypeOperators, DeriveDataTypeable, FlexibleContexts, RankNTypes, ExistentialQuantification #-}
module Wf.Web.Api
( apiRoutes
, ApiDefinition(..)
, ApiT(..)
, ApiInfo(..)
, getParameter
) where

import qualified Data.List as L (lookup)
import qualified Data.ByteString as B (ByteString)
import Data.Typeable (Typeable)
import Data.Reflection (Given, give, given)
import qualified Wf.Web.Routing as R (RouteDefinition, Parameter, route, routes)
import qualified Network.HTTP.Types as HTTP (Method)

data ApiDefinition request response i o m = ApiDefinition
    { apiName :: String
    , apiRouteDefinition :: R.RouteDefinition
    , apiRequestParser :: request -> m i
    , apiResponseRenderer :: o -> m response
    , apiImplement :: (Given ApiInfo) => i -> m o
    , apiBefore :: m ()
    , apiAfter :: m ()
    } deriving (Typeable)

data ApiInfo = ApiInfo
    { apiInfoApiName :: String
    , apiInfoRouteDefinition :: R.RouteDefinition
    , apiInfoParameters :: [R.Parameter]
    } deriving (Typeable)

newtype ApiT request response m = ApiT { unApiT :: forall i o. ApiDefinition request response i o m }

apiRoutes :: (Monad m) => m response -> [ApiT request response m] -> request -> HTTP.Method -> B.ByteString -> m response
apiRoutes defaultApp apis request = R.routes defaultApp (map entry apis)
    where
    entry (ApiT api) = R.route (apiRouteDefinition api) (exec api)
    exec api parameters = do
        apiBefore api
        let apiInfo = ApiInfo { apiInfoApiName = apiName api
                              , apiInfoRouteDefinition = apiRouteDefinition api
                              , apiInfoParameters = parameters
                              }
        i <- apiRequestParser api request
        o <- give apiInfo $ apiImplement api i
        r <- apiResponseRenderer api o
        apiAfter api
        return r

getParameter :: Given ApiInfo => B.ByteString -> Maybe B.ByteString
getParameter name = L.lookup name $ apiInfoParameters given
