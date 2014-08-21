{-# LANGUAGE TypeOperators, DeriveDataTypeable, FlexibleContexts, RankNTypes, ExistentialQuantification #-}
module Wf.Web.Api
( apiRoutes
, ApiDefinition(..)
, ApiInfo(..)
, getParameter
, createApi
, getApi
, postApi
, createApiWith
, getApiWith
, postApiWith
) where

import qualified Data.List as L (lookup)
import qualified Data.ByteString as B (ByteString)
import Data.Typeable (Typeable)
import Data.Reflection (Given, give, given)
import qualified Wf.Web.Routing as R (RouteDefinition(..), RouteMethod(..), Parameter, route, routes, parseRoute)
import qualified Network.HTTP.Types as HTTP (Method, methodGet, methodPost)

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

createApi :: (Monad m) => String -> R.RouteDefinition -> (Given ApiInfo => request -> m response) -> ApiDefinition request response m
createApi name route app =
    ApiDefinition
    { apiName = name
    , apiRouteDefinition = route
    , apiImplement = app
    , apiBefore = return ()
    , apiAfter = return ()
    }

getApi, postApi :: (Monad m) => String -> (Given ApiInfo => request -> m response) -> ApiDefinition request response m
getApi route = createApi route R.RouteDefinition { R.routeDefinitionMethod = R.RouteMethodSpecific HTTP.methodGet, R.routeDefinitionPattern = R.parseRoute route }
postApi route = createApi route R.RouteDefinition { R.routeDefinitionMethod = R.RouteMethodSpecific HTTP.methodPost, R.routeDefinitionPattern = R.parseRoute route }


createApiWith
    :: (Monad m)
    => String
    -> R.RouteDefinition
    -> (Given ApiInfo => request -> m input)
    -> (Given ApiInfo => input -> m output)
    -> (Given ApiInfo => output -> m response)
    -> ApiDefinition request response m
createApiWith name route parser implement renderer =
    createApi name route (\request -> parser request >>= implement >>= renderer)

getApiWith, postApiWith
    :: (Monad m)
    => String
    -> (Given ApiInfo => request -> m input)
    -> (Given ApiInfo => input -> m output)
    -> (Given ApiInfo => output -> m response)
    -> ApiDefinition request response m
getApiWith route = createApiWith route R.RouteDefinition { R.routeDefinitionMethod = R.RouteMethodSpecific HTTP.methodGet, R.routeDefinitionPattern = R.parseRoute route }
postApiWith route = createApiWith route R.RouteDefinition { R.routeDefinitionMethod = R.RouteMethodSpecific HTTP.methodPost, R.routeDefinitionPattern = R.parseRoute route }

