{-# LANGUAGE OverloadedStrings, FlexibleInstances, DeriveDataTypeable, MultiParamTypeClasses #-}
module Wf.Web.Routing
( routes
, route
, get
, post
, parseRoute
, RouteDefinition(..)
, Parameter
) where

import Control.Monad (msum, when, liftM)
import Data.Maybe (fromMaybe)
import qualified Data.ByteString as B (ByteString, empty)
import qualified Data.ByteString.Char8 as B (head, split, pack)
import qualified Data.List as L (break)
import Data.String (IsString(..))
import Data.Typeable (Typeable)
import qualified Network.HTTP.Types as HTTP (Method, methodGet, methodPost)

type Parameter = (B.ByteString, B.ByteString)


routes :: a -> [HTTP.Method -> B.ByteString -> Maybe a] -> HTTP.Method -> B.ByteString -> a
routes defaultApp rs method path = fromMaybe defaultApp (msum . map (\a -> a method path) $ rs)


get, post :: [RoutePattern] -> ([Parameter] -> a) -> HTTP.Method -> B.ByteString -> Maybe a
get = route . RouteDefinition (RouteMethodSpecific HTTP.methodGet)
post = route . RouteDefinition (RouteMethodSpecific HTTP.methodPost)


route :: RouteDefinition -> ([Parameter] -> a) -> HTTP.Method -> B.ByteString -> Maybe a
route definition app requestMethod requestPath =
    liftM app $ matchRouteDefinition definition requestMethod requestPath


data RoutePattern =
    RoutePath B.ByteString |
    RouteParameter B.ByteString
    deriving (Show, Eq, Typeable)


data RouteMethod =
    RouteMethodSpecific HTTP.Method |
    RouteMethodOneOf [HTTP.Method] |
    RouteMethodAny
    deriving (Show, Eq, Typeable)


data RouteDefinition = RouteDefinition
    { routeDefinitionMethod :: RouteMethod
    , routeDefinitionPattern :: [RoutePattern]
    } deriving (Show, Eq, Typeable)


matchRouteMethod :: RouteMethod -> HTTP.Method -> Bool
matchRouteMethod (RouteMethodSpecific a) b = a == b
matchRouteMethod (RouteMethodOneOf as) b = b `elem` as
matchRouteMethod RouteMethodAny _ = True


matchRoutePattern :: [RoutePattern] -> B.ByteString -> Maybe [Parameter]
matchRoutePattern _ "" = Nothing
matchRoutePattern pattern url = do
    when (B.head url /= '/') Nothing
    let path = filter (/= "") . B.split '/' $ url
    when (length pattern /= length path) Nothing
    sequence . filter (/= emp) . fmap match $ zip pattern path
    where match (RoutePath a, b) = if a == b then emp else Nothing
          match (RouteParameter a, b) = Just (a, b)
          emp = Just (B.empty, B.empty)


matchRouteDefinition :: RouteDefinition -> HTTP.Method -> B.ByteString -> Maybe [Parameter]
matchRouteDefinition definition method path =
    let dMethod = routeDefinitionMethod definition
        dPattern = routeDefinitionPattern definition
    in if matchRouteMethod dMethod method
          then matchRoutePattern dPattern path
          else Nothing


parseRoute :: String -> [RoutePattern]
parseRoute ('/' : s) = filter (/= RoutePath "") . split $ s
    where split a = case L.break (== '/') a of
                         (h, '/' : t) -> routePatternFromString h : split t
                         (h, _) -> [routePatternFromString h]
parseRoute _ = error "invalid route pattern"


routePatternFromString :: String -> RoutePattern
routePatternFromString ":" = error "invalid route parameter pattern"
routePatternFromString (':' : s) = RouteParameter $ B.pack s
routePatternFromString s = RoutePath $ B.pack s


instance IsString [RoutePattern] where
    fromString = parseRoute
