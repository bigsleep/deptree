{-# LANGUAGE OverloadedStrings, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
module Application.Routes
( routes
, route
, route'
, get
, get'
, post
, post'
, parseRoute
) where

import Control.Monad (msum, when, liftM)
import Data.Maybe (fromMaybe)
import qualified Data.ByteString as B (ByteString, empty)
import qualified Data.ByteString.Char8 as B (head, split, pack)
import qualified Data.List as L (break)
import qualified Network.HTTP.Types as HTTP (Method, methodGet, methodPost)

type Parameter = (B.ByteString, B.ByteString)


routes :: Monad m => m a -> [HTTP.Method -> B.ByteString -> Maybe (m a)] -> HTTP.Method -> B.ByteString -> m a
routes d rs method path = fromMaybe d (msum . map (\a -> a method path) $ rs)


get, post :: Monad m => [RoutePattern] -> m a -> HTTP.Method -> B.ByteString -> Maybe (m a)
get = route HTTP.methodGet
post = route HTTP.methodPost


get', post' :: Monad m => [RoutePattern] -> ([Parameter] -> m a) -> HTTP.Method -> B.ByteString -> Maybe (m a)
get' = route' HTTP.methodGet
post' = route' HTTP.methodPost


route :: HTTP.Method -> [RoutePattern] -> m a -> HTTP.Method -> B.ByteString -> Maybe (m a)
route method pattern app requestMethod requestPath =
    if requestMethod == method
       then matchRoute pattern requestPath >> return app
       else Nothing


route' :: HTTP.Method -> [RoutePattern] -> ([Parameter] -> m a) -> HTTP.Method -> B.ByteString -> Maybe (m a)
route' method pattern app requestMethod requestPath =
    if requestMethod == method
       then liftM app (matchRoute pattern requestPath)
       else Nothing


data RoutePattern =
    RoutePath B.ByteString |
    RouteParameter B.ByteString
    deriving (Show, Eq)


matchRoute :: [RoutePattern] -> B.ByteString -> Maybe [Parameter]
matchRoute _ "" = Nothing
matchRoute pattern url = do
    when (B.head url /= '/') Nothing
    let path = filter (/= "") . B.split '/' $ url
    when (length pattern /= length path) Nothing
    sequence . filter (/= emp) . fmap match $ zip pattern path
    where match (RoutePath a, b) = if a == b then emp else Nothing
          match (RouteParameter a, b) = Just (a, b)
          emp = Just (B.empty, B.empty)


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
