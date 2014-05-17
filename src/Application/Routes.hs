{-# LANGUAGE OverloadedStrings, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
module Application.Routes
( routes
, route
, get
, post
, parseRoute
) where

import Control.Eff (Eff)
import Control.Monad (msum, when, liftM)
import Data.Maybe (fromMaybe)
import qualified Data.ByteString as B (ByteString, empty)
import qualified Data.ByteString.Char8 as B (head, split, pack)
import qualified Data.List as L (break)
import qualified Network.HTTP.Types as HTTP (Method, methodGet, methodPost)
import qualified Network.Wai as W (Request, requestMethod, rawPathInfo)

routes :: Eff r a -> e -> [e -> Maybe (Eff r a)] -> Eff r a
routes d req rs = fromMaybe d (msum . map ($ req) $ rs)

get, post :: Adaptable app (Eff r a) => [RoutePattern] -> app -> W.Request -> Maybe (Eff r a)
get = route HTTP.methodGet
post = route HTTP.methodPost

route :: (Adaptable app (Eff r a)) => HTTP.Method -> [RoutePattern] -> app -> W.Request -> Maybe (Eff r a)
route method pattern app req = if W.requestMethod req == method
                                  then liftM (adapt app) (matchRoute pattern (W.rawPathInfo req))
                                  else Nothing

data RoutePattern =
    RoutePath B.ByteString |
    RouteParameter B.ByteString
    deriving (Show, Eq)

matchRoute :: [RoutePattern] -> B.ByteString -> Maybe [(B.ByteString, B.ByteString)]
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

class Adaptable a b where
    adapt :: a -> [(B.ByteString, B.ByteString)] -> b

instance Adaptable (Eff r a) (Eff r a) where
    adapt = const

instance Adaptable ([(B.ByteString, B.ByteString)] -> Eff r a) (Eff r a) where
    adapt = id
