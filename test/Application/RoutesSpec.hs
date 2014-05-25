{-# LANGUAGE OverloadedStrings #-}
module Application.RoutesSpec
( routesSpec
) where

import qualified Network.HTTP.Types as HTTP (Method, methodGet, methodPost)
import qualified Data.ByteString as B (ByteString, concat)
import qualified Data.List as L (lookup)

import qualified Application.Routes as R

import Test.Hspec (Spec, describe, it, shouldBe)

routesSpec :: Spec
routesSpec = describe "routes" $
    it "dispatch correctly" $ do
        myapp HTTP.methodGet "/" `shouldBe` Just "root"
        myapp HTTP.methodPost "/" `shouldBe` Nothing
        myapp HTTP.methodGet "/////////////" `shouldBe` Just "root"
        myapp HTTP.methodGet "/news" `shouldBe` Just "news"
        myapp HTTP.methodGet "/blog/2014/03/21" `shouldBe` Just "blog-2014-03-21"
        myapp HTTP.methodGet "/blog////2014///03//21/////" `shouldBe` Just "blog-2014-03-21"
        myapp HTTP.methodGet "/blog/2013/03/21" `shouldBe` Just "blog-2013-03-21"
        myapp HTTP.methodGet "/blog/2014/03/21/00/00" `shouldBe` Nothing
        myapp HTTP.methodPost "/user/register" `shouldBe` Just "register"

myapp :: HTTP.Method -> B.ByteString -> Maybe B.ByteString
myapp = R.routes Nothing
    [ R.get (p "/") (return "root")
    , R.get (p "/news") (return "news")
    , R.get' (p "/blog/:year/:month/:day") blog
    , R.post (p "/user/register") (return "register")
    ]
    where p = R.parseRoute

blog :: [(B.ByteString, B.ByteString)] -> Maybe B.ByteString
blog params = do
    year <- getParam "year"
    month <- getParam "month"
    day <- getParam "day"
    return . B.concat $ ["blog-", year, "-", month, "-", day]
    where getParam n = L.lookup n params
