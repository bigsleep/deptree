{-# LANGUAGE TypeOperators, OverloadedStrings, FlexibleContexts, TemplateHaskell #-}
module Wf.Web.JsonApiSpec
( jsonApiSpec
) where

import Control.Eff (Eff, Member, (:>))
import Control.Eff.Exception (Exc, runExc)
import Control.Eff.Lift (Lift, runLift)
import Control.Monad (sequence_)
import Control.Exception (SomeException(..))

import qualified Network.HTTP.Types as HTTP (Method, status200, status404, methodGet, methodPost, hContentType, hContentLength)

import qualified Data.ByteString as B (ByteString)
import qualified Data.ByteString.Char8 as B (pack)
import qualified Data.ByteString.Lazy as L (ByteString)
import qualified Data.ByteString.Lazy as L (ByteString, length)
import Data.Typeable (cast)
import Data.Either (isLeft)
import Data.Maybe (isJust)
import qualified Data.Aeson as DA (FromJSON(..), ToJSON(..), Value(..), encode, decode, object, (.=), (.:))
import qualified Data.Aeson.TH as DA (deriveJSON, defaultOptions)
import qualified Data.Map as M

import Wf.Web.Api (apiRoutes)
import Wf.Web.JsonApi (jsonApi, jsonPostApi, jsonGetApi, JsonInput(..), JsonOutput(..), JsonParseError)
import Wf.Network.Http.Types (Request(..), Response(..), defaultRequest, defaultResponse)
import Wf.Network.Http.Response (setStatus, setBody)
import Wf.Application.Exception (Exception)
import Wf.Application.Logger (Logger)
import Wf.Control.Eff.Logger (runLoggerStdIO, LogLevel(..))

import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy, expectationFailure)

jsonApiSpec :: Spec
jsonApiSpec = describe "json api" . it "create json api" $ do
    let rootInput = ()
    execCase $ testCase HTTP.methodGet "/" rootInput (shouldResponseNormal . rootApp $ rootInput)

    let rootBadInput = "admin" :: String
    execCase $ testCase HTTP.methodGet "/" rootBadInput shouldRequestParseError

    let addInput = (1, 2) :: (Int, Int)
    execCase $ testCase HTTP.methodGet "/add" addInput (shouldResponseNormal (3 :: Int))

    let addBadInput = (1, 2, 3) :: (Int, Int, Int)
    execCase $ testCase HTTP.methodGet "/add" addBadInput shouldRequestParseError

    let dicInput = [1, 2, 3, 4, 5] :: [Int]
    execCase $ testCase HTTP.methodPost "/dic" dicInput (shouldResponseNormal . dicApp $ dicInput)

    let dicBadInput = ["1", "2", "3", "4"] :: [String]
    execCase $ testCase HTTP.methodPost "/dic" dicBadInput shouldRequestParseError

    -- not found case
    execCase $ testCase HTTP.methodGet "/xxx" () (\(Right res) -> res `shouldBe` notFoundApp)

    where
    testCase :: (DA.ToJSON a) => HTTP.Method -> B.ByteString -> a -> (Either SomeException (Response L.ByteString) -> IO ()) -> TestCase
    testCase method path body = TestCase method path defaultRequest { requestBody = DA.encode . JsonInput $ body }

    execCase (TestCase method path request satisfy) =
        satisfy =<< (runLift . runLoggerStdIO DEBUG . runExc) (routes request method path)

    shouldResponseNormal :: (DA.ToJSON a) => a -> Either SomeException (Response L.ByteString) -> IO ()
    shouldResponseNormal body (Right r) = do
        let body' = DA.encode . JsonOutput $ body
            contentType = (HTTP.hContentType, "application/json")
            contentLength = (HTTP.hContentLength, B.pack . show . L.length $ body')
            headers = [contentType, contentLength]
        r `shouldBe` Response HTTP.status200 headers body'
    shouldResponseNormal _ _ = expectationFailure "Left"

    shouldRequestParseError (Left (SomeException r)) = shouldSatisfy (cast r :: Maybe JsonParseError) isJust
    shouldRequestParseError _ = expectationFailure "Right"

type M = Eff (Exception :> Logger :> Lift IO :> ())

data TestCase = TestCase HTTP.Method B.ByteString (Request L.ByteString) (Either SomeException (Response L.ByteString) -> IO ())

routes :: Request L.ByteString -> HTTP.Method -> B.ByteString -> M (Response L.ByteString)
routes = apiRoutes (return notFoundApp)
    [ jsonGetApi "/" (return . rootApp)
    , jsonGetApi "/add" (return . addApp)
    , jsonPostApi "/dic" (return . dicApp)
    ]

notFoundApp :: Response L.ByteString
notFoundApp = setBody "<h1>Not Found</h1>" . setStatus HTTP.status404 $ defaultResponse ()

rootApp :: () -> String
rootApp _ = "<p>hello</p>"

addApp :: (Integer, Integer) -> Integer
addApp (a, b) = a + b

dicApp :: [Int] -> M.Map String Int
dicApp xs = M.fromList $ zip (map show xs) xs
