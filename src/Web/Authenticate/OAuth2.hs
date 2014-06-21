{-# LANGUAGE OverloadedStrings, FlexibleContexts, DeriveDataTypeable #-}
module Web.Authenticate.OAuth2
( OAuth2(..)
, redirectToAuthorizationServer
, getAccessToken
, getTokenInfo
) where

import Control.Eff (Eff, Member)
import Control.Eff.HttpClient (HttpClient, httpClient)
import qualified Control.Eff.Session as Session (Session, sput, sget, sttl, renderSetCookie)
import qualified Control.Exception (Exception)
import Control.Monad (unless)

import qualified Data.ByteString as B (ByteString, append)
import qualified Data.ByteString.Char8 as B (unpack)
import qualified Data.Text.Encoding as T (encodeUtf8)
import Data.Maybe (isJust)
import Data.Typeable (Typeable)
import qualified Data.Aeson as DA (Value(..), Result(..), decode, (.:?))
import qualified Data.Aeson.Types as DA (parse)

import qualified Network.Wai as Wai (Response, responseLBS)
import qualified Network.HTTP.Client as N (Request(..), RequestBody(..), Response(..), parseUrl)
import qualified Network.HTTP.Types as HTTP (methodPost, renderQuery, status200, status302)

import Application.Exception (Exception, throwException)



data OAuth2 = OAuth2
    { oauth2AuthorizationServerName :: B.ByteString
    , oauth2AuthorizationUri :: B.ByteString
    , oauth2TokenUri :: B.ByteString
    , oauth2ClientId :: B.ByteString
    , oauth2ClientSecret :: B.ByteString
    , oauth2RedirectUri :: B.ByteString
    , oauth2Scope :: B.ByteString
    } deriving (Show, Eq, Typeable)


data OAuth2Error = OAuth2Error String deriving (Show, Typeable)

instance Control.Exception.Exception OAuth2Error


redirectToAuthorizationServer
    :: (Member Session.Session r)
    => OAuth2 -> B.ByteString -> Eff r Wai.Response
redirectToAuthorizationServer oauth2 state = do
    Session.sput "state" state
    Session.sttl 30
    setCookie <- Session.renderSetCookie
    let params = [ ("client_id", Just $ oauth2ClientId oauth2)
                 , ("response_type", Just "code")
                 , ("scope", Just $ oauth2Scope oauth2)
                 , ("redirect_uri", Just $ oauth2RedirectUri oauth2)
                 , ("state", Just state)
                 ]
        url = oauth2AuthorizationUri oauth2 `B.append` HTTP.renderQuery True params
        headers = [("Location", url), setCookie]
        response = Wai.responseLBS HTTP.status302 headers ""
    return response


getAccessToken
    :: (Member HttpClient r, Member Exception r, Member Session.Session r)
    => OAuth2 -> B.ByteString -> B.ByteString -> Eff r B.ByteString
getAccessToken oauth2 code state = do
    let params = [ ("code", Just code)
                 , ("redirect_uri", Just $ oauth2RedirectUri oauth2)
                 , ("client_id", Just $ oauth2ClientId oauth2)
                 , ("client_secret", Just $ oauth2ClientSecret oauth2)
                 , ("grant_type", Just "authorization_code")
                 ]
        body = HTTP.renderQuery False params
        headers = [ ("Content-Type", "application/x-www-form-urlencoded")
                  , ("Accept", "application/json")
                  ]

    reqInit <- case N.parseUrl . B.unpack . oauth2AuthorizationUri $ oauth2 of
                    Right r -> return r
                    Left e -> throwException e

    let req = reqInit { N.method = HTTP.methodPost
                      , N.secure = True
                      , N.requestHeaders = headers
                      , N.requestBody = N.RequestBodyBS body
                      }

    res <- httpClient req

    unless (N.responseStatus res == HTTP.status200) .
        throwException . OAuth2Error $ "authorization server returned other than status200. response: " ++ show res

    json <- case DA.decode $ N.responseBody res of
                 Just a -> return a
                 Nothing -> throwException . OAuth2Error $ "json parse failed. response: " ++ show res

    state' <- Session.sget "state"
    unless (isJust state' && state' == Just state) .
        throwException . OAuth2Error $ "state token check failed. response: " ++ show res ++ " state: " ++ show (Just state) ++ " /= " ++ show state'
    g json "access_token"

    where g obj key = case DA.parse (DA..:? key) obj of
                           DA.Success (Just (DA.String v)) -> return (T.encodeUtf8 v)
                           _ -> throwException . OAuth2Error $ "no field. obj: " ++ show obj ++ " key: " ++ show key


getTokenInfo
    :: (Member HttpClient r, Member Exception r)
    => OAuth2 -> B.ByteString -> Eff r DA.Value
getTokenInfo oauth2 accessToken = do
    let headers = [ ("Authorization", "OAuth " `B.append` accessToken)
                  , ("Accept", "application/json")
                  ]

    reqInit <- case N.parseUrl . B.unpack . oauth2TokenUri $ oauth2 of
                    Right r -> return r
                    Left e -> throwException e

    let req = reqInit { N.method = HTTP.methodPost
                      , N.secure = True
                      , N.requestHeaders = headers
                      }

    res <- httpClient req

    unless (N.responseStatus res == HTTP.status200) .
        throwException . OAuth2Error $ "token server returned other than status200. response: " ++ show res

    case DA.decode $ N.responseBody res of
         Just a -> return a
         Nothing -> throwException . OAuth2Error $ "json parse failed. response: " ++ show res
