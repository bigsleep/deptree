{-# LANGUAGE DeriveDataTypeable #-}
module Web.Authenticate.OAuth2
( OAuth2(..)
) where

import qualified Data.ByteString as B (ByteString)
import Data.Typeable (Typeable)

data OAuth2 = OAuth2
    { oauth2AuthorizationServerName :: B.ByteString
    , oauth2AuthorizationUri :: B.ByteString
    , oauth2AccessTokenUri :: B.ByteString
    , oauth2ClientId :: B.ByteString
    , oauth2ClientSecret :: B.ByteString
    , oauth2RedirectUri :: B.ByteString
    } deriving (Show, Eq, Typeable)
