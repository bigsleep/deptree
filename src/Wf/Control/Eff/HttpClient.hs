{-# LANGUAGE TypeOperators, FlexibleContexts, DeriveDataTypeable, DeriveFunctor #-}
module Wf.Control.Eff.HttpClient
( HttpClient(..)
, httpClient
, runHttpClient
, runHttpClientMock
) where
import Control.Eff ((:>), VE(..), Eff, Member, SetMember, admin, handleRelay, inj, send)
import Control.Eff.Lift (Lift, lift)
import Data.Typeable (Typeable)
import qualified Data.ByteString.Lazy as L (ByteString)
import qualified Network.HTTP.Client as N (Request(..), Response(..), newManager, defaultManagerSettings, httpLbs)


data HttpClient a = HttpClient N.Request (N.Response L.ByteString -> a) deriving (Typeable, Functor)

httpClient :: (Member HttpClient r) => N.Request -> Eff r (N.Response L.ByteString)
httpClient req = send $ inj . HttpClient req

runHttpClient :: (Member (Lift IO) r, SetMember Lift (Lift IO) r) => Eff (HttpClient :> r) a -> Eff r a
runHttpClient eff = do
    m <- lift (N.newManager N.defaultManagerSettings)
    loop m . admin $ eff
    where loop _ (Val a) = return a
          loop m (E u) = handleRelay u (loop m) $
                            \(HttpClient req f) -> lift (N.httpLbs req m) >>= loop m . f

runHttpClientMock :: (N.Request -> N.Response L.ByteString) -> Eff (HttpClient :> r) a -> Eff r a
runHttpClientMock server = loop . admin
    where loop (Val a) = return a
          loop (E u) = handleRelay u loop $ \(HttpClient req f) -> loop . f $ server req


