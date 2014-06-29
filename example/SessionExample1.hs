{-# LANGUAGE OverloadedStrings, FlexibleContexts, TypeOperators #-}
module Main where

import Control.Eff (Member, Eff, (:>))
import Control.Eff.State.Strict (State, evalState)
import Control.Eff.Reader.Strict (Reader, ask, runReader)
import Control.Eff.Exception (runExc)
import Control.Eff.Lift (Lift, runLift)
import Control.Exception (SomeException)

import Wf.Control.Eff.Kvs (Kvs)
import Wf.Control.Eff.Logger (LogLevel(..), runLoggerStdIO)
import Wf.Control.Eff.Session (Session, sget, sput, renderSetCookie)
import Wf.Control.Eff.Run.Session (runSession)
import Wf.Control.Eff.Run.Kvs.Redis (runKvsRedis)
import Wf.Web.Session (SessionState, defaultSessionState, SessionKvs)
import Wf.Application.Time (Time, getCurrentTime)
import Wf.Application.Exception (Exception)
import Wf.Application.Logger (Logger, logDebug)

import qualified Network.Wai as Wai (Request, Response, defaultRequest, requestHeaders, responseLBS, responseStatus, responseHeaders)
import qualified Network.HTTP.Types as HTTP (status200, status500)
import qualified Database.Redis as Redis (ConnectInfo(..), defaultConnectInfo, PortID(..))
import qualified Network.Wai.Handler.Warp as Warp (run)

import qualified Data.ByteString.Lazy as L (ByteString, append)
import qualified Data.ByteString.Lazy.Char8 as L (pack)
import qualified Data.ByteString.Char8 as B (pack, unpack)

app :: (Member Session r, Member (Reader Wai.Request) r, Member Logger r) => Eff r Wai.Response
app = do
    req <- ask
    logDebug . show $ Wai.requestHeaders req
    count <- sget "count"
    exec (fmap (read . B.unpack) count)

    where
    exec Nothing = do
        sput "count" (B.pack . show $ (1 :: Integer))
        setCookie <- renderSetCookie
        let headers = [setCookie]
        let body = "<h1>You area new here</h1>"
        return $ Wai.responseLBS HTTP.status200 headers body

    exec (Just c) = do
        sput "count" (B.pack . show $ (c + 1 :: Integer))
        let headers = []
        let body = "<h1>You visited here " `L.append` (L.pack . show $ c) `L.append` " times</h1>"
        return $ Wai.responseLBS HTTP.status200 headers body

main :: IO ()
main = do
    Warp.run 8080 server
    where
    errorResponse = Wai.responseLBS HTTP.status500 [] ""
    server request respond = do
        r <- run app request
        case r of
             Right res -> respond res
             Left _ -> respond errorResponse

redisConnectInfo :: Redis.ConnectInfo
redisConnectInfo = Redis.defaultConnectInfo { Redis.connectDatabase = 2 }

run ::
    Eff (  Session
        :> Kvs SessionKvs
        :> State SessionState
        :> Reader Time
        :> Reader Wai.Request
        :> Exception
        :> Logger
        :> Lift IO
        :> ()) Wai.Response ->
    Wai.Request ->
    IO (Either SomeException Wai.Response)
run eff request = do
    t <- getCurrentTime
    runLift
        . runLoggerStdIO DEBUG
        . runExc
        . flip runReader request
        . flip runReader t
        . evalState defaultSessionState
        . runKvsRedis redisConnectInfo
        . runSession "SID" False 10 $ eff
