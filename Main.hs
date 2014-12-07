{-# LANGUAGE OverloadedStrings, TypeOperators #-}
module Main where

import Root (rootApp)
import Update (runUpdateWorker, DepTree)
import Settings (Settings(..))
import DepTree (depTreeApp)

import Control.Eff ((:>), Eff)
import Control.Eff.Lift (Lift, lift, runLift)
import Control.Eff.Exception (runExc)

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (TVar, newTVar, atomically)
import qualified Control.Exception (SomeException(..))

import qualified Data.Aeson as DA (decode)
import qualified Data.HashMap.Strict as HM (empty)
import qualified Data.ByteString.Lazy as L (ByteString, readFile)

import qualified Network.Wai.Handler.Warp as Warp (run)
import qualified Network.HTTP.Types as HTTP (status404, status500)
import qualified Network.Wai as Wai (Request, Response)

import Wf.Application.Exception (Exception)
import Wf.Application.Logger (Logger, LogLevel(..), runLoggerStdIO)
import Wf.Network.Wai (toWaiApplication, toWaiResponse)
import Wf.Network.Http.Response (Response, setStatus, defaultResponse)
import Wf.Web.Api (apiRoutes, getApi)


main :: IO ()
main = do
    settings <- loadSettings
    let interval = settingsIntervalMinutes settings
        port = settingsPort settings

    root <- atomically $ newTVar "<p>constructing</p>"
    dtree <- atomically $ newTVar HM.empty

    _ <- forkIO $ runUpdateWorker interval root dtree

    Warp.run port . toWaiApplication . run $ routes root dtree

    where
    loadSettings :: IO Settings
    loadSettings = do
        a <- fmap DA.decode . L.readFile $ "config/settings.json"
        case a of
             Just settings -> return settings
             Nothing -> error "load failure"

type M = Eff
    (  Logger
    :> Exception
    :> Lift IO
    :> ())

routes :: TVar L.ByteString -> TVar DepTree -> Wai.Request -> M Wai.Response
routes root dtree = apiRoutes notFound rs
    where
    rs = [ getApi "/" (rootApp root)
         , getApi "/:package" (depTreeApp dtree)
         ]


notFound :: (Monad m) => m (Response L.ByteString)
notFound = return . setStatus HTTP.status404 . defaultResponse $ ""


run :: (Wai.Request -> M Wai.Response)
    -> Wai.Request -> IO Wai.Response
run app = run'
    where
    run' = runLift
        . (>>= handleError)
        . runExc
        . runLoggerStdIO WARN
        . app
    internalError = setStatus HTTP.status500 . defaultResponse $ ()
    handleError (Left (Control.Exception.SomeException e)) = do
        lift $ print e
        return . toWaiResponse $ internalError
    handleError (Right r) = return r
