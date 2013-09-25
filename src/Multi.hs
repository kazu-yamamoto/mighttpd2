{-# LANGUAGE OverloadedStrings #-}

module Multi (multi, masterMainLoop) where

import Control.Concurrent
import Control.Exception
import qualified Control.Exception as E (catch)
import Control.Monad
import Network.Wai.Logger
import Network.Wai.Logger.Prefork
import Process (findChildren, PsResult, dummyResult)
import System.Exit
import System.Posix

import Program.Mighty

import Config
import Log
import Report
import Single
import State
import Types

----------------------------------------------------------------

multi :: Option -> RouteDB -> Service -> LogType -> Stater -> Reporter -> IO [ProcessID]
multi opt route service logtype stt rpt = do
    report rpt "Master Mighty started"
    ignoreSigChild
    cids <- replicateM workers $ forkProcess $ do
        lgr <- initLogger FromSocket logtype
        -- killed by signal
        void . forkIO $ single opt route service rpt stt lgr
        mainLoop rpt stt lgr
    closeService service
    setHandler sigStop   $ stopHandler cids
    setHandler sigINT    $ stopHandler cids -- C-c from keyboard when debugging
    setHandler sigRetire $ retireHandler cids
    setHandler sigReload $ reloadHandler cids
    setHandler sigInfo   $ infoHandler cids
    return cids
  where
    workers = opt_worker_processes opt
    stopHandler cids   = Catch $ do
        report rpt "Master Mighty finished"
        finReporter rpt
        -- No logging
        mapM_ (sendSignal sigStop) cids
        exitImmediately ExitSuccess
    retireHandler cids = Catch $ do
        report rpt "Master Mighty retiring"
        goRetiring stt
        mapM_ (sendSignal sigRetire) cids
    reloadHandler cids = Catch $ ifRouteFileIsValid rpt opt $ \_ -> do
        report rpt "Master Mighty reloaded"
        mapM_ (sendSignal sigReload) cids
    infoHandler cids   = Catch $ mapM_ (sendSignal sigInfo) cids

----------------------------------------------------------------

masterMainLoop :: Reporter -> ProcessID -> IO ()
masterMainLoop rpt myid = do
    threadDelay 10000000
    cs <- findChildren myid `E.catch` handler
    if null cs then do -- FIXME serverStatus st == Retiring
        report rpt "Master Mighty retired"
        finReporter rpt
        -- No logging
        exitSuccess
      else
        masterMainLoop rpt myid
  where
    handler :: SomeException -> IO [PsResult]
    handler _ = return [dummyResult]
