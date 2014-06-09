{-# LANGUAGE DeriveDataTypeable #-}
-- | This module houses the functions for booting up a MuPromote Kudo Promotion
-- Processor as a standalone executable.
module MuPromote.Processor.Kudo.Main (
  EventLog(..),
  resourceMain
  ) where

import Control.Applicative
import qualified Data.Map as M
import Data.String
import Data.Typeable

import Network.Wai
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp
import Network.Wai.Utils

import System.EncapsulatedResources

import MuPromote.Common.Persist
import MuPromote.Processor.Kudo
import MuPromote.Processor.Kudo.Application ( kudoApiMiddleware )

-- | The type used for event logs. Log entries are sent to the 'Log-Resource'
-- resource given in the 'config' resource.
data EventLog = LogServerStarted | LogDebug String
  deriving (Eq, Typeable, Show)

type LogAct = EventLog -> IO ()

-- | The actual 'main' action. It lives as a self-contained 'Resource'.
resourceMain :: ResourceM ()
resourceMain = do

  logAct <- wireLogRes
  uiApp  <- wireUIRes logAct
  evStore <- wireStorageRes logAct

  -- Start server
  sockRh <- M.lookup "Listen-Socket" <$> resChildren
  liftIO $ logAct $ LogDebug "Getting bound sock"
  startWarpAct <- case sockRh of
    Just sockRh' -> do
      sock <- getBoundSock sockRh'
      return $ runSettingsSocket (processorWarpSettings logAct) sock
    Nothing -> return $ runSettings (processorWarpSettings logAct)

  liftIO $ do
    let kudo = spawnKudo evStore
    startWarpAct (kudoApiMiddleware kudo uiApp)

-- | The settings used by the Warp server.
processorWarpSettings :: (EventLog -> IO ()) -> Settings
processorWarpSettings logAct =
  setBeforeMainLoop (logAct LogServerStarted) . setPort 3002 $ defaultSettings

-- | Get the resource to send logs to. Optional.
wireLogRes :: ResourceM LogAct
wireLogRes = do
  subResources <- resChildren
  case M.lookup "Log-Resource" subResources of
    Just logRh -> sendIO logRh
    Nothing -> return $ const $ return ()

-- | Get the resource of static web UI. Optional.
wireUIRes :: LogAct -> ResourceM Application
wireUIRes logAct = do
  subResources <- resChildren
  case M.lookup "WebUI-Dir" subResources of
    Just uiBaseDirRh -> do
      baseDir <- getDirPath uiBaseDirRh
      liftIO $ logAct (LogDebug $ "WebUI-Dir: " ++ baseDir)
      return $ staticApp $ defaultFileServerSettings (fromString baseDir)
    Nothing -> return $ const (return notFoundResp)

wireStorageRes :: LogAct -> ResourceM (EventStore ProcessorAction)
wireStorageRes logAct = do
  subResources <- resChildren
  case M.lookup "Storage" subResources of
    Just storageRh -> do
      dir <- getDirPath storageRh
      liftIO $ dirBackedEventStore dir
    Nothing -> do
      liftIO $ logAct (LogDebug "No 'Storage' resource given")
      destroyRes
