{-# LANGUAGE DeriveDataTypeable #-}
-- | This module houses the functions for booting up a MuPromote Node as a
-- standalone executable.
module MuPromote.Node.Main (
  EventLog(..),
  resourceMain
  ) where

import Control.Applicative
import Control.Monad.IO.Class ( liftIO )

import qualified Data.Map as M
import Data.String
import Data.Typeable

import Network.Wai.Handler.Warp
  ( defaultSettings, runSettings, runSettingsSocket, Settings,
  setBeforeMainLoop, setPort )

import Network.Wai
import Network.Wai.Application.Static
import Network.Wai.Utils

import System.EncapsulatedResources

import MuPromote.Node.Base
import MuPromote.Node.Persist
import MuPromote.Node.PromotionProviderClient
import MuPromote.Node.Web

-- | The type used for event logs. Log entries are sent to the 'Log-Resource'
-- resource given in the 'config' resource.
data EventLog = LogServerStarted
  | LogDebug String
  deriving (Eq, Typeable)

type LogAct = EventLog -> IO ()

-- | The actual 'main' action. It lives as a self-contained 'Resource'. When
-- first invoked, the resource is empty (though a 'Resource'-aware compilation
-- procedure could seed the resource with other sub-resources), and default
-- versions of config-file resources and database resources are created. This
-- division is necessary to maintain the illusion that resources have
-- 'persistent/start-stop' semantics.
resourceMain :: ResourceM ()
resourceMain = do

  logAct <- wireLogRes
  uiApp  <- wireUIRes logAct
  evStore <- wireStorageRes

  -- Get the socket to listen to
  sockRh <- M.lookup "Listen-Socket" <$> resChildren

  -- An action to start the server, depending on whether a Listen-Socket was
  -- given.
  liftIO $ logAct $ LogDebug "Getting bound sock"
  startWarpAct <- case sockRh of
    Just sockRh' -> do
      sock <- getBoundSock sockRh'
      return $ runSettingsSocket (nodeWarpSettings logAct) sock
    Nothing -> return $ runSettings (nodeWarpSettings logAct)

  -- Construct the node.
  liftIO $ do
    node <- spawnNode evStore nilProviderClient
    startWarpAct (nodeApiMiddleware node uiApp)

-- | The settings used by the Warp server.
nodeWarpSettings :: (EventLog -> IO ()) -> Settings
nodeWarpSettings logAct =
  setBeforeMainLoop (logAct LogServerStarted) . setPort 3001 $ defaultSettings

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

wireStorageRes :: LogAct -> ResourceM (EventStore NodeAction)
wireStorageRes logAct = do
  subResources <- resChildren
  case M.lookup "Storage" subResources of
    Just storageRh -> do
      dir <- getDirPath storageRh
      liftIO $ dirBackedEventStore dir
    Nothing -> do
      liftIO $ logAct (LogDebug "No 'Storage' resource given")
      destroyRes
