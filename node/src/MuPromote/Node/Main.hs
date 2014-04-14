{-# LANGUAGE DeriveDataTypeable #-}
-- | This module houses the functions for booting up a MuPromote Node as a
-- standalone executable.
module MuPromote.Node.Main (
  EventLog(..),
  resourceMain,
  main
  ) where

import Control.Monad.IO.Class ( liftIO )

import qualified Data.Map as M
import Data.String
import Data.Typeable

import Network.Wai.Handler.Warp
  ( defaultSettings, runSettings, runSettingsSocket, Settings,
  setBeforeMainLoop )

import Network.Wai.Application.Static
import Network.Wai.Utils

import System.EncapsulatedResources
  ( getBoundSock, getDirPath, resChildren, ResourceM, sendIO )

import MuPromote.Node.Base (spawnNode)
import MuPromote.Node.PromotionProviderClient (nilProviderClient)
import MuPromote.Node.Web ( nodeApiMiddleware )

-- | The type used for event logs. Log entries are sent to the 'Log-Resource'
-- resource given in the 'config' resource.
data EventLog = LogServerStarted
  | LogDebug String
  deriving (Eq, Typeable)

-- | The actual 'main' action. It lives as a self-contained 'Resource'. When
-- first invoked, the resource is empty (though a 'Resource'-aware compilation
-- procedure could seed the resource with other sub-resources), and default
-- versions of config-file resources and database resources are created. This
-- division is necessary to maintain the illusion that resources have
-- 'persistent/start-stop' semantics.
resourceMain :: ResourceM ()
resourceMain = do

  -- The node is configured by means of its child resources.
  subResources <- resChildren

  -- Get the socket to listen to
  let sockRh = M.lookup "Listen-Socket" subResources

  -- Get the resource to send logs to. Optional.
  logAct <- case M.lookup "Log-Resource" subResources of
    Just logRh -> sendIO logRh
    Nothing -> return $ const $ return ()

  -- Get the resource of static web UI. Optional.
  uiApp <- case M.lookup "WebUI-Dir" subResources of
    Just uiBaseDirRh -> do
      baseDir <- getDirPath uiBaseDirRh
      liftIO $ logAct (LogDebug $ "WebUI-Dir: " ++ baseDir)
      return $ staticApp $ defaultFileServerSettings (fromString baseDir)
    Nothing -> return $ const (return notFoundResp)

  liftIO $ logAct $ LogDebug "Getting bound sock"

  -- An action to start the server, depending on whether a Listen-Socket was
  -- given.
  startWarpAct <- case sockRh of
    Just sockRh' -> do
      sock <- getBoundSock sockRh'
      return $ runSettingsSocket (nodeWarpSettings logAct) sock
    Nothing -> return $ runSettings (nodeWarpSettings logAct)

  -- construct the node.
  liftIO $ do
    node <- spawnNode nilProviderClient
    startWarpAct (nodeApiMiddleware node uiApp)

-- | The settings used by the Warp server.
nodeWarpSettings :: (EventLog -> IO ()) -> Settings
nodeWarpSettings logAct =
  setBeforeMainLoop (logAct LogServerStarted) defaultSettings

-- | The 'main' action initializes the Node instance 'Resource' context that
-- 'mainComposed' lives inside. If the resource is empty/doesn't exist it
-- creates it in the surrounding OS (eg by creating a ~/.muPromoteNode/ dir etc. as
-- determined by the 'Resource's library)
main :: IO ()
main = error "Not implemented: main"
