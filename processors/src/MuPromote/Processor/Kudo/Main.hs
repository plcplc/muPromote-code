{-# LANGUAGE DeriveDataTypeable #-}
-- | This module houses the functions for booting up a MuPromote Kudo Promotion
-- Processor as a standalone executable.
module MuPromote.Processor.Kudo.Main (
  EventLog(..),
  resourceMain
  ) where

import Control.Applicative
import Control.Monad.IO.Class ( liftIO )
import Data.Typeable

import Network.Wai.Handler.Warp ( defaultSettings, runSettingsSocket, Settings, setBeforeMainLoop )
import System.IO ( hGetContents )

import System.EncapsulatedResources
  ( getBoundSock, lookupRes, resAsFileHandle, resource, ResourceM, ResourceHandle, send, sendIO )

import MuPromote.Processor.KudoWeb ( kudoApp )

-- | The type used to represent the configuration of the promotion provdier. The principal
-- source of such a configuration is through the resource named 'config',
-- handled by 'read'-ing and 'show'-ing 'ProcessorConfig's.
type ProcessorConfig = [(String, String)]

-- | The type used for event logs. Log entries are sent to the 'Log-Resource'
-- resource given in the 'config' resource.
data EventLog = EventLogEntry String
  deriving (Typeable)

-- | The actual 'main' action. It lives as a self-contained 'Resource'. When
-- first invoked, the resource is empty (though a 'Resource'-aware compilation
-- procedure could seed the resource with other sub-resources), and default
-- versions of config-file resources and database resources are created. This
-- division is necessary to maintain the illusion that resources have
-- 'persistent/start-stop' semantics.
resourceMain :: ResourceM ResourceHandle
resourceMain = resource "Kudo promotion provider" $ do

  -- Source config file
  configRh <- lookupRes "config"
  cfgH <- resAsFileHandle configRh
  cfg  <- liftIO $ (read :: String -> ProcessorConfig) <$> hGetContents cfgH

  -- Start server
  let Just sockResName = lookup "Listen-Socket" cfg
  let Just logResName = lookup "Log-Resource" cfg

  logRes <- lookupRes logResName
  send logRes (EventLogEntry "lookupRes logResName")
  logAct <- sendIO logRes

  sockRh <- lookupRes sockResName
  send logRes (EventLogEntry "lookupRes sockResName")

  sock   <- getBoundSock sockRh
  liftIO $ kudoApp >>= runSettingsSocket (providerWarpSettings logAct) sock

-- | The settings used by the Warp server.
providerWarpSettings :: (EventLog -> IO ()) -> Settings
providerWarpSettings logAct =
  setBeforeMainLoop (logAct $ EventLogEntry "Warp server started") defaultSettings

-- | The 'main' action initializes the Node instance 'Resource' context that
-- 'mainComposed' lives inside. If the resource is empty/doesn't exist it
-- creates it in the surrounding OS (eg by creating a /etc/muPromote/kudo/ dir etc. as
-- determined by the 'Resource's library)
--  main :: IO ()
--  main = error "Not implemented: main"
