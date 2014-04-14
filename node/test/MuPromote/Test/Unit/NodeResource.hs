{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- This module tests that the MuPromote Node behaves as expected in its Resource environment.
module MuPromote.Test.Unit.NodeResource ( nodeResourceSpecs ) where

-- | Library includes
import Control.Applicative
import Control.Monad

import Data.ByteString.Lazy as LBS
import Data.Text as T
import Data.Text.Encoding as T
import Data.Typeable

import Prelude hiding (log)

import Test.Hspec

import Network.HTTP.Client

import MuPromote.Common.HTTP.Client ( socketManagerSettings )
import Network.HTTP.Rest.Client
import System.EncapsulatedResources
import MuPromote.Common.NodeSignature ( getEnrolledItemsSig, postEnrolledItemsSig )

import System.EncapsulatedResources.Test.EncapsulatedResources
import MuPromote.Test.Unit.PromotableItem ( item2, item3 )

-- | AUT includes
import qualified MuPromote.Node.Main as Node

data TestLog = TestLogDone
  deriving (Typeable)

data LogItems a = Item a | DebugMsg String
  deriving (Typeable, Show, Eq)

-- | A test pattern that wires Node.resourceMain.  'nodeResTest setup log'
-- wires the node resource and runs the 'setup' action in the node resource.
-- The 'log' function is given a ResourceHandle for the socket resource, and
-- runs in a Resource that is designated reciever of log messages from the
-- Node. The test terminates with the 'log' action.
nodeResTest :: ResourceM () -> (ResourceHandle -> ResourceM ()) -> ResourceM ()
nodeResTest extraNodeSetup logRes = do

  root <- currentRes
  sockRh <- sockRes  "Node communication socket"

  logRh <- resource "Logger resource" $ do
    logRes sockRh
    send root TestLogDone

  resource "Node" $ do
    proxyRes "Listen-Socket" sockRh
    proxyRes "Log-Resource" logRh
    extraNodeSetup
    Node.resourceMain

  recieve [Match $ \ TestLogDone -> destroyRes]

nodeResourceSpecs :: Spec
nodeResourceSpecs = do
  describe "The MuProomte Node resource" $ do

    it "can be started" $ testResources 100 ( \log -> do
      nodeResTest (return ())
        (const $ do
          untilLogged log Node.LogServerStarted
          liftIO $ log $ Item "Warp server started"
        ))
      (\logCh -> do
        logL <- logToList logCh
        shouldContain logL [Item ("Warp server started" :: String)]
      )

    it "has an api for enrolling items" $ testResources 200 ( \log -> do
      -- Enroll two items, and check that you get correct sums back when listing.
      nodeResTest (return ()) $ \sockRh -> do
        -- Wait until the node server has started
        untilLogged log Node.LogServerStarted
        liftIO $ log (DebugMsg "node resource online")

        clientSockH <- getConnectedSock sockRh
        let sockSettings = socketManagerSettings clientSockH
        nodeManager <- liftIO $ newManager sockSettings
        let enrollReq = liftIO . (requestResource nodeManager "localhost" postEnrolledItemsSig)
        let listReq = liftIO $ requestResource nodeManager "localhost"  getEnrolledItemsSig

        enrollReq [(1.2, item2), (1.0, item3)]
        enrollReq [(1.3, item2)]

        enrolled <- listReq
        liftIO $ mapM_ (log . Item) enrolled
      )
      (\logCh -> do
        logL <- logToList logCh
        shouldContain logL [Item (2.5, item2), Item (1.0, item3)]
      )

    let testFile  = "<html><head><title>Test</title></head><body><h1>Test</h1></body></html>"
    it "can serve static files" $ testResources 100 (\log -> do
      nodeResTest
        (void $ dirRes "WebUI-Dir" $ do
          fileRes "index.html" testFile
          return ())
        ( \sockRh -> do
          -- Wait until the node server has started
          untilLogged log Node.LogServerStarted
          liftIO $ log (DebugMsg "node resource online")

          clientSockH <- getConnectedSock sockRh
          let sockSettings = socketManagerSettings clientSockH
          nodeManager <- liftIO $ newManager sockSettings

          request <- parseUrl "http://localhost/"
          resp <- liftIO $ responseBody <$> httpLbs request nodeManager
          liftIO $ log (Item $ T.unpack $ T.decodeUtf8 $ LBS.toStrict $ resp))
      )
      (\logCh -> do
        logL <- logToList logCh
        shouldContain logL [Item testFile]
      )

untilLogged :: TestLogger IO (LogItems a) -> Node.EventLog -> ResourceM ()
untilLogged log msg = do
  done <- recieve [ Match $ \ msgRecv -> do
    case msgRecv of
      Node.LogDebug msg' -> liftIO $ log $ DebugMsg msg'
      _ -> return ()
    return (msg == msgRecv) ]
  if done then untilLogged log msg else return ()
