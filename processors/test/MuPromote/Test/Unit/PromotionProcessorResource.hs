{-# LANGUAGE DeriveDataTypeable #-}
-- This module tests that the MuPromote Promotion kudo processor behaves as expected in its Resource environment.
module MuPromote.Test.Unit.PromotionProcessorResource ( processorResourceSpecs ) where

-- | Library includes
import Control.Monad
import Data.Typeable
import Prelude hiding (log)

import Test.Hspec

import System.EncapsulatedResources
import System.EncapsulatedResources.Test.EncapsulatedResources

-- | AUT includes
import qualified MuPromote.Processor.Kudo.Main as Kudo

data TestLog = TestLogDone
  deriving (Typeable)

data LogItems a = Item a | DebugMsg String
  deriving (Typeable, Show, Eq)

processorResourceSpecs :: Spec
processorResourceSpecs =
  describe "The MuProomte Kudo resource" $

    it "can be started" $ testResources 100 ( \log ->
      kudoResTest (return ())
        (const $ do
          untilLogged log Kudo.LogServerStarted
          liftIO $ log $ Item "Warp server started"
        ))
      (\logCh -> do
        logL <- logToList logCh
        shouldContain logL [Item ("Warp server started" :: String)]
      )

-- | A test pattern that wires Kudo.resourceMain.  'kudoResTest setup log'
-- wires the node resource and runs the 'setup' action in the processor resource.
-- The 'log' function is given a ResourceHandle for the socket resource, and
-- runs in a Resource that is designated reciever of log messages from the
-- processor The test terminates along with the 'log' action.
kudoResTest :: ResourceM () -> (ResourceHandle -> ResourceM ()) -> ResourceM ()
kudoResTest extraSetup logRes = do

  root <- currentRes
  sockRh <- sockRes  "Communication Socket"

  logRh <- resource "Logger resource" $ do
    logRes sockRh
    send root TestLogDone

  resource "Kudo-Processor" $ do
    proxyRes "Listen-Socket" sockRh
    proxyRes "Log-Resource" logRh
    dirRes "Storage" (return ())
    extraSetup
    Kudo.resourceMain

  recieve [Match $ \ TestLogDone -> destroyRes]

untilLogged :: TestLogger IO (LogItems a) -> Kudo.EventLog -> ResourceM ()
untilLogged log msg = do
  done <- recieve [ Match $ \ msgRecv -> do
    case msgRecv of
      Kudo.LogDebug msg' -> liftIO $ log $ DebugMsg msg'
      _ -> return ()
    return (msg == msgRecv) ]
  when done $ untilLogged log msg
