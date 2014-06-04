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

processorResourceSpecs :: Spec
processorResourceSpecs = do
  describe "The MuProomte Kudo resource" $ do

    it "can be started" $ testResources 100 ( \log -> lookupableRes $ do

      root <- currentRes

      let logRes  = "logger resource"
      let processorSock = "processor communication socket"

      sockRes processorSock

      fileRes "config" (show [
        ("Listen-Socket", processorSock),
        ("Log-Resource", logRes)
        ])

      resource logRes $ lookupableRes $ forever $
        recieve [
          Match $ \ (Kudo.EventLogEntry msg) -> do
            log msg
            when (msg == "Warp server started")
              (send root TestLogDone)
          ]

      Kudo.resourceMain

      recieve [Match $ \ TestLogDone -> destroyRes]

      )
      (\logCh -> do
        logL <- logToList logCh
        shouldContain logL ["Warp server started"]
      )

