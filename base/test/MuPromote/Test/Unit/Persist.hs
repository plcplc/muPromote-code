{-# LANGUAGE ScopedTypeVariables #-}
module MuPromote.Test.Unit.Persist where

-- | Library includes
-- import Data.SafeCopy
import Prelude hiding (log)
import System.EncapsulatedResources

import System.EncapsulatedResources.Test.EncapsulatedResources
import Test.Hspec

-- | AUT includes
import MuPromote.Common.Persist


persistSpec :: Spec
persistSpec =
  describe "The event/report persistance" $
  context "(The filesystem-backed backed)" $ do

    it "Can record events" $ testPersist $ \store -> do
      let expected = 42
      appendEvent store expected
      actual <- lookupEvent store 0
      shouldBe actual (Just expected)

    it "Can evaluate reports" $ testPersist $ \store -> do
      let evs0 = [0..99]
      let evs1 = [100..199]
      let initial = 0
      let report = registerReport store "sum" (0 :: Int) (+)

      actualInitSt <- evalReport report
      shouldBe actualInitSt initial

      mapM_ (appendEvent store) evs0
      let expected0 = sum evs0
      actual0 <- evalReport report
      shouldBe actual0 expected0

      mapM_ (appendEvent store) evs1
      let expected1 = sum $ evs0 ++ evs1
      actual1 <- evalReport report
      shouldBe actual1 expected1


-- | Wrap the creation of a persistent eventstore resource.
testPersist :: (EventStore Int -> Expectation) -> Expectation
testPersist testAct = testResources' 100 $ do
  persistStoreRH <- dirRes "persistStore" $ return ()
  persistStoreFP <- getDirPath persistStoreRH
  liftIO $ do
    store <- dirBackedEventStore persistStoreFP
    testAct store

-- | Version of 'testResources' without logging.
testResources' :: Int -> ResourceM () -> Expectation
testResources' timeoutMillis resAct =
  testResources timeoutMillis
    (\(_ :: TestLogger IO ()) -> resAct) (const (return ()))
