-- | Here we test the interfacing with promotion processors, using Kudo as a
-- mock.
module MuPromote.Test.Unit.PromotionProcessor (
  processorSpecs
  ) where

-- | Library includes.
import Test.Hspec

-- | AUT includes.
import MuPromote.Common.Persist
import MuPromote.Processor.Kudo as K
import MuPromote.Processor.Kudo.Operations as K

-- | Test includes.
import MuPromote.Test.Unit.PromotableItem (item2, item3)

-- | The specs of processor tests.
processorSpecs :: Spec
processorSpecs = describe "The Kudo processor"
  executePromoteKudo

-- | Executing promotions on a kudo processor.
executePromoteKudo :: Spec
executePromoteKudo =
  describe "Receiving promotion execute requests" $ do
    it "increases the count of promoted items" $ do
      processor <- spawnMockedProcessor
      K.executePromote processor [(0.5, item2),(0.5, item3)]
      highScore <- K.listHighscore processor

      shouldBe ((0.5, item2) `elem` highScore) True
      shouldBe ((0.5, item3) `elem` highScore) True

    it "properly sums recieved requests." $ do
      processor <- spawnMockedProcessor
      K.executePromote processor [(0.5, item2),(0.5, item3)]
      K.executePromote processor [(0.5, item2),(0.5, item3)]
      highScore <- K.listHighscore processor

      shouldBe ((1, item2) `elem` highScore) True
      shouldBe ((1, item3) `elem` highScore) True

spawnMockedProcessor :: IO KudoProcessor
spawnMockedProcessor = do
  es <- memoryBackedEventStore
  return (K.spawnKudo es)
