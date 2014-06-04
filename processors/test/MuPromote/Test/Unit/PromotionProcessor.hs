-- | Here we test the interfacing with promotion processors, using Kudo as a
-- mock.
module MuPromote.Test.Unit.PromotionProcessor (
  processorSpecs
  ) where

-- | Library includes.
import Test.Hspec

-- | AUT includes.
import MuPromote.Processor.Kudo as K

-- | Test includes.
import MuPromote.Test.Unit.PromotableItem (item2, item3)

-- | The specs of processor tests.
processorSpecs :: Spec
processorSpecs = describe "The Kudo processor" $ do
  executePromoteKudo

-- | Executing promotions on a kudo processor.
executePromoteKudo :: Spec
executePromoteKudo = do
  describe "Receiving promotion execute requests" $ do
    it "increases the count of promoted items" $ do
      processor <- K.spawnKudo
      K.executePromote processor [(0.5, item2),(0.5, item3)]
      highScore <- K.listHighscore processor

      shouldBe (elem (0.5, item2) highScore) True
      shouldBe (elem (0.5, item3) highScore) True

    it "properly sums recieved requests." $ do
      processor <- K.spawnKudo
      K.executePromote processor [(0.5, item2),(0.5, item3)]
      K.executePromote processor [(0.5, item2),(0.5, item3)]
      highScore <- K.listHighscore processor

      shouldBe (elem (1, item2) highScore) True
      shouldBe (elem (1, item3) highScore) True
