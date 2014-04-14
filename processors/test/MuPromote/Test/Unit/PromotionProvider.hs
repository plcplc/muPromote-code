-- | Here we test the interfacing with promotion providers, using Kudo as a
-- mock.
module MuPromote.Test.Unit.PromotionProvider (
  providerSpecs
  ) where

-- | Library includes.
import Test.Hspec

-- | AUT includes.
import MuPromote.Provider.Kudo as K

-- | Test includes.
import MuPromote.Test.Unit.PromotableItem (item2, item3)

-- | The specs of provider tests.
providerSpecs :: Spec
providerSpecs = describe "The Kudo provider" $ do
  executePromoteKudo

-- | Executing promotions on a kudo provider.
executePromoteKudo :: Spec
executePromoteKudo = do
  describe "Receiving promotion execute requests" $ do
    it "increases the count of promoted items" $ do
      provider <- K.spawnKudo
      K.executePromote provider [(0.5, item2),(0.5, item3)]
      highScore <- K.listHighscore provider

      shouldBe (elem (0.5, item2) highScore) True
      shouldBe (elem (0.5, item3) highScore) True

    it "properly sums recieved requests." $ do
      provider <- K.spawnKudo
      K.executePromote provider [(0.5, item2),(0.5, item3)]
      K.executePromote provider [(0.5, item2),(0.5, item3)]
      highScore <- K.listHighscore provider

      shouldBe (elem (1, item2) highScore) True
      shouldBe (elem (1, item3) highScore) True
