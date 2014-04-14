-- | This module specifies actions and properties of 'PromotableItem's.
module MuPromote.Test.Unit.PromotableItem (

  -- * Specs of promotions
  promotableItemSpecs,

  -- ** Promotable Item handling
  promotableItem,

  -- * Test data
  item1Str,
  item2,
  item3

  ) where

import Data.Maybe (isJust)
import Test.Hspec

import MuPromote.Common.PromotableItem
       (parsePromotable, renderPromotable, PromotableItem(..))

-- | The collected specs testing promotion.
promotableItemSpecs :: Spec
promotableItemSpecs = do
  promotableItem

-- | Specification for promotable items.
--
--   * Promotable items have a textual representation, used for marshalling.
--   Thus, we need to be able to read and write them.
promotableItem :: Spec
promotableItem =
  describe "Promotable items" $ do

    it "can be read from a string" $ example $ do
      let res = parsePromotable item1Str
      res `shouldSatisfy` isJust

    it "can render to a string" $ example $ do
      let res = renderPromotable item2
      length res `shouldSatisfy` (> 0)

    it "can read its own rendering" $ example $ do
      let actual = item3
      let itemStr = renderPromotable actual
      let itemParsed = parsePromotable itemStr
      itemParsed `shouldSatisfy` isJust
      let Just expected = itemParsed
      actual `shouldBe` expected

-- | A marshalled promotable item.
item1Str :: String
item1Str = "PromotableItem{name=\"KDE\", promotionProvider=\"Kudo high score\"}"

-- | A test item
item2 :: PromotableItem
item2 = PromotableItem{name="Gnome", promotionProvider="Kudo high score"}

-- | Another test item
item3 :: PromotableItem
item3 = PromotableItem{name="Pirate Party", promotionProvider="flattr"}
