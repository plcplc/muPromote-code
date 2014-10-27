{-# LANGUAGE OverloadedStrings #-}
-- | This module specifies actions and properties of 'PromotableItem's.
module MuPromote.Test.Unit.PromotableItem (

  -- * Test data
  item1Str,
  item2,
  item3

  ) where

import MuPromote.Common.PromotableItem

-- | A marshalled promotable item.
item1Str :: String
item1Str = "{\"name\":\"foo\"}"

-- | A test item
item2 :: PromotableItem
item2 = piObject [("name", textAtom "Gnome")]

-- | Another test item
item3 :: PromotableItem
item3 = piObject [("name", textAtom "Pirate Party")]
