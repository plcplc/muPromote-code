{-# LANGUAGE DeriveGeneric #-}
-- | This module defines 'PromotableItem's and functions handling them.
module MuPromote.Common.PromotableItem
  (
    -- * Promotable items
    PromotableItem(..),
    parsePromotable,
    renderPromotable,

    -- * Managing weight-item collections.
    mergeItems,
    addItem

  ) where

import Data.Aeson
import Data.SafeCopy
import Data.Serialize
import GHC.Generics

-- | The data type representing Promotable Items.
data PromotableItem = PromotableItem {
  -- | A name referring to the item, known by the promotion provider
  name :: String,

  -- | The provider that handles this promotable.
  promotionProcessor :: String
} deriving (Eq, Generic, Read, Show)

instance FromJSON PromotableItem
instance ToJSON PromotableItem

-- | A function for deserializing 'PromotableItem's.
parsePromotable :: String -> Maybe PromotableItem
parsePromotable str = case reads str of
  [] -> Nothing
  (p,""):_ -> Just p
  _ -> Nothing

-- | A function for serializing 'PromotableItem's.
renderPromotable :: PromotableItem -> String
renderPromotable = show

-- | Merge the PromotableItems xs into ys, summing accordingly.
mergeItems :: [(Double, PromotableItem)] -> [(Double, PromotableItem)] -> [(Double, PromotableItem)]
mergeItems xs ys = foldr addItem xs ys

-- | Actually add (and sum) the weight+item to enrolled items.
addItem :: (Double, PromotableItem) -> [(Double, PromotableItem)] -> [(Double, PromotableItem)]
addItem wi [] = [wi]
addItem (w,i) ((w',i'):r) | i == i' = (w+w', i):r
addItem wi (wi':r) = wi':(addItem wi r)

instance SafeCopy PromotableItem where

  version = 0

  putCopy pItem = contain $ do
    put $ name pItem
    put $ promotionProcessor pItem

  getCopy = contain $ do
    nm <- get
    pp <- get
    return $ PromotableItem nm pp
