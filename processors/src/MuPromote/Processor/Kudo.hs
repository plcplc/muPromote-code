-- | This module defines the pseudo promotion processor for Kudos
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module MuPromote.Processor.Kudo (

  spawnKudo,
  KudoProcessor(..),
  ProcessorAction(..)

  ) where

import Data.SafeCopy
import Data.Serialize

import MuPromote.Common.Persist
import MuPromote.Common.PromotableItem (PromotableItem)

-- | The data type for the actions that a promotion processor can perform.
data ProcessorAction =
  ExecutedPromoteAction! [(Double, PromotableItem)]
  deriving (Eq, Show)

-- | Data type representing the kudo service internally.
data KudoProcessor = KudoProcessor {
  kpEventStore :: EventStore ProcessorAction
  }

instance SafeCopy ProcessorAction where

  version = 0

  putCopy (ExecutedPromoteAction witems) = contain $ do
    putWord8 0
    safePut witems

  getCopy = contain $ do
    tag <- getWord8
    case tag of
      0 -> do
        wItems <- safeGet
        return $ ExecutedPromoteAction wItems

-- | Spawn a new instance of the Kudo highscore test processor.
spawnKudo :: EventStore ProcessorAction -> KudoProcessor
spawnKudo = KudoProcessor
