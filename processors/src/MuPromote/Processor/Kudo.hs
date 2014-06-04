{-# LANGUAGE RecordWildCards #-}
-- | This module defines the pseudo promotion processor for Kudos
module MuPromote.Processor.Kudo (

  spawnKudo,
  executePromote,
  listHighscore

  ) where

import Control.Applicative
import Control.Concurrent.STM (TVar, newTVarIO, modifyTVar, atomically, readTVar)

import MuPromote.Common.PromotableItem (PromotableItem, mergeItems)

-- Development note: For flexibility, incubate the processor web service
-- interface + implementation here. Later it moves to a separate package.

{- file Readme.md:



-}

-- | Data type representing the kudo service internally.
data KudoHandle = KudoHandle {
  kudos :: TVar [(Double, PromotableItem)]
  }

-- | Spawn a new instance of the Kudo highscore test processor.
spawnKudo :: IO KudoHandle
spawnKudo = KudoHandle <$> newTVarIO []

-- | Execute a promotion on a kudo instance.
executePromote :: KudoHandle -> [(Double, PromotableItem)] -> IO ()
executePromote (KudoHandle{..}) newKudos = atomically $
  modifyTVar kudos (mergeItems newKudos)

listHighscore :: KudoHandle -> IO [(Double, PromotableItem)]
listHighscore (KudoHandle{..}) = atomically $ readTVar kudos
