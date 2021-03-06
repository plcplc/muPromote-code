-- | This module defines the basic node lifecycle operations and types.
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module MuPromote.Node.Base (

  -- * Node instance
  Node(..),
  spawnNode,

  -- * Node actions
  NodeAction(..),

  ) where

import MuPromote.Common.PromotableItem (PromotableItem)
import MuPromote.Common.Persist
import MuPromote.Node.PromotionProcessorClient

import Data.SafeCopy
import Data.Serialize

-- | The data type of the state of the node.
data Node = Node {

  -- | The append-only list of actions that have been performed with the node.
  nodeEventStore :: EventStore NodeAction,

  -- | A handle to the promotion processor that the node interfaces with.
  nodeProcessor :: PromotionProcessorClient

  }

instance SafeCopy NodeAction where

  version = 0

  putCopy (EnrollItemAction witem) = contain $ do
    putWord8 0
    safePut witem
  putCopy ExecutePromoteInitiatedAction = contain $ putWord8 1
  putCopy ExecutePromoteCompletedAction = contain $ putWord8 2

  getCopy = contain $ do
    tag <- getWord8
    case tag of
      0 -> do
        wItem <- safeGet
        return $ EnrollItemAction wItem
      1 -> return ExecutePromoteInitiatedAction
      2 -> return ExecutePromoteCompletedAction

-- | A data type for the state-changing actions that may be performed with a node.
data NodeAction =
  -- | Action for enrolling an item.
  EnrollItemAction! (PromotableItem, Double)

  -- | Action signalling that the node has started communicating with the
  -- provider about finalizing the currently enrolled item promotions
  | ExecutePromoteInitiatedAction

  -- | Action signalling the last 'ExecutePromoteInitiatedAction' completed
  -- successfully. Thus, all 'EnrollItemAction's prior to the latest
  -- 'ExecutePromoteInitiatedAction' are now considered executed.
  | ExecutePromoteCompletedAction

-- | Construct a node instance, given a provider.
spawnNode :: EventStore NodeAction -> PromotionProcessorClient -> Node
spawnNode = Node
