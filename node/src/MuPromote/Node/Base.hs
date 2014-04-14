-- | This module defines the basic node lifecycle operations and types.
module MuPromote.Node.Base (

  -- * Node instance
  Node(..),
  spawnNode,

  -- * Node actions
  NodeAction(..),

  ) where

import Control.Applicative
import Control.Concurrent.STM (TVar, newTVarIO)

import MuPromote.Common.PromotableItem (PromotableItem)
import MuPromote.Node.PromotionProviderClient

-- | The data type of the state of the node.
data Node = Node {
  -- | The append-only list of actions that have been performed with the node.
  nodeActions :: TVar [NodeAction],

  -- | A handle to the provider that the node interfaces with.
  nodeProvider :: PromotionProviderClient
  }

-- | A data type for the state-changing actions that may be performed with a node.
data NodeAction =
  -- | Action for enrolling an item.
  EnrollItemAction (Double, PromotableItem)

  -- | Action signalling that the node has started communicating with the
  -- provider about finalizing the currently enrolled item promotions
  | ExecutePromoteInitiatedAction

  -- | Action signalling the last 'ExecutePromoteInitiatedAction' completed
  -- successfully. Thus, all 'EnrollItemAction's prior to the latest
  -- 'ExecutePromoteInitiatedAction' are now considered executed.
  | ExecutePromoteCompletedAction

  -- | Action signalling that the last 'ExecutePromoteInitiatedAction' did not
  -- finalize successfully, and no further attempts will be made to finalize
  -- it.
  | ExecutePromoteCancelledAction

-- | Construct a node instance, given a provider.
spawnNode :: PromotionProviderClient -> IO Node
spawnNode p = Node <$> newTVarIO [] <*> pure p
