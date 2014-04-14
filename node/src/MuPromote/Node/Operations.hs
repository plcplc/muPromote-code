-- | This module houses the high level node actions that make up the external
-- node API.
module MuPromote.Node.Operations (

  -- * Manipulating the items enrolled for promotion
  enrollItem,
  enrolledItems,

  -- * Promoting
  executePromote

  ) where

import Control.Concurrent.STM (atomically, modifyTVar, readTVar, STM, TVar)
import MuPromote.Common.PromotableItem (PromotableItem(..), addItem)
import qualified MuPromote.Node.PromotionProviderClient as PC
import MuPromote.Node.Base

-- | Register an item for enrollment in the node.
enrollItem :: Node -> Double -> PromotableItem -> IO ()
enrollItem node weight item = atomically $ recordAction node (EnrollItemAction (weight, item))

-- | Dump the list of enrolled items.
enrolledItems :: Node -> IO [(Double, PromotableItem)]
enrolledItems node = atomically $ enrolledItemStm (nodeActions node)

-- | A report on the NodeAction history that holds the set of currently enrolled items and weights.
enrolledItemStm :: TVar [NodeAction] -> STM [(Double, PromotableItem)]
enrolledItemStm actionsTV = do
  actions <- readTVar actionsTV
  return $ reduceEnrolled actions [] False

  where

    -- Reduce with 'addItem' on the action history, finishing at the latest
    -- completed 'ExecutePromoteInitiatedAction'.
    reduceEnrolled :: [NodeAction] -> [(Double, PromotableItem)] -> Bool -> [(Double, PromotableItem)]
    -- cave: lazy addItem?
    reduceEnrolled (EnrollItemAction iw : actions) acc latestExecCanceled = reduceEnrolled actions (addItem iw acc) latestExecCanceled
    reduceEnrolled (ExecutePromoteCancelledAction : actions) acc _        = reduceEnrolled actions acc True
    reduceEnrolled (ExecutePromoteCompletedAction : _) _ True             = error "Nonsensical action sequence. Can't cancel completed execute"
    reduceEnrolled (ExecutePromoteCompletedAction : actions) acc False    = reduceEnrolled actions acc False
    reduceEnrolled (ExecutePromoteInitiatedAction : _) acc False          = acc
    reduceEnrolled (ExecutePromoteInitiatedAction : actions) acc True     = reduceEnrolled actions acc False
    reduceEnrolled [] acc _ = acc
    -- Note that the above seems like it could be split to rather depend on a
    -- 'executePromoteState'-ish report, that slides along back in time.

-- | Promote the enrolled PromotableItems.
executePromote :: Node -> IO ()
executePromote node = do

  items <- atomically $ do
    items <- enrolledItemStm (nodeActions node)
    recordAction node ExecutePromoteInitiatedAction
    return items

  PC.executePromote (nodeProvider node) items

  -- for now, execution just finishes immidiately.
  atomically $ recordAction node ExecutePromoteCompletedAction

-- | Log a new action to the node action history.
recordAction :: Node -> NodeAction -> STM ()
recordAction node action = modifyTVar (nodeActions node) (action :)
