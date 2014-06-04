-- | This module houses the high level node actions that make up the external
-- node API.
module MuPromote.Node.Operations (

  -- * Manipulating the items enrolled for promotion
  enrollItem,
  enrolledItems,

  -- * Promoting
  executePromote

  ) where

import Control.Applicative
import Data.SafeCopy
import MuPromote.Common.Persist
import MuPromote.Common.PromotableItem (PromotableItem(..), addItem)
import qualified MuPromote.Node.PromotionProcessorClient as PC
import MuPromote.Node.Base

-- | A data type representing the contents of the enrolled itmes report.
data EnrolledItemsReport = EnrolledItemsReport {
  -- | The currently enrolled, unprocessed items.
  eirEnrolledItems        :: [(Double, PromotableItem)],
  -- | The items currently being sent to the promotion processor if any.
  eirProcessingInitiated  :: Maybe ItemProcessingBatch,
  -- | The sequence number to be used for the next item processing batch.
  eirNextSeqNum :: Integer
  }

data ItemProcessingBatch = ItemProcessingBatch {
  ipbSeqNum        :: Integer,
  ipbWeightedItems :: [(Double, PromotableItem)]
  }

instance SafeCopy EnrolledItemsReport where

  version = 0

  getCopy = contain $
    EnrolledItemsReport <$> safeGet <*> safeGet <*> safeGet

  putCopy eir = contain $ do
    safePut $ eirEnrolledItems eir
    safePut $ eirProcessingInitiated eir
    safePut $ eirNextSeqNum eir

instance SafeCopy ItemProcessingBatch where

  version = 0

  getCopy = contain $
    ItemProcessingBatch <$> safeGet <*> safeGet

  putCopy ipb = contain $ do
    safePut $ ipbSeqNum ipb
    safePut $ ipbWeightedItems ipb

-- | Register an item for enrollment in the node.
enrollItem :: Node -> Double -> PromotableItem -> IO ()
enrollItem node weight item =
  appendEvent (nodeEventStore node) (EnrollItemAction (weight, item))

-- | Dump the list of enrolled items.
enrolledItems :: Node -> IO [(Double, PromotableItem)]
enrolledItems node = eirEnrolledItems <$> evalReport (enrolledItemsReport node)

-- | A report on the NodeAction history that holds the set of currently
-- enrolled (yet to be executed) items and weights.
enrolledItemsReport :: Node -> Report NodeAction EnrolledItemsReport
enrolledItemsReport node =
  registerReport (nodeEventStore node) "Enrolled Items" emptyReport go

  where

    emptyReport = EnrolledItemsReport [] Nothing 0

    go :: EnrolledItemsReport -> NodeAction -> EnrolledItemsReport

    go st (EnrollItemAction wItem)      = st { eirEnrolledItems = addItem wItem (eirEnrolledItems st)}

    go st ExecutePromoteInitiatedAction = st {
      eirEnrolledItems = [],
      eirProcessingInitiated = Just ItemProcessingBatch {
        ipbSeqNum = eirNextSeqNum st,
        ipbWeightedItems = eirEnrolledItems st },
      eirNextSeqNum = succ $ eirNextSeqNum st }

    go st ExecutePromoteCompletedAction = st { eirProcessingInitiated = Nothing }

-- | Promote the enrolled PromotableItems. Beware, not safe for concurrency yet!
executePromote :: Node -> IO ()
executePromote node = do
  appendEvent (nodeEventStore node) ExecutePromoteInitiatedAction

  report <- evalReport (enrolledItemsReport node)
  case eirProcessingInitiated report of
    Just (ItemProcessingBatch _ wItems) ->
      PC.executePromote (nodeProcessor node) wItems
    Nothing -> return ()

  appendEvent (nodeEventStore node) ExecutePromoteCompletedAction
