-- | This module tests the operations that makes up the μPromote node interface.
module MuPromote.Test.Unit.Operations (

  -- * Specs of operations
  operationsSpecs,

  -- ** Placing promotions
  presentItemWithWeight,
  performPromote,
  performPromoteResets,
  performPromoteProcessor

  ) where

-- | Library includes.
import Control.Applicative
import Control.Concurrent.STM (TVar, newTVarIO, modifyTVar, atomically, readTVar)
import Test.Hspec

-- | AUT includes.
import MuPromote.Common.Persist
import MuPromote.Common.PromotableItem (PromotableItem)
import MuPromote.Node.Base as B
import MuPromote.Node.Operations as O
import MuPromote.Node.PromotionProcessorClient as PC

-- | Test includes.
import MuPromote.Test.Unit.PromotableItem (item2, item3)

-- | The specs of operations.
operationsSpecs :: Spec
operationsSpecs = do
  presentItemWithWeight
  performPromote

-- | The first part of promoting is to present the node with the items to
-- promote, together with their desired weight.
presentItemWithWeight :: Spec
presentItemWithWeight =
  describe "Presenting items to the node" $ do

    it "remembers them accordingly" $ do
      node <- fmap snd spawnMockedNode
      O.enrollItem node 1.1 item2
      O.enrollItem node 1.2 item3

      items <- O.enrolledItems node
      shouldBe ((1.1, item2) `elem` items) True
      shouldBe ((1.2, item3) `elem` items) True

    it "sums weights accordingly" $ do
      node <- fmap snd spawnMockedNode
      O.enrollItem node 1.1 item2
      O.enrollItem node 1.2 item2

      items <- O.enrolledItems node
      shouldBe ((2.3, item2) `elem` items) True

-- | Actually execute the promotion action.
performPromote :: Spec
performPromote =
  describe "Actually promoting" $ do
    performPromoteResets
    performPromoteProcessor

-- | Performing a promotion resets the state of enrolled items.
performPromoteResets :: Spec
performPromoteResets =
  it "resets the pending items?" $ do
    node <- fmap snd spawnMockedNode
    O.enrollItem node 1.1 item2

    O.executePromote node
    items <- O.enrolledItems node
    shouldBe items []

-- | Executing a promote action interfaces with a promotion provider.
performPromoteProcessor :: Spec
performPromoteProcessor =
  it "interfaces with the promotion provider" $ do

    (spy, node) <- spawnMockedNode
    O.enrollItem node 1.2 item2

    O.executePromote node

    -- Assert that the mock server has recieved a request of [(2.3, item2)].
    executed <- atomically $ readTVar $ unMock spy
    shouldBe executed [(1.2, item2)]

-- | A data type that represents a handle to a mock promotion provider.
data MockProcessorClientState = MockProcessorClientState {
  -- | The total sequence of executePromote calls recorded.
  unMock :: TVar [(Double, PromotableItem)]
  }

-- | a function to initialize a new mock promotion provdier client.
spawnMockProcessorClient :: IO (MockProcessorClientState, PC.PromotionProcessorClient)
spawnMockProcessorClient = do
  state <- MockProcessorClientState <$> newTVarIO []
  return (state, PromotionProcessorClient {
    -- Just append the revieved items+weights.
    PC.executePromote = \iws -> atomically $ modifyTVar (unMock state) (++ iws),
    listHighScore = error "not implemented."
  })

spawnMockedNode :: IO (MockProcessorClientState, Node)
spawnMockedNode = do
  (spy, mockProcessorClient) <- spawnMockProcessorClient
  es <- memoryBackedEventStore
  return (spy, B.spawnNode es mockProcessorClient)
