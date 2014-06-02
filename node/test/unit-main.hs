-- | This is the main module of the unit test suite.
module Main where

import MuPromote.Test.Unit.Runner(runSpec)

-- | Common tests
import MuPromote.Test.Unit.PromotableItem (promotableItemSpecs)

-- | Node tests
import MuPromote.Test.Unit.Operations (operationsSpecs)
import MuPromote.Test.Unit.NodeResource (nodeResourceSpecs)
import MuPromote.Test.Unit.PromotionProcessorClient (promotionProcessorClientSpecs)
import MuPromote.Test.Unit.Persist (persistSpec)

-- | Run the unittest test suite.
main :: IO ()
main = runSpec $ do
  promotableItemSpecs
  operationsSpecs
  nodeResourceSpecs
  promotionProcessorClientSpecs
  persistSpec
