{-# LANGUAGE OverloadedStrings #-}
-- | This is the main module of the unit test suite.
module Main where

import MuPromote.Test.Unit.Runner(runSpec)

-- | Node tests
import MuPromote.Test.Unit.Operations (operationsSpecs)
import MuPromote.Test.Unit.NodeResource (nodeResourceSpecs)
import MuPromote.Test.Unit.PromotionProcessorClient (promotionProcessorClientSpecs)

-- | Run the unittest test suite.
main :: IO ()
main = runSpec $ do
  operationsSpecs
  nodeResourceSpecs
  promotionProcessorClientSpecs
