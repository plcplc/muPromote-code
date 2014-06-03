-- | This is the main module of the unit test suite.
module Main where

import MuPromote.Test.Unit.Runner(runSpec)

-- | Promotion provider tests
import MuPromote.Test.Unit.PromotionProcessor (providerSpecs)
import MuPromote.Test.Unit.PromotionProcessorResource (providerResourceSpecs)
import MuPromote.Test.Unit.PromotionProcessorWeb (providerWebSpecs)

-- | Run the unittest test suite.
main :: IO ()
main = runSpec $ do
  providerSpecs
  providerResourceSpecs
  providerWebSpecs
