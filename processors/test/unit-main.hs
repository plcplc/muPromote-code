-- | This is the main module of the unit test suite.
module Main where

import MuPromote.Test.Unit.Runner(runSpec)

-- | Promotion processor tests
import MuPromote.Test.Unit.PromotionProcessor (processorSpecs)
import MuPromote.Test.Unit.PromotionProcessorResource (processorResourceSpecs)

-- | Run the unittest test suite.
main :: IO ()
main = runSpec $ do
  processorSpecs
  processorResourceSpecs
