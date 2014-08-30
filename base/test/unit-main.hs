-- | This is the main module of the unit test suite.
module Main where

import MuPromote.Test.Unit.Runner(runSpec)

-- | Node tests
import MuPromote.Test.Unit.Persist (persistSpec)
import MuPromote.Test.Unit.PromotableItem (promotableItemSpec)

-- | Run the unittest test suite.
main :: IO ()
main = runSpec $ do
  persistSpec
  promotableItemSpec
