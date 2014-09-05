-- | This is the main module of the test suite.
module Main where

import Test.Hspec

import MuPromote.Test.Unit.Runner(runSpec)

-- | Common tests
-- import MuPromote.Test.Unit.PromotableItem (promotableItemSpecs)

-- | Node tests
import MuPromote.Test.Integration.NixCollector (collectInstalledSpecs)

-- | Run the test suite.
main :: IO ()
main = runSpec $
  describe "Integration tests (against the current host system)"
    collectInstalledSpecs
