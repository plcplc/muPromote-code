-- | This is the main module of the unit test suite.
module Main where

import MuPromote.Test.Unit.Runner(runSpec)

-- | Node tests
import MuPromote.Test.Unit.Persist (persistSpec)

-- | Run the unittest test suite.
main :: IO ()
main = runSpec
  persistSpec
