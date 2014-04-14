-- | This is the main module of the unit test suite.
module Main where

import MuPromote.Test.Unit.Runner(runSpec)

-- | Promotion provider tests
import MuPromote.Test.Unit.PromotionProvider (providerSpecs)
import MuPromote.Test.Unit.PromotionProviderResource (providerResourceSpecs)
import MuPromote.Test.Unit.PromotionProviderWeb (providerWebSpecs)

-- | Run the unittest test suite.
main :: IO ()
main = runSpec $ do
  providerSpecs
  providerResourceSpecs
  providerWebSpecs
