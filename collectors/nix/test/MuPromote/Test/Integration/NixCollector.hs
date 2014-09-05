module MuPromote.Test.Integration.NixCollector (
  collectInstalledSpecs
  ) where

import Data.Maybe
import Test.Hspec
import Test.Hspec.Expectations.Contrib

import MuPromote.Collector.Nix

collectInstalledSpecs :: Spec
collectInstalledSpecs =
  describe "The nix-expression collector" $ do
    it "Parses the json output of nix-env" $ do
      v <- nixEnvSystem "nix-env"
      shouldSatisfy v isJust
    it "Divines promotable items from the output of nix-env" $ do
      Just v <- nixEnvSystem "nix-env"
      let eitherItems = nixEnvPromotableItems v
      shouldSatisfy (eitherItems >> return "<items>") isRight
      let Right items = eitherItems
      mapM_ ((`shouldSatisfy` isRight) . (>> return "<item>")) items
