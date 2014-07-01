module MuPromote.Test.Integration.NixCollector (
  collectInstalledSpecs
  ) where

import Control.Monad
import Test.Hspec

import MuPromote.Collector.Nix

collectInstalledSpecs :: Spec
collectInstalledSpecs =
  describe "The nix-expression collector" $
    it "successfully parses the json output of nix-env" $
      void collectNixEnv
