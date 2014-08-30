{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module MuPromote.Test.Unit.PromotableItem where

import Control.Applicative
import qualified Data.Aeson as DA
import qualified Data.HashMap.Lazy as HM
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LBS
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Monadic
import System.Timeout

import MuPromote.Common.PromotableItem

promotableItemSpec :: Spec
promotableItemSpec =
  -- We'd better do some profiling. Limiting size like this may be detrimental to test quality.
  describe "PromotableItems" $ modifyMaxSize (const 20) $
    prop "have isomorphic Aeson serialization" $ \ (p :: PromotableItem) ->
      (not $ isDataAtom p) ==> (monadicIO $ do
        -- Make a test time out after 10 seconds.
        res <- run $ timeout 100000000 $ do
          let r = case codec p of
                    Nothing  -> False
                    Just (p' :: PromotableItem) -> p' == p
          seq r (return r)
        assert $ maybe False id res)

  where
    codec = DA.decode . DA.encode

instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary

instance Arbitrary AtomData where
  arbitrary = LBS.pack <$> arbitrary

instance Arbitrary PromotableItem where

  arbitrary = sized $ \size -> do
    constructor <- elements [0 :: Int ..2]
    case constructor of
      0 -> PIOKV . HM.fromList <$> resize (div size 2) arbitrary
      1 -> PIOList <$> resize (div size 2) arbitrary
      2 -> oneof [
            textAtom <$> arbitrary,
            boolAtom <$> arbitrary,
            numberAtom . fromInteger <$> arbitrary,
            PIODataAtom <$> arbitrary <*> arbitrary ]
      _ -> error "Impossible case"

  shrink (PIOKV l) = HM.elems l
  shrink (PIOList l) = l
  shrink (PIODataAtom _ _) = []
