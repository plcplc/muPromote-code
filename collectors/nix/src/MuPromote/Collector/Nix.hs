{-# LANGUAGE OverloadedStrings #-}
module MuPromote.Collector.Nix
  (
    nixEnvSystem,
    nixEnvPromotableItems
  )
  where

import Data.Aeson as DA
import Data.Aeson.Types
import qualified Data.HashMap.Lazy as HM
import Data.Maybe
import Data.Monoid
import Control.Monad
import Control.Applicative
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import MuPromote.Common.PromotableItem
import System.Process

-- Code that actually belongs here:

-- | A newtype wrapper around PromotableItem to make a 'FromJSON' instance
-- that does not conflict with the one for 'PromotableItem', and is
-- specialised to extracting the relevant parts of the output from nix-env.
newtype NixEnvPI = NixEnvPI { unNixEnvPI :: PromotableItem }

instance FromJSON NixEnvPI where
  parseJSON (Object o) = do

    name <- o .: "name"
    meta <- o .: "meta"
    -- A cursory glance on the union of the "meta" attrset of the installed
    -- nix packages on my system reveals these keys:
    -- ["position", "license", "description", "platforms", "homePage",
    -- "maintainers", "homepage", "branch", "longDescription"]
    description <- fieldSubError name "description" (meta .:?)
    license <- parseLicense name meta
    maintainers <- fieldSubError name "maintainers" (meta .:?)
    url <- getFirst . mconcat <$> sequence
      [First <$> fieldSubError name "homepage" (meta .:?),
       First <$> fieldSubError name "homePage" (meta .:?)]
    return . NixEnvPI $
      piObject $
      [
        ("Name", textAtom name),
        ("Category Tags", piList $ map textAtom ["Software package", "Nix package expression"])
      ] ++
        catMaybes [
          (,) "Description" . textAtom <$> description,
          (,) "License" . textAtom <$> license,
          (,) "Url" . textAtom <$> url,
          (,) "Maintainers" . piList . map textAtom <$> maintainers
        ]

    where

      -- In the wonderful, untyped world of nixpkgs, licences are sometimes
      -- a string, sometimes a more elaborate object with links and
      -- abbreviations..
      parseLicense :: T.Text -> Object -> Parser (Maybe T.Text)
      parseLicense n pkgObj =
        modifyFailure (("(" ++ T.unpack n ++ ") Parsing of field 'license' failed: ") ++) $
          (Just <$> pkgObj .: "license") <|>
          (pkgObj .: "license" >>= (.:? "ShortName")) <|>
          pure Nothing

      fieldSubError :: T.Text -> T.Text -> (T.Text -> Parser a) -> Parser a
      fieldSubError n f p =
        modifyFailure (("(" ++ T.unpack n ++ ") Parsing of field '" ++ T.unpack f ++ "' failed: ") ++) (p f)

  parseJSON _ = mzero


str2bs :: String -> LBS.ByteString
str2bs = LBS.fromStrict . TE.encodeUtf8 . T.pack

-- | Collect the set of installed "packages" using nix-env. The argument is
-- the path of the 'nix-env' executable.
nixEnvSystem :: FilePath -> IO (Maybe Value)
nixEnvSystem nixEnv =
  DA.decode . str2bs
    <$> readProcess nixEnv args ""
  where
    args = ["--query", "--installed", "--meta", "--json"]

-- | Parse the values of an object.
jsObjVals :: Value -> Parser [Value]
jsObjVals = withObject "Value is not an object" (return . HM.elems)

-- | Transform the json output of nix-env into promotable items.
nixEnvPromotableItems :: Value -> Either String [Either String PromotableItem]
nixEnvPromotableItems = parseEither jsObjVals >=> return . map (fmap unNixEnvPI . parseEither parseJSON)


