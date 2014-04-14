{-# LANGUAGE TemplateHaskell #-}
-- | This module holds the common code relating to the model of software packages employed in MuPromote.
-- These types are specifically used in the client-server API.
module MuPromote.Common.Package.Package where

import Data.Aeson
import Data.Aeson.TH

import Data.Char(toLower)

import MuPromote.Common.Misc

-- | A type representing software packages installable by a package manager.
data Package = Package {
  pkgName :: String,
  pkgUri :: String,
  pkgPackageManager :: Maybe String,
  pkgAttributes :: [(String,String)] -- ^ a raw dump of whatever is provided by the package manager for later serverside analysis
}

$(deriveJSON ((toLower <:> id) . drop 3) ''Package)
