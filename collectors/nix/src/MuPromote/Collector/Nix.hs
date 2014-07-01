module MuPromote.Collector.Nix where

import Control.Applicative
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe
import Data.Text as T
import Data.Text.Encoding as TE
import System.Process

str2bs :: String -> LBS.ByteString
str2bs = LBS.fromStrict . TE.encodeUtf8 . T.pack

collectNixEnv :: IO Value
collectNixEnv = do
  jsBS <- str2bs <$> readProcess "nix-env" ["--query", "--installed", "--meta", "--json"] ""
  case decode jsBS of
    Just x -> return x
    Nothing -> error "unable to parse the output from nix-env"
