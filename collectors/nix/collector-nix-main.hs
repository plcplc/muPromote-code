module Main where

import System.Environment
import MuPromote.Collector.Nix.Main
import Control.Monad.Trans.Error

-- | This PromotableItem collector for nix expressions simply collect items
-- and post them to a node (currently hardwired in source code).
main :: IO ()
main = do
  [hostName] <- getArgs
  res <- runErrorT $ collectItems >>= postItems hostName
  case res of
    Left err -> print err
    Right () -> print "Success"
