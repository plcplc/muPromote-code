-- | This is the Main module for the Node.
module Main where


import Control.Applicative
import Data.Monoid
import System.Environment
import System.EncapsulatedResources
import System.Process

import qualified MuPromote.Node.Main as Node

main :: IO ()
main = do

  system "rm -rf node-devel-deployment"
  system "mkdir -p node-devel-deployment/muPromote-node"
  system "cp -rv node-ui/ node-devel-deployment/muPromote-node/WebUI-Dir"

  runResourceM def { rpsRootDir = Just "node-devel-deployment" }
    "muPromote-node" $ do

    dirRes "WebUI-Dir" (return ())
    dirRes "Storage" (return ())

    Node.resourceMain

headMaybe :: [a] -> Maybe a
headMaybe []    = Nothing
headMaybe (x:_) = Just x
