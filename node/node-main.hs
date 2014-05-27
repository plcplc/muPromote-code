-- | This is the Main module for the Node.
module Main where


import Control.Applicative
import Data.Monoid
import System.Environment
import System.EncapsulatedResources

import qualified MuPromote.Node.Main as Node

-- | The 'main' action initializes the Node instance 'Resource' context that
-- 'mainComposed' lives inside. If the resource is empty/doesn't exist it
-- creates it in the surrounding OS (eg by creating a ~/.muPromoteNode/ dir etc. as
-- determined by the 'Resource's library).
--
-- On linux this function just initializes the node resource in the users
-- XDG_DATA_HOME, usually '~/.local/share'.
main :: IO ()
main = do

  -- Check args. If none given, use xdg_data_home.
  resFP <- getFirst . mconcat . map First <$> sequence
    [ headMaybe <$> getArgs,
      lookupEnv "XDG_DATA_HOME",
      -- The default per absence of XDG_DATA_HOME, as per standard:
      -- http://standards.freedesktop.org/basedir-spec/basedir-spec-latest.html
      fmap (++ "/.local/share") <$> lookupEnv "HOME"
    ]

  runResourceM def { rpsRootDir = resFP }
    "muPromote-node" $ do

    dirRes "WebUI-Dir" (return ())
    dirRes "Storage" (return ())

    Node.resourceMain

headMaybe :: [a] -> Maybe a
headMaybe []    = Nothing
headMaybe (x:_) = Just x
