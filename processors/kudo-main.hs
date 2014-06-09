{-# LANGUAGE ScopedTypeVariables #-}
-- | This is the Main module for the Kudo promotion processor.
module Main where

import Control.Applicative
import Control.Monad
import Data.Monoid
import System.Environment
import System.EncapsulatedResources

import qualified MuPromote.Processor.Kudo.Main as Kudo

-- | The 'main' action initializes the Node instance 'Resource' context that
-- 'resourceMain' lives inside.
main :: IO ()
main = do

  -- Check args. If none given, explode.
  [resFP] <- getArgs

  runResourceM def { rpsRootDir = Just resFP }

    "muPromote-kudo" $ do

    -- Logger that outputs to stdout.
    logRh <- resource "Log-Resource" $
      forever $ recieve [ Match $ \(l :: Kudo.EventLog) -> liftIO $ print l ]

    send logRh (Kudo.LogDebug "Dafoos")

    dirRes "WebUI-Dir" (return ())
    dirRes "Storage" (return ())

    Kudo.resourceMain
