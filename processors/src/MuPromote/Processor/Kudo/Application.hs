{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module houses the networked interface to the kudo promotion processor.
module MuPromote.Processor.Kudo.Application (

  -- * The resource signatures of the Kudo processor web service.
  executePromoteSig,
  highScoreSig,

  -- * The Kudo provides encoded as a WAI middleware.
  kudoApiApp

  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Monoid

import Network.HTTP.Types.Header
import Network.HTTP.Types.Status

import MuPromote.Common.ProcessorSignature
import Network.HTTP.Rest.Server
import MuPromote.Processor.Kudo
import MuPromote.Processor.Kudo.Operations

kudoApiApp :: KudoProcessor -> PartialApplication
kudoApiApp kudo =
  serveRest executePromoteSig
    (\wItms -> do
      res <- liftIO $ executePromote kudo wItms
      return (created201, [(hLocation, "/test/highScore")], res))
  <> serveRest highScoreSig
      (\() -> do
        res <- liftIO $ listHighscore kudo
        return (status200, [(hContentType, "application/json")], res))
