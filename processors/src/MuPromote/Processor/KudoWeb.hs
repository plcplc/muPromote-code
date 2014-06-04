{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module houses the networked interface to the kudo promotion processor.
module MuPromote.Processor.KudoWeb (

  -- * The resource signatures of the Kudo processor web service.
  executePromoteSig,
  highScoreSig,

  -- * The Kudo provides encoded as a WAI 'Application'.
  kudoApp

  ) where

import Control.Monad.IO.Class (liftIO)

import Network.HTTP.Types.Header (hContentType, hLocation)
import Network.HTTP.Types.Status (status200, created201, status404)

import Network.Wai (Application, Response, responseLBS)

import MuPromote.Common.ProcessorSignature
import Network.HTTP.Rest.Server
import MuPromote.Processor.Kudo

kudoApp :: IO Application
kudoApp = do
  kudo <- spawnKudo
  return $ -- begs for continuation monad or something..
    serveRest executePromoteSig
      (\wItms -> do
        res <- liftIO $ executePromote kudo wItms
        return (created201, [(hLocation, "/test/highScore")], res)
        )
      (serveRest highScoreSig
        (\() -> do
          res <- liftIO $ listHighscore kudo
          return (status200, [(hContentType, "application/json")], res)
          )
        (\_ -> return notFoundResp))

-- | The good old 404.
notFoundResp :: Response
notFoundResp = responseLBS status404 [(hContentType, "text/plain")] "Not found."
