{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module houses the networked interface to the MuPromote node.
module MuPromote.Node.Web (

  -- * the WAI-Application for the Node
  nodeApiApp,
  nodeApiMiddleware
  ) where

import Control.Monad.IO.Class (liftIO)

import Network.HTTP.Types.Header (hContentType, hLocation)
import Network.HTTP.Types.Status (status200, created201)

import Network.Wai (Application, Middleware)
import Network.Wai.Utils

import MuPromote.Common.NodeSignature
import Network.HTTP.Rest.Server

import MuPromote.Node.Base (Node)
import MuPromote.Node.Operations (enrollItem, enrolledItems)

-- | The WAI Middleware that exposes the Node over http. Delegates to sub app
-- on no matches.
nodeApiMiddleware :: Node -> Middleware
nodeApiMiddleware node app =
  serveRest postEnrolledItemsSig
    (\ wItms -> do
      liftIO $ mapM_ (uncurry $ enrollItem node) wItms
      return (created201, [(hLocation, "/test/enrolledItems")], ())
      )
    (serveRest getEnrolledItemsSig
      (\ () -> do
        res <- liftIO $ enrolledItems node
        return (status200, [(hContentType, "application/json")], res)
        )
      app
    )

-- | The WAI Application that exposes the Node over http. Yields 404 on no
-- matches.
nodeApiApp :: Node -> IO Application
nodeApiApp node = do
  let mid = nodeApiMiddleware node
  return $ mid (const (return notFoundResp))
