{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module houses the networked interface to the MuPromote node.
module MuPromote.Node.Web (

  -- * the WAI-Application for the Node
  nodeApiApp,
  ) where

import Control.Applicative
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as LBS
import Data.Monoid
import qualified Data.Map as M

import MuPromote.Common.NodeSignature

import Network.HTTP.Rest.Server

import MuPromote.Node.Base (Node)
import MuPromote.Node.Operations (enrollItem, enrolledItems)

-- | The WAI Application that exposes the Node over http. Yields 404 on no
-- matches.
nodeApiApp :: Node -> PartialApplication IO LBS.ByteString
nodeApiApp node =
    serve getEnrolledItemsSig
      (M.toList <$> (liftIO $ enrolledItems node))
        <>
    serve postEnrolledItemsSig
      (\ wItms -> liftIO $ mapM_ (uncurry $ enrollItem node) wItms)
