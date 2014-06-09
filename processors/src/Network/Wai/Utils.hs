{-# LANGUAGE OverloadedStrings #-}
-- | This module defines various WAI utilities.
module Network.Wai.Utils where

import Network.HTTP.Types.Header
import Network.HTTP.Types.Status
import Network.Wai

-- | The good old 404.
notFoundResp :: Response
notFoundResp = responseLBS status404 [(hContentType, "text/plain")] "Not found."
