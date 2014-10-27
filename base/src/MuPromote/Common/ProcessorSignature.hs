{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
-- | This module defines the Rest signatures that make up the web interface to
-- promotion providers.
module MuPromote.Common.ProcessorSignature where

import Data.Proxy
import Network.HTTP.Rest.Signature
import Network.HTTP.Rest.Encoding.JSON

import MuPromote.Common.PromotableItem (PromotableItem)

-- | The http api for executePromote
type ExecutePromoteSig = Proxy (RestSig
  (S "test" :/: S "executePromote" :/: Nil)
  ('HttpPost [(PromotableItem, Double)] ()) JSONEncoding :: *)

executePromoteSigPx :: ExecutePromoteSig
executePromoteSigPx = Proxy

-- | The http api for highScore
type HighScoreSig = Proxy (RestSig
    (S "test" :/: S "highScore" :/: Nil)
    ('HttpGet [(PromotableItem, Double)]) JSONEncoding :: *)

highScoreSigPx :: HighScoreSig
highScoreSigPx = Proxy
