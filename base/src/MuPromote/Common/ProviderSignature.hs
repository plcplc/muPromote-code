{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
-- | This module defines the Rest signatures that make up the web interface to
-- promotion providers.
module MuPromote.Common.ProviderSignature where

import MuPromote.Common.PromotableItem (PromotableItem)
import Network.HTTP.Rest.Signature

-- | The http api for executePromote
executePromoteSig :: RestSig
  (S "test" :/: S "executePromote" :/: Nil)
  ('HttpPost [(Double, PromotableItem)] ())

executePromoteSig = RestResource

-- | The http api for highScore
highScoreSig :: RestSig
    (S "test" :/: S "highScore" :/: Nil)
    ('HttpGet [(Double, PromotableItem)])

highScoreSig = RestResource
