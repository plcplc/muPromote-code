-- vim note: let b:syntastic_checkers = ['hdevtools'] :
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
-- | This module defines the Rest signatures that make up the web api to
-- the muPromote Node.
module MuPromote.Common.NodeSignature (

  -- * Enrolling items
  --
  -- '/test/enrolledItems' is a REST-resource, where POST'ing adds new items to
  -- the list of enrolled items, and GET'ing gets the accumulated list.
  getEnrolledItemsSig, postEnrolledItemsSig

  ) where

import MuPromote.Common.PromotableItem (PromotableItem)
import Network.HTTP.Rest.Signature

-- | The http api for enrolling a list of items.
postEnrolledItemsSig :: RestSig
  (S "test" :/: S "enrolledItems" :/: Nil)
  ('HttpPost [(Double, PromotableItem)] ())

postEnrolledItemsSig = RestResource

-- | The http api for getting the list of enrolled items.
getEnrolledItemsSig :: RestSig
  (S "test" :/: S "enrolledItems" :/: Nil)
  ('HttpGet [(Double, PromotableItem)])

getEnrolledItemsSig = RestResource
