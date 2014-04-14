-- | This module hosts the types and typeclasses that define the clientside
-- interface to promotion providers.
module MuPromote.Node.PromotionProviderClient
  (
  nilProviderClient,
  httpProviderClient,
  PromotionProviderClient(..)
  )where

import Data.Text
import Network.HTTP.Client (closeManager, newManager, ManagerSettings)

import MuPromote.Common.PromotableItem (PromotableItem)
import MuPromote.Common.ProviderSignature (executePromoteSig, highScoreSig)
import Network.HTTP.Rest.Client

-- | A datatype that contains actions for interacting with a particular promotion provider.
data PromotionProviderClient = PromotionProviderClient {

  -- | Execute (register) the promotion of a set of items with weights.
  executePromote :: [(Double, PromotableItem)] -> IO (),

  -- | List the total of registered promotions
  listHighScore :: IO [(Double, PromotableItem)]

  }

-- | A trivial provider client that does nothing.
nilProviderClient :: PromotionProviderClient
nilProviderClient = PromotionProviderClient (const (return ())) (return [])

-- | A provider client using HTTP.
httpProviderClient :: ManagerSettings -> Text -> IO PromotionProviderClient
httpProviderClient managerSettings hostname =
  return PromotionProviderClient {
    executePromote = \items -> do
      manager <- newManager managerSettings
      res <- requestResource manager hostname executePromoteSig items
      closeManager manager
      return res,
    listHighScore = do
      manager <- newManager managerSettings
      res <- requestResource manager hostname highScoreSig
      closeManager manager
      return res
    }
