-- | This module hosts the types and typeclasses that define the clientside
-- interface to promotion providers.
module MuPromote.Node.PromotionProcessorClient
  (
  nilProcessorClient,
  httpProcessorClient,
  PromotionProcessorClient(..)
  )where

import qualified Data.Map as M
import Data.Text as T
import Network.HTTP.Client

import MuPromote.Common.PromotableItem (PromotableItem)
import MuPromote.Common.ProcessorSignature
import Network.HTTP.Rest.Client
import Network.HTTP.Rest.Client.HttpClient

-- | A datatype that contains actions for interacting with a particular promotion provider.
data PromotionProcessorClient = PromotionProcessorClient {

  -- | Execute (register) the promotion of a set of items with weights.
  executePromote :: M.Map PromotableItem Double -> IO (),

  -- | List the total of registered promotions
  listHighScore :: IO [(PromotableItem, Double)]

  }

-- | A trivial provider client that does nothing.
nilProcessorClient :: PromotionProcessorClient
nilProcessorClient = PromotionProcessorClient (const (return ())) (return [])

-- | A provider client using HTTP.
httpProcessorClient :: ManagerSettings -> Text -> IO PromotionProcessorClient
httpProcessorClient managerSettings hostname =
  return PromotionProcessorClient {
    executePromote = \items -> do
      manager <- newManager managerSettings
      req <- parseUrl ("http://" ++ T.unpack hostname)
      Success res <- requestHttpClient manager req executePromoteSigPx (M.toList items)
      closeManager manager
      return res,
    listHighScore = do
      manager <- newManager managerSettings
      req <- parseUrl ("http://" ++ T.unpack hostname)
      Success res <- requestHttpClient manager req highScoreSigPx
      closeManager manager
      return res
    }
