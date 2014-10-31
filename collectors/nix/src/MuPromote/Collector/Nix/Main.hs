module MuPromote.Collector.Nix.Main where

import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Trans.Error
import qualified Data.Text as T
import MuPromote.Common.PromotableItem
import MuPromote.Common.NodeSignature
import MuPromote.Collector.Nix
import Network.HTTP.Rest.Client
import Network.HTTP.Rest.Client.HttpClient
import Network.HTTP.Client

collectItems :: ErrorT String IO [PromotableItem]
collectItems = do
  nixEnvJson <- maybeErrorT "nix-env output did not parse as valid JSON" =<<
    (lift $ nixEnvSystem "/run/current-system/sw/bin/nix-env")
  itemsEither <- ErrorT . return $ nixEnvPromotableItems nixEnvJson
  sequence $ map (ErrorT . return) itemsEither

postItems :: String -> [PromotableItem] -> ErrorT String IO ()
postItems url items = do
  let wItems = zip items [1,1..]
  req <- parseUrl url
  res <- ErrorT $ Right <$> (
    requestHttpClientDef req postEnrolledItemsSig wItems
    )
  case res of
    ResponseNoParse -> throwError "ResponseNoParse"
    Success x -> return x

maybeErrorT :: (Error e, Monad m) => e -> Maybe a -> ErrorT e m a
maybeErrorT errNothing mX = maybe (throwError errNothing) return mX
