module MuPromote.Collector.Nix.Main where

import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Trans.Error
import qualified Data.Text as T
import MuPromote.Common.PromotableItem
import MuPromote.Common.NodeSignature
import MuPromote.Collector.Nix
import Network.HTTP.Rest.Client

collectItems :: ErrorT String IO [PromotableItem]
collectItems = do
  nixEnvJson <- maybeErrorT "nix-env output did not parse as valid JSON" =<<
    (lift $ nixEnvSystem "/run/current-system/sw/bin/nix-env")
  itemsEither <- ErrorT . return $ nixEnvPromotableItems nixEnvJson
  sequence $ map (ErrorT . return) itemsEither

postItems :: String -> [PromotableItem] -> ErrorT String IO ()
postItems hostName items = do
  let wItems = zip [1,1..] items
   -- Unfortunately requestResourceDef throws exceptions,
   -- and I don't feel I have the time to refactor it/catch them :-(
  ErrorT $ Right <$>
    requestResourceDef (T.pack hostName) postEnrolledItemsSig wItems

maybeErrorT :: (Error e, Monad m) => e -> Maybe a -> ErrorT e m a
maybeErrorT errNothing mX = maybe (throwError errNothing) return mX
