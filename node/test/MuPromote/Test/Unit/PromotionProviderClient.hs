{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
-- This module tests that the MuPromote http based
-- PromotionProviderClient behaves as expected.
module MuPromote.Test.Unit.PromotionProviderClient ( promotionProviderClientSpecs ) where

import Control.Concurrent
import Control.Exception
import Control.Monad.IO.Class

import Network.HTTP.Types.Header
import Network.HTTP.Types.Status
import Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

import Prelude hiding (log)
import Test.Hspec

import MuPromote.Common.HTTP.Client ( socketManagerSettings )
import MuPromote.Common.PromotableItem
import System.EncapsulatedResources

import System.EncapsulatedResources.Test.EncapsulatedResources
import MuPromote.Test.Unit.PromotableItem ( item2 )

-- | AUT includes
import qualified MuPromote.Node.PromotionProviderClient as PPC
import MuPromote.Common.ProviderSignature
import Network.HTTP.Rest.Server

promotionProviderClientSpecs :: Spec
promotionProviderClientSpecs =
  describe "The http promotion provider client" $ do
    let hostname = "testprovider"
    let highScoreData = [(42, item2)]

    it "can connect to the highscore rest resource" $ testResources 1000 (\log -> do

      -- A communication socket, destroyed when signalled to.
      resource "socket resource holder" $ do
        testSockR <- sockRes "testSock"
        p <- parentRes
        myRh <- currentRes
        send p (myRh, testSockR)
        recieve [Match (\ () -> return ())]
        destroyRes

      (sockHolderRh, testSockR) <- recieve [Match (\ (holderRh,rh) -> return (holderRh, rh))]

      -- A resource for the client
      clientR <- resource "provider client" $ do

        recieve [ Match (\ () -> return ()) ]
        sock <- getConnectedSock testSockR

        liftIO $ do
          pcc <-  PPC.httpProviderClient (socketManagerSettings sock) hostname
          res <- handle (\(SomeException a)-> log (show a) >> return [])
            (PPC.listHighScore pcc)
          log $ show res
        destroyRes

      -- A resource for the server
      resource "provider server" $ do

        sock <- getBoundSock testSockR
        notifyClient <- sendIO clientR

        liftIO $ do

          exitMV <- newEmptyMVar

          thdId <- forkIO $
            Warp.runSettingsSocket (
            Warp.setBeforeMainLoop (notifyClient ()) $ Warp.setOnClose (\_ -> putMVar exitMV ()) Warp.defaultSettings)
            sock (mockProvider log highScoreData)

          -- Kill the server once it has served its request
          takeMVar exitMV
          log "Closing"
          killThread thdId

        -- signal the socket holder resource to exit.
        send sockHolderRh ()

        destroyRes

      awaitChildren )

      (\logCh -> do
        logL <- logToList logCh

        -- Assert that
        -- * the server recieved a request addressed to it,
        -- * that the client recieved its data from the server,
        -- * and that the server wilfully exited.
        shouldContain logL [show ("Host" :: String, hostname), show highScoreData, "Closing"]
      )
  where

-- | The good old 404.
notFoundResp :: Wai.Response
notFoundResp = responseLBS status404 [(hContentType, "text/plain")] "Not found."

-- | A mock promotion provider that logs recieved requests.
mockProvider :: (String -> IO ()) ->  [(Double, PromotableItem)] -> Application
mockProvider log highScore req = do

  -- reqBody <- lazyRequestBody req
  -- log $ "Request" ++ (show $ (requestMethod req, httpVersion req, requestHeaders req, rawPathInfo req, reqBody))

  log $ show $ head $ Wai.requestHeaders req

  serveRest
    highScoreSig
    (\ () -> return (status200, [(hContentType, "application/json")], highScore))
    (const $ return notFoundResp)
    req

