{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
-- This module tests that the MuPromote http based
-- PromotionProcessorClient behaves as expected.
module MuPromote.Test.Unit.PromotionProcessorClient ( promotionProcessorClientSpecs ) where

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
import qualified MuPromote.Node.PromotionProcessorClient as PPC
import MuPromote.Common.ProcessorSignature
import Network.HTTP.Rest.Server
import Network.HTTP.Rest.Server.Wai

promotionProcessorClientSpecs :: Spec
promotionProcessorClientSpecs =
  describe "The http promotion provider client" $ do
    let hostname = "testprovider"
    let highScoreData = [(item2, 42)]

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
          pcc <-  PPC.httpProcessorClient (socketManagerSettings sock) hostname
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
            sock (mockProcessor log highScoreData)

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
-- notFoundResp :: Wai.Response
-- notFoundResp = responseLBS status404 [(hContentType, "text/plain")] "Not found."

-- | A mock promotion provider that logs recieved requests.
mockProcessor :: (String -> IO ()) ->  [(PromotableItem, Double)] -> Application
mockProcessor log highScore req respCont = do

  -- reqBody <- lazyRequestBody req
  -- log $ "Request" ++ (show $ (requestMethod req, httpVersion req, requestHeaders req, rawPathInfo req, reqBody))

  log $ show $ head $ Wai.requestHeaders req

  runPWA (liftPA $ serve highScoreSigPx
    (return highScore :: IO [(PromotableItem, Double)]))
    req respCont

