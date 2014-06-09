{-# LANGUAGE OverloadedStrings #-}

-- | This module tests the web interface to promotion processors.
module MuPromote.Test.Unit.PromotionProcessorWeb (
  processorWebSpecs
  ) where

-- | Library includes.
import Data.Aeson (decode, encode)
import Data.Maybe (isJust)
import Test.Hspec
import Network.Wai (Request(..))
import Network.Wai.Test
  (assertContentType, assertHeader, assertStatus, defaultRequest,
  runSession, request, srequest, SResponse, SRequest(..), simpleBody)

-- | AUT includes.
import Network.Wai.Utils
import MuPromote.Common.Persist
import MuPromote.Processor.Kudo as K
import MuPromote.Processor.Kudo.Application as KA
import MuPromote.Common.PromotableItem

-- | Test data includes.
import MuPromote.Test.Unit.PromotableItem (item2, item3)

-- | The specs of processor tests.
processorWebSpecs :: Spec
processorWebSpecs = describe "The processor REST http interface"
  executePromoteKudo

-- | Executing promotions on a kudo processor.
executePromoteKudo :: Spec
executePromoteKudo =
  describe "Receiving promotion execute requests" $
    it "increases the count of promoted items" $ do
      kudo <- spawnMockedProcessor
      let processorApp = KA.kudoApiMiddleware kudo $ const (return notFoundResp)

      maybeHighScore <- runSession (do

        execResp <- srequest $ mkExecuteRequest [(0.5, item2),(0.5, item3)]

        -- Response should be: 'Created, look at /test/highScore for your resource'.
        assertStatus 201 execResp
        assertHeader "Location" "/test/highScore" execResp

        -- (It's a bit abusive to say that the created resource is /test/highScore.
        -- It would be more appropriate to do something like 'GET /test/executePromote/{id : Int}'
        -- and then store the transactions rather than just the total aggregate.)

        -- Check that the returned location indeed hosts the created resource.
        highScoreResp <- request mkHighScoreRequest
        assertStatus 200 highScoreResp
        assertContentType "application/json" highScoreResp

        return $ decodeHighScoreResp highScoreResp

        ) processorApp

      shouldSatisfy maybeHighScore isJust

      let Just highScore = maybeHighScore
      shouldBe ((0.5, item2)`elem` highScore) True
      shouldBe ((0.5, item3) `elem` highScore) True

-- | Synthesize an executedPromotions request
mkExecuteRequest :: [(Double, PromotableItem)] -> SRequest
mkExecuteRequest itemWs = SRequest
  (defaultRequest {
    requestMethod = "POST",
    pathInfo = ["test","executePromote"]
  }) (encode itemWs)

-- | Synthesize a highScore request
mkHighScoreRequest :: Request
mkHighScoreRequest = defaultRequest {
  requestMethod = "GET",
  pathInfo = ["test","highScore"]
  }

decodeHighScoreResp :: SResponse -> Maybe [(Double, PromotableItem)]
decodeHighScoreResp resp = decode $ simpleBody resp

spawnMockedProcessor :: IO KudoProcessor
spawnMockedProcessor = do
  es <- memoryBackedEventStore
  return (K.spawnKudo es)
