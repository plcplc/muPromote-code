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
import MuPromote.Processor.KudoWeb as KW
import MuPromote.Common.PromotableItem

-- | Test data includes.
import MuPromote.Test.Unit.PromotableItem (item2, item3)

-- | The specs of processor tests.
processorWebSpecs :: Spec
processorWebSpecs = describe "The processor REST http interface (using kudo for backend incidentally)" $ do
  executePromoteKudo

-- | Executing promotions on a kudo processor.
executePromoteKudo :: Spec
executePromoteKudo = do
  describe "Receiving promotion execute requests" $ do
    it "increases the count of promoted items" $ do
      processorApp <- KW.kudoApp

      maybeHighScore <- runSession (do

        execResp <- srequest $ mkExecuteRequest [(0.5, item2),(0.5, item3)]

        -- Response should be: 'Created, look at /test/highScore for your resource'.
        assertStatus 201 execResp
        assertHeader "Location" "/test/highScore" execResp

        -- (It's a bit abusive to say that the created resource is /test/highScore.
        -- It would be more appropriate to do something like 'GET /test/executePromote/{id : Int}'
        -- and then store the transactions rather than just the total aggregate.)

        -- Check that the returned location indeed hosts the created resource.
        highScoreResp <- request $ mkHighScoreRequest
        assertStatus 200 highScoreResp
        assertContentType "application/json" highScoreResp

        return $ decodeHighScoreResp highScoreResp

        ) processorApp

      shouldSatisfy maybeHighScore isJust

      let Just highScore = maybeHighScore
      shouldBe (elem (0.5, item2) highScore) True
      shouldBe (elem (0.5, item3) highScore) True

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

