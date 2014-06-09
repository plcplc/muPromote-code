-- | This module houses the web service operations that nodes can perform
-- with a kudo processor.
module MuPromote.Processor.Kudo.Operations where

import MuPromote.Common.Persist
import MuPromote.Common.PromotableItem
import MuPromote.Processor.Kudo

-- | Execute a promotion on a kudo instance.
executePromote :: KudoProcessor -> [(Double, PromotableItem)] -> IO ()
executePromote kp = appendEvent (kpEventStore kp) . ExecutedPromoteAction

listHighscore :: KudoProcessor -> IO [(Double, PromotableItem)]
listHighscore kp = evalReport $ registerReport (kpEventStore kp) "listHighscore" [] highscoreReport

  where

    highscoreReport :: [(Double, PromotableItem)] -> ProcessorAction -> [(Double, PromotableItem)]
    highscoreReport acc (ExecutedPromoteAction witems) = mergeItems acc witems


