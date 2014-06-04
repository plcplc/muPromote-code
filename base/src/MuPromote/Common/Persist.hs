{-# LANGUAGE GADTs #-}
-- | This module houses the persitence backend for node operations. It
-- supports appending events and caching reports.
module MuPromote.Common.Persist
  (
    EventStore,
    Report,
    appendEvent,
    dirBackedEventStore,
    memoryBackedEventStore,
    evalReport,
    lookupEvent,
    registerReport
  )
  where

import Control.Applicative
import Control.Arrow
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.Map as M
import Data.Maybe
import Data.SafeCopy
import qualified Data.Serialize as DS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import System.Directory
import System.IO.Error

-- | The type for persistance stores. It supports appending events and
-- evaluating reports, which are like cached database queries restricted
-- to associative functions on the set of events. Note that an EventStore
-- captures only the persisting-interface and does not give any type or
-- serialization guarantees. While the backend interface is agnostic of
-- serialization infrastructure, the rest of this api uses SafeCopy.
data EventStoreBackend = EventStoreBackend {
  esAppendEvent :: BS.ByteString -> IO (),
  esNumEvents   :: IO Int,
  esLookupEvent :: Int -> IO (Maybe BS.ByteString),
  -- | Read a stored report (identified by name), represented as (evNum, state).
  esReadReport  :: String -> IO (Maybe (Int, BS.ByteString)),
  -- | Store a report evaluation, represented as (evNum, state).
  esStoreReport :: String -> (Int, BS.ByteString) -> IO ()
  }

newtype EventStore a = ES { unES :: EventStoreBackend }

-- | The type of report functions, i.e. aggregations on the event log.
-- Reports are evaluated in the context of an event store.
-- Report functions are always associative.
data Report a st = (SafeCopy a, SafeCopy st) => Report {
  repES     :: EventStore a,
  repName   :: String,
  repInitSt :: st,
  repFn     :: st -> a -> st
  }

-- | Open an EventStore, creating it if it doesn't exist. The given
-- filepath should refer to a directory.
-- CURRENTLY NOT THREADSAFE.
dirBackedEventStore :: SafeCopy a => FilePath -> IO (EventStore a)
dirBackedEventStore dir = do
  let eventDir  = dir ++ "/events"
  let reportDir = dir ++ "/reports"
  mapM_ (createDirectoryIfMissing True) [eventDir, reportDir]

  return . ES $ EventStoreBackend {

    esAppendEvent = \bs -> do
      fileName <- fmap show (numProperDirChildren eventDir)
      BS.writeFile (eventDir ++ "/" ++ fileName) bs,

    esNumEvents = numProperDirChildren eventDir,

    esLookupEvent = \num -> exn2Maybe isDoesNotExistError $
      BS.readFile $ eventDir ++ "/" ++ show num,

    esReadReport = \reportName -> exn2Maybe isDoesNotExistError $ do
      let encReportName = encBase64 reportName
      either error id . DS.decode <$> BS.readFile (reportDir ++ "/" ++ encReportName),

    esStoreReport = \reportName contents -> do
      let encReportName = encBase64 reportName
      BS.writeFile (reportDir ++ "/" ++ encReportName) (DS.encode contents)

    }

  where

    -- | Count the proper children of a directory, i.e. disregarding '.'
    -- and '..'.
    numProperDirChildren :: FilePath -> IO Int
    numProperDirChildren = fmap ((+ negate 2) . length) . getDirectoryContents

    -- | 'try', specialised to IOException.
    tryIOEx :: IO a -> IO (Either IOException a)
    tryIOEx = try

    -- | Convert certain thrown exceptions to Maybe-actions.
    exn2Maybe :: (IOException -> Bool) -> IO a -> IO (Maybe a)
    exn2Maybe exPred act =
      either (\ex -> if exPred ex then Nothing else throw ex) Just
        <$> tryIOEx act

    -- | Base64-encoding for strings.
    encBase64 :: String -> String
    encBase64 = T.unpack . T.decodeUtf8 . B64.encode . T.encodeUtf8 . T.pack

-- | An 'EventStore a', backed by MVars.
memoryBackedEventStore :: SafeCopy a => IO (EventStore a)
memoryBackedEventStore = do
  reportsMV <- newMVar M.empty
  actionsMV <- newMVar []
  return $ ES EventStoreBackend {
    esAppendEvent = \bs -> modifyMVar_ actionsMV (return . (bs:)),
    esNumEvents = fmap length (readMVar actionsMV),
    esLookupEvent = \ix -> do
      evs <- readMVar actionsMV
      -- events are reversed relative to list indexing
      return $ case drop (length evs - succ ix) evs of
        [] -> Nothing
        ev:_ -> Just ev,
    esReadReport = \reportName -> fmap (M.lookup reportName) (readMVar reportsMV),
    esStoreReport = \reportName val -> modifyMVar_ reportsMV (return . M.insert reportName val)
    }

-- | Append an event to an EventStore.
appendEvent :: SafeCopy a => EventStore a -> a -> IO ()
appendEvent (ES esBackend) = esAppendEvent esBackend . DS.runPut . safePut

registerReport ::
  (SafeCopy a, SafeCopy st)
  => EventStore a -> String -> st -> (st -> a -> st) -> Report a st
registerReport = Report

numEvents :: EventStore a -> IO Int
numEvents = esNumEvents . unES

lookupEvent :: SafeCopy a => EventStore a -> Int -> IO (Maybe a)
lookupEvent (ES esBackend) = ((partialDeserialise <$>) <$>) . esLookupEvent esBackend

-- | Ignore deserialization errors, throwing exceptions instead of yielding Either.
partialDeserialise :: SafeCopy a => BS.ByteString -> a
partialDeserialise =
  either (error "Deserialization error") id . DS.runGet safeGet

-- | Evaluate a report. If any events have entered the store since last
-- evaluation the stored report is updated.
evalReport :: (SafeCopy a, SafeCopy st) => Report a st -> IO st
evalReport report = do
  let es = repES report
  -- Extract the latest evaluation with index. If esReadReport yields
  -- Nothing the report has yet to be evaluated, and iteration starts from
  -- 0 and the inital state.
  (nextIx, latestEval) <-
    maybe (0, repInitSt report) (succ *** partialDeserialise)
    <$> esReadReport (unES es) (repName report)
  lastIx <- pred <$> numEvents es
  -- Iterate from the index after latestIx until the last presently
  -- recorded.
  res <- foldM (iterateEvalReport es) latestEval [nextIx .. lastIx]
  esStoreReport (unES es) (repName report) (lastIx, DS.runPut $ safePut res)
  return res

  where

    iterateEvalReport es st ix = do
      ev <- fromMaybe (error $ "Non-existing event " ++ show ix ++ " in evalReport")
        <$> lookupEvent es ix
      return (repFn report st ev)

