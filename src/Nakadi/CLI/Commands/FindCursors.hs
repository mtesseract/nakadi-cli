-- | Implementation for 'find-cursors' command

{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Nakadi.CLI.Commands.FindCursors (commandSpec) where

import           Nakadi.CLI.Prelude

import           Conduit
import           Control.Lens
import           Control.Monad.Logger
import           Data.Aeson
import qualified Data.Conduit.List    as Conduit.List
import           Data.Time.ISO8601
import qualified Data.Vector          as Vector
import qualified Nakadi
import qualified Nakadi.CLI.Lenses    as L
import           Options.Applicative

data Opts =
  Opts { _optsEventType :: String
       , _optsTimestamp :: String }

data Parameters =
  Parameters { eventType :: Nakadi.EventTypeName
             , timestamp :: UTCTime
             } deriving (Show)

commandSpec :: Command
commandSpec = Command
  { _name        = "find-cursors"
  , _description = "Try to translate a timestamp to a cursor"
  , _parser      = parser }

parser :: Parser Invocation
parser = do
  opts <- Opts
          <$> strOption (long "event-type"
                         <> help "Specify event type"
                         <> metavar "EVENT-TYPE")
          <*> strOption (long "timestamp"
                         <> help "Specify ISO8601 timestamp"
                         <> metavar "TIMESTAMP")
  pure Invocation { _run = prepare opts >>= run }

partitionStatToCursorPair :: Nakadi.Partition -> (Nakadi.Cursor, Nakadi.Cursor)
partitionStatToCursorPair Nakadi.Partition { .. } =
  (makeCursor _oldestAvailableOffset, makeCursor _newestAvailableOffset)
  where makeCursor offset = Nakadi.Cursor { Nakadi._partition = _partition
                                          , Nakadi._offset    = offset }

run :: Parameters -> App ()
run params @ Parameters { .. } = do
  ctx <- ask
  print params
  partitionStats <- Nakadi.eventTypePartitions (ctx^.L.nakadiConfig) eventType
  print partitionStats
  let cursorPairs = map partitionStatToCursorPair partitionStats
  forM_ cursorPairs $ \(cursorMin, cursorMax) ->
    findCursor params timestamp cursorMin cursorMax >>= print

findCursor :: Parameters
           -> UTCTime
           -> Nakadi.Cursor
           -> Nakadi.Cursor
           -> App Nakadi.Cursor
findCursor parameters @ Parameters { .. } target lower upper = do
  ctx <- ask
  -- Need to reduce the upper cursor so that we don't have to wait for new events.
  (upper':_) <- Nakadi.eventTypeCursorsShift (ctx^.L.nakadiConfig) eventType [upper] (-1)
  let toTimestamp   = cursorToTimestamp parameters
      computeMiddle = middleCursor (ctx^.L.nakadiConfig) eventType
  narrow toTimestamp computeMiddle target lower upper'

middleCursor :: ( MonadIO m
                , MonadLogger m
                , MonadThrow m
                , MonadCatch m )
             => Nakadi.Config
             -> Nakadi.EventTypeName
             -> Nakadi.Cursor
             -> Nakadi.Cursor
             -> m Nakadi.Cursor
middleCursor config eventType lowerCursor upperCursor = do
  n <- Nakadi.eventTypeCursorDistance config eventType lowerCursor upperCursor
  let nMiddle = round ((fromIntegral n :: Double) / 2)
  Nakadi.eventTypeCursorsShift config eventType [lowerCursor] nMiddle >>= \case
    cursor:_ -> return cursor
    _        -> throwString "FIXME"

prepare :: Opts -> App Parameters
prepare Opts { .. } = do
  timestamp <- case parseISO8601 _optsTimestamp of
    Just t  -> return t
    Nothing -> throw $ AppExceptionMessage "Timestamp not parsable"
  return Parameters { eventType = Nakadi.EventTypeName . pack $ _optsEventType
                    , timestamp = timestamp }

narrow :: (MonadIO m, Eq c, Ord o, Show c)
       => (c -> m o)      -- Convert a 'c' into something orderable
       -> (c -> c -> m c) -- Given two 'c's, obtain a 'c' equidistant to the given 'c's.
       -> o               -- Target 'o'
       -> c               -- Initial lower bound
       -> c               -- Initial upper bound
       -> m c             -- Resulting 'c' for the provided target 'o'.
narrow toOrd computeMiddle target = go
  where go lowerBound upperBound
          | lowerBound == upperBound = return lowerBound
          | otherwise = do
              middle        <- computeMiddle lowerBound upperBound
              middleOrd     <- toOrd middle
              lowerBoundOrd <- toOrd lowerBound
              upperBoundOrd <- toOrd upperBound
              -- Keep in mind that there is no order on the occured_at fields.
              if not (lowerBoundOrd <= middleOrd && middleOrd <= upperBoundOrd)
                then return lowerBound
                else if middleOrd == lowerBoundOrd
                     then return middle -- Middle did not increase, simply return it.
                     else case compare target middleOrd of
                            EQ -> return middle -- Found it exactly.
                            LT -> go lowerBound middle -- Adjust bounds
                            GT -> go middle upperBound -- Dito

-- FIXME: There is one potential issue in narrow: It is not reliable
-- if there exist several events with the same timestamp.

firstEvent :: Nakadi.EventStreamBatch Value -> Maybe (Nakadi.Event Value)
firstEvent batch =
  let x = batch^.L.events.non empty
  in x Vector.!? 0

cursorToTimestamp :: Parameters -> Nakadi.Cursor -> App UTCTime
cursorToTimestamp Parameters { .. } cursor = do
  ctx <- ask
  let consumeParameters = Nakadi.defaultConsumeParameters & Nakadi.setBatchLimit 1
  firstBatch <- runResourceT $ do
    source <- Nakadi.eventTypeSource (ctx^.L.nakadiConfig) (Just consumeParameters) eventType (Just [cursor])
    runConduit $ source .| Conduit.List.head
  case firstBatch >>= firstEvent of
    Just event -> return $ event^.L.metadata.L.occurredAt.to Nakadi.unTimestamp
    Nothing    -> throw $ AppExceptionMessage "Failed to receive Event for Cursor"
