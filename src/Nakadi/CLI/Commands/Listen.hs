-- | Implementation for 'listen' command

{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}

module Nakadi.CLI.Commands.Listen
  ( commandSpec
  ) where

import           Nakadi.CLI.Prelude

import           Conduit
import           Control.Lens
import           Data.Aeson
import           Data.ByteString.Lazy.Char8 (hPutStrLn)
import qualified Data.Conduit.Combinators   as Conduit
import qualified Nakadi
import qualified Nakadi.CLI.Lenses          as Lens
import           Options.Applicative

data Opts = Opts { batchLimit :: Int
                 , eventType  :: String }

data Parameters = Parameters { batchLimit :: Int
                             , eventType  :: Nakadi.EventTypeName
                             } deriving (Show)

commandSpec :: Command
commandSpec = Command
  { _name        = "listen"
  , _description = "Listen on a Nakadi Stream"
  , _parser      = parser }

defaultBatchLimit :: Int
defaultBatchLimit = 100

parser :: Parser Invocation
parser = do
  opts <- Opts
          <$> option auto (long "batch-limit"
                           <> help "Specify batch limit"
                           <> value defaultBatchLimit
                           <> showDefault
                           <> metavar "INT")
          <*> strOption (long "event-type"
                         <> help "Specify event type"
                         <> metavar "EVENT-TYPE")
  pure Invocation { _run = prepare opts >>= run }

run :: Parameters -> App ()
run params @ Parameters { .. } = do
  ctx <- ask
  print params
  runResourceT $ do
    source <- Nakadi.eventTypeSource (ctx ^. Lens.nakadiConfig) Nothing eventType Nothing
    runConduit $ source .| Conduit.mapM_ processBatch

prepare :: Opts -> App Parameters
prepare Opts { .. } =
  return Parameters { batchLimit = batchLimit
                    , eventType  = Nakadi.EventTypeName . pack $ eventType }

processBatch :: MonadIO m => Nakadi.EventStreamBatch Value -> m ()
processBatch = liftIO . hPutStrLn stdout . encode
