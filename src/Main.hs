{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import           Control.Lens                    hiding (Context)
import           Control.Monad.Logger
import           Data.Version
import qualified Nakadi
import qualified Nakadi.CLI.Commands.FindCursors as FindCursors
import qualified Nakadi.CLI.Commands.Listen      as Listen
import qualified Nakadi.CLI.Lenses               as Lens
import           Nakadi.CLI.Prelude
import           Network.HTTP.Simple
import           Options.Applicative
import           Paths_nakadi_cli
import           System.Environment

-- | Main entry point. Responsible for option parsing and executing
-- runApp via runWithOptions.
main :: IO ()
main = do
  opts <- execParser mainParser
  runWithOptions (_run (_invocation opts)) opts

-- | Given parsed command line options, produce an application
-- context.
makeContext :: Options -> IO Context
makeContext opts = do
  token <- fmap pack <$> lookupEnv "TOKEN"
  requestTemplate <- parseRequest (opts ^. Lens.endpoint)
                     <&> case token of
                           Just t -> addRequestHeader "Authorization" ("Bearer " <> encodeUtf8 t)
                           Nothing    -> identity
  nakadiConfig <- Nakadi.newNakadiConfig Nothing requestTemplate
  return Context { _nakadiConfig = nakadiConfig
                 , _token = encodeUtf8 <$> token }

-- | Execute the given application.
runWithOptions :: App () -> Options -> IO ()
runWithOptions app opts = do
  ctx <- makeContext opts
  flip runReaderT ctx . runStderrLoggingT $ app

-- | Supported commands.
commands :: [Command]
commands = [ Listen.commandSpec
           , FindCursors.commandSpec
           ]

-- | Command line option parser including meta information.
mainParser :: ParserInfo Options
mainParser =
  info (helper
        <*> infoOption (showVersion version) (long "version" <> help "Show version")
        <*> parseOpts)
       (fullDesc
        <> progDesc "CLI for the Nakadi Event Brooker"
        <> header "nakadi-cli")

-- | Parseer for command line options. Implements dispatching on list
-- of supported commands.
parseOpts :: Parser Options
parseOpts = Options
  <$> strOption (long "endpoint"
                 <> metavar "URL"
                 <> help "Nakadi Endpoint")
  <*> makeSubcommandsParser commands

prepareCommandParser :: Command -> Mod CommandFields Invocation
prepareCommandParser cmd =
  command (cmd ^. Lens.name)
          (info (cmd ^. Lens.parser)
                (progDesc (cmd ^. Lens.description)))

makeSubcommandsParser :: [Command] -> Parser Invocation
makeSubcommandsParser = hsubparser . mconcat . map prepareCommandParser
