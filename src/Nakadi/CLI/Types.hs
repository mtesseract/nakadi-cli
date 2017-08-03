{-# LANGUAGE DuplicateRecordFields #-}

-- | Types

module Nakadi.CLI.Types where

import           Control.Exception.Safe
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.ByteString        (ByteString)
import           Data.Text              (Text)
import qualified Nakadi
import           Options.Applicative
import           Prelude

newtype Invocation = Invocation { _run :: App () }

data Options = Options
  { _endpoint   :: String
  , _invocation :: Invocation
  }

data Command = Command { _name        :: String
                       , _description :: String
                       , _parser      :: Parser Invocation
                       }
data Context = Context
  { _nakadiConfig :: Nakadi.Config
  , _token        :: Maybe ByteString
  }

type App = LoggingT (ReaderT Context IO)

data AppException = AppExceptionMessage Text deriving (Typeable, Show)

instance Exception AppException
