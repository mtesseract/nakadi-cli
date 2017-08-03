module Nakadi.CLI.Prelude
  ( module ClassyPrelude
  , module Nakadi.CLI.Types
  , identity
  ) where

import           ClassyPrelude    hiding (id)
import           Nakadi.CLI.Types

identity :: a -> a
identity a = a
