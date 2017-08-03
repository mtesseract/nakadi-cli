{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}

module Nakadi.CLI.Lenses where

import           Nakadi.CLI.Prelude

import           Control.Lens       hiding (Context)
import           Nakadi

makeFieldsNoPrefix ''Options
makeFieldsNoPrefix ''Context
makeFieldsNoPrefix ''Command

makeFieldsNoPrefix ''Cursor
makeFieldsNoPrefix ''EventStreamBatch
makeFieldsNoPrefix ''Event
makeFieldsNoPrefix ''Metadata
makeFieldsNoPrefix ''PartitionStat
makeFieldsNoPrefix ''Config
makeFieldsNoPrefix ''Timestamp
