{-# LANGUAGE DeriveGeneric #-}

-- | Functions related to obtaining information about the 'db-analyser' run.
--
-- Metadata includes information such as:
--
-- - RTS flags.
-- - Compiler version.
-- - OS and architecture.
--
-- See 'Metadata' and 'getMetadata' for more details.
--
module Cardano.Tools.DBAnalyser.Analysis.BenchmarkLedgerOps.Metadata (
    Metadata (..)
  , getMetadata
  ) where

import           Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Version
import           Data.Word (Word32, Word64)
import           GHC.Generics (Generic)
import qualified GHC.RTS.Flags as RTS
import qualified System.Info

data Metadata = Metadata {
    rtsGCMaxStkSize             :: Word32
  , rtsGCMaxHeapSize            :: Word32
  , rtsConcurrentCtxtSwitchTime :: Word64
  , rtsParNCapabilities         :: Word32
  , compilerVersion             :: String
  , compilerName                :: String
  , operatingSystem             :: String
  , machineArchitecture         :: String
  } deriving (Generic, Show, Eq)

instance ToJSON Metadata where
  toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

instance FromJSON Metadata

getMetadata :: IO Metadata
getMetadata = do
  rtsFlags <- RTS.getRTSFlags
  pure $ Metadata {
      rtsGCMaxStkSize             = RTS.maxStkSize     $ RTS.gcFlags rtsFlags
    , rtsGCMaxHeapSize            = RTS.maxHeapSize    $ RTS.gcFlags rtsFlags
    , rtsConcurrentCtxtSwitchTime = RTS.ctxtSwitchTime $ RTS.concurrentFlags rtsFlags
    , rtsParNCapabilities         = RTS.nCapabilities  $ RTS.parFlags rtsFlags
    , compilerVersion             = Data.Version.showVersion System.Info.compilerVersion
    , compilerName                = System.Info.compilerName
    , operatingSystem             = System.Info.os
    , machineArchitecture         = System.Info.arch
    }
