{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Tools.DBAnalyser.HasAnalysis
  ( HasAnalysis (..)
  , HasProtocolInfo (..)
  , LSMConfig (..)
  , SizeInBytes
  , WithLedgerState (..)
  ) where

import Data.Map.Strict (Map)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.HeaderValidation (HasAnnTip (..))
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Node.ProtocolInfo
import Ouroboros.Consensus.Storage.Serialisation (SizeInBytes)
import Ouroboros.Consensus.Util.Condense (Condense)
import TextBuilder (TextBuilder)

{-------------------------------------------------------------------------------
  HasAnalysis
-------------------------------------------------------------------------------}

data WithLedgerState blk = WithLedgerState
  { wlsBlk :: blk
  , wlsStateBefore :: LedgerState blk
  -- ^ The (mk-free) ledger state before the block was applied.
  , wlsValuesBefore :: Values blk
  -- ^ The values to be consumed by the block (read against 'wlsStateBefore').
  , wlsStateAfter :: LedgerState blk
  -- ^ The (mk-free) ledger state after the block was applied.
  , wlsValuesAfter :: Values blk
  -- ^ The values produced by the block.
  }

class (HasAnnTip blk, GetPrevHash blk, Condense (HeaderHash blk)) => HasAnalysis blk where
  countTxOutputs :: blk -> Int
  blockTxSizes :: blk -> [SizeInBytes]
  knownEBBs :: proxy blk -> Map (HeaderHash blk) (ChainHash blk)

  -- | Emit trace markers at points in processing.
  emitTraces :: WithLedgerState blk -> [String]

  -- | This method was introduced for the sake of the 'BenchmarkLedgerOps' pass.
  blockStats :: blk -> [TextBuilder]

  -- | This function allows to define different metrics about block application.
  --
  -- The block application metrics will be stored in a CSV file. This
  -- method is used by 'db-analyser' to define the headers of the
  -- resulting data, and how to compute, for a given row, each column
  -- of the metrics.
  --
  -- The first component of each element in 'blockApplicationMetrics'
  -- represents a header in the resulting CSV file.
  --
  -- Given a block application 'x :: WithLedgerState blk', the metrics
  -- for that block application are calculated using the second
  -- component of 'blockApplicationMetrics'.
  --
  -- The block application metrics are mapped to an IO action because
  -- certain metrics such as the size of data need to be performed in
  -- the IO monad.
  blockApplicationMetrics :: [(TextBuilder, WithLedgerState blk -> IO TextBuilder)]

class HasProtocolInfo blk where
  data Args blk
  mkProtocolInfo :: Args blk -> IO (ProtocolInfo blk)

  -- | The LSM-trees configuration to use when the LedgerDB runs on the LSM
  -- backend. Defaults to no configuration (no snapshot exporting); only the
  -- Cardano instance overrides this, reading the values from the node
  -- configuration file.
  mkLSMConfig :: Args blk -> IO LSMConfig
  mkLSMConfig _ = pure (LSMConfig Nothing)

-- | LSM-trees configuration relevant to db-analyser.
data LSMConfig = LSMConfig
  { lsmConfigExportPath :: Maybe FilePath
  -- ^ The directory (relative to the LedgerDB filesystem root) into which the
  -- LSM backend exports snapshots as it takes them. When 'Nothing', snapshots
  -- are not exported.
  }
