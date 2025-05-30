{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Tools.DBAnalyser.HasAnalysis
  ( HasAnalysis (..)
  , HasProtocolInfo (..)
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
  , wlsStateBefore :: LedgerState blk ValuesMK
  -- ^ This ledger state contains only the values to be consumed by the block
  , wlsStateAfter :: LedgerState blk ValuesMK
  -- ^ This ledger state contains only the values produced by the block
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
