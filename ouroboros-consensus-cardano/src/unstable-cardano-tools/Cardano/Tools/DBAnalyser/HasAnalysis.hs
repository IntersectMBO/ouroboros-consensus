{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Tools.DBAnalyser.HasAnalysis
  ( HasAnalysis (..)
  , HasProtocolInfo (..)
  , SizeInBytes
  , WithLedgerState (..)
  ) where

import Cardano.Tools.DBAnalyser.Types (LedgerDBBackend)
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
  mkProtocolInfo = fmap fst . mkProtocolInfoAndBackend

  -- | The protocol info, together with the LedgerDB backend (and its settings)
  -- that the node configuration file selects, which db-analyser uses when the
  -- command line does not select one. Both are produced by a single call so that
  -- the configuration only has to be read once.
  --
  -- Defaults to no backend (nothing to read one from); only the Cardano instance
  -- overrides this.
  mkProtocolInfoAndBackend :: Args blk -> IO (ProtocolInfo blk, Maybe LedgerDBBackend)
  mkProtocolInfoAndBackend args = (\pInfo -> (pInfo, Nothing)) <$> mkProtocolInfo args

  -- The two defaults above are mutually recursive, so without this pragma GHC
  -- would infer an empty minimal complete definition and let an instance that
  -- defines neither method diverge at runtime without any warning.
  {-# MINIMAL mkProtocolInfo | mkProtocolInfoAndBackend #-}
