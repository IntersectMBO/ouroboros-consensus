{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Cardano.Tools.DBAnalyser.HasAnalysis
  ( HasAnalysis (..)
  , HasProtocolInfo (..)
  , HasFeatures (..)
  , SizeInBytes
  , WithLedgerState (..)
  , TxFeatures(..)
  ) where

import Data.Functor.Identity (Identity)
import Data.Map.Strict (Map)
import GHC.Generics (Generic)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.HeaderValidation (HasAnnTip (..))
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Node.ProtocolInfo
import Ouroboros.Consensus.Storage.Serialisation (SizeInBytes)
import Ouroboros.Consensus.Util.Condense (Condense)
import TextBuilder (TextBuilder)
import Cardano.Ledger.BaseTypes (ProtVer)
import qualified Barbies
import Data.Text (Text)

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

class (HasAnnTip blk, GetPrevHash blk, Condense (HeaderHash blk), HasFeatures blk) => HasAnalysis blk where
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

  txFeatures :: WithLedgerState blk -> [TxFeatures Identity]

-- | Like 'BlockFeatures' but for transaction features.
data TxFeatures f = MkTxFeatures
  { src_block :: f BlockNo
    -- ^ Identifier of the block which this transaction is from
  , num_script_wits :: f Int
    -- ^ Number of script witnesses
  , num_addr_wits:: f Int
    -- ^ Number of regular address witnesses
  , size_script_wits :: f Int
    -- ^ Size of inline script witnesses
  , size_ref_scripts :: f Int
    -- ^ Size of (resolved) reference scripts
  , size_datum :: f Int
    -- ^ Size of the datum
  , num_inputs :: f Int
    -- ^ Number of inputs
  , size_inputs :: f Int
    -- ^ Size of (resolved) inputs
  , num_abs_inputs :: f Int
    -- ^ The number of inputs which aren't in the UtxO (should always be 0)
  , num_outputs :: f Int
    -- ^ Number of outputs
  , num_ref_inputs :: f Int
    -- ^ Number of reference inputs
  , size_ref_inputs :: f Int
    -- ^ Size of (resolved) reference inputs
  , num_abs_ref_inputs :: f Int
    -- ^ The number of reference inputs which aren't in the UtxO (should always be 0)
  , num_certs :: f Int
    -- ^ Total number of certs
  , num_pool_certs :: f Int
    -- ^ Number of certs which are pool certs
  , num_gov_certs :: f Int
    -- ^ Number of certs which are governance certs
  , num_deleg_certs :: f Int
    -- ^ Number of certs which are deleg certs
  , min_fee :: f Int
    -- ^ TODO
  }
  deriving (Generic, Barbies.FunctorB, Barbies.TraversableB, Barbies.ApplicativeB, Barbies.ConstraintsB)

class HasProtocolInfo blk where
  data Args blk
  mkProtocolInfo :: Args blk -> IO (ProtocolInfo blk)

-- | These are the methods that go into the dumpBlockFeatures analysis. They are
-- split in a separate type class to group them logically.
class HasFeatures blk where
  protVer :: blk -> ProtVer

  -- | The name of the era in which the block was emitted. This is plain text,
  -- simply meant to help filtering out undesired era for later analysis.
  eraName :: blk -> Text
