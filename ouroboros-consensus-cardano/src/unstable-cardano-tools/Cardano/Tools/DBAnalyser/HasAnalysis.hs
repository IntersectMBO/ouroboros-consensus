{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Tools.DBAnalyser.HasAnalysis
  ( HasAnalysis (..)
  , HasProtocolInfo (..)
  , HasFeatures (..)
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
import Lens.Micro
import Data.Set (Set)
import Cardano.Ledger.Api
import Data.Text (Text)
import Cardano.Ledger.BaseTypes (ProtVer)
import Cardano.Ledger.TxIn as Ledger

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

class HasProtocolInfo blk where
  data Args blk
  mkProtocolInfo :: Args blk -> IO (ProtocolInfo blk)

-- | These are the methods that go into the dumpBlockFeatures analysis. They are
-- split in a separate type class to group them logically.
class HasFeatures blk where
  protVer :: blk -> ProtVer

  -- | The type of transaction that blocks of type @blk@ contains.
  type TxOf blk

  -- | Iterates over all the transactions of a block.
  txs :: SimpleFold blk (TxOf blk)

  -- | The set of all inputs in a transaction.
  inputs :: proxy blk -> SimpleGetter (TxOf blk) (Set Ledger.TxIn)
  -- | Number of outputs in the transaction. We don't expose a set of, or an
  -- iterator over, outputs to avoid introducing an extra type family.
  numOutputs :: proxy blk -> TxOf blk -> Int
  -- | The set of only reference inputs in a transaction.
  referenceInputs :: proxy blk -> SimpleGetter (TxOf blk) (Set Ledger.TxIn)

  -- | The type of sets of witnesses that transactions in blocks of type @blk@
  -- can have.
  type WitsOf blk

  -- | The set of all the witnesses of a given transaction
  wits :: proxy blk -> SimpleGetter (TxOf blk) (WitsOf blk)

  -- | The set of only address witnesses contain in a witness set.
  addrWits :: proxy blk -> Lens' (WitsOf blk) (Set (WitVKey Witness))

  -- | The size of the datum in a witness set.
  datumSize :: proxy blk -> WitsOf blk -> Int

  -- | The type of script witnesses that a transaction in @blk@ can have.
  type ScriptType blk

  -- | The set of only the script witnesses in the witness set, indexed by their
  -- hash.
  scriptWits :: proxy blk -> SimpleGetter (WitsOf blk) (Map ScriptHash (ScriptType blk))

  -- | The size of a given script.
  scriptSize :: proxy blk -> ScriptType blk -> Int

  -- | The type of certificates that transactions in blocks of types @blk@ can
  -- use. The main use of this type is to give us the means to classify
  -- certificate in several categories.
  type CertsOf blk

  -- | Iterates over all the certificates of a transaction.
  certs :: proxy blk -> SimpleFold (TxOf blk) (CertsOf blk)

  -- | 'filterPoolCert', 'filterGovCert', 'filterDelegCert' tests if a
  -- certificate is in the given category. They are implemented as affine folds:
  -- a fold which traverses 0 or 1 value (and, in this case, is the identity
  -- when it traverses 1 value). This affinity constraint isn't enforced in
  -- types.
  filterPoolCert :: proxy blk -> SimpleFold (CertsOf blk) (CertsOf blk)
  filterGovCert :: proxy blk -> SimpleFold (CertsOf blk) (CertsOf blk)
  filterDelegCert :: proxy blk -> SimpleFold (CertsOf blk) (CertsOf blk)
  
  -- | The name of the era in which the block was emitted. This is plain text,
  -- simply meant to help filtering out undesired era for later analysis.
  eraName :: blk -> Text

  -- | The UTxO map from the current ledger state. It is important that this
  -- stays a map and not an iterator because we are going to lookup inputs in
  -- the map (but never otherwise traverse it). This is used for to extract the
  -- size of inputs.
  --
  -- We're representing outputs merely as their sizes. This avoids introducing a
  -- new type family, and is enough for our purpose.
  utxoSummary :: WithLedgerState blk -> Map Ledger.TxIn Int
