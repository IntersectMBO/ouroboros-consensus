{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Observe.ConsensusJson (ConsensusJson (..)) where

import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Ledger.SupportsMempool

import Data.Aeson (Value, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Short as Short
import qualified Data.Text.Encoding as Text
import qualified Ouroboros.Consensus.HardFork.Combinator as HFC
import qualified Ouroboros.Consensus.Ledger.SupportsMempool as Core
import qualified Ouroboros.Consensus.Mempool as Core
import Ouroboros.Consensus.Mempool.TxSeq (TxSeqMeasure)
import qualified Ouroboros.Consensus.Mempool.TxSeq as Core
import qualified Ouroboros.Consensus.Node.Tracers as Core
import qualified Ouroboros.Consensus.Shelley.Ledger as Shelley

-- | Class for JSON encoding of Consensus types.
-- Avoiding scattered Orphans problem
-- TODO(bladyjoker): add fromConsensusJson
class ConsensusJson a where
  toConsensusJson :: a -> Value

instance
  (HasHeader blk, ConsensusJson (HeaderHash blk), ConsensusJson (TxMeasure blk)) =>
  ConsensusJson (Core.ForgedBlock blk)
  where
  toConsensusJson fb =
    Aeson.object $
      [ "ledgerTip" .= toConsensusJson (Core.fbLedgerTip fb)
      , "newBlockHash" .= toConsensusJson (blockHash (Core.fbNewBlock fb))
      , "newBlockSize" .= toConsensusJson (Core.fbNewBlockSize fb)
      , "mempoolSize" .= toConsensusJson (Core.fbMempoolSize fb)
      ]
        ++ optionalEndorserFields
   where
    optionalEndorserFields = case Core.fbMaybeNewEndorserBlock fb of
      Nothing -> []
      Just _newEndorserBlock ->
        [ "newEndorserBlockHash" .= ("TODO: EB hash" :: String)
        , "newEndorserBlockSize" .= toConsensusJson (Core.fbNewEndorserBlockSize fb)
        , "mempoolRestSize" .= toConsensusJson (Core.fbMempoolRestSize fb)
        ]

instance ConsensusJson a => ConsensusJson (TxSeqMeasure a) where
  toConsensusJson tsm =
    Aeson.object $
      [ "txCount" .= (Aeson.toJSON . Core.mCount $ tsm)
      , "minTicketNo" .= (Aeson.toJSON . Core.unTicketNo . Core.mMinTicket $ tsm)
      , "maxTicketNo" .= (Aeson.toJSON . Core.unTicketNo . Core.mMaxTicket $ tsm)
      , "txSize" .= (toConsensusJson . Core.mSize $ tsm)
      ]

instance ConsensusJson (HeaderHash blk) => ConsensusJson (Point blk) where
  toConsensusJson GenesisPoint = "genesis (origin)"
  toConsensusJson (BlockPoint slot hash) =
    Aeson.object
      [ "slot" .= slot
      , "hash" .= toConsensusJson hash
      ]

instance ConsensusJson Shelley.ConwayMeasure where
  toConsensusJson m =
    Aeson.object
      [ "txSizeBytes" .= toConsensusJson (txMeasureMetricTxSizeBytes m)
      , "exUnitsMemory" .= txMeasureMetricExUnitsMemory m
      , "exUnitsSteps" .= txMeasureMetricExUnitsSteps m
      , "refScriptsSizeBytes" .= toConsensusJson (txMeasureMetricRefScriptsSizeBytes m)
      ]

instance HFC.CanHardFork xs => ConsensusJson (HFC.OneEraHash xs) where
  toConsensusJson = Aeson.toJSON . Text.decodeLatin1 . B16.encode . Short.fromShort . HFC.getOneEraHash

instance ConsensusJson Core.ByteSize32 where
  toConsensusJson = Aeson.toJSON . unByteSize32

instance ConsensusJson Core.MempoolSize where
  toConsensusJson ms =
    Aeson.object
      [ "txCount" .= Core.msNumTxs ms
      , "txBytes" .= toConsensusJson (Core.msNumBytes ms)
      ]

instance ConsensusJson (Core.IgnoringOverflow Core.ByteSize32) where
  toConsensusJson = toConsensusJson . Core.unIgnoringOverflow
