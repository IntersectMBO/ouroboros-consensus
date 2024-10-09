{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Ouroboros.Consensus.Shelley.Ledger.Forge (forgeShelleyBlock) where

import qualified Cardano.Ledger.Core as Core (Tx)
import qualified Cardano.Ledger.Era as SL (hashTxSeq, toTxSeq)
import qualified Cardano.Ledger.Shelley.API as SL (Block (..), extractTx)
import qualified Cardano.Ledger.Shelley.BlockChain as SL (bBodySize)
import qualified Cardano.Protocol.TPraos.BHeader as SL
import           Control.Exception
import qualified Data.Sequence.Strict as Seq
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Protocol.Abstract (CanBeLeader, IsLeader)
import           Ouroboros.Consensus.Protocol.Ledger.HotKey (HotKey)
import           Ouroboros.Consensus.Shelley.Eras (EraCrypto)
import           Ouroboros.Consensus.Shelley.Ledger.Block
import           Ouroboros.Consensus.Shelley.Ledger.Config
                     (shelleyProtocolVersion)
import           Ouroboros.Consensus.Shelley.Ledger.Integrity
import           Ouroboros.Consensus.Shelley.Ledger.Mempool
import           Ouroboros.Consensus.Shelley.Protocol.Abstract (ProtoCrypto,
                     ProtocolHeaderSupportsKES (configSlotsPerKESPeriod),
                     mkHeader)

{-------------------------------------------------------------------------------
  Forging
-------------------------------------------------------------------------------}

forgeShelleyBlock ::
     forall m era proto mk.
     (ShelleyCompatible proto era, Monad m)
  => HotKey (EraCrypto era) m
  -> CanBeLeader proto
  -> TopLevelConfig (ShelleyBlock proto era)
  -> BlockNo                                       -- ^ Current block number
  -> SlotNo                                        -- ^ Current slot number
  -> TickedLedgerState (ShelleyBlock proto era) mk -- ^ Current ledger
  -> [Validated (GenTx (ShelleyBlock proto era))]  -- ^ Txs to include
  -> IsLeader proto
  -> m (ShelleyBlock proto era)
forgeShelleyBlock
  hotKey
  cbl
  cfg
  curNo
  curSlot
  tickedLedger
  txs
  isLeader = do
    hdr <- mkHeader @_ @(ProtoCrypto proto) hotKey cbl isLeader
      curSlot curNo prevHash (SL.hashTxSeq @era body) actualBodySize protocolVersion
    let blk = mkShelleyBlock $ SL.Block hdr body
    return $
      assert (verifyBlockIntegrity (configSlotsPerKESPeriod $ configConsensus cfg) blk) $
      blk
  where
    protocolVersion = shelleyProtocolVersion $ configBlock cfg

    body =
        SL.toTxSeq @era
      $ Seq.fromList
      $ fmap extractTx txs

    actualBodySize = SL.bBodySize protocolVersion body

    extractTx :: Validated (GenTx (ShelleyBlock proto era)) -> Core.Tx era
    extractTx (ShelleyValidatedTx _txid vtx) = SL.extractTx vtx

    prevHash :: SL.PrevHash (EraCrypto era)
    prevHash =
        toShelleyPrevHash @era @proto
      . castHash
      . getTipHash
      $ tickedLedger
