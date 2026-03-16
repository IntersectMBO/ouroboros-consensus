{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Ouroboros.Consensus.Shelley.Ledger.Forge (forgeShelleyBlock) where

import qualified Cardano.Ledger.Block as SL
import qualified Cardano.Ledger.Core as Core (Tx)
import qualified Cardano.Ledger.Core as SL (hashTxSeq, toTxSeq)
import qualified Cardano.Ledger.Shelley.API as SL (extractTx)
import qualified Cardano.Ledger.Shelley.BlockChain as SL (bBodySize)
import Cardano.Prelude (nonEmpty)
import qualified Cardano.Protocol.TPraos.BHeader as SL
import Control.Exception
import qualified Data.ByteString as BL
import qualified Data.Sequence.Strict as Seq
import LeiosDemoTypes
  ( EbHash (ebHashBytes)
  , ForgedLeiosEb (point)
  , LeiosPoint (pointEbHash)
  , forgeLeiosEb
  )
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.SupportsMempool
import Ouroboros.Consensus.Protocol.Abstract (CanBeLeader, IsLeader)
import Ouroboros.Consensus.Protocol.Ledger.HotKey (HotKey)
import Ouroboros.Consensus.Shelley.Ledger.Block
import Ouroboros.Consensus.Shelley.Ledger.Config
  ( shelleyProtocolVersion
  )
import Ouroboros.Consensus.Shelley.Ledger.Integrity
import Ouroboros.Consensus.Shelley.Ledger.Mempool
import Ouroboros.Consensus.Shelley.Protocol.Abstract
  ( ProtoCrypto
  , ProtocolHeaderSupportsKES (configSlotsPerKESPeriod)
  , mkHeader
  )

{-------------------------------------------------------------------------------
  Forging
-------------------------------------------------------------------------------}

forgeShelleyBlock ::
  forall m era proto mk.
  (ShelleyCompatible proto era, Monad m) =>
  HotKey (ProtoCrypto proto) m ->
  CanBeLeader proto ->
  TopLevelConfig (ShelleyBlock proto era) ->
  -- | Current block number
  BlockNo ->
  -- | Current slot number
  SlotNo ->
  -- | Current ledger
  TickedLedgerState (ShelleyBlock proto era) mk ->
  -- | RB Txs to include
  [Validated (GenTx (ShelleyBlock proto era))] ->
  -- | EB Txs to include
  [Validated (GenTx (ShelleyBlock proto era))] ->
  IsLeader proto ->
  m (ShelleyBlock proto era, Maybe ForgedLeiosEb)
forgeShelleyBlock
  hotKey
  cbl
  cfg
  curNo
  curSlot
  tickedLedger
  rbTxs
  ebTxs
  isLeader = do
    hdr <-
      mkHeader @_ @(ProtoCrypto proto) -- FIXME(bladyjoker): EB announcement in header
        hotKey
        cbl
        isLeader
        curSlot
        curNo
        prevHash
        (SL.hashTxSeq @era body) -- FIXME(bladyjoker): SL.hashTxSeq OR Certified EbHash???
        actualBodySize
        protocolVersion
    -- Only build an EB if ebTxs is not empty
    let mayEb = forgeLeiosEb curSlot <$> nonEmpty (extractTx <$> ebTxs)
        blk =
          mkShelleyBlock $
            SL.Block hdr body (toLedgerEbHash . pointEbHash . point <$> mayEb) False -- FIXME(bladyjoker): We need `mayCertifiedEb` in the arguments to `forgeShelleyBlock`
    return $
      assert (verifyBlockIntegrity (configSlotsPerKESPeriod $ configConsensus cfg) blk) $
        (blk, mayEb)
   where
    protocolVersion = shelleyProtocolVersion $ configBlock cfg

    body =
      SL.toTxSeq @era $
        Seq.fromList $
          fmap extractTx rbTxs

    actualBodySize = SL.bBodySize protocolVersion body

    extractTx :: Validated (GenTx (ShelleyBlock proto era)) -> Core.Tx era
    extractTx (ShelleyValidatedTx _txid vtx) = SL.extractTx vtx

    prevHash :: SL.PrevHash
    prevHash =
      toShelleyPrevHash @proto
        . castHash
        . getTipHash
        $ tickedLedger

-- -.-
toLedgerEbHash :: EbHash -> SL.EbHash
toLedgerEbHash = SL.EbHash . BL.fromStrict . ebHashBytes
