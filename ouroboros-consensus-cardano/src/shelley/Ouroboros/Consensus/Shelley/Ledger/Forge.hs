{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Ouroboros.Consensus.Shelley.Ledger.Forge (forgeShelleyBlock) where

import qualified Cardano.Ledger.Block as SL
import qualified Cardano.Ledger.Core as Core (Tx)
import qualified Cardano.Ledger.Core as SL (toTxSeq)
import qualified Cardano.Ledger.Shelley.API as SL (extractTx)
import Cardano.Prelude (nonEmpty)
import qualified Cardano.Protocol.TPraos.BHeader as SL
import Control.Exception
import qualified Data.ByteString as BL
import qualified Data.Sequence.Strict as Seq
import LeiosDemoTypes
  ( ForgedLeiosEb (point)
  , LeiosCertificate (unLeiosCertificate)
  , LeiosPoint (pointEbHash)
  , forgeLeiosEb
  )
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.SupportsMempool
import Ouroboros.Consensus.Protocol.Abstract (CanBeLeader, IsLeader)
import Ouroboros.Consensus.Protocol.Ledger.HotKey (HotKey)
import qualified Ouroboros.Consensus.Protocol.Praos.Header as Praos
import Ouroboros.Consensus.Shelley.Ledger.Block
import Ouroboros.Consensus.Shelley.Ledger.Config
  ( shelleyProtocolVersion
  )
import Ouroboros.Consensus.Shelley.Ledger.Integrity
import Ouroboros.Consensus.Shelley.Ledger.Ledger
  ( toTxSeq
  )
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
  ForgeType ->
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
  forgeType
  hotKey
  cbl
  cfg
  curNo
  curSlot
  tickedLedger
  rbTxs
  ebTxs
  isLeader = do
    -- Only build an EB if ebTxs is not empty
    let
      -- Current EB to announce
      mayEb = forgeLeiosEb curSlot <$> nonEmpty (extractTx <$> ebTxs)

    blk <- case forgeType of
      ForgeTxsRb -> do
        let body = SL.BodyInline rbTxs'

        hdr <-
          mkHeader @_ @(ProtoCrypto proto)
            hotKey
            cbl
            isLeader
            curSlot
            curNo
            prevHash
            (SL.hashBody @era body)
            (SL.bodyBytesSize protocolVersion body)
            Praos.LedgerBlock
            protocolVersion
            (pointEbHash . point <$> mayEb)
        let blk =
              mkShelleyBlock $
                SL.Block
                  hdr
                  body
        return blk
      ForgeCertRb cert ebClosure -> do
        let body = SL.BodyCertificate (toLedgerCert cert) (Just . toTxSeq $ ebClosure)
        hdr <-
          mkHeader @_ @(ProtoCrypto proto)
            hotKey
            cbl
            isLeader
            curSlot
            curNo
            prevHash
            (SL.hashBody @era body)
            (SL.bodyBytesSize protocolVersion body)
            Praos.LeiosCertificate
            protocolVersion
            Nothing -- FIXME(bladyjoker): Skip announcement when certifying https://github.com/input-output-hk/ouroboros-leios/issues/838
        let blk =
              mkShelleyBlock $
                SL.Block
                  hdr
                  body
        return blk

    return $
      assert (verifyBlockIntegrity (configSlotsPerKESPeriod $ configConsensus cfg) blk) $
        (blk, mayEb)
   where
    protocolVersion = shelleyProtocolVersion $ configBlock cfg

    rbTxs' =
      SL.toTxSeq @era $
        Seq.fromList $
          fmap extractTx rbTxs

    extractTx :: Validated (GenTx (ShelleyBlock proto era)) -> Core.Tx era
    extractTx (ShelleyValidatedTx _txid vtx) = SL.extractTx vtx

    prevHash :: SL.PrevHash
    prevHash =
      toShelleyPrevHash @proto
        . castHash
        . getTipHash
        $ tickedLedger

toLedgerCert :: LeiosCertificate -> SL.Certificate
toLedgerCert = SL.Certificate . BL.fromStrict . unLeiosCertificate
