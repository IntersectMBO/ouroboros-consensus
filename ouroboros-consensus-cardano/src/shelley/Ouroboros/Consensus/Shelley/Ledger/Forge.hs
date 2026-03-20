{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Ouroboros.Consensus.Shelley.Ledger.Forge (forgeShelleyBlock) where

import qualified Cardano.Ledger.Block as SL
import qualified Cardano.Ledger.Core as Core (Tx)
import qualified Cardano.Ledger.Core as SL (EraSegWits (TxSeq), toTxSeq)
import qualified Cardano.Ledger.Shelley.API as SL (extractTx)
import qualified Cardano.Ledger.Shelley.BlockChain as SL (bBodySize)
import Cardano.Prelude (nonEmpty)
import qualified Cardano.Protocol.TPraos.BHeader as SL
import Control.Exception
import qualified Data.ByteString as BL
import qualified Data.Sequence.Strict as Seq
import qualified Debug.Trace as Debug
import LeiosDemoDb
  ( LeiosDbHandle (leiosDbQueryCertificateByPoint, leiosDbQueryCompletedEbByPoint)
  )
import LeiosDemoTypes
  ( EbHash (ebHashBytes)
  , ForgedLeiosEb (point)
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
import Ouroboros.Consensus.Shelley.Ledger.Block
import Ouroboros.Consensus.Shelley.Ledger.Config
  ( shelleyProtocolVersion
  )
import Ouroboros.Consensus.Shelley.Ledger.Integrity
import Ouroboros.Consensus.Shelley.Ledger.Ledger
  ( ShelleyLedgerLeiosState (sllsMaybeAnnouncedEb, sllsTooSoonToCertify)
  , Ticked (tickedShelleyLedgerLeiosState)
  , toTxSeq
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
  LeiosDbHandle m ->
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
  m (ShelleyBlock proto era, Maybe ForgedLeiosEb) -- TODO(bladyjoker): Returns (ShelleyBlock proto era, Maybe ForgedLeiosEb and Maybe CertifiedEb?)...
forgeShelleyBlock
  leiosDb
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
      mayAnnouncedEb = toLedgerEbHash . pointEbHash . point <$> mayEb
      -- Current Ledger state
      ledgerLeiosSt = tickedShelleyLedgerLeiosState tickedLedger

    (body, certifiesEb) <- case sllsMaybeAnnouncedEb ledgerLeiosSt of
      Nothing -> return $ (SL.BodyInline rbTxs', False)
      Just annEbPoint ->
        if sllsTooSoonToCertify ledgerLeiosSt
          then return $ (SL.BodyInline rbTxs', False)
          else do
            mayEbClosure <- leiosDbQueryCompletedEbByPoint' leiosDb annEbPoint
            case mayEbClosure of
              Nothing -> return (SL.BodyInline rbTxs', False) -- This happens when EBs haven't been fully downloaded
              Just ebClosure -> do
                mayCert <- leiosDbQueryCertificateByPoint leiosDb annEbPoint
                case mayCert of
                  Nothing -> return (SL.BodyInline rbTxs', False) -- This happens when EBs have been downloaded but voting hasn't completed
                  Just cert ->
                    return $
                      Debug.trace
                        (show ("certifying", cert, annEbPoint))
                        (SL.BodyCertificate (toLedgerCert cert) (Just ebClosure), True)
    hdr <-
      mkHeader @_ @(ProtoCrypto proto) -- FIXME(bladyjoker): EB announcement in header
        hotKey
        cbl
        isLeader
        curSlot
        curNo
        prevHash
        (SL.hashBody @era body) -- FIXME(bladyjoker): SL.hashBody = SL.hashTxSeq `or` SL.hashCert
        actualBodySize
        protocolVersion

    let blk =
          mkShelleyBlock $
            SL.Block
              hdr
              body
              (if certifiesEb then Nothing else mayAnnouncedEb) -- TODO(bladyjoker): Skip announcement when certifying
              certifiesEb

    return $
      assert (verifyBlockIntegrity (configSlotsPerKESPeriod $ configConsensus cfg) blk) $
        (blk, mayEb)
   where
    protocolVersion = shelleyProtocolVersion $ configBlock cfg

    rbTxs' =
      SL.toTxSeq @era $
        Seq.fromList $
          fmap extractTx rbTxs

    actualBodySize = SL.bBodySize protocolVersion rbTxs'

    extractTx :: Validated (GenTx (ShelleyBlock proto era)) -> Core.Tx era
    extractTx (ShelleyValidatedTx _txid vtx) = SL.extractTx vtx

    prevHash :: SL.PrevHash
    prevHash =
      toShelleyPrevHash @proto
        . castHash
        . getTipHash
        $ tickedLedger

-- TODO(bladyjoker): Find a home for these
-- -.-
toLedgerEbHash :: EbHash -> SL.EbHash
toLedgerEbHash = SL.EbHash . BL.fromStrict . ebHashBytes

toLedgerCert :: LeiosCertificate -> SL.Certificate
toLedgerCert = SL.Certificate . BL.fromStrict . unLeiosCertificate

leiosDbQueryCompletedEbByPoint' ::
  forall m era.
  (Monad m, ShelleyBasedEra era) => LeiosDbHandle m -> LeiosPoint -> m (Maybe (SL.TxSeq era))
leiosDbQueryCompletedEbByPoint' leiosDb ebPoint = do
  res <- leiosDbQueryCompletedEbByPoint leiosDb ebPoint
  return $ toTxSeq <$> res
