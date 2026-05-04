{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Ouroboros.Consensus.Shelley.Ledger.Forge (forgeShelleyBlock) where

import Cardano.Binary (serialize')
import qualified Cardano.Ledger.Block as SL
import qualified Cardano.Ledger.Core as Core (Tx)
import qualified Cardano.Ledger.Core as SL (toTxSeq)
import qualified Cardano.Ledger.Shelley.API as SL (extractTx)
import Cardano.Prelude (nonEmpty)
import qualified Cardano.Protocol.TPraos.BHeader as SL
import Control.Exception
import Control.Tracer (traceWith)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Sequence.Strict as Seq
import LeiosDemoDb
  ( LeiosDbConnection (leiosDbQueryCertificateByPoint, leiosDbQueryCompletedEbByPoint)
  )
import LeiosDemoTypes
  ( EbAnnouncement (EbAnnouncement, ebAnnouncementHash)
  , ForgedLeiosEb (point)
  , LeiosCertificate (leiosCertificateEbPoint)
  , LeiosPoint (MkLeiosPoint, pointEbHash)
  , TraceLeiosKernel (MkTraceLeiosKernel)
  , TxHash
  , encodeLeiosPoint
  , forgeLeiosEb
  , leiosEbBytesSize
  , minCertificationGap
  )
import qualified LeiosDemoTypes as Leios
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.SupportsMempool
import Ouroboros.Consensus.Protocol.Abstract (CanBeLeader)
import Ouroboros.Consensus.Protocol.Ledger.HotKey (HotKey)
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
  forall m era proto.
  (ShelleyCompatible proto era, Monad m) =>
  HotKey (ProtoCrypto proto) m ->
  CanBeLeader proto ->
  -- | The previous EB announcement and last slot, if any. 'Nothing'
  -- when the protocol does not support Leios (e.g. TPraos) or when
  -- there is no previous announcement.
  Maybe (EbAnnouncement, WithOrigin SlotNo) ->
  ForgeBlockArgs m (ShelleyBlock proto era) ->
  m (ShelleyBlock proto era, Maybe ForgedLeiosEb)
forgeShelleyBlock hotKey cbl mayLeiosInfo ForgeBlockArgs{..} = do
  forgeType <- decideForgeType
  (blk, mayForgedEb) <- case forgeType of
    ForgeTxsRb -> do
      let body = SL.BodyInline rbTxs'
          -- Current EB to announce
          mayForgedEbAnn :: Maybe (ForgedLeiosEb, EbAnnouncement) = do
            forgedEb <- forgeLeiosEb fbCurrentSlotNo <$> nonEmpty (extractTx <$> fbEbTxs)
            let ebAnn = EbAnnouncement (pointEbHash . point $ forgedEb) (leiosEbBytesSize . Leios.body $ forgedEb)
            return (forgedEb, ebAnn)

      hdr <-
        mkHeader @_ @(ProtoCrypto proto)
          hotKey
          cbl
          fbIsLeader
          fbCurrentSlotNo
          fbCurrentBlockNo
          prevHash
          (SL.hashBody @era body)
          (SL.bodyBytesSize protocolVersion body)
          protocolVersion
          (snd <$> mayForgedEbAnn)

      let blk =
            mkShelleyBlock $
              SL.Block
                hdr
                body

      return (blk, fst <$> mayForgedEbAnn)
    ForgeCertRb cert ebClosure -> do
      let body = SL.BodyCertificate (toLedgerCert cert) (Just . toTxSeq $ ebClosure)
      hdr <-
        mkHeader @_ @(ProtoCrypto proto)
          hotKey
          cbl
          fbIsLeader
          fbCurrentSlotNo
          fbCurrentBlockNo
          prevHash
          (SL.hashBody @era body)
          (SL.bodyBytesSize protocolVersion body)
          protocolVersion
          Nothing -- FIXME(bladyjoker): Skip announcement when certifying https://github.com/input-output-hk/ouroboros-leios/issues/838
      let blk =
            mkShelleyBlock $
              SL.Block
                hdr
                body

      return (blk, Nothing)

  return $
    assert (verifyBlockIntegrity (configSlotsPerKESPeriod $ configConsensus fbConfig) blk) $
      (blk, mayForgedEb)
 where
  protocolVersion = shelleyProtocolVersion $ configBlock fbConfig

  rbTxs' =
    SL.toTxSeq @era $
      Seq.fromList $
        fmap extractTx fbRbTxs

  extractTx :: Validated (GenTx (ShelleyBlock proto era)) -> Core.Tx era
  extractTx (ShelleyValidatedTx _txid vtx) = SL.extractTx vtx

  prevHash :: SL.PrevHash
  prevHash =
    toShelleyPrevHash @proto
      . castHash
      . getTipHash
      $ fbCurrentTickedLedgerState

  -- \| Determine whether to certify a previously announced EB or just
  -- include regular transactions. Uses the chain-dependent state from
  -- the header state to find the previous EB announcement and slot.
  decideForgeType :: m ForgeType
  decideForgeType =
    case mayLeiosInfo of
      Nothing -> pure ForgeTxsRb
      Just (EbAnnouncement{ebAnnouncementHash}, prevSlot) ->
        case prevSlot of
          Origin -> pure ForgeTxsRb
          NotOrigin prevSlotNo
            | unSlotNo fbCurrentSlotNo - unSlotNo prevSlotNo <= minCertificationGap -> do
                traceWith fbLeiosTracer $ MkTraceLeiosKernel "Too soon to certify"
                pure ForgeTxsRb
            | otherwise -> do
                let ebPoint =
                      MkLeiosPoint
                        { pointSlotNo = prevSlotNo
                        , pointEbHash = ebAnnouncementHash
                        }
                mayEbClosure <- leiosDbQueryCompletedEbByPoint fbLeiosDb ebPoint
                case mayEbClosure of
                  Nothing -> do
                    traceWith fbLeiosTracer $ MkTraceLeiosKernel $ "EB not downloaded " <> show ebPoint
                    pure ForgeTxsRb
                  Just ebClosure -> do
                    mayCert <- leiosDbQueryCertificateByPoint fbLeiosDb ebPoint
                    case mayCert of
                      Nothing -> do
                        traceWith fbLeiosTracer $
                          MkTraceLeiosKernel $
                            "EB downloaded but no certificate " <> show ebPoint
                        pure ForgeTxsRb
                      Just cert -> do
                        traceWith fbLeiosTracer $
                          MkTraceLeiosKernel $
                            "EB downloaded " <> show ebPoint <> " and certified " <> show cert
                        pure $ ForgeCertRb cert ebClosure

-- | Local forge type decision: certify an EB or include regular transactions.
data ForgeType
  = ForgeTxsRb
  | ForgeCertRb Leios.LeiosCertificate [(TxHash, ByteString)]

toLedgerCert :: LeiosCertificate -> SL.Certificate
toLedgerCert = SL.Certificate . BSL.fromStrict . serialize' . encodeLeiosPoint . leiosCertificateEbPoint
