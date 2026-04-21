{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
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
  ( EbAnnouncement (EbAnnouncement)
  , ForgedLeiosEb (body, point)
  , LeiosCertificate (unLeiosCertificate)
  , LeiosPoint (pointEbHash)
  , forgeLeiosEb
  , leiosEbBytesSize
  )
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
  ForgeBlockArgs (ShelleyBlock proto era) ->
  m (ShelleyBlock proto era, Maybe ForgedLeiosEb)
forgeShelleyBlock hotKey cbl ForgeBlockArgs{..} = do
  -- Only build an EB if ebTxs is not empty
  let
    -- Current EB to announce
    mayForgedEbAnn :: Maybe (ForgedLeiosEb, EbAnnouncement) = do
      forgedEb <- forgeLeiosEb fbCurrentSlotNo <$> nonEmpty (extractTx <$> fbEbTxs)
      let ebAnn = EbAnnouncement (pointEbHash . point $ forgedEb) (leiosEbBytesSize . body $ forgedEb)
      return (forgedEb, ebAnn)

  blk <- case fbForgeType of
    ForgeTxsRb -> do
      let body = SL.BodyInline rbTxs'

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
      return blk
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
      return blk

  return $
    assert (verifyBlockIntegrity (configSlotsPerKESPeriod $ configConsensus fbConfig) blk) $
      (blk, fst <$> mayForgedEbAnn) -- FIXME
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

toLedgerCert :: LeiosCertificate -> SL.Certificate
toLedgerCert = SL.Certificate . BL.fromStrict . unLeiosCertificate
