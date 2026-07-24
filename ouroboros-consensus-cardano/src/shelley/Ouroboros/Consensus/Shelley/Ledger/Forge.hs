{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
-- FIXME: resolve 'Validated' deprecations
{-# OPTIONS_GHC -Wno-deprecations #-}

module Ouroboros.Consensus.Shelley.Ledger.Forge (forgeShelleyBlock) where

import Cardano.Crypto.Leios (LeiosCert)
import qualified Cardano.Ledger.Core as Core (TopTx, Tx)
import qualified Cardano.Ledger.Core as SL
  ( BlockBody
  , blockBodySize
  , hashBlockBody
  , mkBasicBlockBody
  , txSeqBlockBodyL
  )
import Cardano.Ledger.Dijkstra.BlockBody (leiosCertBlockBodyL)
import qualified Cardano.Ledger.Shelley.API as SL (Block (..), extractTx)
import Cardano.Prelude (nonEmpty)
import qualified Cardano.Protocol.TPraos.BHeader as SL
import Control.Exception
import Control.Monad (void, when)
import Control.Tracer (traceWith)
import Data.ByteString.Short (fromShort)
import Data.Maybe (isJust)
import Data.Maybe.Strict (StrictMaybe (..), maybeToStrictMaybe)
import qualified Data.Sequence.Strict as Seq
import qualified Data.Typeable as Typeable
import LeiosDemoDb (LeiosDbConnection (..))
import LeiosDemoTypes
  ( EbAnnouncement (..)
  , ForgedLeiosEb (..)
  , LeiosPoint (..)
  , RbHash (..)
  , TraceLeiosKernel (..)
  , forgeLeiosEb
  , hashLeiosEb
  , leiosEbBytesSize
  )
import Lens.Micro ((&), (.~))
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.SupportsMempool
import Ouroboros.Consensus.Protocol.Abstract (CanBeLeader)
import Ouroboros.Consensus.Protocol.Ledger.HotKey (HotKey)
import Ouroboros.Consensus.Protocol.Praos.Header (HeaderLeiosExtension (..))
import Ouroboros.Consensus.Shelley.Eras (DijkstraEra)
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
  forall m era proto.
  (ShelleyCompatible proto era, Monad m) =>
  HotKey (ProtoCrypto proto) m ->
  CanBeLeader proto ->
  ForgeBlockArgs m (ShelleyBlock proto era) ->
  m (ShelleyBlock proto era)
forgeShelleyBlock hotKey cbl ForgeBlockArgs{..} = do
  -- Forge an RB and attempt to announce an EB and/or certify a previously announced one:
  --
  --  * Certify: if the forge loop decided to certify a previously-announced
  --    EB ('fbMayLeiosCert' is a 'Just'), embed the certificate in the block body.
  --
  --  * Announce: forge and store a new EB from 'fbEbTxs' and announce it on this RB's header.
  --    When we are also certifying, 'fbEbTxs' contains transactions from the mempool that has already
  --    been rebased onto the post-certificate ledger state.
  mayEbAnn <-
    case Typeable.eqT @era @DijkstraEra of
      Just Refl -> mkAndStoreEb
      Nothing -> pure Nothing
  let rbBody = mkBody fbMayLeiosCert
      actualRbBodySize = SL.blockBodySize protocolVersion rbBody
  hdr <-
    mkHeader @_ @(ProtoCrypto proto)
      hotKey
      cbl
      fbIsLeader
      fbCurrentSlotNo
      fbCurrentBlockNo
      prevHash
      (SL.hashBlockBody @era rbBody)
      actualRbBodySize
      protocolVersion
      $ SJust
        HeaderLeiosExtension
          { containsCert = isJust fbMayLeiosCert
          , ebAnnouncement = maybeToStrictMaybe $ snd <$> mayEbAnn
          }

  let blk = mkShelleyBlock $ SL.Block hdr rbBody
  case fst <$> mayEbAnn of
    Just (forgedEb :: ForgedLeiosEb) -> do
      let announcingRbHashBytes =
            fromShort
              . toShortRawHash (Proxy @(ShelleyBlock proto era))
              $ blk.shelleyBlockHeaderHash
      traceWith fbLeiosTracer $
        TraceLeiosBlockAnnounced
          { announcingRbHashBytes = announcingRbHashBytes
          , announcedEbPoint = forgedEb.point
          }
      when (isJust fbMayLeiosCert) $
        traceWith fbLeiosTracer $
          TraceLeiosCertifiedAndAnnounced{atSlot = fbCurrentSlotNo, rbHash = MkRbHash announcingRbHashBytes}
    Nothing -> pure ()
  return $
    assert (verifyBlockIntegrity (configSlotsPerKESPeriod $ configConsensus fbConfig) blk) $
      blk
 where
  protocolVersion = shelleyProtocolVersion $ configBlock fbConfig

  -- A certifying Dijkstra block carries no rb-txs on the wire: the
  -- transaction sequence is resolved from the certified EB at apply
  -- time (see 'resolveLeiosBlock'). This matches the prototype's
  -- 'BodyCertificate cert Nothing' shape — without it, the wire body
  -- contains rb-txs that the receiver hashes but the apply-time body
  -- doesn't, so any subsequent re-hashing would diverge.
  mkBody :: Maybe LeiosCert -> SL.BlockBody era
  mkBody mayLeiosCert =
    let txs = case mayLeiosCert of
          Just _ -> Seq.empty
          Nothing -> Seq.fromList (fmap extractTx fbRbTxs)
        base = SL.mkBasicBlockBody & SL.txSeqBlockBodyL .~ txs
     in case Typeable.eqT @era @DijkstraEra of
          Just Refl -> base & leiosCertBlockBodyL .~ maybeToStrictMaybe mayLeiosCert
          Nothing -> base

  extractTx :: Validated (GenTx (ShelleyBlock proto era)) -> Core.Tx Core.TopTx era
  extractTx (ShelleyValidatedTx _txid vtx) = SL.extractTx vtx

  prevHash :: SL.PrevHash
  prevHash =
    toShelleyPrevHash @proto
      . castHash
      . getTipHash
      $ fbCurrentTickedLedgerState

  -- Produce an EB from fbEbTxs, store it into fbLeiosDb, and return the
  -- announcement to embed in the header. An honest forger only emits an
  -- EB when it has txs to put in it; empty mempool ⇒ no EB ⇒ no
  -- announcement (matches the original prototype). Persists the EB into
  -- 'LeiosDb' before returning, so the closure is available locally
  -- before the header carrying the announcement is finalised and
  -- diffused — a peer that fetches our header will be able to pull the
  -- closure from us in the same round-trip.
  mkAndStoreEb :: m (Maybe (ForgedLeiosEb, EbAnnouncement))
  mkAndStoreEb = case nonEmpty (fmap extractTx fbEbTxs) of
    Nothing -> pure Nothing
    Just ebTxs -> do
      let forgedEb = forgeLeiosEb fbCurrentSlotNo ebTxs
          ebHash = hashLeiosEb forgedEb.body
          ebSize = leiosEbBytesSize forgedEb.body
          ebAnn =
            EbAnnouncement
              { ebAnnouncementHash = ebHash
              , ebAnnouncementSize = ebSize
              }
          ebPoint =
            MkLeiosPoint
              { pointSlotNo = fbCurrentSlotNo
              , pointEbHash = ebHash
              }
      traceWith fbLeiosTracer $
        TraceLeiosBlockForged
          { slot = fbCurrentSlotNo
          , eb = forgedEb.body
          , ebMeasure = ByteSize32 ebSize
          , mempoolRestMeasure = ByteSize32 0
          }
      leiosDbInsertEbPoint fbLeiosDb ebPoint ebSize
      void $ leiosDbInsertEbBody fbLeiosDb ebPoint forgedEb.body
      void $ leiosDbInsertTxs fbLeiosDb forgedEb.txClosure
      traceWith fbLeiosTracer $
        TraceLeiosBlockStored
          { slot = fbCurrentSlotNo
          , eb = forgedEb.body
          }
      pure (Just (forgedEb, ebAnn))
