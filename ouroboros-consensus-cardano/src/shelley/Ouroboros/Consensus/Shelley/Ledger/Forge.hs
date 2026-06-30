{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Ouroboros.Consensus.Shelley.Ledger.Forge (forgeShelleyBlock) where

import Cardano.Crypto.Leios (LeiosCert)
import Cardano.Ledger.BaseTypes (StrictMaybe (..))
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
import Control.Monad (void)
import Control.Tracer (traceWith)
import Data.ByteString.Short (fromShort)
import Data.Maybe.Strict (isSJust)
import qualified Data.Sequence.Strict as Seq
import qualified Data.Typeable as Typeable
import LeiosDemoDb
  ( LeiosDbConnection (..)
  , leiosDbQueryCompletedEbByHash
  )
import LeiosDemoTypes
  ( EbAnnouncement (..)
  , ForgedLeiosEb (..)
  , LeiosPoint (..)
  , RbHash (..)
  , TraceLeiosKernel (..)
  , forgeLeiosEb
  , hashLeiosEb
  , leiosEbBytesSize
  , minCertificationGap
  )
import LeiosVoteState (LeiosVoteState (queryCert))
import Lens.Micro ((&), (.~))
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.SupportsMempool
import Ouroboros.Consensus.Protocol.Abstract (CanBeLeader)
import Ouroboros.Consensus.Protocol.Ledger.HotKey (HotKey)
import Ouroboros.Consensus.Shelley.Eras (DijkstraEra)
import Ouroboros.Consensus.Shelley.Ledger.Block
import Ouroboros.Consensus.Shelley.Ledger.Config
  ( shelleyProtocolVersion
  )
import Ouroboros.Consensus.Shelley.Ledger.Integrity
import Ouroboros.Consensus.Shelley.Ledger.Mempool
import Ouroboros.Consensus.Shelley.Protocol.Abstract
  ( ProtoCrypto
  , ProtocolHeaderSupportsKES (configSlotsPerKESPeriod, protocolStateLeiosInfo)
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
  -- For the Dijkstra era only: either certify a previously-announced EB
  -- (and embed a Leios certificate in the block body), or — if no
  -- previous announcement is ready to be certified — possibly forge a
  -- new EB and announce it on this block's header. Other eras do
  -- neither.
  (mayEbAnn, mayLeiosCert) <-
    case Typeable.eqT @era @DijkstraEra of
      Just Refl -> decideLeios
      Nothing -> pure (Nothing, SNothing)
  let rbBody = mkBody mayLeiosCert
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
      (snd <$> mayEbAnn)
      (isSJust mayLeiosCert)

  let blk = mkShelleyBlock $ SL.Block hdr rbBody
  case fst <$> mayEbAnn of
    Just forgedEb -> do
      let rbHashBytes =
            fromShort $
              toShortRawHash (Proxy @(ShelleyBlock proto era)) $
                blk.shelleyBlockHeaderHash
          ebPoint =
            MkLeiosPoint
              { pointSlotNo = fbCurrentSlotNo
              , pointEbHash = hashLeiosEb . body $ forgedEb
              }
      traceWith fbLeiosTracer $
        TraceLeiosBlockAnnounced
          { announcingRbHashBytes = rbHashBytes
          , announcedEbPoint = ebPoint
          }
      storeEb ebPoint (MkRbHash rbHashBytes) forgedEb
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
  mkBody :: StrictMaybe LeiosCert -> SL.BlockBody era
  mkBody mayLeiosCert =
    let txs = case mayLeiosCert of
          SJust _ -> Seq.empty
          SNothing -> Seq.fromList (fmap extractTx fbRbTxs)
        base = SL.mkBasicBlockBody & SL.txSeqBlockBodyL .~ txs
     in case Typeable.eqT @era @DijkstraEra of
          Just Refl -> base & leiosCertBlockBodyL .~ mayLeiosCert
          Nothing -> base

  extractTx :: Validated (GenTx (ShelleyBlock proto era)) -> Core.Tx Core.TopTx era
  extractTx (ShelleyValidatedTx _txid vtx) = SL.extractTx vtx

  prevHash :: SL.PrevHash
  prevHash =
    toShelleyPrevHash @proto
      . castHash
      . getTipHash
      $ fbCurrentTickedLedgerState

  -- Dijkstra-only: certify a previously-announced EB on this chain if
  -- one is ready (announced + downloaded + gap > minCertificationGap +
  -- certificate available in LeiosDb), suppressing this block's own EB
  -- announcement. Otherwise fall back to forging a new EB (when the
  -- mempool has txs for one). Mirrors the prototype's 'decideForgeType'.
  decideLeios :: m (Maybe (ForgedLeiosEb, EbAnnouncement), StrictMaybe LeiosCert)
  decideLeios = do
    cert <- decideCertify
    case cert of
      SJust _ -> pure (Nothing, cert)
      SNothing -> do
        ann <- mkEb
        pure (ann, SNothing)

  decideCertify :: m (StrictMaybe LeiosCert)
  decideCertify =
    case fbChainDepState >>= protocolStateLeiosInfo (Proxy @proto) of
      Nothing -> pure SNothing
      Just (_, Origin) -> pure SNothing
      Just (ann, NotOrigin prevSlotNo)
        | unSlotNo fbCurrentSlotNo - unSlotNo prevSlotNo <= minCertificationGap ->
            pure SNothing
        | otherwise -> do
            let ebPoint =
                  MkLeiosPoint
                    { pointSlotNo = prevSlotNo
                    , pointEbHash = ebAnnouncementHash ann
                    }
            -- TODO: Why exactly do we guard against this? Also, shouldn't we
            -- detect it the other way around: if we have a cert, but not
            -- downloaded it ourselves -> warning!
            mClosure <- leiosDbQueryCompletedEbByHash fbLeiosDb (pointEbHash ebPoint)
            case mClosure of
              Nothing -> do
                traceWith fbLeiosTracer $
                  MkTraceLeiosKernel $
                    "EB not yet downloaded: " <> show ebPoint
                pure SNothing
              Just _ -> do
                -- we get the hash of the previous block header because eb certification must
                -- happen withing one block (this is the linear aspect of linear leios).
                let announcingRbHash = case getTipHash fbCurrentTickedLedgerState of
                      BlockHash h -> MkRbHash (toRawHash (Proxy @(ShelleyBlock proto era)) h)
                      GenesisHash -> error "decideCertify: cannot certify on top of genesis"
                mCert <- queryCert fbLeiosVoteState announcingRbHash
                case mCert of
                  Nothing -> do
                    traceWith fbLeiosTracer $
                      MkTraceLeiosKernel $
                        "EB downloaded but no certificate: " <> show ebPoint
                    pure SNothing
                  Just cert -> do
                    traceWith fbLeiosTracer $
                      TraceLeiosBlockCertified
                        { atSlot = fbCurrentSlotNo
                        , certifiedPoint = ebPoint
                        }
                    pure (SJust cert)

  -- Produce an EB from fbEbTxs, store it into fbLeiosDb, and return the
  -- announcement to embed in the header. An honest forger only emits an
  -- EB when it has txs to put in it; empty mempool ⇒ no EB ⇒ no
  -- announcement (matches the original prototype).
  mkEb :: m (Maybe (ForgedLeiosEb, EbAnnouncement))
  mkEb = case nonEmpty (fmap extractTx fbEbTxs) of
    Nothing -> pure Nothing
    Just ebTxs -> do
      let forgedEb = forgeLeiosEb fbCurrentSlotNo ebTxs
          ebHash = hashLeiosEb forgedEb.body
          ebSize = leiosEbBytesSize (forgedEb.body)
          ebAnn =
            EbAnnouncement
              { ebAnnouncementHash = ebHash
              , ebAnnouncementSize = ebSize
              }
      traceWith fbLeiosTracer $
        TraceLeiosBlockForged
          { slot = fbCurrentSlotNo
          , eb = forgedEb.body
          , ebMeasure = ByteSize32 ebSize
          , mempoolRestMeasure = ByteSize32 0
          }

      pure (Just (forgedEb, ebAnn))

  storeEb :: LeiosPoint -> RbHash -> ForgedLeiosEb -> m ()
  storeEb point announcingRbHash forgedEb = do
    let ebSize = leiosEbBytesSize (forgedEb.body)
    leiosDbInsertEbPoint fbLeiosDb point ebSize
    leiosDbInsertEbBody fbLeiosDb point (forgedEb.body)
    void $ leiosDbInsertTxs fbLeiosDb (Just announcingRbHash) (forgedEb.txClosure)
    traceWith fbLeiosTracer $
      TraceLeiosBlockStored
        { slot = point.pointSlotNo
        , eb = forgedEb.body
        }
