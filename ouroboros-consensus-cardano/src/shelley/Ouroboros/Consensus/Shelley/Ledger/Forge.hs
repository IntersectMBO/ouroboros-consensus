{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Ouroboros.Consensus.Shelley.Ledger.Forge
  ( ebTxBytes
  , forgeShelleyBlock
  ) where

import Cardano.Ledger.Binary (serialize')
import Cardano.Ledger.Block (EbReferencesAnnouncement (..))
import qualified Cardano.Ledger.Core as Core (EraTx, TopTx, Tx)
import qualified Cardano.Ledger.Core as SL
  ( blockBodySize
  , eraProtVerLow
  , hashBlockBody
  , mkBasicBlockBody
  , txSeqBlockBodyL
  )
import Cardano.Ledger.Hashes (unsafeMakeSafeHash)
import qualified Cardano.Ledger.Shelley.API as SL (Block (..), extractValidatedTx)
import qualified Cardano.Protocol.TPraos.BlockHeader as SL
import Control.Exception
import Data.ByteString (ByteString)
import Data.Foldable (asum)
import Data.List.NonEmpty (nonEmpty)
import Data.Maybe.Strict (maybeToStrictMaybe)
import qualified Data.Sequence.Strict as Seq
import Lens.Micro ((&), (.~))
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.SupportsMempool
import Ouroboros.Consensus.Leios.Types
  ( EndorserBlock
  , encodeEndorserBlockSize
  , endorserBlockFromTxBytes
  , endorserBlockHash
  )
import Ouroboros.Consensus.Protocol.Abstract (CanBeLeader)
import Ouroboros.Consensus.Protocol.Ledger.HotKey (HotKey)
import Ouroboros.Consensus.Protocol.Praos.Common (WhenLeios, pureLeiosOnly)
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
  ( ShelleyCompatible proto era
  , Applicative (WhenLeios proto)
  , Foldable (WhenLeios proto)
  , Monad m
  ) =>
  HotKey (ProtoCrypto proto) m ->
  CanBeLeader proto ->
  ForgeBlockArgs (ShelleyBlock proto era) ->
  m (ForgeBlockResult (ShelleyBlock proto era))
forgeShelleyBlock
  hotKey
  cbl
  ForgeBlockArgs{..} =
    do
      hdr <-
        mkHeader @_ @(ProtoCrypto proto)
          (Proxy @era)
          hotKey
          cbl
          fbIsLeader
          fbCurrentSlotNo
          fbCurrentBlockNo
          prevHash
          (SL.hashBlockBody @era body)
          actualBodySize
          protocolVersion
          leiosFields
      let blk = mkShelleyBlock $ SL.Block hdr body
      return
        ForgeBlockResult
          { rankingBlock =
              assert (verifyBlockIntegrity (configSlotsPerKESPeriod $ configConsensus fbConfig) blk) blk
          , endorserBlock = asum forgedEb
          }
   where
    protocolVersion = shelleyProtocolVersion $ configBlock fbConfig

    -- Only a protocol with Leios has a value in 'WhenLeios', so the other
    -- protocols build no endorser block. CIP-0164 does not allow an empty
    -- endorser block.
    forgedEb :: WhenLeios proto (Maybe EndorserBlock)
    forgedEb =
      pureLeiosOnly @proto $
        endorserBlockFromTxBytes . fmap (ebTxBytes . extractTx) <$> nonEmpty fbEbTxs

    -- TODO Forging does not yet certify endorser blocks.
    leiosFields = (,) False . maybeToStrictMaybe . fmap announce <$> forgedEb

    announce eb =
      EbReferencesAnnouncement
        { ebReferencesAnnouncementHash = unsafeMakeSafeHash (endorserBlockHash eb)
        , ebReferencesAnnouncementSize = encodeEndorserBlockSize eb
        }

    body =
      SL.mkBasicBlockBody
        & SL.txSeqBlockBodyL
          .~ Seq.fromList (fmap extractTx fbTxs)

    actualBodySize = SL.blockBodySize protocolVersion body

    extractTx :: Validated (GenTx (ShelleyBlock proto era)) -> Core.Tx Core.TopTx era
    extractTx (ShelleyValidatedTx _txid vtx) = SL.extractValidatedTx vtx

    prevHash :: SL.PrevHash
    prevHash =
      toShelleyPrevHash @proto
        . castHash
        . getTipHash
        $ fbCurrentTickedLedgerState

-- | The bytes of a transaction that an endorser block references.
--
-- The length of these bytes must not exceed 'txByteSizeWithOverhead'.
-- Otherwise the endorser block can exceed the size that
-- 'txEbMeasureDijkstra' reserved for it, and header validation rejects the
-- block with 'LeiosEbTooBig'.
ebTxBytes :: forall era. Core.EraTx era => Core.Tx Core.TopTx era -> ByteString
ebTxBytes = serialize' (SL.eraProtVerLow @era)
