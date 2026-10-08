{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Ouroboros.Consensus.Shelley.Ledger.Forge
  ( forgeShelleyBlock
  , forgeShelleyBlockWithTxs
  ) where

import qualified Cardano.Ledger.Core as Core (TopTx, Tx)
import qualified Cardano.Ledger.Core as SL
  ( blockBodySize
  , hashBlockBody
  , mkBasicBlockBody
  , txSeqBlockBodyL
  )
import qualified Cardano.Ledger.Shelley.API as SL (Block (..), extractValidatedTx)
import qualified Cardano.Protocol.TPraos.BlockHeader as SL
import Control.Exception
import qualified Data.Sequence.Strict as Seq
import Lens.Micro ((&), (.~))
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.SupportsMempool
import Ouroboros.Consensus.Mempool.API (MempoolMeasure)
import Ouroboros.Consensus.Protocol.Abstract (CanBeLeader)
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

-- | Forge a block without an endorser block. 'selectBlockTxs' selects its
-- transactions, the ranking-block part of 'fbMempoolSnapshot'.
forgeShelleyBlock ::
  forall m era proto.
  (ShelleyCompatible proto era, TxLimits (ShelleyBlock proto era), Monad m) =>
  HotKey (ProtoCrypto proto) m ->
  CanBeLeader proto ->
  ForgeBlockArgs (ShelleyBlock proto era) ->
  m (ForgedBlock (ShelleyBlock proto era))
forgeShelleyBlock hotKey cbl args =
  forgeShelleyBlockWithTxs hotKey cbl args (selectBlockTxs args)

-- | Forge a block with the given transactions and their total measure. It
-- ignores 'fbMempoolSnapshot'. The caller selects transactions that fit
-- 'blockCapacityTxMeasure' of 'fbCurrentTickedLedgerState' and that apply in
-- order to that state, as a prefix of the snapshot does.
-- 'Ouroboros.Consensus.Shelley.Node.Leios.leiosSharedBlockForging' partitions
-- the snapshot itself and then calls this function.
forgeShelleyBlockWithTxs ::
  forall m era proto.
  (ShelleyCompatible proto era, Monad m) =>
  HotKey (ProtoCrypto proto) m ->
  CanBeLeader proto ->
  ForgeBlockArgs (ShelleyBlock proto era) ->
  ([Validated (GenTx (ShelleyBlock proto era))], MempoolMeasure (ShelleyBlock proto era)) ->
  m (ForgedBlock (ShelleyBlock proto era))
forgeShelleyBlockWithTxs
  hotKey
  cbl
  ForgeBlockArgs{..}
  (txs, txsMeasure) =
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
      let blk = mkShelleyBlock $ SL.Block hdr body
      return
        ForgedBlock
          { forgedBlock =
              assert (verifyBlockIntegrity (configSlotsPerKESPeriod $ configConsensus fbConfig) blk) $
                blk
          , forgedTxs = txs
          , forgedTxsMeasure = txsMeasure
          }
   where
    protocolVersion = shelleyProtocolVersion $ configBlock fbConfig

    body =
      SL.mkBasicBlockBody
        & SL.txSeqBlockBodyL
          .~ Seq.fromList (fmap extractTx txs)

    actualBodySize = SL.blockBodySize protocolVersion body

    extractTx :: Validated (GenTx (ShelleyBlock proto era)) -> Core.Tx Core.TopTx era
    extractTx (ShelleyValidatedTx _txid vtx) = SL.extractValidatedTx vtx

    prevHash :: SL.PrevHash
    prevHash =
      toShelleyPrevHash @proto
        . castHash
        . getTipHash
        $ fbCurrentTickedLedgerState
