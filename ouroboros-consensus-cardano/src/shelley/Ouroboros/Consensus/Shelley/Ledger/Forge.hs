{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Ouroboros.Consensus.Shelley.Ledger.Forge (forgeShelleyBlock) where

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

forgeShelleyBlock ::
  forall m era proto.
  (ShelleyCompatible proto era, TxLimits (ShelleyBlock proto era), Monad m) =>
  HotKey (ProtoCrypto proto) m ->
  CanBeLeader proto ->
  ForgeBlockArgs (ShelleyBlock proto era) ->
  m (ForgedBlock (ShelleyBlock proto era))
forgeShelleyBlock
  hotKey
  cbl
  args@ForgeBlockArgs{..} =
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

    (txs, txsMeasure) = selectBlockTxs args

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
