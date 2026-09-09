{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
-- TODO: Ledger has a few deprecations that we are ignoring for now
{-# OPTIONS_GHC -Wno-deprecations #-}

module Ouroboros.Consensus.Shelley.Ledger.Forge (forgeShelleyBlock) where

import qualified Cardano.Ledger.Core as Core (TopTx, Tx)
import qualified Cardano.Ledger.Core as SL
  ( blockBodySize
  , hashBlockBody
  , mkBasicBlockBody
  , txSeqBlockBodyL
  )
import qualified Cardano.Ledger.Shelley.API as SL (Block (..), extractTx)
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
  (ShelleyCompatible proto era, Monad m) =>
  HotKey (ProtoCrypto proto) m ->
  CanBeLeader proto ->
  ForgeBlockArgs (ShelleyBlock proto era) ->
  m (ShelleyBlock proto era)
forgeShelleyBlock
  hotKey
  cbl
  ForgeBlockArgs{..} =
    do
      hdr <-
        mkHeader @_ @(ProtoCrypto proto)
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
      return $
        assert (verifyBlockIntegrity (configSlotsPerKESPeriod $ configConsensus fbConfig) blk) $
          blk
   where
    protocolVersion = shelleyProtocolVersion $ configBlock fbConfig

    body =
      SL.mkBasicBlockBody
        & SL.txSeqBlockBodyL
          .~ Seq.fromList (fmap extractTx fbTxs)

    actualBodySize = SL.blockBodySize protocolVersion body

    extractTx :: Validated (GenTx (ShelleyBlock proto era)) -> Core.Tx Core.TopTx era
    extractTx (ShelleyValidatedTx _txid vtx) = SL.extractTx vtx

    prevHash :: SL.PrevHash
    prevHash =
      toShelleyPrevHash @proto
        . castHash
        . getTipHash
        $ fbCurrentTickedLedgerState
