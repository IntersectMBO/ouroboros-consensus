{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Shelley.Node.Leios
  ( -- * BlockForging
    TraceLeiosForge (..)
  , leiosSharedBlockForging

    -- * Endorser blocks
  , mkLeiosEb
  ) where

import Cardano.Binary (serialize')
import qualified Cardano.Crypto.Hash as Hash
import qualified Cardano.Ledger.Api.Era as L
import qualified Cardano.Ledger.Shelley.API as SL (extractValidatedTx)
import qualified Cardano.Protocol.TPraos.OCert as Absolute
import Control.Tracer (Tracer, traceWith)
import qualified Data.ByteString as BS
import Data.Foldable (for_)
import qualified Data.Text as T
import qualified Data.Vector.Strict as V
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Config (configConsensus, configLedger)
import Ouroboros.Consensus.Ledger.SupportsMempool
  ( GenTx
  , TxLimits (..)
  )
import Ouroboros.Consensus.Leios.Types (LeiosEb (..), TxHash (..))
import Ouroboros.Consensus.Mempool.API
  ( MempoolMeasure
  , MempoolSnapshot (..)
  )
import qualified Ouroboros.Consensus.Protocol.Ledger.HotKey as HotKey
import Ouroboros.Consensus.Protocol.Leios
  ( ConsensusConfig (leiosPraosConfig)
  , Leios
  )
import Ouroboros.Consensus.Protocol.Praos (praosCheckCanForge)
import Ouroboros.Consensus.Shelley.Eras (ShelleyBasedEra)
import Ouroboros.Consensus.Shelley.Ledger
  ( ShelleyBlock
  , ShelleyCompatible
  , Validated (ShelleyValidatedTx)
  , forgeShelleyBlockWithTxs
  )
import Ouroboros.Consensus.Shelley.Node.Common
  ( ShelleyLeaderCredentials (..)
  )
import Ouroboros.Consensus.Shelley.Protocol.Leios ()
import Ouroboros.Consensus.Util.IOLike (IOLike)

{-------------------------------------------------------------------------------
  BlockForging
-------------------------------------------------------------------------------}

-- | Events of 'leiosSharedBlockForging'.
data TraceLeiosForge blk
  = -- | The forge built an endorser block from the endorser-block part of the
    -- mempool snapshot. Nothing stores the endorser block, and the ranking
    -- block does not announce it.
    --
    -- It holds the slot of the ranking block, the endorser block, and the
    -- total measure of the endorser block's transactions.
    --
    -- 'forgeBlock' emits this event on the forging thread, after it signs the
    -- ranking block and before @Ouroboros.Consensus.NodeKernel.Forge.forge@
    -- adds that block to the ChainDB. Forcing the 'LeiosEb', even its length,
    -- serialises and hashes every endorser-block transaction, up to
    -- @ppMaxEndorserBlockTxsSizeL@ bytes. The slot and the measure need no
    -- such work. If a tracer renders the 'LeiosEb' on the forging thread, the
    -- forge waits for that work. A tracer that passes the event to an
    -- asynchronous backend does not make the forge wait.
    TraceForgedLeiosEb SlotNo LeiosEb (MempoolMeasure blk)

deriving instance
  (Eq (TxMeasurePhase1 blk), Eq (TxMeasurePhase2 blk), Eq (TxEbMeasure blk)) =>
  Eq (TraceLeiosForge blk)
deriving instance
  (Show (TxMeasurePhase1 blk), Show (TxMeasurePhase2 blk), Show (TxEbMeasure blk)) =>
  Show (TraceLeiosForge blk)

-- | Create a 'BlockForging' record safely using the given 'Hotkey'.
--
-- The name of the era (separated by a @_@) will be appended to each
-- 'forgeLabel'.
--
-- 'forgeBlock' puts the ranking-block part of the mempool snapshot in the
-- block. If the endorser-block part is not empty, 'forgeBlock' builds an
-- endorser block from it and traces that endorser block.
leiosSharedBlockForging ::
  forall m c era.
  ( ShelleyCompatible (Leios c) era
  , TxLimits (ShelleyBlock (Leios c) era)
  , IOLike m
  ) =>
  Tracer m (TraceLeiosForge (ShelleyBlock (Leios c) era)) ->
  HotKey.HotKey c m ->
  (SlotNo -> Absolute.KESPeriod) ->
  ShelleyLeaderCredentials c ->
  BlockForging m (ShelleyBlock (Leios c) era)
leiosSharedBlockForging
  tracer
  hotKey
  slotToPeriod
  ShelleyLeaderCredentials
    { shelleyLeaderCredentialsCanBeLeader = canBeLeader
    , shelleyLeaderCredentialsLabel = label
    } =
    BlockForging
      { forgeLabel = label <> "_" <> T.pack (L.eraName @era)
      , canBeLeader
      , updateForgeState = \_ curSlot _ ->
          forgeStateUpdateInfoFromUpdateInfo
            <$> HotKey.evolve hotKey (slotToPeriod curSlot)
      , checkCanForge = \cfg curSlot _tickedChainDepState _isLeader ->
          praosCheckCanForge
            (leiosPraosConfig (configConsensus cfg))
            curSlot
      , forgeBlock = forgeLeiosBlock
      , finalize = HotKey.finalize hotKey
      }
   where
    forgeLeiosBlock ::
      ForgeBlockArgs (ShelleyBlock (Leios c) era) ->
      m (ForgedBlock (ShelleyBlock (Leios c) era))
    forgeLeiosBlock
      args@ForgeBlockArgs
        { fbConfig
        , fbCurrentSlotNo
        , fbCurrentTickedLedgerState
        , fbMempoolSnapshot
        } = do
        forged <- forgeShelleyBlockWithTxs hotKey canBeLeader args (rbTxs, rbTxsMeasure)
        for_ (mkLeiosEb ebTxs) $ \eb ->
          traceWith tracer $ TraceForgedLeiosEb fbCurrentSlotNo eb ebTxsMeasure
        pure forged
       where
        ledgerConfig = configLedger fbConfig
        -- One 'snapshotPartition' gives both parts, so the endorser-block part
        -- starts right after the last transaction in the block.
        (rbTxs, rbTxsMeasure, ebTxs, ebTxsMeasure) =
          snapshotPartition
            fbMempoolSnapshot
            (blockCapacityTxMeasure ledgerConfig fbCurrentTickedLedgerState)
            (ebCapacityTxMeasure ledgerConfig fbCurrentTickedLedgerState)

{-------------------------------------------------------------------------------
  Endorser blocks
-------------------------------------------------------------------------------}

-- | The endorser block that references the given transactions, in the order
-- of the list. It gives 'Nothing' for an empty list.
--
-- A reference holds the Blake2b-256 hash and the length of the bytes that
-- 'Cardano.Binary.toCBOR' writes for the ledger transaction
-- ('SL.extractValidatedTx'). The 'Cardano.Binary.ToCBOR' instance of 'GenTx'
-- wraps the same bytes with 'Ouroboros.Network.Block.wrapCBORinCBOR'.
-- 'Ouroboros.Consensus.Node.Serialisation.encodeNodeToNode' uses that
-- instance, so these are the transaction bytes that peers exchange.
mkLeiosEb ::
  ShelleyBasedEra era =>
  [Validated (GenTx (ShelleyBlock proto era))] ->
  Maybe LeiosEb
mkLeiosEb [] = Nothing
mkLeiosEb txs = Just $ MkLeiosEb $ V.fromList $ map reference txs
 where
  -- 'V.fromList' forces each pair only to WHNF. 'seq' forces the hash and
  -- the size too, so a reference does not keep the transaction alive.
  reference (ShelleyValidatedTx _txid vtx) = txHash `seq` txSize `seq` (txHash, txSize)
   where
    bytes = serialize' $ SL.extractValidatedTx vtx
    txHash = MkTxHash $ Hash.hashToBytes $ Hash.hashWith @Hash.Blake2b_256 id bytes
    txSize = fromIntegral $ BS.length bytes
