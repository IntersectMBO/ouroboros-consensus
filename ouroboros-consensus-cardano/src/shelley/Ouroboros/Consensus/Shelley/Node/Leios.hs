{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Ouroboros.Consensus.Shelley.Node.Leios
  ( -- * BlockForging
    leiosSharedBlockForging

    -- * Endorser blocks
  , mkLeiosEb
  ) where

import Cardano.Binary (serialize')
import qualified Cardano.Crypto.Hash as Hash
import qualified Cardano.Ledger.Api.Era as L
import qualified Cardano.Ledger.Shelley.API as SL (extractValidatedTx)
import qualified Cardano.Protocol.TPraos.OCert as Absolute
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Vector.Strict as V
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Config (configConsensus)
import Ouroboros.Consensus.Ledger.SupportsMempool
  ( GenTx
  , TxLimits
  )
import Ouroboros.Consensus.Leios.Types (LeiosEb (..), TxHash (..))
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
  , forgeShelleyBlock
  )
import Ouroboros.Consensus.Shelley.Node.Common
  ( ShelleyLeaderCredentials (..)
  )
import Ouroboros.Consensus.Shelley.Protocol.Leios ()
import Ouroboros.Consensus.Util.IOLike (IOLike)

{-------------------------------------------------------------------------------
  BlockForging
-------------------------------------------------------------------------------}

-- | Create a 'BlockForging' record safely using the given 'Hotkey'.
--
-- The name of the era (separated by a @_@) will be appended to each
-- 'forgeLabel'.
leiosSharedBlockForging ::
  forall m c era.
  ( ShelleyCompatible (Leios c) era
  , TxLimits (ShelleyBlock (Leios c) era)
  , IOLike m
  ) =>
  HotKey.HotKey c m ->
  (SlotNo -> Absolute.KESPeriod) ->
  ShelleyLeaderCredentials c ->
  BlockForging m (ShelleyBlock (Leios c) era)
leiosSharedBlockForging
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
      , forgeBlock = forgeShelleyBlock hotKey canBeLeader
      , finalize = HotKey.finalize hotKey
      }

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
