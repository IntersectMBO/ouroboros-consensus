{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Small block-indexed classes used by the Consensus tracing instances.
--
-- These were originally defined in @Cardano.Node.Queries@ in @cardano-node@;
-- they are pure projections over Consensus block types (no dependency on
-- @cardano-api@), so they live here next to the tracing instances that need
-- them.
module Ouroboros.Consensus.Tracing.Queries
  ( ConvertTxId (..)
  , HasKESInfo (..)
  , GetKESInfo (..)
  ) where

import qualified Cardano.Crypto.Hash as Crypto
import qualified Cardano.Crypto.Hashing as Byron.Crypto
import qualified Cardano.Ledger.Hashes as Ledger
import qualified Cardano.Ledger.TxIn as Ledger
import           Data.ByteString (ByteString)
import           Data.SOP
import           Ouroboros.Consensus.Block (ForgeStateInfo, ForgeStateUpdateError)
import           Ouroboros.Consensus.Byron.Ledger.Block (ByronBlock)
import           Ouroboros.Consensus.Byron.Ledger.Mempool (TxId (..))
import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.Ledger.SupportsMempool (GenTx, TxId)
import qualified Ouroboros.Consensus.Protocol.Ledger.HotKey as HotKey
import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)
import           Ouroboros.Consensus.Shelley.Ledger.Mempool (TxId (..))
import           Ouroboros.Consensus.TypeFamilyWrappers

--
-- * TxId -> ByteString projection
--
-- | Convert a transaction ID to raw bytes.
class ConvertTxId blk where
  txIdToRawBytes :: TxId (GenTx blk) -> ByteString

instance ConvertTxId ByronBlock where
  txIdToRawBytes (ByronTxId txId) = Byron.Crypto.abstractHashToBytes txId
  txIdToRawBytes (ByronDlgId dlgId) = Byron.Crypto.abstractHashToBytes dlgId
  txIdToRawBytes (ByronUpdateProposalId upId) =
    Byron.Crypto.abstractHashToBytes upId
  txIdToRawBytes (ByronUpdateVoteId voteId) =
    Byron.Crypto.abstractHashToBytes voteId

instance ConvertTxId (ShelleyBlock protocol c) where
  txIdToRawBytes (ShelleyTxId txId) =
    Crypto.hashToBytes . Ledger.extractHash . Ledger.unTxId $ txId

instance All ConvertTxId xs
      => ConvertTxId (HardForkBlock xs) where
  txIdToRawBytes =
    hcollapse
      . hcmap (Proxy @ConvertTxId) (K . txIdToRawBytes . unwrapGenTxId)
      . getOneEraGenTxId
      . getHardForkGenTxId

--
-- * HasKESInfo
--
class HasKESInfo blk where
  getKESInfo :: Proxy blk -> ForgeStateUpdateError blk -> Maybe HotKey.KESInfo
  getKESInfo _ _ = Nothing

instance HasKESInfo (ShelleyBlock protocol era) where
  getKESInfo _ (HotKey.KESCouldNotEvolve ki _) = Just ki
  getKESInfo _ (HotKey.KESKeyAlreadyPoisoned ki _) = Just ki

instance HasKESInfo ByronBlock

instance All HasKESInfo xs => HasKESInfo (HardForkBlock xs) where
  getKESInfo _ =
      hcollapse
    . hcmap (Proxy @HasKESInfo) getOne
    . getOneEraForgeStateUpdateError
   where
    getOne :: forall blk. HasKESInfo blk
           => WrapForgeStateUpdateError blk
           -> K (Maybe HotKey.KESInfo) blk
    getOne = K . getKESInfo (Proxy @blk) . unwrapForgeStateUpdateError

--
-- * GetKESInfo
--
class GetKESInfo blk where
  getKESInfoFromStateInfo :: Proxy blk -> ForgeStateInfo blk -> Maybe HotKey.KESInfo
  getKESInfoFromStateInfo _ _ = Nothing

instance GetKESInfo (ShelleyBlock protocol era) where
  getKESInfoFromStateInfo _ = Just

instance GetKESInfo ByronBlock

instance All GetKESInfo xs => GetKESInfo (HardForkBlock xs) where
  getKESInfoFromStateInfo _ forgeStateInfo =
      case forgeStateInfo of
        CurrentEraLacksBlockForging _ -> Nothing
        CurrentEraForgeStateUpdated currentEraForgeStateInfo ->
            hcollapse
          . hcmap (Proxy @GetKESInfo) getOne
          . getOneEraForgeStateInfo
          $ currentEraForgeStateInfo
    where
      getOne :: forall blk. GetKESInfo blk
             => WrapForgeStateInfo blk
             -> K (Maybe HotKey.KESInfo) blk
      getOne = K . getKESInfoFromStateInfo (Proxy @blk) . unwrapForgeStateInfo
