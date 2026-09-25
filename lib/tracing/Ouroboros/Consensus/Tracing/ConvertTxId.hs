{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Projection of a transaction id to its raw bytes.
--
-- Used by the tracing instances to render transaction ids, but it is a plain
-- projection over Consensus block types with no dependency on the logging
-- framework.
module Ouroboros.Consensus.Tracing.ConvertTxId
  ( ConvertTxId (..)
  ) where

import qualified Cardano.Crypto.Hash as Crypto
import qualified Cardano.Crypto.Hashing as Byron.Crypto
import qualified Cardano.Ledger.Hashes as Ledger
import qualified Cardano.Ledger.TxIn as Ledger
import Data.ByteString (ByteString)
import Data.SOP
import Ouroboros.Consensus.Byron.Ledger.Block (ByronBlock)
import Ouroboros.Consensus.Byron.Ledger.Mempool (TxId (..))
import Ouroboros.Consensus.HardFork.Combinator
import Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)
import Ouroboros.Consensus.Shelley.Ledger.Mempool (TxId (..))
import Ouroboros.Consensus.TypeFamilyWrappers (unwrapGenTxId)

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

instance
  All ConvertTxId xs =>
  ConvertTxId (HardForkBlock xs)
  where
  txIdToRawBytes =
    hcollapse
      . hcmap (Proxy @ConvertTxId) (K . txIdToRawBytes . unwrapGenTxId)
      . getOneEraGenTxId
      . getHardForkGenTxId
