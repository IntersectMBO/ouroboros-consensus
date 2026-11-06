{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | One whole-transaction encoding for forging and local fetch discovery.
module Ouroboros.Consensus.Shelley.Ledger.TxBytes (serializeTxForLeios) where

import Cardano.Ledger.Binary (serialize')
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Core as SL
import Cardano.Ledger.Dijkstra.Tx (toCBORForBlockInclusion)
import Cardano.Ledger.Hashes (SafeToHash)
import Data.ByteString (ByteString)

-- | The bytes of a transaction that an endorser block references.
--
-- These bytes must keep the @is_valid@ flag, because a peer cannot get it
-- from anywhere else. 'EncCBOR' drops the flag in Dijkstra, so we use the
-- block-body encoding.
--
-- Their length must not exceed 'txByteSizeWithOverhead'. Otherwise the
-- endorser block can exceed the size that 'txEbMeasureDijkstra' reserved
-- for it, and header validation rejects the block with 'LeiosEbTooBig'.
serializeTxForLeios ::
  forall era.
  (Core.EraTx era, SafeToHash (Core.Tx Core.TopTx era)) =>
  Core.Tx Core.TopTx era ->
  ByteString
serializeTxForLeios = serialize' (SL.eraProtVerLow @era) . toCBORForBlockInclusion
