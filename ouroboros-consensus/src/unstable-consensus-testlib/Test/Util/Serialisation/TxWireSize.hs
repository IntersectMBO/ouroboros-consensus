{-# LANGUAGE FlexibleContexts #-}

module Test.Util.Serialisation.TxWireSize (
    prop_txWireSize
  , prop_txWireSize_txSubmission
  ) where

import           Codec.CBOR.Write (toLazyByteString)
import qualified Data.ByteString.Lazy as BSL
import           Ouroboros.Consensus.Block.Abstract (CodecConfig)
import           Ouroboros.Consensus.Ledger.SupportsMempool (GenTx,
                     TxLimits (..))
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
                     (BlockNodeToNodeVersion)
import           Ouroboros.Consensus.Node.Serialisation
import           Ouroboros.Network.SizeInBytes
import           Ouroboros.Network.TxSubmission.Inbound.V2.State
                     (const_MAX_TX_SIZE_DISCREPENCY)
import           Test.Tasty.QuickCheck
import           Test.Util.Serialisation.Roundtrip (WithVersion (..))


-- | Verify that `txWriteSize` agrees with the encoded `GenTx` size up to
-- `const_MAX_TX_SIZE_DISCREPENCY` allowed by `tx-submission` mini-protocol.
--
-- NOTE: `tx`s which do not satisfy this property will terminate connections.
--
prop_txWireSize_txSubmission ::
    ( SerialiseNodeToNode blk (GenTx blk)
    , TxLimits blk
    )
  => CodecConfig blk
  -> WithVersion (BlockNodeToNodeVersion blk) (GenTx blk)
  -> Property
prop_txWireSize_txSubmission ccfg (WithVersion version tx) =
      counterexample ("encoded size " ++ show encSize ++ ", computed size " ++ show cmpSize)
    $ counterexample ("diff size " ++ show diffSize)
    $ label (show diffSize)
    $ fromIntegral (abs diffSize) <= const_MAX_TX_SIZE_DISCREPENCY
  where
    encSize, cmpSize :: SizeInBytes

    encSize  = fromIntegral (BSL.length $ toLazyByteString (encodeNodeToNode ccfg version tx))
    cmpSize  = txWireSize tx

    diffSize :: Int
    diffSize = fromIntegral encSize - fromIntegral cmpSize


-- | Verify that `txWriteSize` is very close to the real tx size.
--
-- The `txWireSize` doesn't take into account if HFC is enabled or not.  If it
-- is enabled, the `wireTxSize` for `GenTx (HardForkBlock xs)` will agree with
-- the encoded size, but if it is disabled it will overestimate the value by HFC
-- envelope (at most 3 bytes, 2 for forcible future)
--
prop_txWireSize ::
    ( SerialiseNodeToNode blk (GenTx blk)
    , TxLimits blk
    )
  => (GenTx blk -> Maybe String)
  -- ^ show tx bytes
  -> CodecConfig blk
  -> WithVersion (BlockNodeToNodeVersion blk) (GenTx blk)
  -> Property
prop_txWireSize getTxBytes ccfg (WithVersion version tx) =
      counterexample ("encoded size " ++ show encSize ++ ", computed size " ++ show cmpSize)
    $ counterexample ("encoded tx:\n" ++ show encoded)
    $ label (show diffSize)
    $ case getTxBytes tx of
        Just txBytes -> counterexample ("tx bytes:\n" ++ txBytes)
        Nothing      -> property
    $ encSize <= cmpSize
      .&&.
      encSize + 3 >= cmpSize


  where
    encoded :: BSL.ByteString
    encoded = toLazyByteString (encodeNodeToNode ccfg version tx)

    encSize, cmpSize :: SizeInBytes
    encSize  = fromIntegral (BSL.length encoded)
    cmpSize  = txWireSize tx

    diffSize :: Int
    diffSize = fromIntegral encSize - fromIntegral cmpSize
