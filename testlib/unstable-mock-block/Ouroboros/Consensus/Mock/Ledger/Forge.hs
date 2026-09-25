{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.Mock.Ledger.Forge
  ( ForgeExt (..)
  , forgeSimple
  ) where

import Cardano.Binary (toCBOR)
import Cardano.Crypto.Hash (hashWithSerialiser)
import Codec.Serialise (Serialise (..), serialise)
import qualified Data.ByteString.Lazy as Lazy
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.SupportsMempool (LedgerSupportsMempool (txForgetValidated))
import Ouroboros.Consensus.Mock.Ledger.Block
import Ouroboros.Consensus.Protocol.Abstract
import Ouroboros.Network.SizeInBytes

-- | Construct the protocol specific part of the block
--
-- This is used in 'forgeSimple', which takes care of the generic part of the
-- mock block.
--
-- Note: this is a newtype and not a type class to allow for things in the
-- closure. For example, if Praos had to use a stateful KES key, it could
-- refer to it in its closure.
newtype ForgeExt c ext = ForgeExt
  { forgeExt ::
      TopLevelConfig (SimpleBlock c ext) ->
      IsLeader (BlockProtocol (SimpleBlock c ext)) ->
      SimpleBlock' c ext () ->
      SimpleBlock c ext
  }

forgeSimple ::
  forall c ext.
  MockProtocolSpecific c ext =>
  ForgeExt c ext ->
  ForgeBlockArgs (SimpleBlock c ext) ->
  SimpleBlock c ext
forgeSimple ForgeExt{forgeExt} ForgeBlockArgs{..} =
  forgeExt fbConfig fbIsLeader $
    SimpleBlock
      { simpleHeader = mkSimpleHeader encode stdHeader ()
      , simpleBody = body
      }
 where
  body :: SimpleBody
  body = SimpleBody{simpleTxs = (simpleGenTx . txForgetValidated) <$> fbTxs}

  stdHeader :: SimpleStdHeader c ext
  stdHeader =
    SimpleStdHeader
      { simplePrev = castHash $ getTipHash fbCurrentTickedLedgerState
      , simpleSlotNo = fbCurrentSlotNo
      , simpleBlockNo = fbCurrentBlockNo
      , simpleBodyHash = hashWithSerialiser toCBOR body
      , simpleBodySize = bodySize
      }

  bodySize :: SizeInBytes
  bodySize = SizeInBytes $ fromIntegral $ Lazy.length $ serialise body
