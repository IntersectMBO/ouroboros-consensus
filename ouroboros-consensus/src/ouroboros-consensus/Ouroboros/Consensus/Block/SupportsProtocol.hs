{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Ouroboros.Consensus.Block.SupportsProtocol (BlockSupportsProtocol (..), selectView) where

import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block.Abstract
import Ouroboros.Consensus.Protocol.Abstract

{-------------------------------------------------------------------------------
  Supported blocks
-------------------------------------------------------------------------------}

-- | Evidence that a block supports its protocol
class
  ( GetHeader blk
  , GetPrevHash blk
  , ConsensusProtocol (BlockProtocol blk)
  , NoThunks (Header blk)
  , NoThunks (BlockConfig blk)
  , NoThunks (CodecConfig blk)
  , NoThunks (StorageConfig blk)
  ) =>
  BlockSupportsProtocol blk
  where
  validateView ::
    BlockConfig blk ->
    Header blk ->
    ValidateView (BlockProtocol blk)

  tiebreakerView ::
    BlockConfig blk ->
    Header blk ->
    TiebreakerView (BlockProtocol blk)
  -- Default chain selection just looks at longest chains
  default tiebreakerView ::
    TiebreakerView (BlockProtocol blk) ~ NoTiebreaker =>
    BlockConfig blk ->
    Header blk ->
    TiebreakerView (BlockProtocol blk)
  tiebreakerView _ _ = NoTiebreaker

  projectChainOrderConfig ::
    BlockConfig blk ->
    ChainOrderConfig (SelectView (BlockProtocol blk))
  default projectChainOrderConfig ::
    ChainOrderConfig (SelectView (BlockProtocol blk)) ~ () =>
    BlockConfig blk ->
    ChainOrderConfig (SelectView (BlockProtocol blk))
  projectChainOrderConfig _ = ()

selectView ::
  BlockSupportsProtocol blk =>
  BlockConfig blk ->
  Header blk ->
  SelectView (BlockProtocol blk)
selectView bcfg hdr =
  SelectView
    { svBlockNo = blockNo hdr
    , svTiebreakerView = tiebreakerView bcfg hdr
    }
