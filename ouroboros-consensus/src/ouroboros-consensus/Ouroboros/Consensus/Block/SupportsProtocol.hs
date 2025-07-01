{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Ouroboros.Consensus.Block.SupportsProtocol (BlockSupportsProtocol (..)) where

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
  -- | Extracts a 'ValidateView' from a block's 'Header' for use by
  -- the consensus protocol during block validation.
  --
  -- This function should return only the information necessary from
  -- the header to perform protocol-specific validation checks, for
  -- instance issuer id or proof of the right to produce a block in a
  -- given slot.
  --
  -- This function enables  the "header/body split" optimization.
  validateView ::
    BlockConfig blk ->
    Header blk ->
    ValidateView (BlockProtocol blk)

  selectView ::
    BlockConfig blk ->
    Header blk ->
    SelectView (BlockProtocol blk)
  -- Default chain selection just looks at longest chains
  default selectView ::
    SelectView (BlockProtocol blk) ~ BlockNo =>
    BlockConfig blk ->
    Header blk ->
    SelectView (BlockProtocol blk)
  selectView _ = blockNo

  projectChainOrderConfig ::
    BlockConfig blk ->
    ChainOrderConfig (SelectView (BlockProtocol blk))
  default projectChainOrderConfig ::
    ChainOrderConfig (SelectView (BlockProtocol blk)) ~ () =>
    BlockConfig blk ->
    ChainOrderConfig (SelectView (BlockProtocol blk))
  projectChainOrderConfig _ = ()
