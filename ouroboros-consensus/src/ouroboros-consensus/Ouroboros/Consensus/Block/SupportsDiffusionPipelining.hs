{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | See 'BlockSupportsDiffusionPipelining'.
module Ouroboros.Consensus.Block.SupportsDiffusionPipelining (
    BlockSupportsDiffusionPipelining (..)
  , updateTentativeHeaderState
    -- * @DerivingVia@ helpers
    -- ** 'DisableDiffusionPipelining'
  , DisableDiffusionPipelining (..)
    -- ** 'SelectViewDiffusionPipelining'
  , SelectViewDiffusionPipelining (..)
  , SelectViewTentativeState (..)
    -- * Data family instances
  , BlockConfig (..)
  , Header (..)
  ) where

import           Control.Monad (guard)
import           Data.Coerce
import           Data.Kind
import           Data.Proxy
import           GHC.Generics (Generic)
import           NoThunks.Class
import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.Block.SupportsProtocol
import           Ouroboros.Consensus.Protocol.Abstract

-- | Block functionality required to support __Block Diffusion Pipelining via
-- Delayed Validation__ (DPvDV).
--
-- == High-level context
--
-- With DPvDV, a node is, under certain conditions, already announcing a new
-- block to its downstream peers /before/ it has fully validated the block body.
-- Concretely, the node maintains a /tentative header/ that, if present, extends
-- the current selection, and is announced via the ChainSync servers to
-- downstream peers.
--
-- Ideally, the body turns out to be valid, in which case the tentative header
-- is set to 'Nothing', and the selection is updated to now include the header
-- at its tip.
--
-- If the body corresponding to the tentative header turns out to be invalid (we
-- call such a header a /trap header/), the tentative header is cleared, and the
-- ChainSync servers send a rollback instruction. In this case, the network
-- wasted work in diffusing, storing and validating this block. If abused, this
-- could cause an unbounded amount of work for honest nodes. Hence, we need to
-- enforce that our upstream nodes adhere to an appropriate criterion related to
-- trap headers, and so must also restrict ourselves accordingly such that
-- downstream nodes do not disconnect from us.
--
-- == Semantics
--
-- This type class allows to define a block-specific criterion determining
-- whether a header that might turn out to be a trap header is allowed to be set
-- as the tentative header.
--
-- This is used in two places:
--
--  - The ChainSel logic. We make sure that we only set the tentative header if
--    this criterion is fulfilled.
--
--  - The BlockFetch clients, in combination with the ChainSel validation logic.
--    For every upstream BlockFetch peer, we make sure that the invalid blocks
--    this peer sent adhere to the pipelining criterion.
--
-- Concretely, the abstract Consensus layer maintains 'TentativeHeaderState's
-- (one in ChainSel, and one for each (BlockFetch) upstream peer). Suppose that
-- @hdr@ either might turn out or is already known to be a trap header. Then
--
-- @'applyTentativeHeaderView' ('Proxy' \@blk) thv st@
--
-- (where @thv = 'tentativeHeaderView' bcfg hdr@) will return
--
--  - 'Nothing' if @hdr@ does not satisfy the pipelining criterion.
--
--      - In ChainSel, this means that @hdr@ should not be pipelined, as it
--        would violate the criterion if it turns out to be a trap header.
--
--      - In the BlockFetch punishment logic, this means that we disconnect from
--        the peer that sent the corresponding invalid block.
--
--  - @'Just' st'@ if @hdr@ does satisfy the pipelining criterion. If the @hdr@
--    is (in the BlockFetch punishment logic) or turns out to be (in ChainSel) a
--    trap header, the 'TentativeHeaderState' should be updated to the returned
--    @st'@.
--
-- == Requirements
--
-- === Safety
--
-- The criterion is sufficiently strict such that an adversary can not induce an
-- unbounded amount of work for honest nodes.
--
-- === Consistent validity under subsequences
--
-- Suppose that over some period of time, an honest node advertised the headers
-- @hdrs :: [Header blk]@ as its trap tentative headers. A downstream honest
-- node might only observe a subsequence of this list (there's no guarantee that
-- every ChainSync server sends every selected tip), but must still consider our
-- behavior as valid.
--
-- Hence, for every subsequence @thvs'@ of @thvs = 'tentativeHeaderView' bcfg
-- '<$>' hdrs@, we need to have
--
-- @'Data.Maybe.isJust' hdrs'Valid@
--
-- for all @st :: 'TentativeHeaderState' blk@ and
--
-- @
-- hdrsValid  = 'Data.Foldable.foldlM' ('flip' $ 'applyTentativeHeaderView' p) st thvs
-- hdrs'Valid = 'Data.Foldable.foldlM' ('flip' $ 'applyTentativeHeaderView' p) st thvs'
-- @
--
-- where @'Data.Maybe.isJust' hdrsValid@ and @p :: 'Proxy' blk@.
--
-- === Efficiently enforcible
--
-- The 'TentativeHeaderState' must have bounded size, and
-- 'applyTentativeHeaderView' must be efficient and /objective/ (different nodes
-- must agree on its result for the same header and state).
--
-- As a historical example for establishing objectivity, see the [removal of the
-- isSelfIssued tiebreaker in the chain
-- order](https://github.com/IntersectMBO/ouroboros-network/pull/3688/commits/6bfeaf6877a473af0973a3ff3c14cc19d4a6af2e).
--
-- === Usefulness despite adversarial activity
--
-- It must not be possible for an adversary to easily manipulate the
-- 'TentativeHeaderState' in such a way that almost no headers can be pipelined
-- anymore. It /is/ acceptable if DPvDV is less effective in scenarios involving
-- an adversary with a very large amount of resources (like stake).
class
  ( Show     (TentativeHeaderState blk)
  , NoThunks (TentativeHeaderState blk)
  , Show     (TentativeHeaderView  blk)
  ) => BlockSupportsDiffusionPipelining blk where
  -- | State that is maintained to judge whether a header can be pipelined. It
  -- can be thought of as a summary of all past trap tentative headers.
  type TentativeHeaderState blk :: Type

  -- | View on a header required for updating the 'TentativeHeaderState'.
  type TentativeHeaderView blk :: Type

  -- | The initial 'TentativeHeaderState'. This is used as the initial value on
  -- node startup, as well as by the HFC instance for new eras.
  initialTentativeHeaderState :: Proxy blk -> TentativeHeaderState blk

  -- | See 'TentativeHeaderView'.
  tentativeHeaderView ::
       BlockConfig blk
    -> Header blk
    -> TentativeHeaderView blk

  -- | Apply a 'TentativeHeaderView' to the 'TentativeHeaderState'. This returns
  -- @'Just' st@ to indicate that the underlying header can be pipelined, and
  -- that the 'TentativeHeaderState' must be updated to @st@ if the header
  -- turns/turned out to be a trap header (ie the corresponding block body is
  -- invalid).
  --
  -- Also see 'updateTentativeHeaderState'.
  applyTentativeHeaderView ::
       Proxy blk
    -> TentativeHeaderView blk
       -- ^ Extracted using 'tentativeHeaderView' from a (valid) header whose
       -- block body is either not yet known to be valid, or definitely invalid.
    -> TentativeHeaderState blk
       -- ^ The most recent 'TentativeHeaderState' in this particular context.
    -> Maybe (TentativeHeaderState blk)
       -- ^ The new 'TentativeHeaderState' in case the header satisfies the
       -- pipelining criterion and is a trap header.

-- | Composition of 'tentativeHeaderView' and 'applyTentativeHeaderView'.
updateTentativeHeaderState ::
     forall blk. BlockSupportsDiffusionPipelining blk
  => BlockConfig blk
  -> Header blk
  -> TentativeHeaderState blk
  -> Maybe (TentativeHeaderState blk)
updateTentativeHeaderState bcfg hdr =
    applyTentativeHeaderView (Proxy @blk) (tentativeHeaderView bcfg hdr)

{-------------------------------------------------------------------------------
  DerivingVia helpers
-------------------------------------------------------------------------------}

-- | A @DerivingVia@ helper to implement 'BlockSupportsDiffusionPipelining' for
-- blocks where no header should ever be pipelined.
--
-- > deriving via DisableDiffusionPipelining MyBlock
-- >   instance BlockSupportsDiffusionPipelining MyBlock
newtype DisableDiffusionPipelining blk = DisableDiffusionPipelining blk

newtype instance Header (DisableDiffusionPipelining blk) =
  DisableDiffusionPipeliningHeader (Header blk)

newtype instance BlockConfig (DisableDiffusionPipelining blk) =
  DisableDiffusionPipeliningBlockConfig (BlockConfig blk)

instance BlockSupportsDiffusionPipelining (DisableDiffusionPipelining blk) where
  type TentativeHeaderState _ = ()

  type TentativeHeaderView _ = ()

  initialTentativeHeaderState _ = ()

  tentativeHeaderView _ _ = ()

  applyTentativeHeaderView _ () () = Nothing

-- | A @DerivingVia@ helper to implement 'BlockSupportsDiffusionPipelining' for
-- blocks where a header should be pipelined iff it has a better 'SelectView'
-- than the last tentative trap header.
--
-- > deriving via DisableDiffusionPipelining MyBlock
-- >   instance BlockSupportsProtocol blk
-- >   => BlockSupportsDiffusionPipelining MyBlock
--
-- This requires that the 'SelectView' is totally ordered via 'Ord', in
-- particular that the order is transitive.
--
-- For example, if @'SelectView' ~ 'BlockNo'@, this means that a header can be
-- pipelined if it has a larger block number than the last tentative trap
-- header. So if someone diffused a trap header for a particular block height,
-- no other block can be pipelined for that block height. This would limit the
-- /Usefulness despite adversarial activity/ if an attacker diffuses a trap
-- header (and later also a valid block) every time they are elected.
newtype SelectViewDiffusionPipelining blk = SelectViewDiffusionPipelining blk

newtype instance Header (SelectViewDiffusionPipelining blk) =
  SelectViewDiffusionPipeliningHeader (Header blk)

newtype instance BlockConfig (SelectViewDiffusionPipelining blk) =
  SelectViewDiffusionPipeliningBlockConfig (BlockConfig blk)

-- | @'TentativeHeaderState' ('SelectViewDiffusionPipelining' blk) ~ 'SelectViewTentativeState' ('BlockProtocol' blk)@
data SelectViewTentativeState proto =
    LastInvalidSelectView !(SelectView proto)
  | NoLastInvalidSelectView
  deriving stock (Generic)

deriving stock    instance ConsensusProtocol proto => Show     (SelectViewTentativeState proto)
deriving stock    instance ConsensusProtocol proto => Eq       (SelectViewTentativeState proto)
deriving anyclass instance ConsensusProtocol proto => NoThunks (SelectViewTentativeState proto)

instance
  ( BlockSupportsProtocol blk
  , Show (SelectView (BlockProtocol blk))
  , Ord (SelectView (BlockProtocol blk))
  ) => BlockSupportsDiffusionPipelining (SelectViewDiffusionPipelining blk) where
  type TentativeHeaderState (SelectViewDiffusionPipelining blk) =
    SelectViewTentativeState (BlockProtocol blk)

  type TentativeHeaderView (SelectViewDiffusionPipelining blk) =
    SelectView (BlockProtocol blk)

  initialTentativeHeaderState _ = NoLastInvalidSelectView

  tentativeHeaderView = coerce selectView

  applyTentativeHeaderView _ sv' st = do
      case st of
        NoLastInvalidSelectView  -> pure ()
        LastInvalidSelectView sv -> guard $ sv < sv'
      pure $ LastInvalidSelectView sv'
