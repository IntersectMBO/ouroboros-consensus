{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Ouroboros.Consensus.Protocol.Abstract (
    -- * Abstract definition of the Ouroboros protocol
    ConsensusConfig
  , ConsensusProtocol (..)
    -- * Chain order
  , ChainOrder (..)
  , TotalChainOrder (..)
  , preferCandidate
    -- * Convenience re-exports
  , SecurityParam (..)
  ) where

import           Control.Monad.Except
import           Data.Coerce (coerce)
import           Data.Kind (Type)
import           Data.Typeable (Typeable)
import           GHC.Stack
import           NoThunks.Class (NoThunks)
import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.Config.SecurityParam
import           Ouroboros.Consensus.Ticked

-- | Static configuration required to run the consensus protocol
--
-- Every method in the 'ConsensusProtocol' class takes the consensus
-- configuration as a parameter, so having this as a data family rather than a
-- type family resolves most ambiguity.
--
-- Defined out of the class so that protocols can define this type without
-- having to define the entire protocol at the same time (or indeed in the same
-- module).
data family ConsensusConfig p :: Type

-- | The (open) universe of Ouroboros protocols
--
-- This class encodes the part that is independent from any particular
-- block representation.
class ( Show (ChainDepState   p)
      , Show (ValidationErr   p)
      , Show (SelectView      p)
      , Show (LedgerView      p)
      , Eq   (ChainDepState   p)
      , Eq   (ValidationErr   p)
      , ChainOrder (SelectView p)
      , NoThunks (ConsensusConfig p)
      , NoThunks (ChainDepState   p)
      , NoThunks (ValidationErr   p)
      , NoThunks (SelectView      p)
      , Typeable p -- so that p can appear in exceptions
      ) => ConsensusProtocol p where
  -- | Protocol-specific state
  --
  -- NOTE: This chain is blockchain dependent, i.e., updated when new blocks
  -- come in (more precisely, new /headers/), and subject to rollback.
  type family ChainDepState p :: Type

  -- | Evidence that a node /is/ the leader
  type family IsLeader p :: Type

  -- | Evidence that we /can/ be a leader
  type family CanBeLeader p :: Type

  -- | View on a header required for chain selection
  --
  -- Chain selection is implemented by the chain database, which takes care of
  -- two things independent of a choice of consensus protocol: we never switch
  -- to chains that fork off more than @k@ blocks ago, and we never adopt an
  -- invalid chain. The actual comparison of chains however depends on the chain
  -- selection protocol. We define chain selection in terms of a /select view/
  -- on the headers at the tips of those chains: chain A is strictly preferred
  -- over chain B whenever A's select view is greater than B's select view
  -- according to the 'ChainOrder' instance.
  type family SelectView p :: Type
  type SelectView p = BlockNo

  -- | Projection of the ledger state the Ouroboros protocol needs access to
  --
  -- The 'LedgerView' is a summary of the state of the ledger that the consensus
  -- algorithm requires to do its job. Under certain circumstances the consensus
  -- algorithm may require the 'LedgerView' for slots in the past (before the
  -- current tip of the chain) or in the (near) future (beyond the tip of the
  -- current chain, without having seen those future blocks yet).
  --
  -- This puts limitations on what the 'LedgerView' can be. For example, it
  -- cannot be the "current stake distribution", since it is of course
  -- impossible to compute the current stake distibution for a slot in the
  -- future. This means that for a consensus algorithm that requires the
  -- stake distribution such as Praos, the 'LedgerView' for a particular slot
  -- must be the "stake distribution for the purpose of leader selection".
  -- This "relevant" stake distribution /can/ be computed for slots in the
  -- (near) future because it is based on historical stake, not current.
  --
  -- A somewhat unfortunate consequence of this is that some decisions that
  -- ought to live in the consensus layer (such as the decision precisely which
  -- historical stake to sample to determine the relevant stake distribution)
  -- instead live in the ledger layer. It is difficult to disentangle this,
  -- because the ledger may indeed /depend/ on those sampling decisions (for
  -- example, reward calculations /must/ be based on that same stake
  -- distribution).
  --
  -- There are also some /advantages/ to moving these sorts of decisions to the
  -- ledger layer. It means that the consensus algorithm can continue to
  -- function without modifications if we decide that the stake distribution for
  -- leader selection should be based on something else instead (for example,
  -- for some bespoke version of the blockchain we may wish to use a committee
  -- instead of a decentralized blockchain). Having sampling decisions in the
  -- ledger layer rather than the consensus layer means that these decisions can
  -- be made without modifying the consensus algorithm.
  --
  -- Note that for the specific case of Praos, whilst the ledger layer provides
  -- the relevant stake distribution, the precise leader election must still live
  -- in the consensus layer since that depends on the computation (and sampling)
  -- of entropy, which is done consensus side, not ledger side (the reward
  -- calculation does not depend on this).
  type family LedgerView p :: Type

  -- | Validation errors
  type family ValidationErr p :: Type

  -- | View on a header required to validate it
  type family ValidateView p :: Type

  -- | Check if a node is the leader
  checkIsLeader :: HasCallStack
                => ConsensusConfig       p
                -> CanBeLeader           p
                -> SlotNo
                -> Ticked (ChainDepState p)
                -> Maybe (IsLeader       p)

  -- | Tick the 'ChainDepState'
  --
  -- We pass the 'LedgerView' to 'tickChainDepState'. Functions that /take/ a
  -- ticked 'ChainDepState' are not separately passed a ledger view; protocols
  -- that require it, can include it in their ticked 'ChainDepState' type.
  tickChainDepState :: ConsensusConfig p
                    -> LedgerView p
                    -> SlotNo
                    -> ChainDepState p
                    -> Ticked (ChainDepState p)

  -- | Apply a header
  updateChainDepState :: HasCallStack
                      => ConsensusConfig       p
                      -> ValidateView          p
                      -> SlotNo
                      -> Ticked (ChainDepState p)
                      -> Except (ValidationErr p) (ChainDepState p)

  -- | Re-apply a header to the same 'ChainDepState' we have been able to
  -- successfully apply to before.
  --
  -- Since a header can only be applied to a single, specific,
  -- 'ChainDepState', if we apply a previously applied header again it will be
  -- applied in the very same 'ChainDepState', and therefore can't possibly
  -- fail.
  --
  -- It is worth noting that since we already know that the header is valid
  -- w.r.t. the provided 'ChainDepState', no validation checks should be
  -- performed.
  reupdateChainDepState :: HasCallStack
                        => ConsensusConfig       p
                        -> ValidateView          p
                        -> SlotNo
                        -> Ticked (ChainDepState p)
                        -> ChainDepState         p

  -- | We require that protocols support a @k@ security parameter
  protocolSecurityParam :: ConsensusConfig p -> SecurityParam

-- | The chain order of some type; in the Consensus layer, this will always be
-- the 'SelectView' of some 'ConsensusProtocol'.
class Eq a => ChainOrder a where
  -- | Static configuration needed to compare chains.
  type ChainOrderConfig a :: Type

  -- | Compare chains via the information of @a@ as a proxy.
  --
  -- * If this returns 'LT' or 'GT', the latter or former chain is strictly
  --   preferred, respectively, and can eg be adopted if the other one is
  --   currently selected. (With Ouroboros Genesis, there are additional
  --   concerns here based on where the two chains intersect.)
  --
  -- * If this returns 'EQ', the chains are equally preferrable. In that case,
  --   the Ouroboros class of consensus protocols /always/ sticks with the
  --   current chain.
  --
  -- === Requirements
  --
  -- Write @cc a b@ for @'compareChains' cfg a b@ for brevity.
  --
  --  [__Reflexivity__]: @cc a a == EQ@ for all @a@.
  --
  --  [__Antisymmetry__]: For all @a, b@:
  --
  --      * @cc a b == LT@ if and only if @cc b a == GT@
  --      * @cc a b == EQ@ if and only if @cc b a == EQ@
  --      * @cc a b == GT@ if and only if @cc b a == LT@
  --
  --  [__Acyclicity__]: Consider the digraph with nodes @a@ and an edge from @v@
  --      to @w@ if @cc v w == LT@. We require that this graph is /acyclic/.
  --
  --      Intuitively, this means that chain selection can never go into a loop
  --      while repeatedly selecting the same chain.
  --
  --      TODO talk about using a topological sort?
  --
  --  [__Block number precedence__]: @a@ must contain the underlying block
  --      number, and use this as the primary way of comparing chains.
  --
  --      Suppose that we have a function @blockNo :: a -> Natural@. Then
  --      for all @a, b :: a@ with
  --
  --      @'compare' (blockNo a) (blockNo b) /= EQ@
  --
  --      we must have
  --
  --      @'compare' (blockNo a) (blockNo b) == cc a b@
  --
  --      Intuitively, this means that only the logic for breaking ties between
  --      chains with equal block number is customizable via this class.
  --
  -- === Transitivity as a non-requirement
  --
  -- We do /not/ require this relation to be transitive, ie that @cc a b == LT@
  -- and @cc b c == LT@ implies @cc a c == LT@ for all @a, b, c@.
  --
  -- Note that due to the __Block number precedence__ requirement, violations of
  -- transitivity can only occur when @a@, @b@ and @c@ have equal block number.
  --
  -- Generally, it is recommended to write a transitive chain order if possible
  -- (hence inducing a total order on @a@), see 'TotalChainOrder', as it
  -- simplifies reasoning about its behavior. In particular, any transitive
  -- chain order is automatically acyclic.
  --
  -- However, forgoing transitivity can enable more sophisticated tiebreaking
  -- rules that eg exhibit desirable incentive behavior.
  compareChains :: ChainOrderConfig a -> a -> a -> Ordering

-- | A @DerivingVia@ helper in case the chain order is a total order (in
-- particular, transitive).
newtype TotalChainOrder a = TotalChainOrder a
  deriving newtype (Eq)

instance Ord a => ChainOrder (TotalChainOrder a) where
  type ChainOrderConfig (TotalChainOrder a) = ()

  compareChains () = coerce (compare @a)

deriving via TotalChainOrder BlockNo instance ChainOrder BlockNo

-- | Compare a candidate chain to our own
--
-- If both chains are equally preferable, the Ouroboros class of consensus
-- protocols /always/ sticks with the current chain.
preferCandidate :: ConsensusProtocol p
                => proxy      p
                -> ChainOrderConfig (SelectView p)
                -> SelectView p  -- ^ Tip of our chain
                -> SelectView p  -- ^ Tip of the candidate
                -> Bool
preferCandidate _ cfg ours cand = compareChains cfg cand ours == GT
