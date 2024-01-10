{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Ouroboros.Consensus.Protocol.Abstract (
    -- * Abstract definition of the Ouroboros protocol
    ConsensusConfig
  , ConsensusProtocol (..)
    -- * Chain order
  , ChainOrder (..)
  , SimpleChainOrder (..)
    -- * Translation
  , TranslateProto (..)
    -- * Convenience re-exports
  , SecurityParam (..)
  ) where

import           Data.List.NonEmpty
import           Control.Monad.Except
import           Data.Kind (Type)
import           Data.Proxy (Proxy)
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
  -- over chain B whenever A's select view is preferred over B's select view
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

  protocolSecurityParamConsistencyCheck :: ConsensusConfig p
                                        -> Maybe (NonEmpty SecurityParam)

-- | Translate across protocols
class TranslateProto protoFrom protoTo where
  -- | Translate the ledger view.
  translateLedgerView ::
    Proxy (protoFrom, protoTo) -> LedgerView protoFrom    -> LedgerView protoTo
  translateChainDepState ::
    Proxy (protoFrom, protoTo) -> ChainDepState protoFrom -> ChainDepState protoTo

-- | Degenerate instance - we may always translate from a protocol to itself.
instance TranslateProto singleProto singleProto
  where
  translateLedgerView _ = id
  translateChainDepState _ = id

-- | The chain order of some type; in the Consensus layer, this will always be
-- the 'SelectView' of some 'ConsensusProtocol'.
--
-- See 'preferCandidate' for the primary documentation.
--
-- Additionally, we require a total order on this type, such that eg different
-- candidate chains that are preferred over our current selection can be sorted
-- for prioritization. For example, this is used in ChainSel during initial
-- chain selection or when blocks arrive out of order (not the case when the
-- node is caught up), or in the BlockFetch decision logic. Future work could
-- include also recording\/storing arrival information and using that instead
-- of\/in addition to the 'Ord' instance.
class Ord sv => ChainOrder sv where
  type ChainOrderConfig sv :: Type

  -- | Compare a candidate chain to our own.
  --
  -- This method defines when a candidate chain is /strictly/ preferable to our
  -- current chain. If both chains are equally preferable, the Ouroboros class
  -- of consensus protocols /always/ sticks with the current chain.
  --
  -- === Requirements
  --
  -- Write @ours ⊏ cand@ for @'preferCandidate' cfg ours cand@ for brevity.
  --
  --  [__Consistency with 'Ord'__]: When @ours ⊏ cand@, then @ours < cand@.
  --
  --      This means that @cand@ can only be preferred over @ours@ when @cand@
  --      is greater than @ours@ according to the 'Ord' instance.
  --
  --      However, this is not necessarily a sufficient condition; a concrete
  --      implementation may decide to not have @ours ⊏ cand@ despite @ours <
  --      cand@ for some pairs @ours, can@. However, it is recommended to think
  --      about this carefully and rather use 'SimpleChainOrder' if possible,
  --      which defines @ours ⊏ cand@ as @ours < cand@, as it simplifies
  --      reasoning about the chain ordering.
  --
  --      However, forgoing 'SimpleChainOrder' can enable more sophisticated
  --      tiebreaking rules that eg exhibit desirable incentive behavior.
  --
  --  [__Chain extension precedence__]: @a@ must contain the underlying block
  --      number, and use this as the primary way of comparing chains.
  --
  --      Suppose that we have a function @blockNo :: sv -> Natural@. Then for
  --      all @a, b@ with @blockNo a < blockNo b@ we must have @a ⊏ b@.
  --
  --      Intuitively, this means that only the logic for breaking ties between
  --      chains with equal block number is customizable via this class.
  preferCandidate ::
       ChainOrderConfig sv
    -> sv -- ^ Tip of our chain
    -> sv -- ^ Tip of the candidate
    -> Bool

-- | A @DerivingVia@ helper to implement 'preferCandidate' in terms of the 'Ord'
-- instance.
newtype SimpleChainOrder sv = SimpleChainOrder sv
  deriving newtype (Eq, Ord)

instance Ord sv => ChainOrder (SimpleChainOrder sv) where
  type ChainOrderConfig (SimpleChainOrder sv) = ()

  preferCandidate _cfg ours cand = ours < cand

deriving via SimpleChainOrder BlockNo instance ChainOrder BlockNo
