{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Various things common to iterations of the Praos protocol.
module Ouroboros.Consensus.Protocol.Praos.Common
  ( MaxMajorProtVer (..)
  , PraosCanBeLeader (..)
  , PraosTiebreakerView (..)
  , VRFTiebreakerFlavor (..)

    -- * node support
  , PraosNonces (..)
  , PraosProtocolSupportsNode (..)
  ) where

import qualified Cardano.Crypto.VRF as VRF
import Cardano.Ledger.BaseTypes (Nonce)
import qualified Cardano.Ledger.BaseTypes as SL
import Cardano.Ledger.Keys (KeyHash, KeyRole (BlockIssuer))
import qualified Cardano.Ledger.Shelley.API as SL
import Cardano.Protocol.Crypto (Crypto, VRF)
import qualified Cardano.Protocol.TPraos.OCert as OCert
import Cardano.Slotting.Slot (SlotNo)
import Data.Function (on)
import Data.Map.Strict (Map)
import Data.Ord (Down (Down))
import Data.Word (Word64)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Protocol.Abstract

-- | The maximum major protocol version.
--
-- This refers to the largest __ledger__ version that this node supports.
--
-- Once the ledger protocol version (as reported by the ledger state)
-- exceeds this version we will consider all blocks invalid. This is
-- called the "obsolete node check" (see the 'ObsoleteNode' error
-- constructor).
--
-- Major ledger protocol versions are used to trigger both intra and
-- inter era hard forks, which can potentially change the set of
-- ledger rules that are applied.
--
-- Minor ledger protocol versions were intended to signal soft forks
-- but they're currently unused, and they're irrelevant for the
-- consensus logic.
--
-- For Cardano mainnet, the Shelley era has major protocol version
-- __2__.  For more details, see [this
-- table](https://github.com/cardano-foundation/CIPs/blob/master/CIP-0059/feature-table.md)
newtype MaxMajorProtVer = MaxMajorProtVer
  { getMaxMajorProtVer :: SL.Version
  }
  deriving (Eq, Show, Generic)
  deriving newtype NoThunks

-- | View of the tip of a header fragment for deciding between chains of equal
-- length.
data PraosTiebreakerView c = PraosTiebreakerView
  { ptvSlotNo :: SlotNo
  , ptvIssuer :: SL.VKey 'SL.BlockIssuer
  , ptvIssueNo :: Word64
  , ptvTieBreakVRF :: VRF.OutputVRF (VRF c)
  }
  deriving (Show, Eq, Generic, NoThunks)

-- | When to compare the VRF tiebreakers.
data VRFTiebreakerFlavor
  = -- | Always compare the VRF tiebreakers. This is the behavior of all eras
    -- before Conway. Once mainnet has transitioned to Conway, we can remove
    -- this option. (The honest /historical/ Ouroboros chain cannot rely on
    -- tiebreakers to win, so /retroactively/ disabling the tiebreaker won't
    -- matter.)
    UnrestrictedVRFTiebreaker
  | -- | Only compare the VRF tiebreakers when the slot numbers differ by at
    -- most the given number of slots.
    --
    -- The main motivation is as follows:
    --
    -- When two blocks A and B with the same block number differ in their slot
    -- number by more than Δ (the maximum message delay from Praos), say
    -- @slot(A) + Δ < slot(B)@, the issuer of B should have been able to mint a
    -- block with a block number higher than A (eg by minting on top of A) under
    -- normal circumstances. The reason for this not being the case might have
    -- been due to A being sent very late, or due to the issuer of B ignoring A
    -- (intentionally, or due to poor configuration/resource provision). In any
    -- case, we do not want to allow the block that was diffused later to still
    -- win by having a better VRF tiebreaker. This makes it less likely for
    -- properly configured pools to lose blocks because of poorly configured
    -- pools.
    RestrictedVRFTiebreaker SlotNo
  deriving stock (Show, Eq, Generic)
  deriving anyclass NoThunks

-- Used to implement the 'Ord' and 'ChainOrder' instances for Praos.
comparePraos ::
  VRFTiebreakerFlavor ->
  PraosTiebreakerView c ->
  PraosTiebreakerView c ->
  Ordering
comparePraos tiebreakerFlavor =
  when' issueNoArmed (compare `on` ptvIssueNo)
    <> when' vrfArmed (compare `on` Down . ptvTieBreakVRF)
 where
  -- When the predicate @p@ returns 'True', use the given comparison function,
  -- otherwise, no preference.
  when' ::
    (a -> a -> Bool) ->
    (a -> a -> Ordering) ->
    (a -> a -> Ordering)
  when' p comp a1 a2 =
    if p a1 a2 then comp a1 a2 else EQ

  -- Only compare the issue numbers when the issuers and slots are identical.
  -- Note that this case implies the VRFs also coincide.
  issueNoArmed v1 v2 =
    ptvSlotNo v1 == ptvSlotNo v2
      && ptvIssuer v1 == ptvIssuer v2

  -- Whether to do a VRF comparison.
  vrfArmed v1 v2 = case tiebreakerFlavor of
    UnrestrictedVRFTiebreaker -> True
    RestrictedVRFTiebreaker maxDist ->
      slotDist (ptvSlotNo v1) (ptvSlotNo v2) <= maxDist

  slotDist :: SlotNo -> SlotNo -> SlotNo
  slotDist s t
    -- slot numbers are unsigned, so have to take care with subtraction
    | s >= t = s - t
    | otherwise = t - s

-- | We order between chains of equal length as follows:
--
-- 1. If the tip of each chain was issued by the same agent and they have the
--    same slot number, prefer the chain whose tip has the highest ocert issue
--    number.
--
-- 2. By a VRF value from the chain tip, with lower values preferred. See
--    @pTieBreakVRFValue@ for which one is used.
--
-- IMPORTANT: This is not a complete picture of the Praos chain order, do also
-- consult the documentation of 'ChainOrder'.
instance Crypto c => Ord (PraosTiebreakerView c) where
  compare = comparePraos UnrestrictedVRFTiebreaker

-- | IMPORTANT: This is not a 'SimpleChainOrder'; rather, there are
-- 'PraosTiebreakerView's @a, b@ such that @a < b@, but @'not' $
-- 'preferCandidate' cfg a b@, namely for @cfg = 'RestrictedVRFTiebreaker'@.
--
-- === Rules
--
-- Concretely, we have @'preferCandidate' cfg ours cand@ (where @ours@ and
-- @cand@ have equal length) based on the following lexicographical criteria:
--
-- 1. If the tip of each chain was issued by the same agent and had the same
--    slot number, then we prefer the candidate if it has a higher ocert issue
--    number.
--
--     Note that this condition is equivalent to the VRFs being identical, as
--     the VRF is a deterministic function of the issuer VRF key, the slot and
--     the epoch nonce, and VRFs are collision-resistant.
--
-- 2. Depending on the 'VRFTiebreakerFlavor':
--
--     * If 'UnrestrictedVRFTiebreaker': Compare via a VRF value from the chain
--       tip, with lower values preferred. See @pTieBreakVRFValue@ for which one
--       is used.
--
--     * If @'RestrictedVRFTiebreaker' maxDist@: Only do the VRF comparison (as
--       in the previous step) if the slot numbers differ by at most @maxDist@.
--
-- === Non-transitivity of 'RestrictedVRFTiebreaker'
--
-- When using @cfg = 'RestrictedVRFTiebreaker' maxDist@, the chain order is not
-- transitive. As an example, suppose @maxDist = 5@ and consider three
-- 'PraosTiebreakerView's with pairwise different issuers and, as well as
--
-- +------+---+---+---+
-- |      | a | b | c |
-- +======+===+===+===+
-- | Slot | 0 | 3 | 6 |
-- +------+---+---+---+
-- | VRF  | 3 | 2 | 1 |
-- +------+---+---+---+
--
-- Then we have @'preferCandidate' cfg a b@ and @'preferCandidate' b c@, but
-- __not__ @'preferCandidate' a c@ (despite @a < c@).
--
-- === Rationale for the rules
--
-- 1. Consider the scenario where the hot key of a block issuer was compromised,
--    and the attacker is now minting blocks using that identity. The actual
--    block issuer can use their cold key to issue a new hot key with a higher
--    opcert issue number and set up a new pool. Due to this tiebreaker rule,
--    the blocks minted by that pool will take precedence (allowing the actual
--    block issuer to decide on eg the block contents and the predecessor) over
--    blocks with the same block and slot number minted by the attacker, and
--    they will end up on the honest chain quickly, which means that the
--    adversary can't extend any chain containing such a block as it would
--    violate the monotonicity requirement on opcert issue numbers.
--
--     See "3.7 Block Validity and Operational Key Certificates" in "Design
--     Specification for Delegation and Incentives in Cardano" by Kant et al for
--     more context.
--
-- 2. The main motivation to do VRF comparisons is to avoid the "Frankfurt
--    problem":
--
--     With only the first two rules for the chain order, almost all blocks with
--     equal block number are equally preferrable. Consider two block issuers
--     minting blocks in very nearby slots. As we never change our selection
--     from one chain to an equally preferrable one, the first block to arrive
--     at another pool is the one to be adopted, and will be extended the next
--     time the pool is elected if no blocks with a higher block number arrive
--     in the meantime. We observed that this effectively incentivizes block
--     producers to concentrate geographically (historically, in Frankfurt) in
--     order to minimize their diffusion times. This works against the goal of
--     geographic decentralisation.
--
--     Also, with the VRF tiebreaker, a block with a somewhat lower propagation
--     speed has a random chance to be selected instead of the one that arrived
--     first by pools before the next block is forged.
--
--     See 'VRFTiebreakerFlavor' for more context on the exact conditions under
--     which the VRF comparison takes place.
instance Crypto c => ChainOrder (PraosTiebreakerView c) where
  type ChainOrderConfig (PraosTiebreakerView c) = VRFTiebreakerFlavor

  preferCandidate cfg ours cand = comparePraos cfg ours cand == LT

data PraosCanBeLeader c = PraosCanBeLeader
  { praosCanBeLeaderOpCert :: !(OCert.OCert c)
  -- ^ Certificate delegating rights from the stake pool cold key (or
  -- genesis stakeholder delegate cold key) to the online KES key.
  , praosCanBeLeaderColdVerKey :: !(SL.VKey 'SL.BlockIssuer)
  -- ^ Stake pool cold key or genesis stakeholder delegate cold key.
  , praosCanBeLeaderSignKeyVRF :: !(VRF.SignKeyVRF (VRF c))
  }
  deriving Generic

instance Crypto c => NoThunks (PraosCanBeLeader c)

-- | See 'PraosProtocolSupportsNode'
data PraosNonces = PraosNonces
  { candidateNonce :: !Nonce
  , epochNonce :: !Nonce
  , evolvingNonce :: !Nonce
  , labNonce :: !Nonce
  -- ^ Nonce constructed from the hash of the Last Applied Block
  , previousLabNonce :: !Nonce
  -- ^ Nonce corresponding to the LAB nonce of the last block of the previous
  -- epoch
  }

-- | The node has Praos-aware code that inspects nonces in order to support
-- some Cardano API queries that are crucial to the user exprience
--
-- The interface being used for that has grown and needs review, but we're
-- adding to it here under time pressure. See
-- <https://github.com/IntersectMBO/cardano-node/issues/3864>
class ConsensusProtocol p => PraosProtocolSupportsNode p where
  type PraosProtocolSupportsNodeCrypto p

  getPraosNonces :: proxy p -> ChainDepState p -> PraosNonces

  getOpCertCounters :: proxy p -> ChainDepState p -> Map (KeyHash 'BlockIssuer) Word64
