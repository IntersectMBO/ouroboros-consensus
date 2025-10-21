{-# LANGUAGE TypeApplications #-}

-- | Pure Peras voting rules
--
-- This module implements the Peras voting rules in a pure fasion, along with the
-- necessary inpure machinery to retrieve their inputs. These rules are translated
-- as verbatim as possible from:
--
-- https://github.com/cardano-foundation/CIPs/blob/master/CIP-0140/README.md#rules-for-voting-in-a-round
module Ouroboros.Consensus.Peras.Voting
  ( PerasVotingView (..)
  , withPerasVotingView
  , perasVotingRule1A
  , perasVotingRule1B
  , perasVotingRule2A
  , perasVotingRule2B
  , perasVotingRule1
  , perasVotingRule2
  , isPerasVotingAllowed
  )
where

import Data.Coerce (coerce)
import Data.Function (on)
import Data.Set (Set)
import GHC.Word (Word64)
import Ouroboros.Consensus.Block (SlotNo (..))
import Ouroboros.Consensus.Block.Abstract (Point, StandardHash)
import Ouroboros.Consensus.Block.SupportsPeras
  ( PerasRoundNo (..)
  , ValidatedPerasCert
  , getPerasCertRound
  )
import Ouroboros.Consensus.BlockchainTime.WallClock.Types (WithArrivalTime)
import Ouroboros.Consensus.Peras.Params
  ( PerasCertArrivalThreshold (..)
  , PerasCooldownRounds (..)
  , PerasIgnoranceRounds (..)
  , PerasParams (..)
  )
import Ouroboros.Consensus.Util.IOLike (MonadSTM (..))
import Ouroboros.Network.AnchoredFragment (AnchoredFragment)

{-------------------------------------------------------------------------------
  Voting state
-------------------------------------------------------------------------------}

-- | Interface needed to compute Peras voting rules
data PerasVotingView blk = PerasVotingView
  { pvvPerasParams ::
      PerasParams
  -- ^ Peras protocol parameters
  , pvvAllChains ::
      Set (AnchoredFragment blk)
  -- ^ All known chains fragments
  , pvvLatestCertSeen ::
      WithArrivalTime (ValidatedPerasCert blk)
  -- ^ The most recent certificate seen by the voter.
  , pvvLatestCertOnChain ::
      WithArrivalTime (ValidatedPerasCert blk)
  -- ^ The most recent certificate present in some chain.
  , pvvRound ::
      SlotNo ->
      PerasRoundNo
  -- ^ Get the Peras round number corresponding to a slot.
  , pvvRoundStart ::
      SlotNo ->
      SlotNo
  -- ^ Get the slot number corresponding to the start of the Peras round for a slot.
  , pvvBlockSelection ::
      SlotNo ->
      Point blk
  -- ^ The most recent block on the preferred chain that is at least L slots old.
  , pvvExtends ::
      Point blk ->
      WithArrivalTime (ValidatedPerasCert blk) ->
      Set (AnchoredFragment blk) ->
      Bool
  -- ^ Does this block extend the one boosted by a certificate and is present in some chain?
  , pvvArrivalSlot ::
      WithArrivalTime (ValidatedPerasCert blk) ->
      SlotNo
  -- ^ Transform the arrival time of a certificate into its corresponding slot number
  }

-- | Run an STM computation that uses a 'PerasVotingView'
--
-- | TODO replace some of these inputs with computations derived from the context
withPerasVotingView ::
  PerasParams ->
  Set (AnchoredFragment blk) ->
  WithArrivalTime (ValidatedPerasCert blk) ->
  WithArrivalTime (ValidatedPerasCert blk) ->
  (SlotNo -> PerasRoundNo) ->
  (SlotNo -> SlotNo) ->
  (SlotNo -> Point blk) ->
  (Point blk -> WithArrivalTime (ValidatedPerasCert blk) -> Set (AnchoredFragment blk) -> Bool) ->
  (WithArrivalTime (ValidatedPerasCert blk) -> SlotNo) ->
  (PerasVotingView blk -> STM m a) ->
  STM m a
withPerasVotingView
  perasParams
  allChains
  latestCertSeen
  latestCertOnChain
  slotRound
  slotStart
  blockSelection
  extendsCertifiedBlock
  arrivalSlot
  f = do
    f
      PerasVotingView
        { pvvPerasParams = perasParams
        , pvvAllChains = allChains
        , pvvLatestCertSeen = latestCertSeen
        , pvvLatestCertOnChain = latestCertOnChain
        , pvvRound = slotRound
        , pvvRoundStart = slotStart
        , pvvBlockSelection = blockSelection
        , pvvExtends = extendsCertifiedBlock
        , pvvArrivalSlot = arrivalSlot
        }

{-------------------------------------------------------------------------------
  Voting rules
-------------------------------------------------------------------------------}

-- | VR-1A: the voter has seen the certificate for the previous round, and the certificate
-- was received in the first X slots after the start of the round
perasVotingRule1A :: StandardHash blk => SlotNo -> PerasVotingView blk -> Bool
perasVotingRule1A s t =
  and
    [ pvvRound t s == getPerasCertRound (pvvLatestCertSeen t) <> coerce @Word64 1
    , pvvArrivalSlot t (pvvLatestCertSeen t) <= pvvRoundStart t s + _X
    ]
 where
  _X = coerce (perasCertArrivalThreshold (pvvPerasParams t))

-- | VR-1B: the block being voted upon extends the most recently certified block.
perasVotingRule1B :: SlotNo -> PerasVotingView blk -> Bool
perasVotingRule1B s t =
  pvvExtends
    t
    (pvvBlockSelection t s)
    (pvvLatestCertSeen t)
    (pvvAllChains t)

-- | VR-2A: the last certificate a party has seen is from a round at least R rounds previously.
--
-- This enforces the chain-healing period that must occur before leaving a cool-down period.
perasVotingRule2A :: StandardHash blk => PerasRoundNo -> PerasVotingView blk -> Bool
perasVotingRule2A r t =
  r >= getPerasCertRound (pvvLatestCertSeen t) <> _R
 where
  _R = coerce (perasIgnoranceRounds (pvvPerasParams t))

-- | VR-2B: the last certificate included in a party's current chain is from a round exactly
-- c⋅K rounds ago for some c ∈ ℕ with c ≥ 0.
--
-- This enforces chain quality and common prefix before leaving a cool-down period.
perasVotingRule2B :: StandardHash blk => PerasRoundNo -> PerasVotingView blk -> Bool
perasVotingRule2B r t =
  and
    [ r > getPerasCertRound (pvvLatestCertOnChain t)
    , r `rmod` _K == getPerasCertRound (pvvLatestCertOnChain t) `rmod` _K
    ]
 where
  _K = coerce (perasCooldownRounds (pvvPerasParams t))
  rmod = (\a b -> PerasRoundNo (a `mod` b)) `on` unPerasRoundNo

-- | Both VR-1A and VR-1B hold, which is the situation typically occurring when the voting has
-- regularly occurred in preceding rounds.
perasVotingRule1 :: StandardHash blk => SlotNo -> PerasVotingView blk -> Bool
perasVotingRule1 s t =
  and
    [ perasVotingRule1A s t
    , perasVotingRule1B s t
    ]

-- | Both VR-2A and VR-2B hold, which is the situation typically occurring when the chain is
-- about to exit a cool-down period.
perasVotingRule2 :: StandardHash blk => PerasRoundNo -> PerasVotingView blk -> Bool
perasVotingRule2 r t =
  and
    [ perasVotingRule2A r t
    , perasVotingRule2B r t
    ]

-- | If either VR-1A and VR-1B hold, or VR-2A and VR-2B hold, then voting is allowed.
isPerasVotingAllowed :: StandardHash blk => SlotNo -> PerasVotingView blk -> Bool
isPerasVotingAllowed s t =
  or
    [ perasVotingRule1 s t
    , perasVotingRule2 (pvvRound t s) t
    ]
