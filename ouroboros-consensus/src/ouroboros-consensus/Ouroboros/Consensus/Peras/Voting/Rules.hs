{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Pure Peras voting rules
--
-- This module implements the Peras voting rules in a pure fashion. These are
-- translated as verbatim as possible from:
--
-- https://github.com/cardano-foundation/CIPs/blob/master/CIP-0140/README.md#rules-for-voting-in-a-round
--
-- NOTE: in this file, we use uncommon variable names such as `_L` or `_X`
-- because that is their name in the CIP-0140, and we can't have variable names
-- starting with capital letters. Contrary to typical Haskell conventions, those
-- do not denote ignored variables.
module Ouroboros.Consensus.Peras.Voting.Rules
  ( isPerasVotingAllowed
  , PerasVotingRule (..)
  , PerasVotingRulesDecision (..)
  , perasVR1A
  , perasVR1B
  , perasVR2A
  , perasVR2B
  , perasVR1
  , perasVR2
  , perasVotingRules
  )
where

import Data.Word (Word64)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block (WithOrigin (..))
import Ouroboros.Consensus.Block.Abstract
  ( Point
  , SlotNo (..)
  , StandardHash
  )
import Ouroboros.Consensus.Block.SupportsPeras
  ( IsPerasCert (..)
  , PerasCertArrivalThreshold (..)
  , PerasCooldownRounds (..)
  , PerasIgnoranceRounds (..)
  , PerasParams (..)
  , PerasRoundNo (..)
  , onPerasRoundNo
  )
import Ouroboros.Consensus.Peras.Params (perasBootstrapRound)
import Ouroboros.Consensus.Peras.Voting.View
  ( LatestCertSeenView (..)
  , PerasVotingView (..)
  )
import Ouroboros.Consensus.Util.Pred
  ( Evidence (..)
  , Explainable (..)
  , ExplanationMode (..)
  , Pred (..)
  , evalPred
  )

{-------------------------------------------------------------------------------
  Voting rules
-------------------------------------------------------------------------------}

-- | Whether we are allowed to vote according to the rules.
--
-- This type additionally carries the evidence for the decision taken.
data PerasVotingRulesDecision blk
  = Vote (Evidence True PerasVotingRule) (Point blk)
  | NoVote (Evidence False PerasVotingRule)
  deriving (Show, Eq, Generic, NoThunks)

instance StandardHash blk => Explainable (PerasVotingRulesDecision blk) where
  explain mode = \case
    Vote (ETrue e) candidate ->
      "Vote(" <> show candidate <> "," <> explain mode e <> ")"
    NoVote (EFalse e) ->
      "NoVote(" <> explain mode e <> ")"

-- | Evaluate whether voting is allowed or not according to the voting rules
isPerasVotingAllowed ::
  IsPerasCert cert blk =>
  PerasVotingView cert blk ->
  PerasVotingRulesDecision blk
isPerasVotingAllowed pvv =
  evalPred (perasVotingRules pvv) $ \e ->
    case e of
      ETrue{} -> Vote e (candidateBlock pvv)
      EFalse{} -> NoVote e

-- | Voting rules
--
-- Each constructor corresponds to a voting rule as per CIP-0140.
--
-- * VR1x correspond to the "happy" path, i.e., when nodes proceed to vote
-- normally, whereas
-- * VR2x correspond to the "cooldown" path, i.e., when nodes are exiting a
-- cooldown period.
data PerasVotingRule
  = -- | The voter has seen the certificate for the previous round, and the
    -- certificate was received in the first X slots after the start of the round
    VR1A
  | -- | The block being voted upon extends the most recently certified one.
    VR1B
  | -- | The last certificate a party has seen is from a round at least R rounds ago
    VR2A
  | -- | The last certificate included in our preferred chain (or the possible perasBootstrapRound taking precedence if specified and larger than the former)
    -- is from a round exactly c⋅K rounds ago for some c ∈ ℕ with c ≥ 0.
    --
    -- The 'Word64' parameter corresponds to the existential value c.
    VR2B BootstrapStatus Word64
  deriving (Show, Eq)

-- | As per [revised CIP-0140](https://github.com/tweag/CIPs/blob/3476b736857dc2ff9ad4bb21a834a6b0b4dd5ef5/CIP-0140/README.md),
-- the 'perasBootstrapRound :: Maybe PerasRoundNo' parameter overrides the value
-- of the latest certificate seen on the preferred chain for VR-2B if the
-- 'perasBootstrapRound' isn't 'Nothing' and if its value is larger than the
-- round of the latest certificate seen on the preferred chain.
--
-- This type captures the result of the interaction between the
-- 'perasBootstrapRound' and the latest certificate seen on the preferred chain
-- for VR-2B, to make the predicate explainable.
data BootstrapStatus
  = BootstrapEnabledAndOverriddenLatestCertOnChain (Evidence True String)
  | BootstrapEnabledButNotOverriddenLatestCertOnChain (Evidence False String)
  | BootstrapEnabledAndNoLatestCertOnChain PerasRoundNo
  | BootstrapDisabledAndLatestCertOnChain PerasRoundNo
  | BootstrapDisabledAndNoLatestCertOnChain
  deriving (Show, Eq)

instance Explainable PerasVotingRule where
  explain Shallow = \case
    VR1A -> "VR-1A"
    VR1B -> "VR-1B"
    VR2A -> "VR-2A"
    VR2B _ _ -> "VR-2B"
  explain Deep = \case
    VR1A ->
      "voter has seen the certificate for the previous round in time"
    VR1B ->
      "the block being voted upon extends the most recently certified block"
    VR2A ->
      "the last certificate seen is sufficiently old"
    VR2B (BootstrapEnabledAndOverriddenLatestCertOnChain _) c ->
      "the perasBootstrapRound took precedence over the latest certificate on the preferred chain, and the perasBoostrapRound is exactly "
        ++ show c
        ++ " cooldown periods old"
    VR2B (BootstrapEnabledButNotOverriddenLatestCertOnChain _) c ->
      "the perasBootstrapRound did not take precedence over the latest certificate on the preferred chain, and that certificate is from a round exactly "
        ++ show c
        ++ " cooldown periods old"
    VR2B (BootstrapEnabledAndNoLatestCertOnChain _) c ->
      "the perasBootstrapRound took precedence because there is no certificate on the preferred chain at all, and the perasBoostrapRound is exactly "
        ++ show c
        ++ " cooldown periods old"
    VR2B (BootstrapDisabledAndLatestCertOnChain _) c ->
      "the perasBootstrapRound is disabled, and the latest certificate on the preferred chain is from a round exactly "
        ++ show c
        ++ " cooldown periods old"
    VR2B BootstrapDisabledAndNoLatestCertOnChain c ->
      "the perasBootstrapRound is disabled, and there is no certificate on the preferred chain at all, but the current round is exactly "
        ++ show (c + 1)
        ++ " cooldown periods minus 1 round"

-- | VR-1A: the voter has seen the certificate for the previous round, and the
-- certificate was received in the first X slots after the start of the round.
perasVR1A ::
  IsPerasCert cert blk =>
  PerasVotingView cert blk ->
  Pred PerasVotingRule
perasVR1A
  PerasVotingView
    { perasParams
    , currRoundNo
    , latestCertSeen
    } =
    VR1A := vr1a1 :/\: vr1a2
   where
    -- The latest certificate seen is from the previous round
    vr1a1 =
      case latestCertSeen of
        -- We have seen a certificate ==> check its round number
        NotOrigin cert ->
          currRoundNo :==: getPerasCertRound (lcsCert cert) + 1
        -- We have never seen a certificate ==> check if we are voting in round 0
        Origin ->
          currRoundNo :==: PerasRoundNo 0

    -- The latest certificate seen was received within X slots from the start
    -- of its round
    vr1a2 =
      case latestCertSeen of
        -- We have seen a certificate ==> check its arrival time
        NotOrigin cert ->
          lcsArrivalSlot cert :<=: lcsRoundStartSlot cert + _X
        -- We have never seen a certificate ==> vacuously true
        Origin ->
          Bool True

    _X =
      SlotNo $
        unPerasCertArrivalThreshold $
          perasCertArrivalThreshold perasParams

-- | VR-1B: the block being voted upon extends the most recently certified one.
perasVR1B ::
  PerasVotingView cert blk ->
  Pred PerasVotingRule
perasVR1B
  PerasVotingView
    { latestCertSeen
    } =
    VR1B := vr1b
   where
    -- The block being voted upon extends the most recently certified one
    vr1b =
      case latestCertSeen of
        -- We have seen a certificate ==> check that it extends our chain
        NotOrigin cert ->
          Bool (lcsCandidateBlockExtendsCert cert)
        -- We have never seen a certificate ==> vacuously true
        Origin ->
          Bool True

-- | VR-2A: the last certificate a party has seen is from a round at least R
-- rounds ago.
--
-- This enforces the chain-healing period that must occur before leaving a
-- cooldown period.
perasVR2A ::
  IsPerasCert cert blk =>
  PerasVotingView cert blk ->
  Pred PerasVotingRule
perasVR2A
  PerasVotingView
    { perasParams
    , currRoundNo
    , latestCertSeen
    } =
    VR2A := vr2a
   where
    vr2a =
      case latestCertSeen of
        -- We have seen a certificate ==> check its round number
        NotOrigin cert ->
          getPerasCertRound (lcsCert cert) + _R :<=: currRoundNo
        -- We have never seen a certificate ==> check if we are recovering from
        -- an initial cooldown after having initially failed to reach a quorum
        Origin ->
          _R :<=: currRoundNo

    _R =
      PerasRoundNo $
        unPerasIgnoranceRounds $
          perasIgnoranceRounds $
            perasParams

-- | VR-2B: the last certificate included in our preferred chain is from a round
-- exactly c⋅K rounds ago for some c ∈ ℕ with c ≥ 0.
--
-- This enforces chain quality and common prefix before leaving a cooldown
-- period.
perasVR2B ::
  PerasVotingView cert blk ->
  Pred PerasVotingRule
perasVR2B
  PerasVotingView
    { perasParams
    , currRoundNo
    , latestCertOnChainRound
    } =
    VR2B bootstrapStatus c := vr2b
   where
    vr2b =
      -- The reference round is older than the current one
      (currRoundNo :>: referenceRound)
        -- The reference round is c⋅K rounds away from the current one
        :/\: ( (currRoundNo `rmod` _K)
                 :==: (referenceRound `rmod` _K)
             )

    rmod = onPerasRoundNo mod
    rquot = onPerasRoundNo quot

    c = unPerasRoundNo $ currRoundNo `rquot` _K

    _K =
      PerasRoundNo $
        unPerasCooldownRounds $
          perasCooldownRounds $
            perasParams

    (bootstrapStatus, referenceRound) =
      case (perasBootstrapRound perasParams, latestCertOnChainRound) of
        (Just bootstrapRound, NotOrigin latestCertOnChainRoundNo) ->
          evalPred
            ("bootstrapRound > latestCertOnChainRoundNo" := bootstrapRound :>: latestCertOnChainRoundNo)
            $ \case
              etrue@ETrue{} ->
                (BootstrapEnabledAndOverriddenLatestCertOnChain etrue, bootstrapRound)
              efalse@EFalse{} ->
                (BootstrapEnabledButNotOverriddenLatestCertOnChain efalse, latestCertOnChainRoundNo)
        (Just bootstrapRound, Origin) ->
          (BootstrapEnabledAndNoLatestCertOnChain bootstrapRound, bootstrapRound)
        (Nothing, NotOrigin latestCertOnChainRoundNo) ->
          (BootstrapDisabledAndLatestCertOnChain latestCertOnChainRoundNo, latestCertOnChainRoundNo)
        -- NOTE: '_K - 1' here is treating the 'Origin' certificate as being
        -- from round -1.
        (Nothing, Origin) ->
          (BootstrapDisabledAndNoLatestCertOnChain, _K - 1)

-- | Both VR-1A and VR-1B hold, which is the situation typically occurring when
-- the voting has regularly occurred in preceding rounds.
perasVR1 ::
  IsPerasCert cert blk =>
  PerasVotingView cert blk ->
  Pred PerasVotingRule
perasVR1 pvv =
  perasVR1A pvv :/\: perasVR1B pvv

-- | Both VR-2A and VR-2B hold, which is the situation typically occurring when
-- the chain is about to exit a cooldown period.
perasVR2 ::
  IsPerasCert cert blk =>
  PerasVotingView cert blk ->
  Pred PerasVotingRule
perasVR2 pvv =
  perasVR2A pvv :/\: perasVR2B pvv

-- | Voting is allowed if either VR-1A and VR-1B hold, or VR-2A and VR-2B hold.
perasVotingRules ::
  IsPerasCert cert blk =>
  PerasVotingView cert blk ->
  Pred PerasVotingRule
perasVotingRules pvv =
  perasVR1 pvv :\/: perasVR2 pvv
