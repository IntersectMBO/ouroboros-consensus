{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

import Ouroboros.Consensus.Block (WithOrigin (..))
import Ouroboros.Consensus.Block.Abstract
  ( SlotNo (..)
  )
import Ouroboros.Consensus.Block.SupportsPeras
  ( HasPerasCertRound (..)
  , PerasRoundNo (..)
  , getPerasCertRound
  , onPerasRoundNo
  )
import Ouroboros.Consensus.Peras.Params
  ( PerasCertArrivalThreshold (..)
  , PerasCooldownRounds (..)
  , PerasIgnoranceRounds (..)
  , PerasParams (..)
  )
import Ouroboros.Consensus.Peras.Voting.View
  ( LatestCertOnChainView (..)
  , LatestCertSeenView (..)
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
data PerasVotingRulesDecision
  = Vote (Evidence True PerasVotingRule)
  | NoVote (Evidence False PerasVotingRule)
  deriving Show

instance Explainable PerasVotingRulesDecision where
  explain mode = \case
    Vote (ETrue e) -> "Vote(" <> explain mode e <> ")"
    NoVote (EFalse e) -> "NoVote(" <> explain mode e <> ")"

-- | Evaluate whether voting is allowed or not according to the voting rules
isPerasVotingAllowed ::
  HasPerasCertRound cert =>
  PerasVotingView cert ->
  PerasVotingRulesDecision
isPerasVotingAllowed pvv =
  evalPred (perasVotingRules pvv) $ \e ->
    case e of
      ETrue{} -> Vote e
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
  | -- | The last certificate included in our preferred chain is from a round
    -- exactly c⋅K rounds ago for some c ∈ ℕ with c ≥ 0.
    --
    -- The 'PerasRoundNo' parameter corresponds to the existential value c.
    VR2B PerasRoundNo
  deriving (Show, Eq)

instance Explainable PerasVotingRule where
  explain Shallow = \case
    VR1A -> "VR-1A"
    VR1B -> "VR-1B"
    VR2A -> "VR-2A"
    VR2B _ -> "VR-2B"
  explain Deep = \case
    VR1A ->
      "voter has seen the certificate for the previous round in time"
    VR1B ->
      "the block being voted upon extends the most recently certified block"
    VR2A ->
      "the last certificate seen is sufficiently old"
    VR2B _ ->
      "the last certificate on chain is exactly one or more cooldown periods old"

-- | VR-1A: the voter has seen the certificate for the previous round, and the
-- certificate was received in the first X slots after the start of the round.
perasVR1A ::
  HasPerasCertRound cert =>
  PerasVotingView cert ->
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
  PerasVotingView cert ->
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
  HasPerasCertRound cert =>
  PerasVotingView cert ->
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
  HasPerasCertRound cert =>
  PerasVotingView cert ->
  Pred PerasVotingRule
perasVR2B
  PerasVotingView
    { perasParams
    , currRoundNo
    , latestCertOnChain
    } =
    VR2B c := vr2b
   where
    vr2b =
      case latestCertOnChain of
        -- There is a certificate on chain ==> we must check its round number
        NotOrigin cert ->
          -- The certificate comes from a round older than the current one
          (currRoundNo :>: getPerasCertRound (lcocCert cert))
            -- The certificate round is c⋅K rounds away from the current one
            :/\: ( (currRoundNo `rmod` _K)
                     :==: (getPerasCertRound (lcocCert cert) `rmod` _K)
                 )
        -- There is no certificate on chain ==> check if we are recovering
        -- from an initial cooldown after having initially failed to
        -- reach a quorum during bootstrapping.
        --
        -- NOTE: '_K - 1' here is treating the 'Origin' certificate as being
        -- from round -1.
        Origin ->
          currRoundNo `rmod` _K :==: _K - 1

    rmod = onPerasRoundNo mod
    rquot = onPerasRoundNo quot

    c = currRoundNo `rquot` _K

    _K =
      PerasRoundNo $
        unPerasCooldownRounds $
          perasCooldownRounds $
            perasParams

-- | Both VR-1A and VR-1B hold, which is the situation typically occurring when
-- the voting has regularly occurred in preceding rounds.
perasVR1 ::
  HasPerasCertRound cert =>
  PerasVotingView cert ->
  Pred PerasVotingRule
perasVR1 pvv =
  perasVR1A pvv :/\: perasVR1B pvv

-- | Both VR-2A and VR-2B hold, which is the situation typically occurring when
-- the chain is about to exit a cooldown period.
perasVR2 ::
  HasPerasCertRound cert =>
  PerasVotingView cert ->
  Pred PerasVotingRule
perasVR2 pvv =
  perasVR2A pvv :/\: perasVR2B pvv

-- | Voting is allowed if either VR-1A and VR-1B hold, or VR-2A and VR-2B hold.
perasVotingRules ::
  HasPerasCertRound cert =>
  PerasVotingView cert ->
  Pred PerasVotingRule
perasVotingRules pvv =
  perasVR1 pvv :\/: perasVR2 pvv
