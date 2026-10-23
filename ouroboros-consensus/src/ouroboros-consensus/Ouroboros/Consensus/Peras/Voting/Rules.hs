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

import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block
  ( PerasBootstrapStatus (PerasBootstrapDisabled, PerasBootstrapEnabled)
  , WithOrigin (..)
  )
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
  )
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
-- normally after a successful previous round, whereas
-- * VR2x correspond to the "cooldown" path, i.e., when nodes are exiting a
-- cooldown period (or bootstraping for their first ever vote).
data PerasVotingRule
  = -- | The voter has seen the certificate for the previous round, and the
    -- certificate was received in the first X slots after the start of the round
    VR1A
  | -- | The block being voted upon extends the most recently certified one.
    VR1B
  | -- | The last certificate a party has seen (if any) is from a round at least
    -- R rounds ago
    VR2A
  | -- | The last certificate included in our preferred chain (or the possible
    -- perasBootstrapRound taking precedence if specified and larger than the
    -- former) is from a round exactly c⋅K rounds ago for some c ∈ ℕ with c ≥ 0.
    VR2B PerasBootstrapAndRecoveryInteractionOutcome
  deriving (Show, Eq)

-- | As per revised CIP-0140,
-- the 'plvBootstrapRound :: StrictMaybe Word32' ledger parameter, represented
-- as 'perasBootstrapStatus :: PerasBootstrapStatus' in 'PerasParams', overrides
-- the round number of the latest certificate seen on the preferred chain for
-- VR-2B purposes if the bootstrap round is specified and if its value is larger
-- than the round of the latest certificate seen on the preferred chain.
--
-- This type captures the result of the interaction between the
-- 'perasBootstrapRound' and the latest certificate seen on the preferred chain
-- for VR-2B, to make the predicate explainable.
data PerasBootstrapAndRecoveryInteractionOutcome
  = PerasBootstrapEnabledAndOverriddenLatestCertOnChain (Evidence True String) PerasRoundNo
  | PerasBootstrapEnabledButNotOverriddenLatestCertOnChain (Evidence False String) PerasRoundNo
  | PerasBootstrapEnabledAndNoLatestCertOnChain PerasRoundNo
  | PerasBootstrapDisabledAndLatestCertOnChain PerasRoundNo
  | PerasBootstrapDisabledAndNoLatestCertOnChain
  deriving (Show, Eq)

-- | Extract the reference round number from a
-- 'PerasBootstrapAndRecoveryInteractionOutcome', if it exists.
pbrioToMaybeReferenceRound :: PerasBootstrapAndRecoveryInteractionOutcome -> Maybe PerasRoundNo
pbrioToMaybeReferenceRound = \case
  PerasBootstrapEnabledAndOverriddenLatestCertOnChain _ r -> Just r
  PerasBootstrapEnabledButNotOverriddenLatestCertOnChain _ r -> Just r
  PerasBootstrapEnabledAndNoLatestCertOnChain r -> Just r
  PerasBootstrapDisabledAndLatestCertOnChain r -> Just r
  PerasBootstrapDisabledAndNoLatestCertOnChain -> Nothing

-- | Given a 'PerasBootstrapStatus' and the round number of the latest
-- certificate seen on the preferred chain (if any),
-- compute the 'PerasBootstrapAndRecoveryInteractionOutcome' carrying the
-- reference 'PerasRoundNo' for VR-2B.
computePerasBootstrapAndRecoveryInteractionOutcome ::
  PerasBootstrapStatus ->
  -- | latestCertOnChainRoundNo, if present
  WithOrigin PerasRoundNo ->
  PerasBootstrapAndRecoveryInteractionOutcome
computePerasBootstrapAndRecoveryInteractionOutcome = \cases
  (PerasBootstrapEnabled bootstrapRound) (NotOrigin latestCertOnChainRoundNo) ->
    evalPred
      ("bootstrapRound > latestCertOnChainRoundNo" := bootstrapRound :>: latestCertOnChainRoundNo)
      $ \case
        etrue@ETrue{} ->
          PerasBootstrapEnabledAndOverriddenLatestCertOnChain etrue bootstrapRound
        efalse@EFalse{} ->
          PerasBootstrapEnabledButNotOverriddenLatestCertOnChain efalse latestCertOnChainRoundNo
  (PerasBootstrapEnabled bootstrapRound) Origin ->
    PerasBootstrapEnabledAndNoLatestCertOnChain bootstrapRound
  (PerasBootstrapDisabled) (NotOrigin latestCertOnChainRoundNo) ->
    PerasBootstrapDisabledAndLatestCertOnChain latestCertOnChainRoundNo
  (PerasBootstrapDisabled) Origin ->
    PerasBootstrapDisabledAndNoLatestCertOnChain

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
    VR2B (PerasBootstrapEnabledAndOverriddenLatestCertOnChain _ ref) ->
      "the perasBootstrapRound took precedence over the latest certificate on the preferred chain, and the perasBoostrapRound ("
        ++ show ref
        ++ ") is exactly c cooldown periods old for c ∈ ℕ with c ≥ 0"
    VR2B (PerasBootstrapEnabledButNotOverriddenLatestCertOnChain _ ref) ->
      "the perasBootstrapRound did not take precedence over the latest certificate on the preferred chain, and that certificate is from a round ("
        ++ show ref
        ++ ") exactly c cooldown periods old for c ∈ ℕ with c ≥ 0"
    VR2B (PerasBootstrapEnabledAndNoLatestCertOnChain ref) ->
      "the perasBootstrapRound took precedence because there is no certificate on the preferred chain at all, and the perasBoostrapRound ("
        ++ show ref
        ++ ") is exactly c cooldown periods old for c ∈ ℕ with c ≥ 0"
    VR2B (PerasBootstrapDisabledAndLatestCertOnChain ref) ->
      "the perasBootstrapRound is disabled, and the latest certificate on the preferred chain is from a round ("
        ++ show ref
        ++ ") exactly c cooldown periods old for c ∈ ℕ with c ≥ 0"
    VR2B PerasBootstrapDisabledAndNoLatestCertOnChain ->
      "the perasBootstrapRound is disabled, and there is no certificate on the preferred chain at all"

-- | VR-1A: the voter has seen the certificate for the previous round, and the
-- certificate was received in the first X slots after the start of the round.
--
-- Note: If no certificate has been seen for the previous round, VR-1A cannot be satisfied.
-- Voting in such a case can only happen through the recovery path (rules VR-2A and VR-2B)
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
        -- We have never seen a certificate ==> do not allow voting through the healthy path
        Origin ->
          Bool False

    -- The latest certificate seen was received within X slots from the start
    -- of its round
    vr1a2 =
      case latestCertSeen of
        -- We have seen a certificate ==> check its arrival time
        NotOrigin cert ->
          lcsArrivalSlot cert :<=: lcsRoundStartSlot cert + _X
        -- We have never seen a certificate ==> do not allow voting through the healthy path
        Origin ->
          Bool False

    _X =
      SlotNo $
        unPerasCertArrivalThreshold $
          perasCertArrivalThreshold perasParams

-- | VR-1B: the block being voted upon extends the most recently certified one.
--
-- Note: If no certificate has been seen for the previous round, VR-1B cannot be satisfied.
-- Voting in such a case can only happen through the recovery path (rules VR-2A and VR-2B)
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
        -- We have never seen a certificate ==> do not allow voting through the healthy path
        Origin ->
          Bool False

-- | VR-2A: the last certificate a party has seen (if any) is from a round at
-- least R rounds ago.
-- This enforces the chain-healing period that must occur before leaving a
-- cooldown period.
--
-- Note: If no certificate has ever been seen, then VR-2A is considered true, to
-- allow bootstrap.
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
        -- We have never seen a certificate ==> consider VR-2A true to allow bootstrap.
        Origin ->
          Bool True

    _R =
      PerasRoundNo $
        unPerasIgnoranceRounds $
          perasIgnoranceRounds $
            perasParams

-- | The last certificate included in our preferred chain (or the possible
-- perasBootstrapRound taking precedence if specified and larger than the
-- former) is from a round exactly c⋅K rounds ago for some c ∈ ℕ with c ≥ 0.
--
-- Note: c = 0 case is only used for bootstraping.
-- Indeed, when we are recovering after a real cooldown, the current round will
-- be strictly greater than the round of whichever cert is on chain, so c can
-- never be zero in that case.
--
-- In case where boostrap is disabled, and there is no latest certificate on
-- chain, this rule fails.
perasVR2B ::
  PerasVotingView cert blk ->
  Pred PerasVotingRule
perasVR2B
  PerasVotingView
    { perasParams
    , currRoundNo
    , latestCertOnChainRound
    } =
    VR2B perasBootstrapAndRecoveryInteractionOutcome := vr2b
   where
    vr2b =
      case pbrioToMaybeReferenceRound perasBootstrapAndRecoveryInteractionOutcome of
        Just referenceRound ->
          let (c, diffMod) = differenceDivModByK currRoundNo referenceRound
           in -- The current round is younger (larger) or equal to the reference round,
              -- i.e. 'c' = the whole division of their difference by _K must be
              -- non-negative.
              (0 :<=: c)
                -- The current round is exactly c⋅K rounds after the reference round,
                -- i.e. 'diffMod' = the modulus of their difference by _K must be 0.
                :/\: (diffMod :==: 0)
        Nothing ->
          -- In case where bootstrap is disabled, and there is no latest
          -- certificate on chain, this rule fails.
          Bool False

    -- \| Given a reference round, compute the whole division and modulus of the
    -- difference between the current round and the reference round by _K, and
    -- return a tuple (whole division, modulus)
    --
    -- Note: We use 'divMod', rather than 'quotRem', to ensure that when the
    -- difference between the current round and the reference round is negative,
    -- the whole division's result is negative ('div' is truncated towards -∞
    -- unlike 'quot' which is truncated towards 0)
    differenceDivModByK currentRoundNo referenceRound =
      ( toInteger (unPerasRoundNo currentRoundNo)
          - toInteger (unPerasRoundNo referenceRound)
      )
        `divMod` toInteger (unPerasRoundNo _K)

    _K =
      PerasRoundNo $
        unPerasCooldownRounds $
          perasCooldownRounds $
            perasParams

    perasBootstrapAndRecoveryInteractionOutcome =
      computePerasBootstrapAndRecoveryInteractionOutcome
        (perasBootstrapStatus perasParams)
        latestCertOnChainRound

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
