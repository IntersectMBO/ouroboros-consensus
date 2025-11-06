{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

-- | Pure Peras voting rules
--
-- This module implements the Peras voting rules in a pure fasion, along with
-- the necessary inpure machinery to retrieve their inputs. These rules are
-- translated as verbatim as possible from:
--
-- https://github.com/cardano-foundation/CIPs/blob/master/CIP-0140/README.md#rules-for-voting-in-a-round
module Ouroboros.Consensus.Peras.Voting
  ( PerasVotingView (..)
  , mkPerasVotingView
  , isPerasVotingAllowed
  , PerasVotingRule (..)
  , VoteReason (..)
  , NoVoteReason (..)
  , perasVR1A
  , perasVR1B
  , perasVR2A
  , perasVR2B
  , perasVR1
  , perasVR2
  , perasVotingRules
  )
where

import Cardano.Slotting.Slot (WithOrigin)
import Data.Bifunctor (bimap)
import Data.Typeable (Typeable)
import Ouroboros.Consensus.Block (WithOrigin (..))
import Ouroboros.Consensus.Block.Abstract
  ( GetHeader (..)
  , Header
  , SlotNo (..)
  , StandardHash
  , castPoint
  , succWithOrigin
  )
import Ouroboros.Consensus.Block.SupportsPeras
  ( HasPerasCertRound (..)
  , PerasRoundNo (..)
  , ValidatedPerasCert
  , getPerasCertBoostedBlock
  , getPerasCertRound
  , onPerasRoundNo
  )
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
  ( WithArrivalTime (..)
  )
import Ouroboros.Consensus.HardFork.History.EraParams (fromPerasEnabled)
import qualified Ouroboros.Consensus.HardFork.History.Qry as HF
import qualified Ouroboros.Consensus.HardFork.History.Summary as HF
import Ouroboros.Consensus.Peras.Params
  ( PerasBlockMinSlots (..)
  , PerasCertArrivalThreshold (..)
  , PerasCooldownRounds (..)
  , PerasIgnoranceRounds (..)
  , PerasParams (..)
  )
import Ouroboros.Consensus.Util.Pred
  ( Evidence
  , Explainable (..)
  , ExplanationMode (..)
  , Pred (..)
  , evalPred
  )
import Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF

{-------------------------------------------------------------------------------
  Voting interface
-------------------------------------------------------------------------------}

-- | Interface needed to evaluate the Peras voting rules
data PerasVotingView cert = PerasVotingView
  { perasParams :: !PerasParams
  -- ^ Peras protocol parameters.
  , currRoundNo :: !PerasRoundNo
  -- ^ The current Peras round number.
  , latestCertSeen :: !(WithOrigin cert)
  -- ^ The most recent certificate seen by the voter.
  , latestCertOnChain :: !(WithOrigin cert)
  -- ^ The most recent certificate present in some chain.
  , certRoundStart :: cert -> SlotNo
  -- ^ Get the slot number at the start of the Peras round of a certificate.
  , arrivalSlot :: cert -> SlotNo
  -- ^ Get the arrival slot number of a certificate
  , candidateExtendsCert :: cert -> Bool
  -- ^ Does the candidate block extend the one boosted by a certificate?
  }

-- | Construct a 'PerasVotingView'.
--
-- NOTE: this assumes that the client code computes all the needed inputs
-- within the same STM transaction, or the results may be inconsistent.
mkPerasVotingView ::
  ( cert ~ WithArrivalTime (ValidatedPerasCert blk)
  , StandardHash blk
  , Typeable blk
  , GetHeader blk
  ) =>
  PerasParams ->
  PerasRoundNo ->
  WithOrigin cert ->
  WithOrigin cert ->
  AnchoredFragment (Header blk) ->
  HF.Summary blks ->
  PerasVotingView cert
mkPerasVotingView
  perasParams
  currRoundNo
  latestCertSeen
  latestCertOnChain
  currChain
  summary =
    PerasVotingView
      { perasParams = perasParams
      , currRoundNo = currRoundNo
      , latestCertSeen = latestCertSeen
      , latestCertOnChain = latestCertOnChain
      , certRoundStart = certRoundStart
      , arrivalSlot = arrivalSlot
      , candidateExtendsCert = candidateExtendsCert
      }
   where
    -- Run a pure query with the given summary
    pureQuery = flip HF.runQueryPure summary

    -- The Peras block minium slots parameter (L).
    _L = SlotNo (unPerasBlockMinSlots (perasBlockMinSlots perasParams))

    -- Candidate block slot, i.e., that of the block that's at least
    -- 'blockMinSlots' (L) old from the start of the current round.
    --
    -- NOTE: here we need make sure that the result doesn't underflow.
    candidateSlot
      | currRoundStart >= _L = currRoundStart - _L
      | otherwise = SlotNo 0
     where
      currRoundStart = roundStart currRoundNo

    -- The prefix of our current chain leading to the candidate block.
    chainAtCandidate = fst (AF.splitAtSlot candidateSlot currChain)

    -- The slot number at the start of a Peras round.
    --
    -- NOTE: this might throw a 'PastHorizonException' if the caller is not
    -- is not prepared to start voting.
    roundStart roundNo = slotNo
     where
      (slotNo, _) =
        pureQuery (fromPerasEnabledOrFail <$> HF.perasRoundNoToSlot roundNo)

      fromPerasEnabledOrFail =
        fromPerasEnabled
          (error "mkPerasVotingView: Peras is disabled in the current era")

    -- A slightly safer version of 'roundStart' that works with certificates.
    --
    -- NOTE: Instead of using 'roundStart' directly, this makes it more
    -- harder for the user of the voting interface to accidentally pass a
    -- 'PerasRoundNo' that triggers the 'PastHorizonException', as the
    -- existence of a certificate _should_ imply that Peras was enabled
    -- at the time the certificate was issued.
    certRoundStart = roundStart . getPerasCertRound

    -- The arrival slot number of a certificate.
    --
    -- NOTE: this might throw a 'PastHorizonException' if the caller does not
    -- ensure that the arrival time is within the current realizable horizon.
    arrivalSlot cert = slotNo
     where
      (slotNo, _, _) =
        pureQuery (HF.wallclockToSlot (getArrivalTime cert))

    -- Does the candidate block extend the one boosted by a certificate?
    --
    -- This can be trivially tested by checking whether the certificate is
    -- within the bounds of the chain prefix leading to the candidate block.
    --
    -- NOTE: in the case of an extremely old certificate boosting a block
    -- beyond the immutable prefix, this could incorrectly return false even
    -- if the voting candidate technically extends the certificate point.
    -- However, this a boring case that we can safely ignore. Conversely,
    -- the case of a certificate that's too new to be voted for is covered
    -- by using the approriate prefix of our current chain.
    candidateExtendsCert cert =
      AF.withinFragmentBounds certBoostedBlockHeader chainAtCandidate
     where
      certBoostedBlockHeader = castPoint (getPerasCertBoostedBlock cert)

-- | Reason for voting being disallowed
newtype NoVoteReason = NoVoteReason (Evidence PerasVotingRule)
  deriving (Show, Eq, Explainable)

-- | Reason for voting being allowed
newtype VoteReason = VoteReason (Evidence PerasVotingRule)
  deriving (Show, Eq, Explainable)

-- | Evaluate whether voting is allowed or not according to the voting rules
isPerasVotingAllowed ::
  HasPerasCertRound cert =>
  PerasVotingView cert ->
  Either NoVoteReason VoteReason
isPerasVotingAllowed pvv =
  bimap NoVoteReason VoteReason $
    evalPred (perasVotingRules pvv)

{-------------------------------------------------------------------------------
  Voting rules
-------------------------------------------------------------------------------}

-- | Voting rules
--
-- Each constructor corresponds to a voting rule as per CIP-0140.
--
-- * VR1x correspond to the "happy" path, i.e., when nodes proceed to vote
-- normally, whereas
-- * VR2x correspond to the "cool-down" path, i.e., when nodes are exiting a
-- cool-down period.
data PerasVotingRule = VR1A | VR1B | VR2A | VR2B
  deriving (Show, Eq)

instance Explainable PerasVotingRule where
  explain Shallow = \case
    VR1A -> "VR-1A"
    VR1B -> "VR-1B"
    VR2A -> "VR-2A"
    VR2B -> "VR-2B"
  explain Deep = \case
    VR1A ->
      "voter has seen the certificate for the previous round in time"
    VR1B ->
      "the block being voted upon extends the most recently certified block"
    VR2A ->
      "the last certificate seen is sufficiently old"
    VR2B ->
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
    , arrivalSlot
    , certRoundStart
    } =
    VR1A := vr1a1 :/\: vr1a2
   where
    -- The latest certificate seen is from the previous round
    --
    -- NOTE: 'succWithOrigin' handles the 'Origin' case (i.e. when we have
    -- never seen a certificate before) correctly by returning 0, as this is
    -- the only round number that should satisfy this equality when we are
    -- bootstrapping the voting process. In other words, we should be able to
    -- start voting from round 0 even if we have never seen a certificate
    -- before, but failing to do so should trigger a cooldown period
    -- immediately after.
    vr1a1 =
      currRoundNo :==: succWithOrigin (getPerasCertRound <$> latestCertSeen)

    -- The latest certificate seen was received within X slots from the start
    -- of the round
    vr1a2 =
      case latestCertSeen of
        -- We have seen a certificate ==> check its arrival time
        NotOrigin cert -> arrivalSlot cert :<=: certRoundStart cert + _X
        -- We have never seen a certificate ==> vacuously true
        Origin -> Bool True

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
    , candidateExtendsCert
    } =
    VR1B := vr1b
   where
    -- The block being voted upon extends the most recently certified one
    vr1b =
      case latestCertSeen of
        -- We have seen a certificate ==> check that it extends our chain
        NotOrigin cert -> Bool (candidateExtendsCert cert)
        -- We have never seen a certificate ==> vacuously true
        Origin -> Bool True

-- | VR-2A: the last certificate a party has seen is from a round at least R
-- rounds previously. This enforces the chain-healing period that must occur
-- before leaving a cool-down period.
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
      -- NOTE: we use 'succWithOrigin' and '-1' to handle the 'Origin' case
      -- (i.e. when we have never seen a certificate before) correctly,
      -- treating the 'Origin' certificate as being from round -1.
      (succWithOrigin (getPerasCertRound <$> latestCertSeen) - 1 + _R)
        :<=: currRoundNo

    _R =
      PerasRoundNo $
        unPerasIgnoranceRounds $
          perasIgnoranceRounds $
            perasParams

-- | VR-2B: the last certificate included in a party's current chain is from a
-- round exactly c⋅K rounds ago for some c ∈ ℕ with c ≥ 0. This enforces chain
-- quality and common prefix before leaving a cool-down period.
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
    VR2B := vr2b
   where
    vr2b =
      case latestCertOnChain of
        -- There is a certificate on chain ==> we must check its round number
        NotOrigin cert ->
          -- The certificate comes from a round older than the current one
          (currRoundNo :>: getPerasCertRound cert)
            -- The certificate round is c⋅K rounds away from the current one
            :/\: (currRoundNo `rmod` _K :==: getPerasCertRound cert `rmod` _K)
        -- There is no certificate on chain ==> check if we are recovering
        -- from an initial cooldown after having initially failed to
        -- reach a quorum during bootstrapping.
        --
        -- NOTE: '_K - 1' here is treating the 'Origin' certificate as being
        -- from round -1.
        Origin -> currRoundNo `rmod` _K :==: _K - 1

    rmod = onPerasRoundNo mod

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
-- the chain is about to exit a cool-down period.
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
