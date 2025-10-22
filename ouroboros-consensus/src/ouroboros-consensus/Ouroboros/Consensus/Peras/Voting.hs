{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
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

import Data.Bifunctor (bimap)
import Data.Coerce (coerce)
import Data.Typeable (Typeable)
import GHC.Word (Word64)
import Ouroboros.Consensus.Block.Abstract
  ( GetHeader (..)
  , Header
  , SlotNo (..)
  , StandardHash
  , castPoint
  )
import Ouroboros.Consensus.Block.SupportsPeras
  ( HasPerasCertRound
  , PerasRoundNo (..)
  , ValidatedPerasCert
  , getPerasCertBoostedBlock
  , getPerasCertRound
  , onPerasRoundNo
  )
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
  ( WithArrivalTime (..)
  )
import Ouroboros.Consensus.HardFork.History.EraParams
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
  { pvvPerasParams :: !PerasParams
  -- ^ Peras protocol parameters.
  , pvvCurrRoundNo :: !PerasRoundNo
  -- ^ The current Peras round number.
  , pvvLatestCertSeen :: !cert
  -- ^ The most recent certificate seen by the voter.
  , pvvLatestCertOnChain :: !cert
  -- ^ The most recent certificate present in some chain.
  , pvvRoundStart :: PerasRoundNo -> SlotNo
  -- ^ Get the slot number at the start of a Peras round.
  , pvvArrivalSlot :: cert -> SlotNo
  -- ^ Get the arrival slot number of a certificate
  , pvvCandidateExtendsCert :: cert -> Bool
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
  cert ->
  cert ->
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
      { pvvPerasParams = perasParams
      , pvvCurrRoundNo = currRoundNo
      , pvvLatestCertSeen = latestCertSeen
      , pvvLatestCertOnChain = latestCertOnChain
      , pvvRoundStart = roundStart
      , pvvArrivalSlot = arrivalSlot
      , pvvCandidateExtendsCert = candidateExtendsCert
      }
   where
    -- The Peras block minium slots parameter (L).
    blockMinSlots = coerce (perasBlockMinSlots perasParams)

    -- Candidate block slot, i.e., that of the block that's at least
    -- 'blockMinSlots' old from the start of the current round.
    candidateSlot = roundStart currRoundNo - blockMinSlots

    -- The prefix of our current chain leading to the candidate block.
    chainAtCandidate = fst (AF.splitAtSlot candidateSlot currChain)

    -- The slot number at the start of a Peras round.
    --
    -- NOTE: this might throw a 'PastHorizonException' if the caller is not
    -- is not prepared to start voting.
    roundStart roundNo =
      fst $
        fromPerasEnabled
          (error "mkPerasVotingView: Peras is disabled in the current era")
          (HF.runQueryPure (HF.perasRoundNoToSlot roundNo) summary)

    -- The arrival slot number of a certificate.
    --
    -- NOTE: this might throw a 'PastHorizonException' if the caller does not
    -- ensure that the arrival time is within the current realizable horizon.
    arrivalSlot cert = slotNo
     where
      (slotNo, _, _) =
        HF.runQueryPure (HF.wallclockToSlot (getArrivalTime cert)) summary

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
  deriving (Show, Explainable)

-- | Reason for voting being allowed
newtype VoteReason = VoteReason (Evidence PerasVotingRule)
  deriving (Show, Explainable)

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
      "the last certificate seen is from at least R rounds ago"
    VR2B ->
      "the last certificate on chain is from exactly c⋅K rounds ago"

-- | VR-1A: the voter has seen the certificate for the previous round, and the
-- certificate was received in the first X slots after the start of the round.
perasVR1A ::
  HasPerasCertRound cert =>
  PerasVotingView cert -> Pred PerasVotingRule
perasVR1A pvv =
  VR1A
    := (pvvCurrRoundNo pvv :==: latestCertSeenRoundNo <> _1)
    :/\: (latestCertSeenArrivalSlot :<=: latestCertSeenRoundStart + _X)
 where
  _1 = coerce @Word64 1
  _X = coerce (perasCertArrivalThreshold (pvvPerasParams pvv))
  latestCertSeenRoundNo = getPerasCertRound (pvvLatestCertSeen pvv)
  latestCertSeenRoundStart = pvvRoundStart pvv latestCertSeenRoundNo
  latestCertSeenArrivalSlot = pvvArrivalSlot pvv (pvvLatestCertSeen pvv)

-- | VR-1B: the block being voted upon extends the most recently certified one.
perasVR1B :: PerasVotingView cert -> Pred PerasVotingRule
perasVR1B pvv =
  VR1B := Bool (pvvCandidateExtendsCert pvv (pvvLatestCertSeen pvv))

-- | VR-2A: the last certificate a party has seen is from a round at least R
-- rounds previously. This enforces the chain-healing period that must occur
-- before leaving a cool-down period.
perasVR2A ::
  HasPerasCertRound cert =>
  PerasVotingView cert ->
  Pred PerasVotingRule
perasVR2A pvv =
  VR2A := latestCertSeenRoundNo <> _R :<=: currRoundNo
 where
  _R = coerce (perasIgnoranceRounds (pvvPerasParams pvv))
  currRoundNo = pvvCurrRoundNo pvv
  latestCertSeenRoundNo = getPerasCertRound (pvvLatestCertSeen pvv)

-- | VR-2B: the last certificate included in a party's current chain is from a
-- round exactly c⋅K rounds ago for some c ∈ ℕ with c ≥ 0. This enforces chain
-- quality and common prefix before leaving a cool-down period.
perasVR2B ::
  HasPerasCertRound cert =>
  PerasVotingView cert ->
  Pred PerasVotingRule
perasVR2B pvv =
  VR2B
    := (currRoundNo :>: latestCertOnChainRoundNo)
    :/\: (currRoundNo `rmod` _K :==: latestCertOnChainRoundNo `rmod` _K)
 where
  _K = coerce (perasCooldownRounds (pvvPerasParams pvv))
  currRoundNo = pvvCurrRoundNo pvv
  latestCertOnChainRoundNo = getPerasCertRound (pvvLatestCertOnChain pvv)
  rmod = onPerasRoundNo mod

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
