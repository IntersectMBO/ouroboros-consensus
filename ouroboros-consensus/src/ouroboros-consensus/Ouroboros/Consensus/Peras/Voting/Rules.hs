{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- | Pure Peras voting rules
--
-- This module implements the Peras voting rules in a pure fasion, along with
-- the necessary inpure machinery to retrieve their inputs. These rules are
-- translated as verbatim as possible from:
--
-- https://github.com/cardano-foundation/CIPs/blob/master/CIP-0140/README.md#rules-for-voting-in-a-round
--
-- NOTE: most of the code in this module returns values over 'PerasEnabled' to
-- reflect the fact that some of the computations performed here require
-- querying a hard fork summary to map timestamps and slot numbers to their
-- corresponding Peras round numbers, which may not be possible if Peras was not
-- enabled at the relevant points in time. Such a case should be handled
-- appropriately by the caller.
--
-- NOTE: in this file, we use uncommon variable names such as `_L` or `_X`
-- because that is their name in the CIP-0140, and we can't have variable names
-- starting with capital letters. Contrary to typical Haskell conventions, those
-- do not denote ignored variables.
module Ouroboros.Consensus.Peras.Voting.Rules
  ( PerasQry (runPerasQry)
  , perasRoundStart
  , perasChainAtCandidateBlock
  , LatestCertSeenView (..)
  , LatestCertOnChainView (..)
  , PerasVotingView (..)
  , mkPerasVotingView
  , isPerasVotingAllowed
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

import Cardano.Slotting.Slot (WithOrigin)
import Ouroboros.Consensus.Block (WithOrigin (..))
import Ouroboros.Consensus.Block.Abstract
  ( GetHeader (..)
  , Header
  , SlotNo (..)
  , castPoint
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
import Ouroboros.Consensus.HardFork.History.EraParams
  ( PerasEnabled
  , pattern NoPerasEnabled
  , pattern PerasEnabled
  )
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
  ( Evidence (..)
  , Explainable (..)
  , ExplanationMode (..)
  , Pred (..)
  , evalPred
  )
import Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF

{-------------------------------------------------------------------------------
  Voting helpers based on a hard fork summary
-------------------------------------------------------------------------------}

-- | Monad for querying Peras-related information from a hard fork summary.
--
-- This covers two possible error cases:
-- 1. the hard fork summary does not cover the needed point in time
--    (case @Left ...@), or
-- 2. Peras is not enabled at the needed point in time
--    (case @Right NoPerasEnabled@).
newtype PerasQry xs a
  = PerasQry
  { runPerasQry ::
      HF.Summary xs ->
      Either HF.PastHorizonException (PerasEnabled a)
  }

instance Functor (PerasQry xs) where
  fmap f (PerasQry g) =
    PerasQry $ \summary ->
      fmap (fmap f) (g summary)

instance Applicative (PerasQry xs) where
  pure x =
    PerasQry $ \_summary ->
      pure (pure x)
  PerasQry f <*> PerasQry g =
    PerasQry $ \summary -> do
      f' <- f summary
      g' <- g summary
      pure (f' <*> g')

instance Monad (PerasQry xs) where
  PerasQry fa >>= g =
    PerasQry $ \summary -> do
      pea <- fa summary
      case pea of
        PerasEnabled a -> runPerasQry (g a) summary
        NoPerasEnabled -> pure NoPerasEnabled

-- | Arrival slot number of a certificate
perasCertArrivalSlot ::
  WithArrivalTime cert ->
  PerasQry xs SlotNo
perasCertArrivalSlot cert =
  PerasQry $
    fmap (fmap (pure . fst3)) $
      HF.runQuery (HF.wallclockToSlot (getArrivalTime cert))
 where
  fst3 (x, _, _) = x

-- | Slot number at the start of a Peras round
perasRoundStart ::
  PerasRoundNo ->
  PerasQry xs SlotNo
perasRoundStart roundNo =
  PerasQry $
    fmap (fmap (fmap fst)) $
      HF.runQuery (HF.perasRoundNoToSlot roundNo)

-- | Chain prefix leading to the candidate block.
--
-- This corresponds to the last block /before/ the candidate slot horizon,
-- defined as the slot that is at least 'blockMinSlots' (L) old from the start
-- of the current round.
--
-- NOTE: this is where the candidate  is determined according to CIP-0140.
-- This function may evolve in the future if the candidate block selection
-- becomes more complex.
perasChainAtCandidateBlock ::
  GetHeader blk =>
  PerasBlockMinSlots ->
  PerasRoundNo ->
  AnchoredFragment (Header blk) ->
  PerasQry xs (AnchoredFragment (Header blk))
perasChainAtCandidateBlock blockMinSlots currRoundNo currChain = do
  -- Slot number at the start of the current round
  currRoundStart <- perasRoundStart currRoundNo
  -- Minimum number of slots to consider before the candidate block
  let _L = SlotNo (unPerasBlockMinSlots blockMinSlots)
  -- Determine the candidate slot horizon
  -- NOTE: here we need make sure that the result doesn't underflow
  let candidateSlotHorizon
        | currRoundStart >= _L = currRoundStart - _L
        | otherwise = SlotNo 0
  -- Split the chain at the candidate slot horizon
  pure $ fst $ AF.splitAtSlot candidateSlotHorizon currChain

{-------------------------------------------------------------------------------
  Voting interface
-------------------------------------------------------------------------------}

-- | View of the latest certificate seen by the voter
--
-- NOTE: the voting rules depend on the candidate block indirectly. This is
-- reflected in the fact that the voting view does not contain the candidate
-- block or its point, but only whether the candidate block extends the block
-- boosted by the most recent certificate seen by the voter, which is provided
-- to the rules via 'lcsCandidateBlockExtendsCert' here.
data LatestCertSeenView cert
  = LatestCertSeenView
  { lcsCert :: !cert
  -- ^ Latest certificate seen by the voter
  , lcsArrivalSlot :: !SlotNo
  -- ^ Slot number at which this certificate was received
  , lcsRoundStartSlot :: !SlotNo
  -- ^ Starting slot number of the round where this certificate was received
  , lcsCandidateBlockExtendsCert :: !Bool
  -- ^ Does the candidate block extend the one boosted by this certificate?
  }
  deriving Show

-- | View of the latest certificate present in our preferred chain
--
-- NOTE: if we add more fields here in the future, do not forget to add
-- strictness annotations as needed.
newtype LatestCertOnChainView cert
  = LatestCertOnChainView
  { lcocCert :: cert
  -- ^ Latest certificate present in our preferred chain
  }
  deriving Show

-- | Interface needed to evaluate the Peras voting rules
--
-- NOTE: the voting rules depend on the candidate block indirectly. This is
-- reflected in the fact that the voting view does not contain the candidate
-- block or its point, but only whether the candidate block extends the block
-- boosted by the most recent certificate seen by the voter, which is provided
-- to the rules via 'lcsCandidateBlockExtendsCert' inside 'latestCertSeen'.
data PerasVotingView cert = PerasVotingView
  { perasParams :: PerasParams
  -- ^ Peras protocol parameters
  , currRoundNo :: !PerasRoundNo
  -- ^ The current Peras round number
  , latestCertSeen :: !(WithOrigin (LatestCertSeenView cert))
  -- ^ The most recent certificate seen by the voter
  , latestCertOnChain :: !(WithOrigin (LatestCertOnChainView cert))
  -- ^ The most recent certificate present in our preferred chain
  }
  deriving Show

-- | Indicate the status of a block boosted by a certificate w.r.t. the
-- chain's immutable prefix and volatile suffix.
data WithBoostedBlockStatus cert
  = -- | Certificate boosting a block within the immutable prefix
    CertWithImmutableBlock cert
  | -- | Certificate boosting a block within the volatile suffix
    CertWithVolatileBlock cert
  deriving Show

-- | Deconstruct a certificate from its provenance wrapper
forgetBoostedBlockStatus :: WithBoostedBlockStatus cert -> cert
forgetBoostedBlockStatus = \case
  CertWithVolatileBlock cert -> cert
  CertWithImmutableBlock cert -> cert

-- | Construct a 'PerasVotingView'.
--
-- NOTE: this assumes that the client code computes all the needed inputs
-- within the same STM transaction, or the results may be inconsistent.
mkPerasVotingView ::
  ( cert ~ WithArrivalTime (ValidatedPerasCert blk)
  , GetHeader blk
  ) =>
  -- | Peras protocol parameters
  PerasParams ->
  -- | Current Peras round number
  PerasRoundNo ->
  -- | Most recent certificate seen by the voter
  WithOrigin (WithBoostedBlockStatus cert) ->
  -- | Most recent certificate included in some block in our preferred chain
  WithOrigin cert ->
  -- | Prefix leading to the candidate block in the volatile suffix of our
  -- preferred chain
  AnchoredFragment (Header blk) ->
  -- | Constructed voting view
  PerasQry xs (PerasVotingView cert)
mkPerasVotingView
  perasParams
  currRoundNo
  latestCertSeen
  latestCertOnChain
  chainAtCandidateBlock = do
    latestCertSeenView <- traverse mkLatestCertSeenView latestCertSeen
    latestCertOnChainView <- traverse mkLatestCertOnChainView latestCertOnChain
    pure $
      PerasVotingView
        { perasParams = perasParams
        , currRoundNo = currRoundNo
        , latestCertSeen = latestCertSeenView
        , latestCertOnChain = latestCertOnChainView
        }
   where
    mkLatestCertSeenView certWithProvenance = do
      let cert = forgetBoostedBlockStatus certWithProvenance
      roundStartSlot <- perasRoundStart (getPerasCertRound cert)
      arrivalSlot <- perasCertArrivalSlot cert
      let candidateExtendsCert = candidateBlockExtendsCert certWithProvenance
      pure $
        LatestCertSeenView
          { lcsCert = cert
          , lcsArrivalSlot = arrivalSlot
          , lcsRoundStartSlot = roundStartSlot
          , lcsCandidateBlockExtendsCert = candidateExtendsCert
          }

    mkLatestCertOnChainView cert =
      pure $
        LatestCertOnChainView
          { lcocCert = cert
          }

    -- Does the candidate block extend the one boosted by a certificate?
    --
    -- This can be trivially tested by checking whether the certificate is
    -- within the bounds of the volatile chain prefix leading to the candidate
    -- block. Conversely, the case of a certificate pointing to a block that's
    -- too new to be voted for is also covered by this logic, as it won't be
    -- part of the 'chainAtCandidateBlock' fragment.
    --
    -- NOTE: the case of an extremely old certificate boosting a block beyond
    -- the volatile suffix is covered by also providing the status of the
    -- boosted block w.r.t. the chain's immutable prefix and volatile suffix.
    candidateBlockExtendsCert (CertWithImmutableBlock _) =
      -- This case is vacuously true: an immutable block is always part of
      -- any volatile suffix, so the candidate block trivially extends it.
      True
    candidateBlockExtendsCert (CertWithVolatileBlock cert) =
      -- Check whether the boosted block is within the volatile fragment leading
      -- to the candidate block.
      AF.withinFragmentBounds
        (castPoint (getPerasCertBoostedBlock cert))
        chainAtCandidateBlock

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

{-------------------------------------------------------------------------------
  Voting rules
-------------------------------------------------------------------------------}

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
