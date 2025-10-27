{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- | Pure Peras voting rules
--
-- This module implements some machinery to abstract away the impure inputs
-- needed to evaluate the Peras voting rules in a pure fashion.
--
-- NOTE: most of the code in this module returns values over 'PerasEnabled' to
-- reflect the fact that some of the computations performed here require
-- querying a hard fork summary to map timestamps and slot numbers to their
-- corresponding Peras round numbers. This may not be possible if Peras was not
-- enabled at the relevant points in time, and such a case should be handled
-- appropriately by the caller.
module Ouroboros.Consensus.Peras.Voting.View
  ( PerasQryException (..)
  , PerasQry
  , runPerasQry
  , perasRoundStart
  , perasChainAtCandidateBlock
  , LatestCertSeenView (..)
  , LatestCertOnChainView (..)
  , PerasVotingView (..)
  , mkPerasVotingView
  )
where

import Cardano.Slotting.Slot (WithOrigin)
import Control.Exception (Exception)
import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.Reader (MonadReader (..), Reader, runReader)
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
  )
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
  ( WithArrivalTime (..)
  )
import Ouroboros.Consensus.HardFork.History.EraParams
  ( pattern NoPerasEnabled
  , pattern PerasEnabled
  )
import qualified Ouroboros.Consensus.HardFork.History.Qry as HF
import qualified Ouroboros.Consensus.HardFork.History.Summary as HF
import Ouroboros.Consensus.Peras.Params
  ( PerasBlockMinSlots (..)
  , PerasParams (..)
  )
import Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF

{-------------------------------------------------------------------------------
  Voting helpers based on a hard fork summary
-------------------------------------------------------------------------------}

-- | Exceptions thrown when querying Peras-related historical information
data PerasQryException
  = -- | The hard fork summary does not cover the needed point in time.
    PerasQryExceptionPastHorizon HF.PastHorizonException
  | -- | Peras is not enabled at the needed point in time.
    PerasQryExceptionPerasDisabled
  deriving (Show, Exception)

-- | Monad for querying Peras-related information from a hard fork summary.
--
-- This covers two possible error cases:
-- 1. the hard fork summary does not cover the needed point in time, or
-- 2. Peras is not enabled at the needed point in time.
newtype PerasQry xs a
  = PerasQry (ExceptT PerasQryException (Reader (HF.Summary xs)) a)
  deriving newtype (Functor, Applicative, Monad)

-- | Run a 'PerasQry' against a hard fork summary
runPerasQry ::
  HF.Summary xs ->
  PerasQry xs a ->
  Either PerasQryException a
runPerasQry summary (PerasQry qry) =
  runReader (runExceptT qry) summary

-- | Arrival slot number of a certificate
perasCertArrivalSlot ::
  WithArrivalTime cert ->
  PerasQry xs SlotNo
perasCertArrivalSlot cert = PerasQry $ do
  summary <- ask
  case HF.runQuery (HF.wallclockToSlot (getArrivalTime cert)) summary of
    Left pastHorizon ->
      throwError (PerasQryExceptionPastHorizon pastHorizon)
    Right (slotNo, _, _) ->
      return slotNo

-- | Slot number at the start of a Peras round
perasRoundStart ::
  PerasRoundNo ->
  PerasQry xs SlotNo
perasRoundStart roundNo = PerasQry $ do
  summary <- ask
  case HF.runQuery (HF.perasRoundNoToSlot roundNo) summary of
    Left pastHorizon ->
      throwError (PerasQryExceptionPastHorizon pastHorizon)
    Right NoPerasEnabled ->
      throwError PerasQryExceptionPerasDisabled
    Right (PerasEnabled (slotNo, _)) ->
      return slotNo

-- | Chain prefix leading to the candidate block.
--
-- This corresponds to the last block /before/ the candidate slot horizon,
-- defined as the slot that is at least 'blockMinSlots' (L) old from the start
-- of the current round.
--
-- NOTE: this is where the candidate is determined according to CIP-0140.
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
      let lcsCert = forgetBoostedBlockStatus certWithProvenance
      lcsArrivalSlot <- perasCertArrivalSlot lcsCert
      lcsRoundStartSlot <- perasRoundStart (getPerasCertRound lcsCert)
      let lcsCandidateBlockExtendsCert = candidateBlockExtendsCert certWithProvenance
      pure $
        LatestCertSeenView
          { lcsCert
          , lcsArrivalSlot
          , lcsRoundStartSlot
          , lcsCandidateBlockExtendsCert
          }

    mkLatestCertOnChainView lcocCert =
      pure $
        LatestCertOnChainView
          { lcocCert
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
