{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

-- | This module defines the logic needed to evaluate when a Peras certificate
-- must be included in a block.
--
-- NOTE: in this file, we use uncommon variable names such as `_A` because that
-- is their name in the CIP-0140, and we can't have variable names starting
-- with capital letters. Contrary to typical Haskell conventions, those do not
-- denote ignored variables.
module Ouroboros.Consensus.Peras.Cert.Inclusion
  ( LatestCertSeenView (..)
  , LatestCertOnChainView (..)
  , PerasCertInclusionView (..)
  , mkPerasCertInclusionView
  , PerasCertInclusionRule (..)
  , PerasCertInclusionRulesDecision (..)
  , needCert
  , noCertsFromTwoRoundsAgo
  , needCertRules
  ) where

import Ouroboros.Consensus.Block (WithOrigin (..), withOriginToMaybe)
import Ouroboros.Consensus.Block.SupportsPeras
  ( HasPerasCertRound (..)
  , PerasParams
  , PerasRoundNo (..)
  )
import Ouroboros.Consensus.Peras.Params (PerasCertMaxRounds (..), PerasParams (..))
import Ouroboros.Consensus.Storage.PerasCertDB.API (PerasCertSnapshot)
import qualified Ouroboros.Consensus.Storage.PerasCertDB.API as PerasCertDB
import Ouroboros.Consensus.Util.Condense (Condense (..))
import Ouroboros.Consensus.Util.Pred
  ( Evidence (..)
  , Explainable (..)
  , ExplanationMode (..)
  , Pred (..)
  , evalPred
  )

{-------------------------------------------------------------------------------
  Certificate inclusion view
-------------------------------------------------------------------------------}

-- | View of the latest certificate seen by the voter
data LatestCertSeenView cert
  = LatestCertSeenView
  { lcsCert :: !cert
  -- ^ Latest certificate seen by the voter
  , lcsCertRound :: !PerasRoundNo
  -- ^ Round number of the latest certificate seen by the voter
  }
  deriving Show

-- | View of the latest certificate present in our preferred chain
data LatestCertOnChainView cert
  = LatestCertOnChainView
  { lcocRoundNo :: !PerasRoundNo
  -- ^ Round number of the latest certificate present in our preferred chain
  }
  deriving Show

-- | Interface needed to evaluate the Peras cert inclusion rules
data PerasCertInclusionView cert blk = PerasCertInclusionView
  { perasParams :: !PerasParams
  -- ^ Peras protocol parameters
  , currRoundNo :: !PerasRoundNo
  -- ^ The current Peras round number
  , latestCertSeen :: !(LatestCertSeenView cert)
  -- ^ The latest certificate seen by the voter
  , latestCertOnChain :: !(WithOrigin (LatestCertOnChainView cert))
  -- ^ The most recent certificate present in our preferred chain
  , certSnapshot :: !(PerasCertSnapshot blk)
  -- ^ A snapshot of the certificates we have in our database
  }
  deriving Show

-- | Construct a 'PerasCertInclusionView' from the given inputs.
--
-- Returns 'Nothing' if we are trying to construct the view without having a
-- latest certificate seen, which is a precondition to being able to include it
-- in the block we are building.
--
-- NOTE: this assumes that the client code computes all the needed inputs
-- within the same STM transaction, or the results may be inconsistent.
mkPerasCertInclusionView ::
  forall cert blk.
  HasPerasCertRound cert =>
  -- | Peras protocol parameters
  PerasParams ->
  -- | Current Peras round number
  PerasRoundNo ->
  -- | Most recent certificate seen by the voter
  WithOrigin cert ->
  -- | Round number of the latest certificate present in our preferred chain
  WithOrigin PerasRoundNo ->
  -- | Snapshot of the certificates we have in our database
  PerasCertSnapshot blk ->
  -- | Constructed certificate inclusion view
  Maybe (PerasCertInclusionView cert blk)
mkPerasCertInclusionView
  perasParams
  currRoundNo
  latestCertSeen
  latestCertOnChain
  certSnapshopt = do
    latestCertSeenView <- withOriginToMaybe (mkLatestCertSeenView latestCertSeen)
    latestCertOnChainView <- traverse mkLatestCertOnChainView latestCertOnChain
    pure $
      PerasCertInclusionView
        { perasParams = perasParams
        , currRoundNo = currRoundNo
        , latestCertSeen = latestCertSeenView
        , latestCertOnChain = latestCertOnChainView
        , certSnapshot = certSnapshopt
        }
   where
    mkLatestCertSeenView = fmap $ \cert ->
      LatestCertSeenView
        { lcsCert = cert
        , lcsCertRound = getPerasCertRound cert
        }

    mkLatestCertOnChainView roundNo =
      Just $
        LatestCertOnChainView
          { lcocRoundNo = roundNo
          }

{-------------------------------------------------------------------------------
  Certificate inclusion rules
-------------------------------------------------------------------------------}

-- | Whether we are expected to add a certificate to the block we are building
-- according to the inclusion rules.
--
-- These rules are taken as verbatim as possible from the Peras CIP-0140:
-- https://github.com/cardano-foundation/CIPs/blob/master/CIP-0140/README.md#block-creation
--
-- This type additionally carries the evidence for the decision taken.
data PerasCertInclusionRulesDecision cert
  = IncludeCert (Evidence True PerasCertInclusionRule) cert
  | DoNotIncludeCert (Evidence False PerasCertInclusionRule)
  deriving Show

instance
  HasPerasCertRound cert =>
  Explainable (PerasCertInclusionRulesDecision cert)
  where
  explain mode = \case
    IncludeCert (ETrue e) cert ->
      "IncludeCert(" <> show (getPerasCertRound cert) <> "," <> explain mode e <> ")"
    DoNotIncludeCert (EFalse e) ->
      "DoNotIncludeCert(" <> explain mode e <> ")"

-- | Evaluate whether we need to include a certificate in the block we are building.
needCert ::
  PerasCertInclusionView cert blk ->
  PerasCertInclusionRulesDecision cert
needCert pciv =
  evalPred (needCertRules pciv) $ \e ->
    case e of
      ETrue{} -> IncludeCert e (lcsCert (latestCertSeen pciv))
      EFalse{} -> DoNotIncludeCert e

-- | Certificate inclusion rules.
--
-- Each constructor corresponds to one of the members in the conjunction defined
-- in 'needCert' per CIP-0140.
data PerasCertInclusionRule
  = NoCertsFromTwoRoundsAgo
      -- | The current round number
      PerasRoundNo
  | LatestCertSeenIsNotExpired
      -- | The round number of the latest certificate seen by the voter
      PerasRoundNo
  | LatestCertSeenIsNewerThanLatestCertOnChain
      -- | The round number of the latest certificate seen by the voter
      PerasRoundNo
      -- | The round number of the latest certificate present in our preferred
      -- chain, if it exists
      (WithOrigin PerasRoundNo)
  deriving (Show, Eq)

instance Explainable PerasCertInclusionRule where
  explain Shallow = \case
    NoCertsFromTwoRoundsAgo currRoundNo
      | currRoundNo < 2 ->
          "NoCertsFromTwoRoundsAgo(N/A)"
      | otherwise ->
          "NoCertsFromTwoRoundsAgo("
            <> show (currRoundNo - 2)
            <> ")"
    LatestCertSeenIsNotExpired latestCertSeenRound ->
      "LatestCertSeenIsNotExpired("
        <> show latestCertSeenRound
        <> ")"
    LatestCertSeenIsNewerThanLatestCertOnChain latestCertSeenRound Origin ->
      "LatestCertSeenIsNewerThanLatestCertOnChain("
        <> condense latestCertSeenRound
        <> ", N/A)"
    LatestCertSeenIsNewerThanLatestCertOnChain
      latestCertSeenRound
      (NotOrigin latestCertOnChainRound) ->
        "LatestCertSeenIsNewerThanLatestCertOnChain( "
          <> condense latestCertSeenRound
          <> ", "
          <> condense latestCertOnChainRound
          <> ")"
  explain Deep = \case
    NoCertsFromTwoRoundsAgo currRoundNo
      | currRoundNo < 2 ->
          "we are in round "
            <> condense currRoundNo
            <> ", so we cannot have seen a certificate from two rounds ago"
      | otherwise ->
          "we haven't seen a certificate for round "
            <> condense (currRoundNo - 2)
            <> ", which is two rounds ago"
    LatestCertSeenIsNotExpired latestCertSeenRound ->
      "the latest certificate seen (from round "
        <> condense latestCertSeenRound
        <> ") has not expired yet"
    LatestCertSeenIsNewerThanLatestCertOnChain latestCertSeenRound Origin ->
      "there is no latest certificate on chain, "
        <> "so the latest certificate seen (from round "
        <> condense latestCertSeenRound
        <> ") is necessarily newer"
    LatestCertSeenIsNewerThanLatestCertOnChain
      latestCertSeenRound
      (NotOrigin latestCertOnChainRound) ->
        "the latest certificate seen (from round "
          <> condense latestCertSeenRound
          <> ") is newer than the latest certificate on chain"
          <> " (from round "
          <> condense latestCertOnChainRound
          <> ")"

-- | noCertsFromTwoRoundsAgo: we haven't seen a certificate from two rounds ago
noCertsFromTwoRoundsAgo ::
  PerasCertInclusionView cert blk ->
  Pred PerasCertInclusionRule
noCertsFromTwoRoundsAgo
  PerasCertInclusionView
    { currRoundNo
    , certSnapshot
    }
    -- We cannot have possibly seen a certificate from two rounds ago if we are
    -- in round 0 or 1. In that case, this is vacuously false.
    | currRoundNo < 2 =
        NoCertsFromTwoRoundsAgo currRoundNo
          := Bool False
    -- If we are in round 2 or higher, check whether our certificate snapshot
    -- contains a certificate from two rounds ago.
    | otherwise =
        NoCertsFromTwoRoundsAgo currRoundNo
          := Not (Bool containsCertFromTwoRoundsAgo)
   where
    containsCertFromTwoRoundsAgo =
      PerasCertDB.containsCert certSnapshot (currRoundNo - 2)

-- | latestCertSeenIsNotExpired: the latest certificate seen has not yet expired
-- according to the current round number and the Peras protocol parameters
latestCertSeenIsNotExpired ::
  PerasCertInclusionView cert blk ->
  Pred PerasCertInclusionRule
latestCertSeenIsNotExpired
  PerasCertInclusionView
    { perasParams
    , currRoundNo
    , latestCertSeen
    } =
    LatestCertSeenIsNotExpired latestCertSeenRoundNo
      := Bool (currRoundNo <= _A + latestCertSeenRoundNo)
   where
    latestCertSeenRoundNo =
      lcsCertRound latestCertSeen

    _A =
      PerasRoundNo $
        unPerasCertMaxRounds $
          perasCertMaxRounds $
            perasParams

-- | latestCertSeenIsNewerThanLatestCertOnChain: the latest certificate seen is
-- strictly newer than the latest certificate present in our preferred chain
latestCertSeenIsNewerThanLatestCertOnChain ::
  PerasCertInclusionView cert blk ->
  Pred PerasCertInclusionRule
latestCertSeenIsNewerThanLatestCertOnChain
  PerasCertInclusionView
    { latestCertSeen
    , latestCertOnChain
    } =
    case latestCertOnChain of
      -- If there are no certificates on chain, then the latest certificate seen
      -- is newer by definition.
      Origin ->
        LatestCertSeenIsNewerThanLatestCertOnChain
          (lcsCertRound latestCertSeen)
          Origin
          := Bool True
      -- Otherwise, check that the round number of the latest certificate seen
      -- is strictly higher than the one of the latest certificate on chain.
      NotOrigin lcoc ->
        LatestCertSeenIsNewerThanLatestCertOnChain
          (lcsCertRound latestCertSeen)
          (NotOrigin (lcocRoundNo lcoc))
          := ( lcsCertRound latestCertSeen
                 :>: lcocRoundNo lcoc
             )

-- | We need to include a certificate in the block we are building if all the
-- rules in this conjunction are satisfied.
needCertRules ::
  PerasCertInclusionView cert blk ->
  Pred PerasCertInclusionRule
needCertRules pciv =
  noCertsFromTwoRoundsAgo pciv
    :/\: latestCertSeenIsNotExpired pciv
    :/\: latestCertSeenIsNewerThanLatestCertOnChain pciv
