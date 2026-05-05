{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Block.SupportsPeras
  ( BlockSupportsPeras (..)
  , PerasCert (..)
  , PerasVote (..)
  , ValidatedPerasCert (..)
  , ValidatedPerasVote (..)
  , ValidatedPerasVotesWithQuorum
    ( vpvqTarget
    , vpvqVotes
    , vpvqPerasParams
    )
  , votesReachQuorum
  , HasPerasCertRound (..)
  , HasPerasCertBoostedBlock (..)
  , HasPerasCertBoost (..)
  , HasPerasVoteRound (..)
  , HasPerasVoteBlock (..)
  , HasPerasVoteVoterId (..)
  , HasPerasVoteStake (..)
  , HasPerasVoteTarget (..)
  , HasPerasVoteId (..)
  ) where

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import Codec.Serialise (Serialise (..))
import Codec.Serialise.Decoding (decodeListLenOf)
import Codec.Serialise.Encoding (encodeListLen)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Proxy (Proxy (..))
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block.Abstract (HeaderHash, Point, StandardHash)
import Ouroboros.Consensus.BlockchainTime.WallClock.Types (WithArrivalTime (..))
import Ouroboros.Consensus.Peras.Params (PerasParams (..), PerasWeight)
import Ouroboros.Consensus.Peras.Types
  ( PerasRoundNo
  , PerasVoteId (..)
  , PerasVoteStake
  , PerasVoteStakeDistr (..)
  , PerasVoteTarget (..)
  , PerasVoterId (..)
  , lookupPerasVoteStake
  , stakeAboveThreshold
  )
import Ouroboros.Consensus.Util (ShowProxy (..))

-- * Validated types

data ValidatedPerasCert blk = ValidatedPerasCert
  { vpcCert :: !(PerasCert blk)
  , vpcCertBoost :: !PerasWeight
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass NoThunks

data ValidatedPerasVote blk = ValidatedPerasVote
  { vpvVote :: !(PerasVote blk)
  , vpvVoteStake :: !PerasVoteStake
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass NoThunks

-- | A collection of validated Peras votes that:
-- 1. are all for the same target, and
-- 2. have total stake above the quorum threshold for a given 'PerasCfg'.
data ValidatedPerasVotesWithQuorum blk = ValidatedPerasVotesWithQuorum
  { vpvqTarget :: !(PerasVoteTarget blk)
  -- ^ The target that all the votes are for
  , vpvqVotes :: !(NonEmpty (ValidatedPerasVote blk))
  -- ^ The votes that reached quorum for the given target
  , vpvqPerasParams :: !PerasParams
  -- ^ The Peras parameters used to validate that the votes reach quorum
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NoThunks

-- | Smart constructor for 'ValidatedPerasVotesReachingQuorum'.
--
-- This function checks that all votes are for the same target, and that their
-- total stake is above the quorum threshold defined in the given 'PerasCfg'.
-- It returns 'Nothing' if either of these conditions is not met.
votesReachQuorum ::
  StandardHash blk =>
  PerasParams ->
  [ValidatedPerasVote blk] ->
  Maybe (ValidatedPerasVotesWithQuorum blk)
votesReachQuorum params votes =
  case votes of
    -- We need at least one vote to determine who these votes are for, so we
    -- can't vacuously reach a quorum, even if the quorum threshold is 0.
    [] -> Nothing
    -- If we have at least one vote, we must check that all votes are for the
    -- same target, and that their total stake of is above the quorum threshold.
    (v0 : vs)
      | not (allVotesMatchTarget v0 vs) ->
          Nothing
      | not votesHaveEnoughStake ->
          Nothing
      | otherwise ->
          Just
            ValidatedPerasVotesWithQuorum
              { vpvqTarget = getPerasVoteTarget v0
              , vpvqVotes = v0 :| vs
              , vpvqPerasParams = params
              }
 where
  totalVoteStake =
    mconcat (vpvVoteStake <$> votes)
  votesHaveEnoughStake =
    stakeAboveThreshold params totalVoteStake
  allVotesMatchTarget target =
    all ((== (getPerasVoteTarget target)) . getPerasVoteTarget)

-- * BlockSupportsPeras class

class BlockSupportsPeras blk where
  data PerasCert blk

  data PerasVote blk

  data PerasValidationErr blk

  data PerasForgeErr blk

  validatePerasCert ::
    PerasParams ->
    PerasCert blk ->
    Either (PerasValidationErr blk) (ValidatedPerasCert blk)

  validatePerasVote ::
    PerasParams ->
    PerasVoteStakeDistr ->
    PerasVote blk ->
    Either (PerasValidationErr blk) (ValidatedPerasVote blk)

  forgePerasCert ::
    PerasParams ->
    ValidatedPerasVotesWithQuorum blk ->
    Either (PerasForgeErr blk) (ValidatedPerasCert blk)

  -- | Extract a Peras certificate optionally stored in a block.
  --
  -- Returns 'Nothing' if the block does not contain a Peras certificate, or
  -- if the block is from an era that does not support Peras certificates.
  getPerasCertInBlock ::
    blk ->
    Maybe (PerasCert blk)

-- TODO: degenerate instance for all blks to get things to compile
-- see https://github.com/tweag/cardano-peras/issues/73
instance BlockSupportsPeras blk where
  data PerasCert blk = PerasCert
    { pcCertRound :: PerasRoundNo
    , pcCertBoostedBlock :: Point blk
    }
    deriving stock (Generic, Eq, Ord, Show)
    deriving anyclass NoThunks

  data PerasVote blk = PerasVote
    { pvVoteRound :: PerasRoundNo
    , pvVoteBlock :: Point blk
    , pvVoteVoterId :: PerasVoterId
    }
    deriving stock (Generic, Eq, Ord, Show)
    deriving anyclass NoThunks

  -- TODO: enrich with actual error types
  -- see https://github.com/tweag/cardano-peras/issues/120
  data PerasValidationErr blk
    = PerasValidationErr
    deriving stock (Show, Eq)

  -- TODO: enrich with actual error types
  -- see https://github.com/tweag/cardano-peras/issues/120
  data PerasForgeErr blk
    = PerasForgeErr
    deriving stock (Show, Eq)

  -- TODO: perform actual validation against all
  -- possible 'PerasValidationErr' variants
  -- see https://github.com/tweag/cardano-peras/issues/120
  validatePerasCert params cert =
    Right
      ValidatedPerasCert
        { vpcCert = cert
        , vpcCertBoost = perasWeight params
        }

  -- TODO: perform actual validation against all
  -- possible 'PerasValidationErr' variants
  -- see https://github.com/tweag/cardano-peras/issues/120
  validatePerasVote _params stakeDistr vote
    | Just stake <- lookupPerasVoteStake (getPerasVoteVoterId vote) stakeDistr =
        Right
          ValidatedPerasVote
            { vpvVote = vote
            , vpvVoteStake = stake
            }
    | otherwise =
        Left PerasValidationErr

  -- TODO: perform actual validation against all
  -- possible 'PerasForgeErr' variants
  -- see https://github.com/tweag/cardano-peras/issues/120
  forgePerasCert params votes =
    return $
      ValidatedPerasCert
        { vpcCert =
            PerasCert
              { pcCertRound = pvtRoundNo (vpvqTarget votes)
              , pcCertBoostedBlock = pvtBlock (vpvqTarget votes)
              }
        , vpcCertBoost = perasWeight params
        }

  -- TODO: extract actual Peras certificates from blocks when the HFC plumbing
  -- is in place.
  getPerasCertInBlock _ = Nothing

instance ShowProxy blk => ShowProxy (PerasCert blk) where
  showProxy _ = "PerasCert " <> showProxy (Proxy @blk)

instance ShowProxy blk => ShowProxy (PerasVote blk) where
  showProxy _ = "PerasVote " <> showProxy (Proxy @blk)

instance Serialise (HeaderHash blk) => Serialise (PerasCert blk) where
  encode PerasCert{pcCertRound, pcCertBoostedBlock} =
    encodeListLen 2
      <> encode pcCertRound
      <> encode pcCertBoostedBlock
  decode = do
    decodeListLenOf 2
    pcCertRound <- decode
    pcCertBoostedBlock <- decode
    pure $ PerasCert{pcCertRound, pcCertBoostedBlock}

instance Serialise (HeaderHash blk) => Serialise (PerasVote blk) where
  encode PerasVote{pvVoteRound, pvVoteBlock, pvVoteVoterId} =
    encodeListLen 3
      <> encode pvVoteRound
      <> encode pvVoteBlock
      <> toCBOR (unPerasVoterId pvVoteVoterId)
  decode = do
    decodeListLenOf 3
    pvVoteRound <- decode
    pvVoteBlock <- decode
    pvVoteVoterId <- PerasVoterId <$> fromCBOR
    pure $ PerasVote{pvVoteRound, pvVoteBlock, pvVoteVoterId}

-- | Extract the certificate round from a Peras certificate container
class HasPerasCertRound cert where
  getPerasCertRound :: cert -> PerasRoundNo

instance HasPerasCertRound (PerasCert blk) where
  getPerasCertRound = pcCertRound

instance HasPerasCertRound (ValidatedPerasCert blk) where
  getPerasCertRound = getPerasCertRound . vpcCert

instance
  HasPerasCertRound cert =>
  HasPerasCertRound (WithArrivalTime cert)
  where
  getPerasCertRound = getPerasCertRound . forgetArrivalTime

-- | Extract the boosted block point from a Peras certificate container
class HasPerasCertBoostedBlock cert blk | cert -> blk where
  getPerasCertBoostedBlock :: cert -> Point blk

instance HasPerasCertBoostedBlock (PerasCert blk) blk where
  getPerasCertBoostedBlock = pcCertBoostedBlock

instance HasPerasCertBoostedBlock (ValidatedPerasCert blk) blk where
  getPerasCertBoostedBlock = getPerasCertBoostedBlock . vpcCert

instance
  HasPerasCertBoostedBlock cert blk =>
  HasPerasCertBoostedBlock (WithArrivalTime cert) blk
  where
  getPerasCertBoostedBlock = getPerasCertBoostedBlock . forgetArrivalTime

-- | Extract the certificate boost from a Peras certificate container
class HasPerasCertBoost cert where
  getPerasCertBoost :: cert -> PerasWeight

instance HasPerasCertBoost (ValidatedPerasCert blk) where
  getPerasCertBoost = vpcCertBoost

instance
  HasPerasCertBoost cert =>
  HasPerasCertBoost (WithArrivalTime cert)
  where
  getPerasCertBoost = getPerasCertBoost . forgetArrivalTime

-- | Extract the vote round from a Peras vote container
class HasPerasVoteRound vote where
  getPerasVoteRound :: vote -> PerasRoundNo

instance HasPerasVoteRound (PerasVote blk) where
  getPerasVoteRound = pvVoteRound

instance HasPerasVoteRound (ValidatedPerasVote blk) where
  getPerasVoteRound = getPerasVoteRound . vpvVote

instance
  HasPerasVoteRound vote =>
  HasPerasVoteRound (WithArrivalTime vote)
  where
  getPerasVoteRound = getPerasVoteRound . forgetArrivalTime

-- | Extract the vote block point from a Peras vote container
class HasPerasVoteBlock vote blk | vote -> blk where
  getPerasVoteBlock :: vote -> Point blk

instance HasPerasVoteBlock (PerasVote blk) blk where
  getPerasVoteBlock = pvVoteBlock

instance HasPerasVoteBlock (ValidatedPerasVote blk) blk where
  getPerasVoteBlock = getPerasVoteBlock . vpvVote

instance
  HasPerasVoteBlock vote blk =>
  HasPerasVoteBlock (WithArrivalTime vote) blk
  where
  getPerasVoteBlock = getPerasVoteBlock . forgetArrivalTime

-- | Extract the stake pool ID from a Peras vote container
class HasPerasVoteVoterId vote where
  getPerasVoteVoterId :: vote -> PerasVoterId

instance HasPerasVoteVoterId (PerasVote blk) where
  getPerasVoteVoterId = pvVoteVoterId

instance HasPerasVoteVoterId (ValidatedPerasVote blk) where
  getPerasVoteVoterId = getPerasVoteVoterId . vpvVote

instance
  HasPerasVoteVoterId vote =>
  HasPerasVoteVoterId (WithArrivalTime vote)
  where
  getPerasVoteVoterId = getPerasVoteVoterId . forgetArrivalTime

-- | Extract the vote stake from a validated Peras vote container
class HasPerasVoteStake vote where
  getPerasVoteStake :: vote -> PerasVoteStake

instance HasPerasVoteStake (ValidatedPerasVote blk) where
  getPerasVoteStake = vpvVoteStake

instance
  HasPerasVoteStake vote =>
  HasPerasVoteStake (WithArrivalTime vote)
  where
  getPerasVoteStake = getPerasVoteStake . forgetArrivalTime

-- | Extract the vote target from a Peras vote container
class HasPerasVoteTarget vote blk | vote -> blk where
  getPerasVoteTarget :: vote -> PerasVoteTarget blk

instance HasPerasVoteTarget (PerasVote blk) blk where
  getPerasVoteTarget vote =
    PerasVoteTarget
      { pvtRoundNo = pvVoteRound vote
      , pvtBlock = pvVoteBlock vote
      }

instance HasPerasVoteTarget (ValidatedPerasVote blk) blk where
  getPerasVoteTarget = getPerasVoteTarget . vpvVote

instance
  HasPerasVoteTarget vote blk =>
  HasPerasVoteTarget (WithArrivalTime vote) blk
  where
  getPerasVoteTarget = getPerasVoteTarget . forgetArrivalTime

-- | Extract the vote ID from a Peras vote container
class HasPerasVoteId vote blk | vote -> blk where
  getPerasVoteId :: vote -> PerasVoteId blk

instance HasPerasVoteId (PerasVote blk) blk where
  getPerasVoteId vote =
    PerasVoteId
      { pviRoundNo = pvVoteRound vote
      , pviVoterId = pvVoteVoterId vote
      }

instance HasPerasVoteId (ValidatedPerasVote blk) blk where
  getPerasVoteId = getPerasVoteId . vpvVote

instance
  HasPerasVoteId vote blk =>
  HasPerasVoteId (WithArrivalTime vote) blk
  where
  getPerasVoteId = getPerasVoteId . forgetArrivalTime
