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
  , IsPerasVote (..)
  , getPerasVoteId
  , getPerasVoteTarget
  , IsPerasCert (..)
  , ValidatedPerasCert (..)
  , ValidatedPerasVote (..)
  , ValidatedPerasVotesWithQuorum
    ( vpvqTarget
    , vpvqVotes
    , vpvqPerasParams
    )
  , votesReachQuorum
  ) where

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import Codec.Serialise (Serialise (..))
import Codec.Serialise.Decoding (decodeListLenOf)
import Codec.Serialise.Encoding (encodeListLen)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Proxy (Proxy (..))
import GHC.Generics (Generic)
import NoThunks.Class
import Ouroboros.Consensus.Block.Abstract
import Ouroboros.Consensus.BlockchainTime.WallClock.Types (WithArrivalTime (..))
import Ouroboros.Consensus.Peras.Params
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

-- * BlockSupportsPeras class

class
  ( IsPerasVote (PerasVote blk) blk
  , IsPerasCert (PerasCert blk) blk
  ) =>
  BlockSupportsPeras blk
  where
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
    , pcCertBlock :: Point blk
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
              , pcCertBlock = pvtBlock (vpvqTarget votes)
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
  encode PerasCert{pcCertRound, pcCertBlock} =
    encodeListLen 2
      <> encode pcCertRound
      <> encode pcCertBlock
  decode = do
    decodeListLenOf 2
    pcCertRound <- decode
    pcCertBlock <- decode
    pure $ PerasCert{pcCertRound, pcCertBlock}

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

-- * Validated types

data ValidatedPerasVote blk
  = ValidatedPerasVote
  { vpvVote :: !(PerasVote blk)
  , vpvVoteStake :: !PerasVoteStake
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass NoThunks

data ValidatedPerasCert blk
  = ValidatedPerasCert
  { vpcCert :: !(PerasCert blk)
  , vpcCertBoost :: !PerasWeight
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass NoThunks

-- | A collection of validated Peras votes that:
-- 1. are all for the same target, and
-- 2. have total stake above the quorum threshold for a given 'PerasCfg'.
data ValidatedPerasVotesWithQuorum blk
  = ValidatedPerasVotesWithQuorum
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

-- * Convenience projection classes

-- | Types that support being treated as Peras votes
class IsPerasVote vote blk | vote -> blk where
  getPerasVoteRound :: vote -> PerasRoundNo
  getPerasVoteBlock :: vote -> Point blk
  getPerasVoteVoterId :: vote -> PerasVoterId

-- | Extract the vote ID from a Peras vote container
getPerasVoteId :: IsPerasVote vote blk => vote -> PerasVoteId blk
getPerasVoteId vote =
  PerasVoteId
    { pviRoundNo = getPerasVoteRound vote
    , pviVoterId = getPerasVoteVoterId vote
    }

-- | Extract the vote target from a Peras vote container
getPerasVoteTarget :: IsPerasVote vote blk => vote -> PerasVoteTarget blk
getPerasVoteTarget vote =
  PerasVoteTarget
    { pvtRoundNo = getPerasVoteRound vote
    , pvtBlock = getPerasVoteBlock vote
    }

instance IsPerasVote (PerasVote blk) blk where
  getPerasVoteRound = pvVoteRound
  getPerasVoteBlock = pvVoteBlock
  getPerasVoteVoterId = pvVoteVoterId

instance IsPerasVote (ValidatedPerasVote blk) blk where
  getPerasVoteRound = getPerasVoteRound . vpvVote
  getPerasVoteBlock = getPerasVoteBlock . vpvVote
  getPerasVoteVoterId = getPerasVoteVoterId . vpvVote

instance IsPerasVote vote blk => IsPerasVote (WithArrivalTime vote) blk where
  getPerasVoteRound = getPerasVoteRound . forgetArrivalTime
  getPerasVoteBlock = getPerasVoteBlock . forgetArrivalTime
  getPerasVoteVoterId = getPerasVoteVoterId . forgetArrivalTime

-- | Types that support being treated as Peras certificates
class IsPerasCert cert blk | cert -> blk where
  getPerasCertRound :: cert -> PerasRoundNo
  getPerasCertBlock :: cert -> Point blk

instance IsPerasCert (PerasCert blk) blk where
  getPerasCertRound = pcCertRound
  getPerasCertBlock = pcCertBlock

instance IsPerasCert (ValidatedPerasCert blk) blk where
  getPerasCertRound = getPerasCertRound . vpcCert
  getPerasCertBlock = getPerasCertBlock . vpcCert

instance IsPerasCert cert blk => IsPerasCert (WithArrivalTime cert) blk where
  getPerasCertRound = getPerasCertRound . forgetArrivalTime
  getPerasCertBlock = getPerasCertBlock . forgetArrivalTime
