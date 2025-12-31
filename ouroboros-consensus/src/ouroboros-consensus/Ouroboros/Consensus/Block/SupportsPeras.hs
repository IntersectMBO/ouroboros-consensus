{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Block.SupportsPeras
  ( PerasRoundNo (..)
  , onPerasRoundNo
  , PerasVoteId (..)
  , PerasVoteTarget (..)
  , PerasVoterId (..)
  , PerasVoteStake (..)
  , stakeAboveThreshold
  , PerasVoteStakeDistr (..)
  , lookupPerasVoteStake
  , BlockSupportsPeras (..)
  , PerasCert (..)
  , PerasVote (..)
  , ValidatedPerasCert (..)
  , ValidatedPerasVote (..)
  , HasPerasCertRound (..)
  , HasPerasCertBoostedBlock (..)
  , HasPerasCertBoost (..)
  , HasPerasVoteRound (..)
  , HasPerasVoteBlock (..)
  , HasPerasVoteVoterId (..)
  , HasPerasVoteStake (..)
  , HasPerasVoteTarget (..)
  , HasPerasVoteId (..)

    -- * Convenience re-exports
  , module Ouroboros.Consensus.Peras.Params
  ) where

import qualified Cardano.Binary as KeyHash
import Cardano.Ledger.Hashes (KeyHash, KeyRole (..))
import Codec.Serialise (Serialise (..))
import Codec.Serialise.Decoding (decodeListLenOf)
import Codec.Serialise.Encoding (encodeListLen)
import Data.Coerce (coerce)
import qualified Data.Map as Map
import Data.Map.Strict (Map)
import Data.Monoid (Sum (..))
import Data.Proxy (Proxy (..))
import Data.Word (Word64)
import GHC.Generics (Generic)
import NoThunks.Class
import Ouroboros.Consensus.Block.Abstract
import Ouroboros.Consensus.BlockchainTime.WallClock.Types (WithArrivalTime (..))
import Ouroboros.Consensus.Peras.Params
import Ouroboros.Consensus.Util
import Ouroboros.Consensus.Util.Condense
import Quiet (Quiet (..))

{-------------------------------------------------------------------------------
-- * Peras types
-------------------------------------------------------------------------------}

-- ** Round numbers

newtype PerasRoundNo = PerasRoundNo {unPerasRoundNo :: Word64}
  deriving Show via Quiet PerasRoundNo
  deriving stock Generic
  deriving newtype (Enum, Eq, Ord, Num, Bounded, NoThunks, Serialise)

instance Condense PerasRoundNo where
  condense = show . unPerasRoundNo

instance ShowProxy PerasRoundNo where
  showProxy _ = "PerasRoundNo"

-- | Lift a binary operation on 'Word64' to 'PerasRoundNo'
onPerasRoundNo ::
  (Word64 -> Word64 -> Word64) ->
  (PerasRoundNo -> PerasRoundNo -> PerasRoundNo)
onPerasRoundNo = coerce

-- ** Stake pool distributions

newtype PerasVoterId = PerasVoterId
  { unPerasVoterId :: KeyHash StakePool
  }
  deriving newtype NoThunks
  deriving stock (Eq, Ord, Generic)
  deriving Show via Quiet PerasVoterId

-- NOTE: At the moment there is no consensus from researchers/engineers on how
-- we go from the absolute stake of a voter in the ledger to the relative stake
-- of their vote in the voting commitee (given that the quorum is expressed as
-- a relative value of the voting commitee total stake).
--
-- So, for now you can consider this 'Rational' as the best approximation we
-- have at the moment of the concrete type for a relative vote stake that can be
-- compared to the quorum threshold value (also currently a 'Rational').
newtype PerasVoteStake = PerasVoteStake
  { unPerasVoteStake :: Rational
  }
  deriving newtype (Eq, Ord, Num, Fractional, NoThunks, Serialise)
  deriving stock Generic
  deriving Show via Quiet PerasVoteStake
  deriving Semigroup via Sum Rational
  deriving Monoid via Sum Rational

-- | Check whether a given vote stake is above the quorum threshold
stakeAboveThreshold :: PerasParams -> PerasVoteStake -> Bool
stakeAboveThreshold params stake =
  unPerasVoteStake stake
    >= unPerasQuorumStakeThreshold (perasQuorumStakeThreshold params)

newtype PerasVoteStakeDistr = PerasVoteStakeDistr
  { unPerasVoteStakeDistr :: Map PerasVoterId PerasVoteStake
  }
  deriving newtype NoThunks
  deriving stock (Show, Eq, Generic)

data PerasVoteTarget blk = PerasVoteTarget
  { pvtRoundNo :: !PerasRoundNo
  , pvtBlock :: !(Point blk)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass NoThunks

data PerasVoteId blk = PerasVoteId
  { pviRoundNo :: !PerasRoundNo
  , pviVoterId :: !PerasVoterId
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass NoThunks

-- | Lookup the stake of a vote cast by a member of a given stake distribution.
lookupPerasVoteStake ::
  PerasVote blk ->
  PerasVoteStakeDistr ->
  Maybe PerasVoteStake
lookupPerasVoteStake vote distr =
  Map.lookup
    (pvVoteVoterId vote)
    (unPerasVoteStakeDistr distr)

-- ** Validated types

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

{-------------------------------------------------------------------------------
-- * BlockSupportsPeras class
-------------------------------------------------------------------------------}

class
  ( Show (PerasCfg blk)
  , NoThunks (PerasCert blk)
  ) =>
  BlockSupportsPeras blk
  where
  type PerasCfg blk

  data PerasCert blk

  data PerasVote blk

  data PerasValidationErr blk

  data PerasForgeErr blk

  validatePerasCert ::
    PerasCfg blk ->
    PerasCert blk ->
    Either (PerasValidationErr blk) (ValidatedPerasCert blk)

  validatePerasVote ::
    PerasCfg blk ->
    PerasVoteStakeDistr ->
    PerasVote blk ->
    Either (PerasValidationErr blk) (ValidatedPerasVote blk)

  forgePerasCert ::
    PerasCfg blk ->
    PerasVoteTarget blk ->
    [ValidatedPerasVote blk] ->
    Either (PerasForgeErr blk) (ValidatedPerasCert blk)

-- TODO: degenerate instance for all blks to get things to compile
-- see https://github.com/tweag/cardano-peras/issues/73
instance StandardHash blk => BlockSupportsPeras blk where
  type PerasCfg blk = PerasParams

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
    = PerasForgeErrInsufficientVotes
    | PerasForgeErrTargetMismatch
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
    | Just stake <- lookupPerasVoteStake vote stakeDistr =
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
  forgePerasCert params target votes
    | not allVotersMatchTarget =
        Left PerasForgeErrTargetMismatch
    | not votesHaveEnoughStake =
        Left PerasForgeErrInsufficientVotes
    | otherwise =
        return $
          ValidatedPerasCert
            { vpcCert =
                PerasCert
                  { pcCertRound = pvtRoundNo target
                  , pcCertBoostedBlock = pvtBlock target
                  }
            , vpcCertBoost = perasWeight params
            }
   where
    totalVotesStake =
      mconcat (vpvVoteStake <$> votes)

    votesHaveEnoughStake =
      stakeAboveThreshold params totalVotesStake

    allVotersMatchTarget =
      all ((target ==) . getPerasVoteTarget) votes

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
      <> KeyHash.toCBOR (unPerasVoterId pvVoteVoterId)
  decode = do
    decodeListLenOf 3
    pvVoteRound <- decode
    pvVoteBlock <- decode
    pvVoteVoterId <- PerasVoterId <$> KeyHash.fromCBOR
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
