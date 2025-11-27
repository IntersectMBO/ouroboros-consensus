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
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Ouroboros.Consensus.Block.SupportsPeras
  ( PerasRoundNo (..)
  , onPerasRoundNo
  , PerasVoteStake (..)
  , PerasWeight (..)
  , BlockSupportsPeras (..)
  , PerasCert (..)
  , PerasVote (..)
  , PerasVoteTarget
  , PerasCfg (..)
  , ValidatedPerasCert (..)
  , ValidatedPerasVote (..)
  , PerasVoteAggregate (..)
  , PerasVoteAggregateStatus (..)
  , pvasMaybeCert
  , emptyPerasVoteAggregateStatus
  , updatePerasVoteAggregate
  , updatePerasVoteAggregateStatus
  , makePerasCfg
  , HasId (..)
  , HasPerasCertRound (..)
  , HasPerasCertBoostedBlock (..)
  , HasPerasCertBoost (..)
  , HasPerasVoteRound (..)
  , HasPerasVoteVotedBlock (..)
  , HasPerasVoteVoterId (..)
  , HasPerasVoteTarget (..)

    -- * Ouroboros Peras round length
  , PerasRoundLength (..)
  , defaultPerasRoundLength
  ) where

import qualified Cardano.Binary as KeyHash
import Cardano.Ledger.Core (KeyHash, KeyRole (StakePool))
import Codec.Serialise (Serialise (..))
import Codec.Serialise.Decoding (decodeListLenOf)
import Codec.Serialise.Encoding (encodeListLen)
import Control.Applicative ((<|>))
import Data.Coerce (coerce)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import Data.Monoid (Sum (..))
import Data.Proxy (Proxy (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word (Word64)
import GHC.Generics (Generic)
import NoThunks.Class
import Ouroboros.Consensus.Block.Abstract
import Ouroboros.Consensus.BlockchainTime.WallClock.Types (WithArrivalTime (..))
import Ouroboros.Consensus.Util
import Ouroboros.Consensus.Util.Condense
import Quiet (Quiet (..))

class
  ( Ord (IdOf a)
  , Eq (IdOf a)
  , Show (IdOf a)
  , NoThunks (IdOf a)
  , Serialise (IdOf a)
  ) =>
  HasId a
  where
  type IdOf a
  getId :: a -> IdOf a

instance HasId perasObj => HasId (WithArrivalTime perasObj) where
  type IdOf (WithArrivalTime perasObj) = IdOf perasObj
  getId = getId . forgetArrivalTime

newtype PerasRoundNo = PerasRoundNo {unPerasRoundNo :: Word64}
  deriving Show via Quiet PerasRoundNo
  deriving stock Generic
  deriving newtype (Enum, Eq, Ord, Num, Bounded, NoThunks, Serialise)

newtype PerasVoteStake = PerasVoteStake {unPerasVoteStake :: Rational}
  deriving Show via Quiet PerasVoteStake
  deriving stock Generic
  deriving newtype (Enum, Eq, Ord, Num, Fractional, NoThunks, Serialise)
  deriving Semigroup via Sum Rational
  deriving Monoid via Sum Rational

data PerasVoteStakeDistr
getPerasVoteStakeOf :: PerasVoteStakeDistr -> VoterId -> PerasVoteStake
getPerasVoteStakeOf = undefined

-- | TODO: what is the proper underlying type?
type VoterId = KeyHash 'StakePool

instance Condense PerasRoundNo where
  condense = show . unPerasRoundNo

instance ShowProxy PerasRoundNo where
  showProxy _ = "PerasRoundNo"

-- | Lift a binary operation on 'Word64' to 'PerasRoundNo'
onPerasRoundNo ::
  (Word64 -> Word64 -> Word64) ->
  (PerasRoundNo -> PerasRoundNo -> PerasRoundNo)
onPerasRoundNo = coerce

newtype PerasWeight = PerasWeight {unPerasWeight :: Word64}
  deriving Show via Quiet PerasWeight
  deriving stock Generic
  deriving newtype (Eq, Ord, NoThunks)
  deriving (Semigroup, Monoid) via Sum Word64

instance Condense PerasWeight where
  condense = show . unPerasWeight

-- | TODO: this will become a Ledger protocol parameter
-- see https://github.com/tweag/cardano-peras/issues/119
boostPerCert :: PerasWeight
boostPerCert = PerasWeight 15

-- TODO using 'Validated' for extra safety? Or some @.Unsafe@ module?
data ValidatedPerasCert blk = ValidatedPerasCert
  { vpcCert :: !(PerasCert blk)
  , vpcCertBoost :: !PerasWeight
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass NoThunks

instance
  ( HasId (PerasCert blk)
  , Ord (IdOf (PerasCert blk))
  , Eq (IdOf (PerasCert blk))
  , Show (IdOf (PerasCert blk))
  , NoThunks (IdOf (PerasCert blk))
  , Serialise (IdOf (PerasCert blk))
  ) =>
  HasId (ValidatedPerasCert blk)
  where
  type IdOf (ValidatedPerasCert blk) = IdOf (PerasCert blk)
  getId = getId . vpcCert

data ValidatedPerasVote blk = ValidatedPerasVote
  { vpvVote :: !(PerasVote blk)
  , vpvVoteStake :: !PerasVoteStake
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass NoThunks

instance
  ( HasId (PerasVote blk)
  , Ord (IdOf (PerasVote blk))
  , Eq (IdOf (PerasVote blk))
  , Show (IdOf (PerasVote blk))
  , NoThunks (IdOf (PerasVote blk))
  , Serialise (IdOf (PerasVote blk))
  ) =>
  HasId (ValidatedPerasVote blk)
  where
  type IdOf (ValidatedPerasVote blk) = IdOf (PerasVote blk)
  getId = getId . vpvVote

{-------------------------------------------------------------------------------
  Ouroboros Peras round length
-------------------------------------------------------------------------------}

newtype PerasRoundLength = PerasRoundLength {unPerasRoundLength :: Word64}
  deriving stock (Show, Eq, Ord)
  deriving newtype (NoThunks, Num)

-- | See the Protocol parameters section of the Peras design report:
--   https://tweag.github.io/cardano-peras/peras-design.pdf#section.2.1
-- TODO: this will become a Ledger protocol parameter
-- see https://github.com/tweag/cardano-peras/issues/119
defaultPerasRoundLength :: PerasRoundLength
defaultPerasRoundLength = 90

class
  ( Show (PerasCfg blk)
  , NoThunks (PerasCert blk)
  ) =>
  BlockSupportsPeras blk
  where
  data PerasCfg blk

  data PerasCert blk

  data PerasVote blk

  data PerasValidationErr blk

  validatePerasCert ::
    PerasCfg blk ->
    PerasCert blk ->
    Either (PerasValidationErr blk) (ValidatedPerasCert blk)

  validatePerasVote ::
    PerasCfg blk ->
    PerasVote blk ->
    PerasVoteStakeDistr ->
    Either (PerasValidationErr blk) (ValidatedPerasVote blk)

const_PERAS_QUORUM_THRESHOLD :: PerasVoteStake
const_PERAS_QUORUM_THRESHOLD = PerasVoteStake 0.75

data PerasVoteAggregate blk = PerasVoteAggregate
  { pvaTarget :: !(PerasVoteTarget blk)
  , pvaVotes :: !(Map (IdOf (PerasVote blk)) (WithArrivalTime (ValidatedPerasVote blk)))
  , pvaTotalStake :: !PerasVoteStake
  }
  deriving stock (Generic, Eq, Ord, Show)
  deriving anyclass NoThunks

data PerasVoteAggregateStatus blk
  = PerasVoteAggregateQuorumNotReached {pvasVoteAggregate :: !(PerasVoteAggregate blk)}
  | PerasVoteAggregateQuorumReachedAlready
      {pvasVoteAggregate :: !(PerasVoteAggregate blk), pvasCert :: ValidatedPerasCert blk}
  deriving stock (Generic, Eq, Ord, Show)
  deriving anyclass NoThunks

pvasMaybeCert :: PerasVoteAggregateStatus blk -> Maybe (ValidatedPerasCert blk)
pvasMaybeCert aggr = case aggr of
  PerasVoteAggregateQuorumNotReached{} -> Nothing
  PerasVoteAggregateQuorumReachedAlready{pvasCert} -> Just pvasCert

emptyPerasVoteAggregateStatus :: PerasVoteTarget blk -> PerasVoteAggregateStatus blk
emptyPerasVoteAggregateStatus target =
  PerasVoteAggregateQuorumNotReached $
    PerasVoteAggregate
      { pvaTotalStake = PerasVoteStake 0
      , pvaTarget = target
      , pvaVotes = Map.empty
      }

-- | Add a vote to an existing aggregate if it isn't already present, and update
-- the stake accordingly.
-- PRECONDITION: the vote's target must match the aggregate's target.
updatePerasVoteAggregate ::
  StandardHash blk =>
  PerasVoteAggregate blk ->
  WithArrivalTime (ValidatedPerasVote blk) ->
  PerasVoteAggregate blk
updatePerasVoteAggregate
  pva@PerasVoteAggregate
    { pvaTarget = (roundNo, point)
    , pvaVotes = existingVotes
    , pvaTotalStake = initialStake
    }
  vote =
    if not (getPerasVoteRound vote == roundNo && getPerasVoteVotedBlock vote == point)
      then error "updatePerasVoteAggregate: vote target does not match aggregate target"
      else
        let (pvaVotes', pvaTotalStake') =
              case Map.insertLookupWithKey
                (\_k old _new -> old)
                (getId vote)
                vote
                existingVotes of
                (Nothing, votes') ->
                  -- key was NOT present → inserted and stake updated
                  (votes', initialStake + vpvVoteStake (forgetArrivalTime vote))
                (Just _, _) ->
                  -- key WAS already present → votes and stake unchanged
                  (existingVotes, initialStake)
         in pva{pvaVotes = pvaVotes', pvaTotalStake = pvaTotalStake'}

updatePerasVoteAggregateStatus ::
  StandardHash blk =>
  PerasVoteAggregateStatus blk ->
  WithArrivalTime (ValidatedPerasVote blk) ->
  PerasVoteAggregateStatus blk
updatePerasVoteAggregateStatus aggr vote = case aggr of
  PerasVoteAggregateQuorumNotReached{pvasVoteAggregate} ->
    let aggr' = updatePerasVoteAggregate pvasVoteAggregate vote
     in if pvaTotalStake aggr' >= const_PERAS_QUORUM_THRESHOLD
          then
            PerasVoteAggregateQuorumReachedAlready
              { pvasVoteAggregate = aggr'
              , pvasCert =
                  ValidatedPerasCert
                    { vpcCertBoost = boostPerCert
                    , vpcCert = uncurry PerasCert (pvaTarget aggr')
                    }
              }
          else PerasVoteAggregateQuorumNotReached{pvasVoteAggregate = aggr'}
  PerasVoteAggregateQuorumReachedAlready{pvasVoteAggregate, pvasCert} ->
    PerasVoteAggregateQuorumReachedAlready
      { pvasVoteAggregate = updatePerasVoteAggregate pvasVoteAggregate vote
      , pvasCert
      }

type PerasVoteTarget blk = (PerasRoundNo, Point blk)

-- TODO: degenerate instance for all blks to get things to compile
-- see https://github.com/tweag/cardano-peras/issues/73
instance StandardHash blk => BlockSupportsPeras blk where
  newtype PerasCfg blk = PerasCfg
    { -- TODO: eventually, this will come from the
      -- protocol parameters from the ledger state
      -- see https://github.com/tweag/cardano-peras/issues/119
      perasCfgWeightBoost :: PerasWeight
    }
    deriving stock (Show, Eq)

  data PerasCert blk = PerasCert
    { pcCertRound :: PerasRoundNo
    , pcCertBoostedBlock :: Point blk
    }
    deriving stock (Generic, Eq, Ord, Show)
    deriving anyclass NoThunks

  data PerasVote blk = PerasVote
    { pvVoteRound :: PerasRoundNo
    , pvVotedBlock :: Point blk
    , pvVoteVoterId :: VoterId
    }
    deriving stock (Generic, Eq, Ord, Show)
    deriving anyclass NoThunks

  -- TODO: enrich with actual error types
  -- see https://github.com/tweag/cardano-peras/issues/120
  data PerasValidationErr blk
    = PerasValidationErr
    deriving stock (Show, Eq)

  -- TODO: perform actual validation against all
  -- possible 'PerasValidationErr' variants
  -- see https://github.com/tweag/cardano-peras/issues/120
  validatePerasCert cfg cert =
    Right
      ValidatedPerasCert
        { vpcCert = cert
        , vpcCertBoost = perasCfgWeightBoost cfg
        }

  validatePerasVote _cfg vote stakeDistr =
    let stake = getPerasVoteStakeOf stakeDistr (pvVoteVoterId vote)
     in Right (ValidatedPerasVote{vpvVote = vote, vpvVoteStake = stake})

instance HasId (PerasCert blk) where
  type IdOf (PerasCert blk) = PerasRoundNo
  getId = pcCertRound

-- TODO: Orphan instance
instance Serialise (KeyHash 'StakePool) where
  encode = KeyHash.toCBOR
  decode = KeyHash.fromCBOR

instance HasId (PerasVote blk) where
  type IdOf (PerasVote blk) = (PerasRoundNo, VoterId)
  getId vote = (pvVoteRound vote, pvVoteVoterId vote)

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
  encode PerasVote{pvVoteRound, pvVotedBlock, pvVoteVoterId} =
    encodeListLen 3
      <> encode pvVoteRound
      <> encode pvVotedBlock
      <> KeyHash.toCBOR pvVoteVoterId
  decode = do
    decodeListLenOf 3
    pvVoteRound <- decode
    pvVotedBlock <- decode
    pvVoteVoterId <- KeyHash.fromCBOR
    pure $ PerasVote{pvVoteRound, pvVotedBlock, pvVoteVoterId}

-- | Derive a 'PerasCfg' from a 'BlockConfig'
--
-- TODO: this currently doesn't depend on 'BlockConfig' at all, but likely will
-- depend on it in the future
-- see https://github.com/tweag/cardano-peras/issues/73
makePerasCfg :: Maybe (BlockConfig blk) -> PerasCfg blk
makePerasCfg _ =
  PerasCfg
    { perasCfgWeightBoost = boostPerCert
    }

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

class HasPerasVoteVotedBlock vote blk | vote -> blk where
  getPerasVoteVotedBlock :: vote -> Point blk
instance HasPerasVoteVotedBlock (PerasVote blk) blk where
  getPerasVoteVotedBlock = pvVotedBlock
instance HasPerasVoteVotedBlock (ValidatedPerasVote blk) blk where
  getPerasVoteVotedBlock = getPerasVoteVotedBlock . vpvVote
instance
  HasPerasVoteVotedBlock vote blk =>
  HasPerasVoteVotedBlock (WithArrivalTime vote) blk
  where
  getPerasVoteVotedBlock = getPerasVoteVotedBlock . forgetArrivalTime

class HasPerasVoteVoterId vote where
  getPerasVoteVoterId :: vote -> VoterId
instance HasPerasVoteVoterId (PerasVote blk) where
  getPerasVoteVoterId = pvVoteVoterId
instance HasPerasVoteVoterId (ValidatedPerasVote blk) where
  getPerasVoteVoterId = getPerasVoteVoterId . vpvVote
instance
  HasPerasVoteVoterId vote =>
  HasPerasVoteVoterId (WithArrivalTime vote)
  where
  getPerasVoteVoterId = getPerasVoteVoterId . forgetArrivalTime

class HasPerasVoteTarget vote blk | vote -> blk where
  getPerasVoteTarget :: vote -> PerasVoteTarget blk
instance HasPerasVoteTarget (PerasVote blk) blk where
  getPerasVoteTarget vote = (pvVoteRound vote, pvVotedBlock vote)
instance HasPerasVoteTarget (ValidatedPerasVote blk) blk where
  getPerasVoteTarget vote = getPerasVoteTarget (vpvVote vote)
instance
  HasPerasVoteTarget vote blk =>
  HasPerasVoteTarget (WithArrivalTime vote) blk
  where
  getPerasVoteTarget = getPerasVoteTarget . forgetArrivalTime
