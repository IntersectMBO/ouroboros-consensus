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
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Block.SupportsPeras
  ( PerasRoundNo (..)
  , onPerasRoundNo
  , PerasWeight (..)
  , BlockSupportsPeras (..)
  , PerasCert (..)
  , PerasVote (..)
  , PerasCfg (..)
  , ValidatedPerasCert (..)
  , ValidatedPerasVote (..)
  , makePerasCfg
  , HasPerasCertRound (..)
  , HasPerasCertBoostedBlock (..)
  , HasPerasCertBoost (..)
  , HasPerasVoteRound (..)
  , HasPerasVoteVotedBlock (..)
  , HasStakePoolId (..)

    -- * Ouroboros Peras round length
  , PerasRoundLength (..)
  , defaultPerasRoundLength
  ) where

import qualified Cardano.Binary as KeyHash
import Cardano.Ledger.Core (KeyHash, KeyRole (StakePool))
import Cardano.Ledger.State (IndividualPoolStake (..), PoolDistr (PoolDistr, unPoolDistr))
import Codec.Serialise (Serialise (..))
import Codec.Serialise.Decoding (decodeListLenOf)
import Codec.Serialise.Encoding (encodeListLen)
import Data.Coerce (coerce)
import qualified Data.Map as Map
import Data.Monoid (Sum (..))
import Data.Proxy (Proxy (..))
import Data.Word (Word64)
import GHC.Generics (Generic)
import NoThunks.Class
import Ouroboros.Consensus.Block.Abstract
import Ouroboros.Consensus.BlockchainTime.WallClock.Types (WithArrivalTime (..))
import Ouroboros.Consensus.Util
import Ouroboros.Consensus.Util.Condense
import Quiet (Quiet (..))

newtype PerasRoundNo = PerasRoundNo {unPerasRoundNo :: Word64}
  deriving Show via Quiet PerasRoundNo
  deriving stock Generic
  deriving newtype (Enum, Eq, Ord, Num, Bounded, NoThunks, Serialise)

-- | TODO: what is the proper underlying type?
type StakePoolId = KeyHash 'StakePool

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

deriving instance Ord IndividualPoolStake

data ValidatedPerasVote blk = ValidatedPerasVote
  { vpvVote :: !(PerasVote blk)
  , vpvVoteStake :: !IndividualPoolStake
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass NoThunks

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
    PoolDistr ->
    Either (PerasValidationErr blk) (ValidatedPerasVote blk)

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
    , pvVoteStakePoolId :: StakePoolId
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

  validatePerasVote _cfg vote PoolDistr{unPoolDistr} =
    let stake = unPoolDistr Map.! (pvVoteStakePoolId vote)
     in Right (ValidatedPerasVote{vpvVote = vote, vpvVoteStake = stake})

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
  encode PerasVote{pvVoteRound, pvVotedBlock, pvVoteStakePoolId} =
    encodeListLen 3
      <> encode pvVoteRound
      <> encode pvVotedBlock
      <> KeyHash.toCBOR pvVoteStakePoolId
  decode = do
    decodeListLenOf 3
    pvVoteRound <- decode
    pvVotedBlock <- decode
    pvVoteStakePoolId <- KeyHash.fromCBOR
    pure $ PerasVote{pvVoteRound, pvVotedBlock, pvVoteStakePoolId}

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

class HasStakePoolId vote where
  getStakePoolId :: vote -> StakePoolId
instance HasStakePoolId (PerasVote blk) where
  getStakePoolId = pvVoteStakePoolId
instance HasStakePoolId (ValidatedPerasVote blk) where
  getStakePoolId = getStakePoolId . vpvVote
instance
  HasStakePoolId vote =>
  HasStakePoolId (WithArrivalTime vote)
  where
  getStakePoolId = getStakePoolId . forgetArrivalTime
