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
  , PerasWeight (..)
  , BlockSupportsPeras (..)
  , PerasCert (..)
  , PerasCfg (..)
  , ValidatedPerasCert (..)
  , makePerasCfg
  , HasPerasCertRound (..)
  , HasPerasCertBoostedBlock (..)
  , HasPerasCertBoost (..)

    -- * Ouroboros Peras round length
  , PerasRoundLength (..)
  , defaultPerasRoundLength
  ) where

import Codec.Serialise (Serialise (..))
import Codec.Serialise.Decoding (decodeListLenOf)
import Codec.Serialise.Encoding (encodeListLen)
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
  deriving newtype (Enum, Eq, Ord, NoThunks, Serialise)

instance Condense PerasRoundNo where
  condense = show . unPerasRoundNo

instance ShowProxy PerasRoundNo where
  showProxy _ = "PerasRoundNo"

newtype PerasWeight = PerasWeight {unPerasWeight :: Word64}
  deriving Show via Quiet PerasWeight
  deriving stock Generic
  deriving newtype (Eq, Ord, NoThunks)
  deriving (Semigroup, Monoid) via Sum Word64

instance Condense PerasWeight where
  condense = show . unPerasWeight

-- | TODO this will become a Ledger protocol parameter
boostPerCert :: PerasWeight
boostPerCert = PerasWeight 15

-- TODO using 'Validated' for extra safety? Or some @.Unsafe@ module?
data ValidatedPerasCert blk = ValidatedPerasCert
  { vpcCert :: !(PerasCert blk)
  , vpcCertBoost :: !PerasWeight
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
-- TODO this will become a Ledger protocol parameter
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

  data PerasValidationErr blk

  validatePerasCert ::
    PerasCfg blk ->
    PerasCert blk ->
    Either (PerasValidationErr blk) (ValidatedPerasCert blk)

-- TODO degenerate instance for all blks to get things to compile
instance StandardHash blk => BlockSupportsPeras blk where
  newtype PerasCfg blk = PerasCfg
    { -- TODO eventually, this will come from the
      -- protocol parameters from the ledger state
      perasCfgWeightBoost :: PerasWeight
    }
    deriving stock (Show, Eq)

  data PerasCert blk = PerasCert
    { pcCertRound :: PerasRoundNo
    , pcCertBoostedBlock :: Point blk
    }
    deriving stock (Generic, Eq, Ord, Show)
    deriving anyclass NoThunks

  -- TODO enrich with actual error types
  data PerasValidationErr blk
    = PerasValidationErr
    deriving stock (Show, Eq)

  -- TODO perform actual validation against all
  -- possible 'PerasValidationErr' variants
  validatePerasCert cfg cert =
    Right
      ValidatedPerasCert
        { vpcCert = cert
        , vpcCertBoost = perasCfgWeightBoost cfg
        }

instance ShowProxy blk => ShowProxy (PerasCert blk) where
  showProxy _ = "PerasCert " <> showProxy (Proxy @blk)

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

-- | Derive a 'PerasCfg' from a 'BlockConfig'
-- TODO this currently doesn't depend on 'BlockConfig' at all, but likely will
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
