{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Block.SupportsPeras
  ( PerasRoundNo (..)
  , PerasWeight (..)
  , boostPerCert
  , BlockSupportsPeras (..)
  , PerasCert (..)
  ) where

import Data.Monoid (Sum (..))
import Data.Word (Word64)
import GHC.Generics (Generic)
import NoThunks.Class
import Ouroboros.Consensus.Block.Abstract
import Ouroboros.Consensus.Util.Condense
import Quiet (Quiet (..))

newtype PerasRoundNo = PerasRoundNo {unPerasRoundNo :: Word64}
  deriving Show via Quiet PerasRoundNo
  deriving stock Generic
  deriving newtype (Eq, Ord, NoThunks)

instance Condense PerasRoundNo where
  condense = show . unPerasRoundNo

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

class
  NoThunks (PerasCert blk) =>
  BlockSupportsPeras blk
  where
  data PerasCert blk

  perasCertRound :: PerasCert blk -> PerasRoundNo

  perasCertBoostedBlock :: PerasCert blk -> Point blk

-- TODO degenerate instance for all blks to get things to compile
instance StandardHash blk => BlockSupportsPeras blk where
  data PerasCert blk = PerasCert
    { pcCertRound :: PerasRoundNo
    , pcCertBoostedBlock :: Point blk
    }
    deriving stock (Generic, Eq, Ord, Show)
    deriving anyclass NoThunks

  perasCertRound = pcCertRound
  perasCertBoostedBlock = pcCertBoostedBlock
