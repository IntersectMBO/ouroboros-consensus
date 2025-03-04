{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Config.SecurityParam (SecurityParam (..)) where

import           Cardano.Binary
import           Cardano.Ledger.BaseTypes.NonZero
import           Data.Word
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)
import           Quiet

-- | Protocol security parameter
--
-- We interpret this as the number of rollbacks we support.
--
-- i.e., k == 0: we can't roll back at all
--       k == 1: we can roll back at most one block, etc
--
-- NOTE: This talks about the number of /blocks/ we can roll back, not
-- the number of /slots/.
newtype SecurityParam = SecurityParam { maxRollbacks :: NonZero Word64 }
  deriving (Eq, Generic, NoThunks, ToCBOR, FromCBOR)
  deriving Show via Quiet SecurityParam

instance ToCBOR a => ToCBOR (NonZero a) where
  toCBOR = toCBOR . unNonZero

instance (HasZero a, FromCBOR a) => FromCBOR (NonZero a) where
  fromCBOR = do
    a <- fromCBOR
    case nonZero a of
      Nothing -> fail "Non zero expected but zero found!"
      Just a' -> pure a'
