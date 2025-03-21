{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Ouroboros.Storage.LedgerDB.OrphanArbitrary () where

import           Cardano.Ledger.BaseTypes (nonZero)
import           Ouroboros.Consensus.Config.SecurityParam (SecurityParam (..))
import           Ouroboros.Consensus.Util (Flag (..))
import           Test.Cardano.Ledger.Core.Arbitrary ()
import           Test.QuickCheck

{-------------------------------------------------------------------------------
  Orphan Arbitrary instances
-------------------------------------------------------------------------------}

instance Arbitrary SecurityParam where
  arbitrary = SecurityParam <$> choose (0, 6) `suchThatMap` nonZero
  shrink (SecurityParam k) = SecurityParam <$> shrink k

deriving newtype instance Arbitrary (Flag symbol)
