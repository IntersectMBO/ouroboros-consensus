{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Consensus.Storage.LedgerDB.OrphanArbitrary () where

import           Ouroboros.Consensus.Config.SecurityParam (SecurityParam (..))
import           Test.QuickCheck

{-------------------------------------------------------------------------------
  Orphan Arbitrary instances
-------------------------------------------------------------------------------}

instance Arbitrary SecurityParam where
  arbitrary = SecurityParam <$> choose (0, 6)
  shrink (SecurityParam k) = SecurityParam <$> shrink k
