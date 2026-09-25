module Test.Ouroboros.Consensus.QuickCheck.Extras
  ( sized1
  , unsafeMapSuchThatJust
  ) where

import qualified Test.QuickCheck as QC

sized1 :: (Int -> QC.Gen a) -> QC.Gen a
sized1 f = QC.sized (f . succ)

-- | A generator that checks its own satisfaction
--
-- WARNING: 'QC.suchThat' et al often causes a /very/ confusing
-- non-termination when its argument is impossible/extremely unlikely
unsafeMapSuchThatJust :: QC.Gen (Maybe a) -> QC.Gen a
unsafeMapSuchThatJust m = QC.suchThatMap m id
