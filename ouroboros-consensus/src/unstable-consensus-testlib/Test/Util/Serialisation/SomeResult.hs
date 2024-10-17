{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Util.Serialisation.SomeResult (SomeResult (..)) where

import           Data.Typeable
import           Ouroboros.Consensus.Ledger.Query (BlockQuery)

-- | To easily generate all the possible @result@s of the 'Query' GADT, we
-- introduce an existential that also bundles the corresponding 'Query' as
-- evidence. We also capture 'Eq', 'Show', and 'Typeable' constraints, as we
-- need them in the tests.
data SomeResult blk where
  SomeResult :: (Eq result, Show result, Typeable result)
             => BlockQuery blk fp result -> result -> SomeResult blk

instance Show (SomeResult blk) where
  show (SomeResult _ result) = show result

instance Eq (SomeResult blk) where
  SomeResult _ (res1 :: result1) == SomeResult _ (res2 :: result2) =
    case eqT @result1 @result2 of
      Nothing   -> False
      Just Refl -> res1 == res2
