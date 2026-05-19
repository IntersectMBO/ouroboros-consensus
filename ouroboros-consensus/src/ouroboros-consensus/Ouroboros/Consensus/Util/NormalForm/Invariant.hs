{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | 'NoThunks' invariant.
--
-- Due to its expensive nature, this invariant is gated behind the
-- @expensive-invariants@ flag from the @ouroboros-consensus@ package.
module Ouroboros.Consensus.Util.NormalForm.Invariant
  ( -- * Invariant
    noThunksInvariant
  ) where

import NoThunks.Class (NoThunks (..), unsafeNoThunks)

{-------------------------------------------------------------------------------
  Invariant
-------------------------------------------------------------------------------}

noThunksInvariant :: NoThunks a => a -> Maybe String
#ifdef ENABLE_EXPENSIVE_INVARIANTS
noThunksInvariant = fmap show . unsafeNoThunks
#else
noThunksInvariant = const Nothing
#endif
