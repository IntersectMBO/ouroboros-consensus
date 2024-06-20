module NoThunks.Invariant (noThunksInvariant) where

import           NoThunks.Class

noThunksInvariant :: NoThunks a => a -> Maybe String
noThunksInvariant = fmap show . unsafeNoThunks
