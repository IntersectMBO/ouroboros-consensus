module Ouroboros.Consensus.Util.Time
  ( multipleNominalDelay
  , nominalDelay
  , secondsToNominalDiffTime
  ) where

import Data.Time (DiffTime, NominalDiffTime)

{-------------------------------------------------------------------------------
  Operations
-------------------------------------------------------------------------------}

-- | Multiply a 'NominalDiffTime' by an integer
--
-- The right conversions to use are somewhat tricky. The key fact is that
-- 'fromIntegral' interprets its argument as seconds.
multipleNominalDelay :: Integral a => NominalDiffTime -> a -> NominalDiffTime
multipleNominalDelay dur i = dur * fromIntegral i

{-------------------------------------------------------------------------------
  Conversions
-------------------------------------------------------------------------------}

nominalDelay :: NominalDiffTime -> DiffTime
nominalDelay = realToFrac

secondsToNominalDiffTime :: Double -> NominalDiffTime
secondsToNominalDiffTime = realToFrac
