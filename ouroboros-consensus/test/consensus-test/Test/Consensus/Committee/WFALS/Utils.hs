-- | Utility functions for the WFALS tests.
module Test.Consensus.Committee.WFALS.Utils
  ( mkPoolId
  ) where

import qualified Cardano.Crypto.DSIGN.Class as SL
import qualified Cardano.Crypto.Seed as SL
import qualified Cardano.Ledger.Core as SL
import qualified Cardano.Ledger.Keys as SL
import Data.String (IsString (..))
import Ouroboros.Consensus.Committee.Types (PoolId (..))

-- | Create a pool ID from an arbitrary string of any length.
--
-- NOTE: we are assuming that this function preserves uniqueness.
mkPoolId :: String -> PoolId
mkPoolId str =
  PoolId
    . SL.hashKey
    . SL.VKey
    . SL.deriveVerKeyDSIGN
    . SL.genKeyDSIGN
    . SL.mkSeedFromBytes
    . fromString
    $ paddedStr
 where
  paddedStr
    | length str >= neededBytes = take neededBytes str
    | otherwise = str <> replicate (neededBytes - length str) '0'

  neededBytes = 32
