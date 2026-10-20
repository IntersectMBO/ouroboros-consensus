-- | Intended for qualified import
--
-- > import Ouroboros.Consensus.Storage.ChainDB.Init (InitChainDB)
-- > import qualified Ouroboros.Consensus.Storage.ChainDB.Init as InitChainDB
module Ouroboros.Consensus.Storage.ChainDB.Init
  ( InitChainDB (..)
  , fromFull
  , map
  ) where

import Data.Maybe.Strict (StrictMaybe (SNothing))
import Ouroboros.Consensus.Block (WithOrigin (Origin))
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Storage.ChainDB.API (ChainDB)
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import qualified Ouroboros.Consensus.Storage.ChainDB.API.Types.InvalidBlockPunishment as InvalidBlockPunishment
import Ouroboros.Consensus.Util.IOLike
import Prelude hiding (map)

-- | Restricted interface to the 'ChainDB' used on node initialization
data InitChainDB m blk = InitChainDB
  { addTheFirstEbb :: blk -> m ()
  -- ^ Add the EBB that begins the chain
  --
  -- Only Byron has EBBs, so only a chain that starts in Byron calls this, and
  -- only once.
  , getCurrentLedger :: m (LedgerState blk EmptyMK)
  -- ^ Return the current ledger state
  }

fromFull ::
  IOLike m =>
  ChainDB m blk -> InitChainDB m blk
fromFull db =
  InitChainDB
    { addTheFirstEbb =
        -- The first block's predecessor is genesis, and Byron has no Leios.
        ChainDB.addBlock_ db InvalidBlockPunishment.noPunishment Origin SNothing
    , getCurrentLedger =
        atomically $ ledgerState <$> ChainDB.getCurrentLedger db
    }

map ::
  Functor m =>
  (blk' -> blk) ->
  (LedgerState blk EmptyMK -> LedgerState blk' EmptyMK) ->
  InitChainDB m blk ->
  InitChainDB m blk'
map f g db =
  InitChainDB
    { addTheFirstEbb = addTheFirstEbb db . f
    , getCurrentLedger = g <$> getCurrentLedger db
    }
