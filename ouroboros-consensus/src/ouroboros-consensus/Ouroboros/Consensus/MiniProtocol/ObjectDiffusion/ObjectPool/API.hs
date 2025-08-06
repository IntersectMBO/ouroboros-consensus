module Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.ObjectPool.API
  ( ObjectPoolReader (..)
  , ObjectPoolWriter (..)
  ) where

import Control.Concurrent.Class.MonadSTM.Strict (STM)
import Data.Word (Word64)

-- | Interface used by the outbound side of object diffusion as its source of
-- objects to give to the remote side.
data ObjectPoolReader objectId object ticketNo m
  = ObjectPoolReader
  { oprObjectId :: object -> objectId
  -- ^ Return the id of the specified object
  , oprZeroTicketNo :: ticketNo
  -- ^ Ticket number before the first item in the pool.
  , oprObjectsAfter :: ticketNo -> Word64 -> STM m [(ticketNo, objectId, m object)]
  -- ^ Get the list of objects available in the pool with a ticketNo greater
  -- than the specified one. The number of returned objects is capped by the
  -- given Word64. Only the IDs and ticketNos of the objects are directly
  -- accessible; each actual object must be loaded through a monadic action.
  --
  -- TODO: This signature assume that we have all the IDs and ticketNos in
  -- memory, but not the actual objects. This might change if IDs must be loaded
  -- from disk too.
  }

-- | Interface used by the inbound side of object diffusion when receiving
-- objects.
data ObjectPoolWriter objectId object m
  = ObjectPoolWriter
  { opwObjectId :: object -> objectId
  -- ^ Return the id of the specified object
  , opwAddObjects :: [object] -> m ()
  -- ^ Add a batch of objects to the objectPool.
  , opwHasObject :: m (objectId -> Bool)
  -- ^ Check if the object pool contains an object with the given id
  }
