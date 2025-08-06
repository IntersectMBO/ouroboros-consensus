module Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.ObjectPool.API
  ( ObjectPoolReader (..)
  , ObjectPoolSnapshot (..)
  , ObjectPoolWriter (..)
  ) where

import Control.Monad.Class.MonadSTM (STM)
import Ouroboros.Network.SizeInBytes (SizeInBytes)

data ObjectPoolReader objectId object ticketNo m
  = ObjectPoolReader
  { rdrGetObjectId :: object -> objectId
  -- ^ Return the id of the specified object
  , objectPoolGetSnapshot :: STM m (ObjectPoolSnapshot objectId object ticketNo)
  -- ^ Get a snapshot of the object pool
  , objectPoolZeroTicketNo :: ticketNo
  -- ^ Ticket number before the first item in the pool (so objectPoolObjectsAfter objectPoolZeroTicketNo returns all possible objects)
  }
data ObjectPoolSnapshot objectId object ticketNo
  = ObjectPoolSnapshot
  -- TODO: revisit when we will have to load certificates from disk. This method might not be pure anymore
  { objectPoolObjectsAfter :: ticketNo -> [(object, ticketNo, SizeInBytes)]
  -- ^ Get all objects having a ticket number strictly greater than the given one, along with their ticket numbers and sizes
  , objectPoolHasObject :: objectId -> Bool
  -- ^ Check if the object pool contains an object with the given id
  }

data ObjectPoolWriter objectId object m
  = ObjectPoolWriter
  { wrGetObjectId :: object -> objectId
  -- ^ Return the id of the specified object
  , objectPoolAddObjects :: [object] -> m ()
  -- ^ Add a batch of objects to the objectPool.
  }
