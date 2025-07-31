module Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.ObjectPool.API
  ( ObjectPoolReader (..)
  , ObjectPoolSnapshot (..)
  , ObjectPoolWriter (..)
  ) where

import Control.Monad.Class.MonadSTM (STM)
import Ouroboros.Network.SizeInBytes (SizeInBytes)

data ObjectPoolReader objectId object index m
  = ObjectPoolReader
  { rdrGetObjectId :: object -> objectId
  , objectPoolGetSnapshot :: STM m (ObjectPoolSnapshot objectId object index)
  , objectPoolZeroIndex :: index
  }
data ObjectPoolSnapshot objectId object index
  = ObjectPoolSnapshot
  { objectPoolObjectsAfter :: index -> [(object, index, SizeInBytes)]
  , objectPoolHasObject :: objectId -> Bool
  }

data ObjectPoolWriter objectId object m =
     ObjectPoolWriter {
       -- | Return the id of the specified object
       wrGetObjectId          :: object -> objectId,
       -- | Supply a batch of objects to the objectPool.
       objectPoolAddObjects :: [object] -> m ()
    }
