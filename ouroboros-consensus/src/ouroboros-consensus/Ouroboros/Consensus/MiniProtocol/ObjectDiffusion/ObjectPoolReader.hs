-- TODO: This is a copy of `Ouroboros.Network.TxSubmission.Mempool.Reader` in
-- `ouroboros-network`, brought here to make things compile. We might want a
-- different interface at some point.

module Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.ObjectPoolReader
  ( ObjectPoolReader (..)
  , ObjectPoolSnapshot (..)
  , mapObjectPoolSnapshot
  , mapObjectPoolReader
  ) where

import Control.Monad.Class.MonadSTM (MonadSTM, STM)
import Ouroboros.Network.SizeInBytes (SizeInBytes)

data ObjectPoolReader objectId object index m
  = ObjectPoolReader
  { objectPoolGetSnapshot :: STM m (ObjectPoolSnapshot objectId object index)
  , objectPoolZeroIndex :: index
  }

mapObjectPoolReader ::
  MonadSTM m =>
  (object -> object') ->
  ObjectPoolReader objectId object index m ->
  ObjectPoolReader objectId object' index m
mapObjectPoolReader f rdr =
  rdr
    { objectPoolGetSnapshot = mapObjectPoolSnapshot f <$> objectPoolGetSnapshot rdr
    }

data ObjectPoolSnapshot objectId object index
  = ObjectPoolSnapshot
  { objectPoolObjectIdsAfter :: index -> [(objectId, index, SizeInBytes)]
  , objectPoolLookupObject :: index -> Maybe object
  , objectPoolHasObject :: objectId -> Bool
  }

mapObjectPoolSnapshot ::
  (object -> object') ->
  ObjectPoolSnapshot objectId object index ->
  ObjectPoolSnapshot objectId object' index
mapObjectPoolSnapshot f snap =
  snap
    { objectPoolLookupObject = fmap f . objectPoolLookupObject snap
    }
