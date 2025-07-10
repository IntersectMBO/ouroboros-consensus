-- TODO: This is a copy of `Ouroboros.Network.TxSubmission.Mempool.Reader` in
-- `ouroboros-network`, brought here to make things compile. We might want a
-- different interface at some point.

module Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.MockMempoolReader
  ( MockMempoolReader (..)
  , MockMempoolSnapshot (..)
  , mapMockMempoolSnapshot
  , mapMockMempoolReader
  ) where

import Control.Monad.Class.MonadSTM (MonadSTM, STM)
import Ouroboros.Network.SizeInBytes (SizeInBytes)

data MockMempoolReader objectId object index m =
     MockMempoolReader {
       mempoolGetSnapshot :: STM m (MockMempoolSnapshot objectId object index),
       mempoolZeroIndex   :: index
    }

mapMockMempoolReader ::
     MonadSTM m
  => (object -> object')
  -> MockMempoolReader objectId object index m
  -> MockMempoolReader objectId object' index m
mapMockMempoolReader f rdr =
    rdr {
       mempoolGetSnapshot = mapMockMempoolSnapshot f <$> mempoolGetSnapshot rdr
    }

data MockMempoolSnapshot objectId object index =
     MockMempoolSnapshot {
       mempoolObjectIdsAfter :: index -> [(objectId, index, SizeInBytes)],
       mempoolLookupObject   :: index -> Maybe object,
       mempoolHasObject      :: objectId -> Bool
     }

mapMockMempoolSnapshot ::
     (object -> object')
  -> MockMempoolSnapshot objectId object index
  -> MockMempoolSnapshot objectId object' index
mapMockMempoolSnapshot f snap =
     snap {
       mempoolLookupObject = fmap f . mempoolLookupObject snap
     }
