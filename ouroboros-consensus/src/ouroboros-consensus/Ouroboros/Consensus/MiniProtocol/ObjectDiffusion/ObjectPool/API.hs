-- | API for reading from and writing to object pools in the ObjectDiffusion
-- miniprotocol.
--
-- The underlying object pool can be any database, such as a 'PerasCertDb' in
-- Peras certificate diffusion.
--
-- 'ObjectPoolReader' is used on the outbound side of the protocol. Objects in
-- the pool are ordered by a strictly increasing ticket number ('ticketNo'),
-- which represents their time of arrival. Ticket numbers are local to each
-- node, unlike object IDs, which are global. Object IDs are not used for
-- ordering, since objects may arrive slightly out of order from peers.
--
-- To read from the pool, one requests objects with a ticket number strictly
-- greater than the last known one. 'oprZeroTicketNo' provides an initial ticket
-- number for the first request.
--
-- 'ObjectPoolWriter' is used on the inbound side of the protocol. It allows
-- checking whether an object is already present (to avoid re-requesting it) and
-- appending new objects. Ticket numbers are not part of the inbound interface,
-- but are used internally: newly added objects always receive a ticket number
-- strictly greater than those of older ones.
--
-- This API design is inspired by 'MempoolSnapshot' from the TX-submission
-- miniprotocol, see:
-- <https://ouroboros-consensus.cardano.intersectmbo.org/haddocks/ouroboros-consensus/Ouroboros-Consensus-Mempool-API.html#t:MempoolSnapshot>
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
  }

-- | Interface used by the inbound side of object diffusion when receiving
-- objects.
data ObjectPoolWriter objectId object m
  = ObjectPoolWriter
  { opwObjectId :: object -> objectId
  -- ^ Return the id of the specified object
  , opwAddObjects :: [object] -> m ()
  -- ^ Add a batch of objects to the objectPool.
  , opwHasObject :: STM m (objectId -> Bool)
  -- ^ Check if the object pool contains an object with the given id
  }
