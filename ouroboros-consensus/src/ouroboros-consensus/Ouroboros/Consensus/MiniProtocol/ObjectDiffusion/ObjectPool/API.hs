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

    -- * Invariants
  , prop_objectsAfterAreGreaterThanTicket
  , prop_objectsAfterArePresentOnWriter
  ) where

import Control.Concurrent.Class.MonadSTM.Strict (MonadSTM (..), STM)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Word (Word64)

-- | Interface used by the outbound side of object diffusion as its source of
-- objects to give to the remote side.
data ObjectPoolReader objectId object ticketNo m
  = ObjectPoolReader
  { oprObjectId :: object -> objectId
  -- ^ Return the id of the specified object
  , oprZeroTicketNo :: ticketNo
  -- ^ Ticket number before the first item in the pool.
  , oprObjectsAfter :: ticketNo -> Word64 -> STM m (Maybe (m (Map ticketNo object)))
  -- ^ Get the map of objects available in the pool with a 'ticketNo' greater
  -- than the specified one, with the number of returned objects capped to the
  -- specified limit.
  -- Returns 'Nothing' if there are no such objects available atm in the pool,
  -- or 'Just' a monadic action yielding the map of objects otherwise.
  -- The monadic action is there to account for the fact that objects might be
  -- stored on disk.
  -- If the monadic action is executed immediately, the returned map will be
  -- non-empty. But if the consumer delays executing the action too much,
  -- it is possible that the objects get garbage-collected in the meantime, and
  -- thus the returned map could be empty.
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

-- * Invariants

-- | When retrieving objects after a given ticket number, all the objects in the
-- returned map should have a strictly greater ticket number.
prop_objectsAfterAreGreaterThanTicket ::
  (Ord ticketNo, MonadSTM m) =>
  ObjectPoolReader objectId object ticketNo m ->
  ticketNo ->
  Word64 ->
  m Bool
prop_objectsAfterAreGreaterThanTicket opr ticketNo limit = do
  mObjects <- atomically $ oprObjectsAfter opr ticketNo limit
  case mObjects of
    Nothing ->
      -- Vacuosly true if there are no objects available in the pool.
      pure True
    Just getObjects -> do
      -- The returned map may be empty if the consumer delayed executing the
      -- action and the objects got garbage-collected in the meantime. However,
      -- any objects that are still present in the pool should have a ticket
      -- number greater than the specified one.
      objects <- getObjects
      pure $
        all
          (> ticketNo)
          (Map.keys objects)

-- | Objects retrieved from an object pool reader must be present on the writer
-- side at the the retrieval takes place.
prop_objectsAfterArePresentOnWriter ::
  MonadSTM m =>
  ObjectPoolReader objectId object ticketNo m ->
  ObjectPoolWriter objectId object m ->
  ticketNo ->
  Word64 ->
  m Bool
prop_objectsAfterArePresentOnWriter opr opw ticketNo limit = do
  (mObjects, hasObject) <-
    atomically $
      (,)
        <$> oprObjectsAfter opr ticketNo limit
        <*> opwHasObject opw
  case mObjects of
    Nothing ->
      -- Vacuosly true if there are no objects available in the pool.
      pure True
    Just getObjects -> do
      -- The returned map may be empty if the consumer delayed executing the
      -- action and the objects got garbage-collected in the meantime. However,
      -- any objects that are still present in the pool should also be present
      -- on the writer side.
      objects <- getObjects
      pure $
        all
          (hasObject . oprObjectId opr)
          (Map.elems objects)
