-- | Where the ChainDB passes the claim of each new Leios certification it
-- validates.
--
-- The ChainDB passes each on, with the announcement it certifies, as it
-- establishes the claim; see
-- 'Ouroboros.Consensus.Storage.ChainDB.Impl.Args.cdbsOnNewLeiosValidClaim'.
-- LeiosFetch consumes them, to fetch the endorser block each such announcement
-- names.
module Ouroboros.Consensus.Leios.NewValidClaims
  ( NewLeiosValidClaims (..)
  , ignoreNewLeiosValidClaim
  , mkNewLeiosValidClaims
  ) where

import Control.Concurrent.Class.MonadSTM.Strict
  ( MonadSTM
  , STM
  , flushTQueue
  , newTQueueIO
  , readTQueue
  , writeTQueue
  )
import Data.List.NonEmpty (NonEmpty ((:|)))
import Ouroboros.Consensus.Leios.Cert (LeiosClaim)
import Ouroboros.Consensus.Leios.Protocol (LeiosAnnouncement)

-- | The claims of the new Leios certifications the ChainDB validates, each with
-- the announcement it certifies, held from when they are recorded until the one
-- thread that consumes them does.
--
-- Only the code that wires up a node holds both fields: whatever records is
-- given just 'recordNewLeiosValidClaim', and the one thread that consumes them
-- just 'consumeNewLeiosValidClaims'.
--
-- It holds only what that thread has yet to consume. That stays small, as long
-- as the thread consumes them again as soon as it has handled the previous
-- ones, and handles them without waiting long. A committee seat votes at most
-- once per slot, so a quorum certifies at most one announcement per slot unless
-- the committee equivocates; the claims arrive at the rate of elections,
-- whatever the node's peers do.
data NewLeiosValidClaims m = MkNewLeiosValidClaims
  { recordNewLeiosValidClaim :: LeiosClaim -> LeiosAnnouncement -> STM m ()
  -- ^ Cheap, and never blocks
  , consumeNewLeiosValidClaims :: STM m (NonEmpty (LeiosClaim, LeiosAnnouncement))
  -- ^ Those recorded since this last returned, in the order recorded, blocking
  -- until there is one
  }

-- | Holding none yet
mkNewLeiosValidClaims :: MonadSTM m => m (NewLeiosValidClaims m)
mkNewLeiosValidClaims = do
  queue <- newTQueueIO
  pure
    MkNewLeiosValidClaims
      { recordNewLeiosValidClaim = \claim ann -> writeTQueue queue (claim, ann)
      , consumeNewLeiosValidClaims = (:|) <$> readTQueue queue <*> flushTQueue queue
      }

-- | In place of 'recordNewLeiosValidClaim', for what opens a ChainDB but
-- fetches nothing: it records nothing.
ignoreNewLeiosValidClaim :: MonadSTM m => LeiosClaim -> LeiosAnnouncement -> STM m ()
ignoreNewLeiosValidClaim _claim _ann = pure ()
