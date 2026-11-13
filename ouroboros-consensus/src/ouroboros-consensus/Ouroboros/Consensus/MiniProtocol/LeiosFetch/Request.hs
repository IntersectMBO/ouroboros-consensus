-- | Immutable reply-validation context, retained even after another copy wins.
-- Rationale: @docs/fetch-policy/leiosfetch-improved.md#requests-and-admission@.
module Ouroboros.Consensus.MiniProtocol.LeiosFetch.Request (LeiosFetchRequest (..), LeiosBlockRequest (..), LeiosBlockTxsRequest (..)) where

import Data.IntMap.NonEmpty (NEIntMap)
import Ouroboros.Consensus.Leios.Types (LeiosEbByteSizeBounds, LeiosPoint)
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Jobs as Jobs
import Ouroboros.Consensus.Leios.Types (BytesSize)

data LeiosFetchRequest
  = LeiosBlockRequest LeiosBlockRequest
  | LeiosBlockTxsRequest LeiosBlockTxsRequest

-- | A request for an endorser block's body.
data LeiosBlockRequest = MkLeiosBlockRequest
  { lbrPoint :: !LeiosPoint
  , lbrOfferedSize :: !BytesSize
  -- ^ The size the peer offered this endorser block at; not sent, but used by
  -- the reply handler.
  , lbrEbBounds :: !LeiosEbByteSizeBounds
  -- ^ The ledger's bounds on this block; not sent, but used by the reply
  -- handler.
  }

-- | A request for some of an EB's txs: its point and the 'Jobs.LeiosJob's it
-- covers, keyed by job id (so a request can't list a job twice), each with its
-- full commitment. Everything needed to validate the response is carried here,
-- so the reply handler needs no lookup into the (body-less) job pool -- and a
-- redundant response for a job that has since completed still validates. On
-- the wire only the offset bitmap goes to the peer; it is derived from the
-- union of the jobs' offsets (see
-- 'Ouroboros.Consensus.MiniProtocol.LeiosFetch.Type.offsetsToBitmap'), not
-- stored.
data LeiosBlockTxsRequest
  = MkLeiosBlockTxsRequest
      !LeiosPoint
      !(NEIntMap Jobs.LeiosJob)
