{-# LANGUAGE BangPatterns #-}

-- | The unit of Leios tx-fetch work: 'LeiosJob's and the per-EB 'LeiosJobPool'
-- that schedules them.
--
-- TO BE IMPORTED QUALIFIED
--
-- A leaf module (imported by "LeiosDemoTypes") so the pool's structural
-- operations -- greedy partition, least-requested selection, multiplicity
-- bookkeeping -- stay together and depend only on 'IntMap'/'IntSet'.
--
-- The key benefit of jobs is to minimize the bookkeeping footprint and churn.
--
-- TO BE IMPORTED QUALIFIED
module LeiosDemoTypes.LeiosJobs (module LeiosDemoTypes.LeiosJobs) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.IntSet.NonEmpty (NEIntSet)
import qualified Data.IntSet.NonEmpty as NEIntSet
import Data.Word (Word32)

-- | A unit of tx-fetch work: the EB-body offsets fetched by one
-- @MsgLeiosBlockTxsRequest@ -- a bitfield over the body's tx vector.
newtype LeiosJob =
    -- TODO 'LeiosJob' is immutable and only ever fully traversed, so a packed
    -- bitfield (a strict ByteString or unboxed Word64 vector) would be more
    -- compact than the 'IntSet' Patricia tree.
    MkLeiosJob IntSet
  deriving (Eq, Show)

-- | Identifies a 'LeiosJob' within its 'LeiosJobPool' (0-based, stable for the
-- pool's lifetime).
newtype LeiosJobId = MkLeiosJobId Int
  deriving (Eq, Ord, Show)

-- | How many peers currently have an in-flight request for a job.
newtype LeiosJobMultiplicity = MkLeiosJobMultiplicity Int
  deriving (Eq, Ord, Show)

-- | A job together with its current in-flight multiplicity.
data LeiosJobState = MkLeiosJobState !LeiosJob !LeiosJobMultiplicity
  deriving (Eq, Show)

-- | The not-yet-requested jobs for one acquired EB, plus a reverse index by
-- multiplicity so a least-requested job is one 'IntMap.lookupMin' away.
--
-- INVARIANT: 'jobsByMultiplicity' is the exact inverse of the multiplicities in
-- 'jobs' -- every job id in 'jobs' sits in exactly the bucket named by its
-- 'LeiosJobState' multiplicity.
data LeiosJobPool = MkLeiosJobPool
  { jobs :: !(IntMap LeiosJobState)
  -- ^ keyed by 'LeiosJobId'
  , jobsByMultiplicity :: !(IntMap NEIntSet)
  -- ^ keyed by 'LeiosJobMultiplicity'
  }
  deriving (Eq, Show)

-- | Partition the missing txs (each given by its offset within the EB body and
-- its on-the-wire byte size) into jobs, greedily in offset order: a job grows
-- until adding the next tx would exceed @maxJobBytes@ or @maxJobTxCount@, but
-- always holds at least one tx (so an oversized tx would form a solo job, in
-- the unintended case of max tx size exceeding max job size).
mkLeiosJobPool :: Word32 -> Int -> IntMap Word32 -> LeiosJobPool
mkLeiosJobPool maxJobBytes maxJobTxCount misses =
  MkLeiosJobPool
    { jobs =
        IntMap.fromList
          [ (jid, MkLeiosJobState (MkLeiosJob offs) (MkLeiosJobMultiplicity 0))
          | (jid, offs) <- ijbs
          ]
    , jobsByMultiplicity =
        maybe IntMap.empty (IntMap.singleton 0) $
          NEIntSet.nonEmptySet (IntSet.fromList (map fst ijbs))
    }
 where
  ijbs = zip [0 ..] $ case IntMap.toAscList misses of
    [] -> []
    ((off0, sz0) : rest) -> grow (IntSet.singleton off0) sz0 1 rest

  grow !cur !_bytes !_count [] = [cur]
  grow !cur !bytes !count ((off, sz) : rest)
    | count < maxJobTxCount && bytes + sz <= maxJobBytes =
        grow (IntSet.insert off cur) (bytes + sz) (count + 1) rest
    | otherwise = cur : grow (IntSet.singleton off) sz 1 rest

-- | No unfinished jobs remain -- the EB's whole tx-closure has been fetched.
nullLeiosJobPool :: LeiosJobPool -> Bool
nullLeiosJobPool = IntMap.null . jobs

-- | The bitfield of an unfinished job, if it is still in the pool.
lookupJob :: LeiosJobId -> LeiosJobPool -> Maybe LeiosJob
lookupJob (MkLeiosJobId jid) pool =
  (\(MkLeiosJobState job _multiplicity) -> job) <$> IntMap.lookup jid (jobs pool)

-- | Select a least-requested unfinished job (fewest in-flight requests; ties by
-- lowest job id), record one more in-flight request for it, and return its id,
-- its bitfield, and the updated pool. 'Nothing' if the pool is empty.
pickLeastRequestedJob :: LeiosJobPool -> Maybe (LeiosJobId, LeiosJob, LeiosJobPool)
pickLeastRequestedJob pool =
  case IntMap.lookupMin (jobsByMultiplicity pool) of
    Nothing -> Nothing
    Just (m, bucket) ->
      let jid = NEIntSet.findMin bucket
          -- Bump jid from bucket m to m+1, carrying its bitfield out in the same
          -- traversal.
          bump1 mbState = case mbState of
            Nothing -> (Nothing, Nothing)
            Just (MkLeiosJobState job _oldMultiplicity) ->
              (Just job, Just (MkLeiosJobState job (MkLeiosJobMultiplicity (m + 1))))
       in case IntMap.alterF bump1 jid (jobs pool) of
            (Nothing, _) -> Nothing
            (Just job, jobs') ->
              Just
                ( MkLeiosJobId jid
                , job
                , MkLeiosJobPool
                    { jobs = jobs'
                    , jobsByMultiplicity =
                        bucketInsert (m + 1) jid (bucketDelete m jid (jobsByMultiplicity pool))
                    }
                )

-- | Record one fewer in-flight request for a job (on disconnect). A no-op if the
-- job is no longer in the pool.
decrementJobMultiplicity :: LeiosJobId -> LeiosJobPool -> LeiosJobPool
decrementJobMultiplicity (MkLeiosJobId jid) pool =
  case IntMap.alterF decrement1 jid (jobs pool) of
    (Nothing, _) -> pool
    (Just (m, m'), jobs') ->
      MkLeiosJobPool
        { jobs = jobs'
        , jobsByMultiplicity =
            bucketInsert m' jid (bucketDelete m jid (jobsByMultiplicity pool))
        }
 where
  -- One traversal of 'jobs': the pair functor carries the prior and new
  -- multiplicities (for the reverse-index move) alongside the new value.
  decrement1 Nothing = (Nothing, Nothing)
  decrement1 (Just (MkLeiosJobState job (MkLeiosJobMultiplicity m))) =
    let m' = m - 1
     in (Just (m, m'), Just (MkLeiosJobState job (MkLeiosJobMultiplicity m')))

-- | Remove a job from the pool entirely (on its response arriving).
completeJob :: LeiosJobId -> LeiosJobPool -> LeiosJobPool
completeJob (MkLeiosJobId jid) pool =
  case IntMap.alterF delete1 jid (jobs pool) of
    (Nothing, _) -> pool
    (Just m, jobs') ->
      MkLeiosJobPool
        { jobs = jobs'
        , jobsByMultiplicity = bucketDelete m jid (jobsByMultiplicity pool)
        }
 where
  -- One traversal of 'jobs': carry out the removed job's multiplicity (for the
  -- reverse-index delete) while deleting the entry.
  delete1 Nothing = (Nothing, Nothing)
  delete1 (Just (MkLeiosJobState _job (MkLeiosJobMultiplicity m))) = (Just m, Nothing)

bucketDelete :: Int -> Int -> IntMap NEIntSet -> IntMap NEIntSet
bucketDelete m jid = IntMap.update (NEIntSet.nonEmptySet . NEIntSet.delete jid) m

bucketInsert :: Int -> Int -> IntMap NEIntSet -> IntMap NEIntSet
bucketInsert m jid = IntMap.insertWith NEIntSet.union m (NEIntSet.singleton jid)
