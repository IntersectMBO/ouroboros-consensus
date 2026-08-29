{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}

-- | The unit of Leios tx-fetch work: 'LeiosJob's and the per-EB 'LeiosJobPool'
-- that schedules them.
--
-- TO BE IMPORTED QUALIFIED
--
-- A leaf module (imported by "LeiosDemoTypes") so the pool's structural
-- operations -- greedy partition, least-requested selection, multiplicity
-- bookkeeping -- stay together, along with the 'TxHash' carrier and the
-- 'JobRootHash' commitment it computes (so the fetch machinery can name tx hashes
-- and their commitment without a cycle through "LeiosDemoTypes").
--
-- The key benefit of jobs is to minimize the bookkeeping footprint and churn.
--
-- TO BE IMPORTED QUALIFIED
module LeiosDemoTypes.LeiosJobs (module LeiosDemoTypes.LeiosJobs) where

import qualified Cardano.Crypto.Hash as Hash
import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as BS16
import qualified Data.ByteString.Char8 as BS8
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.IntSet.NonEmpty (NEIntSet)
import qualified Data.IntSet.NonEmpty as NEIntSet
import Data.Word (Word32)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import System.Random (StdGen, uniformR)

-- | Hash of a Leios transaction (the 'Cardano.Crypto.Leios.HASH' of its bytes).
newtype TxHash = MkTxHash ByteString
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (NFData, NoThunks)

instance Show TxHash where
  show = prettyTxHash

prettyTxHash :: TxHash -> String
prettyTxHash (MkTxHash bytes) = BS8.unpack (BS16.encode bytes)

-- | A job's commitment to which txs it covers: the Blake2b-256 hash of the
-- concatenated tx hashes (in ascending offset order), via
-- 'jobRootHashOfTxHashes'. It lets an arriving @MsgLeiosBlockTxs@ be validated
-- against the job /without/ retaining the EB body -- crucial, since up to ~10k
-- EBs (each up to ~512 kB) could have txs in flight at once, far too much to
-- hold in memory.
newtype JobRootHash = MkJobRootHash ByteString
  deriving (Eq, Show)

jobRootHashOfTxHashes :: [TxHash] -> JobRootHash
jobRootHashOfTxHashes =
  MkJobRootHash
    . Hash.hashToBytes
    . Hash.hashWith @Hash.Blake2b_256 id
    . BS.concat
    . map (\(MkTxHash bs) -> bs)

-- | A unit of tx-fetch work: the EB-body offsets fetched by one
-- @MsgLeiosBlockTxsRequest@ (a bitfield over the body's tx vector), the total
-- on-the-wire byte size of those txs (for the fetch byte budget), and the
-- 'JobRootHash' commitment used to validate the response.
data LeiosJob
  = -- TODO the offset set is immutable and only ever fully traversed, so a packed
    -- bitfield (a strict ByteString or unboxed Word64 vector) would be more
    -- compact than the 'IntSet' Patricia tree.
    MkLeiosJob !IntSet !Word32 !JobRootHash
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
-- multiplicity so a least-requested job is one 'IntMap.minView' away.
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

-- | Partition the missing txs into jobs, greedily in offset order: a job grows
-- until adding the next tx would exceed @maxJobBytes@ or @maxJobTxCount@, but
-- always holds at least one tx (so an oversized tx would form a solo job, in
-- the unintended case of max tx size exceeding max job size).
--
-- Each miss is its offset within the EB body mapped to its tx hash and its
-- on-the-wire byte size. Each job's 'JobRootHash' commitment is computed here via
-- 'jobRootHashOfTxHashes' over its covered tx hashes.
mkLeiosJobPool ::
  Word32 -> Int -> IntMap (TxHash, Word32) -> LeiosJobPool
mkLeiosJobPool maxJobBytes maxJobTxCount misses =
  MkLeiosJobPool
    { jobs =
        IntMap.fromList
          [ (jid, MkLeiosJobState job (MkLeiosJobMultiplicity 0))
          | (jid, job) <- ijbs
          ]
    , jobsByMultiplicity =
        maybe IntMap.empty (IntMap.singleton 0) $
          NEIntSet.nonEmptySet (IntSet.fromList (map fst ijbs))
    }
 where
  ijbs = zip [0 ..] $ case IntMap.toAscList misses of
    [] -> []
    ((off0, (h0, sz0)) : rest) -> grow (IntSet.singleton off0) sz0 1 [h0] rest

  flush !cur !bytes hashesRev = MkLeiosJob cur bytes (jobRootHashOfTxHashes (reverse hashesRev))

  grow !cur !bytes !_count hashesRev [] = [flush cur bytes hashesRev]
  grow !cur !bytes !count hashesRev ((off, (h, sz)) : rest)
    | count < maxJobTxCount && bytes + sz <= maxJobBytes =
        grow (IntSet.insert off cur) (bytes + sz) (count + 1) (h : hashesRev) rest
    | otherwise = flush cur bytes hashesRev : grow (IntSet.singleton off) sz 1 [h] rest

-- | No unfinished jobs remain -- the EB's whole tx-closure has been fetched.
nullLeiosJobPool :: LeiosJobPool -> Bool
nullLeiosJobPool = IntMap.null . jobs

-- | The bitfield of an unfinished job, if it is still in the pool.
lookupJob :: LeiosJobId -> LeiosJobPool -> Maybe LeiosJob
lookupJob (MkLeiosJobId jid) pool =
  (\(MkLeiosJobState job _multiplicity) -> job) <$> IntMap.lookup jid (jobs pool)

-- | Restrict a map keyed by 'LeiosJobId' to the jobs still unfinished in the
-- pool, dropping entries for jobs already 'completeJob'd. Lets the tx-arrival
-- handler pick out, from a request's covered jobs, the ones we still need to
-- ingest (a redundant delivery of a completed job is dropped).
restrictToPending :: IntMap a -> LeiosJobPool -> IntMap a
restrictToPending m pool = m `IntMap.intersection` jobs pool

-- | The pool with no jobs -- nothing left to fetch.
emptyLeiosJobPool :: LeiosJobPool
emptyLeiosJobPool = MkLeiosJobPool IntMap.empty IntMap.empty

-- | Select a least-requested unfinished job (fewest in-flight requests; ties
-- broken uniformly at random via the supplied PRNG) whose id is /not/ in
-- @excluded@, record one more in-flight request for it, and return its id, its
-- bitfield, the updated pool, and the advanced PRNG. 'Nothing' if every
-- unfinished job is excluded (or the pool is empty), in which case the caller
-- keeps its own PRNG (nothing was drawn).
--
-- The caller passes the job ids this peer already has in flight for the EB, so a
-- peer is never asked for the same job twice.
pickLeastRequestedJobExcept ::
  StdGen -> IntSet -> LeiosJobPool -> Maybe (LeiosJobId, LeiosJob, LeiosJobPool, StdGen)
pickLeastRequestedJobExcept prng excluded pool =
  case eligibleBucket of
    Nothing -> Nothing
    Just (m, diff) ->
      -- Draw a uniform index into the eligible bucket's non-excluded jobs.
      -- 'Data.IntSet' has no indexed access, but a bucket holds at most the ~184
      -- jobs of one EB, so indexing the ascending list is cheap.
      let (i, prng') = uniformR (0, IntSet.size diff - 1) prng
          jid = IntSet.toAscList diff !! i
       in case IntMap.alterF (bump1 m) jid (jobs pool) of
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
                , prng'
                )
 where
  -- Walk multiplicity buckets low-to-high, stopping at the first whose
  -- non-excluded jobs are non-empty; the caller draws a random one from that
  -- 'IntSet' difference. 'foldrWithKey' visits ascending keys and is lazy in the
  -- accumulator, so this stops at the first eligible bucket without materialising
  -- the bucket list.
  --
  -- TODO if we wanted to enforce a limit on the multiplicity of /each job/,
  -- it'd be easy to do so here: only visit the lower-multiplicity buckets
  eligibleBucket =
    IntMap.foldrWithKey
      ( \m bucket rest ->
          let diff = IntSet.difference (NEIntSet.toSet bucket) excluded
           in if IntSet.null diff then rest else Just (m, diff)
      )
      Nothing
      (jobsByMultiplicity pool)

  -- Bump jid from bucket m to m+1, carrying its bitfield out in the same traversal.
  bump1 _m Nothing = (Nothing, Nothing)
  bump1 m (Just (MkLeiosJobState job _oldMultiplicity)) =
    (Just job, Just (MkLeiosJobState job (MkLeiosJobMultiplicity (m + 1))))

-- | Record one fewer in-flight request for a job (on disconnect). A no-op if the
-- job is no longer in the pool.
unpickJob :: LeiosJobId -> LeiosJobPool -> LeiosJobPool
unpickJob (MkLeiosJobId jid) pool =
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
