{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

-- | The unit of Leios tx-fetch work: 'LeiosJob's and the per-EB 'LeiosJobPool'
-- that schedules them.
--
-- The pool's structural operations -- greedy partition, least-requested
-- selection, multiplicity bookkeeping -- stay together here, along with the
-- 'JobCommitment' a job carries.
--
-- The key benefit of jobs is to minimize the bookkeeping footprint and churn.
--
-- TO BE IMPORTED QUALIFIED
module Ouroboros.Consensus.MiniProtocol.LeiosFetch.Jobs
  ( -- * Jobs
    LeiosJob (..)
  , LeiosJobId (..)
  , JobCommitment (..)
  , jobCommitmentOfTxs

    -- * Pools
  , LeiosJobPool
  , mkJobPool
  , emptyJobPool
  , nullJobPool
  , lookupJob
  , restrictToPending
  , pickLeastRequestedJobExcept
  , unpickJob
  , completeJob

    -- * Inspecting pools
  , jobMultiplicities
  , hasExactReverseIndex
  ) where

import qualified Cardano.Crypto.Hash as Hash
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.IntSet.NonEmpty (NEIntSet)
import qualified Data.IntSet.NonEmpty as NEIntSet
import Data.Word (Word32)
import Ouroboros.Consensus.Leios.Types (TxHash)
import System.Random (StdGen, uniformR)

-- | A job's commitment to which txs it covers, at which sizes: the Blake2b-256
-- hash of each tx's hash followed by its size in four big-endian bytes (in
-- ascending offset order), via 'jobCommitmentOfTxs'. It lets an arriving
-- @MsgLeiosBlockTxs@ be validated against the job /without/ retaining the EB
-- body -- crucial, since up to ~10k EBs (each up to ~600 kB) could have txs in
-- flight at once, far too much to hold in memory.
--
-- A tx that is not the size the body declares for it fails the validation,
-- even if it is the tx the body names. So the validation cannot tell a peer
-- that sends the wrong tx from an endorser block that declares the wrong size
-- for the right one: the node never notices such an endorser block, and only
-- disconnects each peer that serves it. Separate commitments to the hashes and
-- to the sizes would tell the two apart, but they would double the
-- commitments' footprint in the heap, which is already non-trivial, and they
-- would only guard against a suboptimal adversary, which could send a tx with
-- the wrong txhash instead of with the wrong size and have wasted the same
-- resources.
newtype JobCommitment = MkJobCommitment ByteString
  deriving (Eq, Show)

jobCommitmentOfTxs :: [(TxHash, Word32)] -> JobCommitment
jobCommitmentOfTxs =
  MkJobCommitment
    . Hash.hashToBytes
    . Hash.hashWith @Hash.Blake2b_256 id
    . BS.toStrict
    . BSB.toLazyByteString
    . foldMap (\(txHash, size) -> BSB.byteString (Hash.hashToBytes txHash) <> BSB.word32BE size)

-- | A unit of tx-fetch work: some of an EB's txs, fetched by one
-- @MsgLeiosBlockTxsRequest@.
data LeiosJob
  = MkLeiosJob
      -- | The txs' offsets in the EB body's tx vector
      --
      -- TODO the offset set is immutable and only ever fully traversed, so a
      -- packed bitfield (a strict ByteString or unboxed Word64 vector) would be
      -- more compact than the 'IntSet' Patricia tree.
      !IntSet
      -- | The txs' total on-the-wire byte size, for the fetch byte budget
      !Word32
      -- | What the response is validated against
      !JobCommitment
  deriving (Eq, Show)

-- | Identifies a 'LeiosJob' within its 'LeiosJobPool' (0-based, stable for the
-- pool's lifetime). A point's pool is built once, so a reply for a point names
-- only that pool's jobs.
newtype LeiosJobId = MkLeiosJobId Int
  deriving (Eq, Ord, Show)

-- | How many peers currently have an in-flight request for a job.
newtype LeiosJobMultiplicity = MkLeiosJobMultiplicity Int
  deriving (Eq, Ord, Show)

-- | A job together with its current in-flight multiplicity.
data LeiosJobState = MkLeiosJobState !LeiosJob !LeiosJobMultiplicity
  deriving (Eq, Show)

-- | The unfinished jobs for one acquired EB, requested or not, plus a reverse
-- index by multiplicity so a least-requested job is one 'IntMap.minView' away.
--
-- INVARIANT: 'jobsByMultiplicity' is the exact inverse of the multiplicities in
-- 'jobs' -- every job id in 'jobs' sits in exactly the bucket named by its
-- 'LeiosJobState' multiplicity.
data LeiosJobPool = UnsafeLeiosJobPool
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
-- on-the-wire byte size. Each job's 'JobCommitment' is computed here via
-- 'jobCommitmentOfTxs' over its covered txs' hashes and sizes.
--
-- The jobs are numbered from 0.
mkJobPool :: Word32 -> Int -> IntMap (TxHash, Word32) -> LeiosJobPool
mkJobPool maxJobBytes maxJobTxCount misses =
  UnsafeLeiosJobPool
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
    ((off0, ref0@(_h0, sz0)) : rest) -> grow (IntSet.singleton off0) sz0 1 [ref0] rest

  flush !cur !bytes refsRev = MkLeiosJob cur bytes (jobCommitmentOfTxs (reverse refsRev))

  grow !cur !bytes !_count refsRev [] = [flush cur bytes refsRev]
  grow !cur !bytes !count refsRev ((off, ref@(_h, sz)) : rest)
    | count < maxJobTxCount && bytes + sz <= maxJobBytes =
        grow (IntSet.insert off cur) (bytes + sz) (count + 1) (ref : refsRev) rest
    | otherwise = flush cur bytes refsRev : grow (IntSet.singleton off) sz 1 [ref] rest

-- | No unfinished jobs remain -- the EB's whole tx-closure has been fetched.
nullJobPool :: LeiosJobPool -> Bool
nullJobPool = IntMap.null . jobs

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
emptyJobPool :: LeiosJobPool
emptyJobPool = UnsafeLeiosJobPool IntMap.empty IntMap.empty

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
                , UnsafeLeiosJobPool
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
-- job is no longer in the pool: a peer disconnecting after its response already
-- arrived and 'completeJob' removed the job hits the @Nothing@ branch below.
unpickJob :: LeiosJobId -> LeiosJobPool -> LeiosJobPool
unpickJob (MkLeiosJobId jid) pool =
  case IntMap.alterF decrement1 jid (jobs pool) of
    (Nothing, _) -> pool
    (Just (m, m'), jobs') ->
      UnsafeLeiosJobPool
        { jobs = jobs'
        , -- Move the job from its old bucket to the decremented one. When the
          -- count was already floored at 0, @m' == m@ and this deletes then
          -- re-inserts into the same bucket -- a net no-op, not special-cased
          -- because the floor is reached only on a surplus disconnect.
          jobsByMultiplicity =
            bucketInsert m' jid (bucketDelete m jid (jobsByMultiplicity pool))
        }
 where
  -- One traversal of 'jobs': the pair functor carries the prior and new
  -- multiplicities (for the reverse-index move) alongside the new value.
  decrement1 Nothing = (Nothing, Nothing)
  -- Floored: the multiplicity is an in-flight count, so it has no meaning below
  -- zero, and a negative one would sort the job into a bucket ahead of every
  -- genuinely unrequested one.
  decrement1 (Just (MkLeiosJobState job (MkLeiosJobMultiplicity m))) =
    let m' = max 0 (m - 1)
     in (Just (m, m'), Just (MkLeiosJobState job (MkLeiosJobMultiplicity m')))

-- | Remove a job from the pool entirely (on its response arriving).
completeJob :: LeiosJobId -> LeiosJobPool -> LeiosJobPool
completeJob (MkLeiosJobId jid) pool =
  case IntMap.alterF delete1 jid (jobs pool) of
    (Nothing, _) -> pool
    (Just m, jobs') ->
      UnsafeLeiosJobPool
        { jobs = jobs'
        , jobsByMultiplicity = bucketDelete m jid (jobsByMultiplicity pool)
        }
 where
  -- One traversal of 'jobs': carry out the removed job's multiplicity (for the
  -- reverse-index delete) while deleting the entry.
  delete1 Nothing = (Nothing, Nothing)
  delete1 (Just (MkLeiosJobState _job (MkLeiosJobMultiplicity m))) = (Just m, Nothing)

-- | How many peers have each unfinished job in flight.
jobMultiplicities :: LeiosJobPool -> IntMap Int
jobMultiplicities =
  IntMap.map (\(MkLeiosJobState _job (MkLeiosJobMultiplicity m)) -> m) . jobs

-- | Whether 'jobsByMultiplicity' is the exact inverse of the multiplicities in
-- 'jobs', as 'LeiosJobPool' requires.
hasExactReverseIndex :: LeiosJobPool -> Bool
hasExactReverseIndex pool =
  IntMap.map NEIntSet.toSet (jobsByMultiplicity pool)
    == IntMap.fromListWith
      IntSet.union
      [(m, IntSet.singleton jid) | (jid, m) <- IntMap.toList (jobMultiplicities pool)]

bucketDelete :: Int -> Int -> IntMap NEIntSet -> IntMap NEIntSet
bucketDelete m jid = IntMap.update (NEIntSet.nonEmptySet . NEIntSet.delete jid) m

bucketInsert :: Int -> Int -> IntMap NEIntSet -> IntMap NEIntSet
bucketInsert m jid = IntMap.insertWith NEIntSet.union m (NEIntSet.singleton jid)
