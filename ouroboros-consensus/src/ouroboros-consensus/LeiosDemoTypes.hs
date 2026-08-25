{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module LeiosDemoTypes (
  module LeiosDemoTypes,

  -- * Re-exports
  module Cardano.Crypto.Leios,
  module TxHashReexports,
  ) where

import Cardano.Binary
  ( Decoder
  , Encoding
  , enforceSize
  , serialize'
  , toCBOR
  , toStrictByteString
  )
import qualified Cardano.Binary as CBOR
import Cardano.Binary.FixedSizeCodec (decodeFixedSized, encodeFixedSized)
import Cardano.Crypto.DSIGN
  ( signDSIGN
  , verifyDSIGN
  )
import qualified Cardano.Crypto.Hash as Hash
import Cardano.Crypto.Leios
  ( AggregationError (..)
  , LeiosCert (..)
  , LeiosCommittee (..)
  , LeiosDSIGN
  , LeiosSeat (..)
  , LeiosSeatId (..)
  , LeiosSignature
  , LeiosSigningKey
  , LeiosVerificationKey
  , VerificationError
  , Weight
  , aggregateLeiosCert
  , getLeiosSeatId
  , leiosCommitteeSize
  , leiosSignContext
  , resolveLeiosSeat
  , verifyLeiosCert
  )
import Cardano.Crypto.Util (SignableRepresentation (..))
import Cardano.Ledger.Core (EraTx, Tx, TxLevel (TopTx))
import Cardano.Prelude (NonEmpty, toList, toString, (&))
import Cardano.Slotting.Slot (SlotNo (SlotNo), WithOrigin, withOrigin)
import Codec.Serialise (Serialise, decode, encode)
import Control.Concurrent.Class.MonadMVar (MVar)
import qualified Control.Concurrent.Class.MonadMVar as MVar
import Control.Concurrent.Class.MonadSTM.Strict (StrictTVar)
import qualified Control.Concurrent.Class.MonadSTM.Strict as StrictSTM
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Bits as Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as BS16
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Short as SBS
import Data.Fixed (Pico)
import qualified Data.Foldable as F
import Data.IntMap.NonEmpty (NEIntMap)
import qualified Data.IntMap.NonEmpty as NEIntMap
import Data.IntSet.NonEmpty (NEIntSet)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Ord (Down (..))
import Data.Ratio ((%))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Set.NonEmpty (NESet)
import qualified Data.Set.NonEmpty as NESet
import LeiosDemoTypes.LeiosJobs as TxHashReexports (TxHash (..), prettyTxHash)
import qualified LeiosDemoTypes.LeiosJobs as Jobs
import Data.String (fromString)
import Cardano.Slotting.Time (RelativeTime)
import Data.Time.Clock (NominalDiffTime)
import Data.Vector.Strict (Vector)
import qualified Data.Vector.Strict as V
import Data.Word (Word16, Word32, Word64)
import Debug.Trace (trace)
import GHC.Generics (Generic)
import LeiosDemoDb.Trace (TraceLeiosDb (..))
import LeiosDemoException (LeiosDbException (..), jsonLeiosDbException)
import LeiosDemoLogic.Announcements.ElBimap (ElId (..))
import LeiosDemoOnlyTestFetch (LeiosFetch, Message (..))
import qualified LeiosDemoOnlyTestFetch as LeiosFetch
import LeiosDemoOnlyTestNotify (LeiosNotify, Message (..))
import qualified LeiosDemoOnlyTestNotify as LeiosNotify
import NoThunks.Class (OnlyCheckWhnfNamed (..))
import qualified Numeric
import Ouroboros.Consensus.Ledger.Basics (EmptyMK, LedgerState)
import Ouroboros.Consensus.Ledger.SupportsMempool
  ( ByteSize32 (..)
  , TxMeasureMetrics
  , txMeasureMetricTxSizeBytes
  )
import Ouroboros.Consensus.Util (ShowProxy (..))
import Ouroboros.Consensus.Util.IOLike (IOLike, NoThunks)
import Ouroboros.Network.PeerSelection.LedgerPeers.Type
  ( IsBigLedgerPeer (..)
  )
import Text.Pretty.Simple (pShow)

-- * Hashes and identities

newtype PeerId a = MkPeerId a
  deriving stock Show
  deriving newtype (Eq, Ord)

-- Hash algorithm used in leios for EBs and txs
type HASH = Hash.Blake2b_256

-- | Hash of an Endorser Block
newtype EbHash = MkEbHash {ebHashBytes :: ByteString}
  deriving newtype (Eq, Ord, NoThunks, Serialise)
  deriving stock Generic

instance Show EbHash where
  show = prettyEbHash

encodeEbHash :: EbHash -> Encoding
encodeEbHash (MkEbHash bytes) = CBOR.encodeBytes bytes

decodeEbHash :: Decoder s EbHash
decodeEbHash = MkEbHash <$> CBOR.decodeBytes

prettyEbHash :: EbHash -> String
prettyEbHash (MkEbHash bytes) = BS8.unpack (BS16.encode bytes)

-- | Hash of a Ranking Block
--
-- A Ranking Block is the Praos Block. While the regular Praos headers are parameterised
-- over 'blk', we choose to keep 'RbHash' monomorphic. Use the 'ConvertRawHash' type class
-- to convert between this type and 'HeaderHash'.
newtype RbHash = MkRbHash {rbHashBytes :: ByteString}
  deriving newtype (Eq, Ord, NoThunks)
  deriving stock Generic

instance Show RbHash where
  show = prettyRbHash

encodeRbHash :: RbHash -> Encoding
encodeRbHash (MkRbHash bytes) = CBOR.encodeBytes bytes

decodeRbHash :: Decoder s RbHash
decodeRbHash = MkRbHash <$> CBOR.decodeBytes

prettyRbHash :: RbHash -> String
prettyRbHash (MkRbHash bytes) = BS8.unpack (BS16.encode bytes)

instance SignableRepresentation RbHash where
  getSignableRepresentation point =
    toStrictByteString $
      encodeRbHash point

-- | Uniquely identifies an endorser block in Leios. Could use 'Block SlotNo
-- EbHash' eventually, but a dedicated type is better to explore.
data LeiosPoint = MkLeiosPoint {pointSlotNo :: SlotNo, pointEbHash :: EbHash}
  deriving stock (Eq, Ord, Generic)
  deriving anyclass NoThunks

instance ShowProxy LeiosPoint where showProxy _ = "LeiosPoint"

-- TODO: prettyprinter instance Pretty?
instance Show LeiosPoint where
  show = prettyLeiosPoint

instance SignableRepresentation LeiosPoint where
  getSignableRepresentation point =
    toStrictByteString $
      -- REVIEW: Flat concatenation expected as what is signed?
      encode point.pointSlotNo
        <> encodeEbHash point.pointEbHash

prettyLeiosPoint :: LeiosPoint -> String
prettyLeiosPoint (MkLeiosPoint (SlotNo slotNo) (MkEbHash bytes)) =
  "(" ++ show slotNo ++ ", " ++ BS8.unpack (BS16.encode bytes) ++ ")"

encodeLeiosPoint :: LeiosPoint -> Encoding
encodeLeiosPoint (MkLeiosPoint ebSlot ebHash) =
  CBOR.encodeListLen 2
    <> encode ebSlot
    <> encodeEbHash ebHash

decodeLeiosPoint :: Decoder s LeiosPoint
decodeLeiosPoint = do
  enforceSize (fromString "LeiosPoint") 2
  MkLeiosPoint <$> decode <*> decodeEbHash

-- | Acquired EB tx closures, aged by the youngest announcement slot ever seen
-- for each EB (tracked here, not derived from the VolatileDB).
data AcquiredLeiosEbs = AcquiredLeiosEbs
  { alebYoungestSlot :: !(Map EbHash SlotNo)
  , alebBySlot :: !(Map SlotNo (NESet EbHash))
  -- ^ INVARIANT: is merely reverse index of 'alebYoungestSlot'
  }
  deriving stock (Show, Generic)

deriving via
  OnlyCheckWhnfNamed "AcquiredLeiosEbs" AcquiredLeiosEbs
  instance
    NoThunks AcquiredLeiosEbs

emptyAcquiredLeiosEbs :: AcquiredLeiosEbs
emptyAcquiredLeiosEbs = AcquiredLeiosEbs Map.empty Map.empty

-- | Use the 'Map' as a 'Set' without allocating the 'Set'.
data AcquiredLeiosEbsSet
  = forall x. MkAcquiredLeiosEbsSet !(Map EbHash x)

acquiredLeiosEbHashes :: AcquiredLeiosEbs -> AcquiredLeiosEbsSet
acquiredLeiosEbHashes = MkAcquiredLeiosEbsSet . alebYoungestSlot

acquiredLeiosEbsSetMember :: EbHash -> AcquiredLeiosEbsSet -> Bool
acquiredLeiosEbsSetMember eb (MkAcquiredLeiosEbsSet m) = Map.member eb m

-- | NOT EXPORTED
--
-- An auxiliary for 'insertAcquiredLeiosEb'.
newtype Alteration a b = MkAlteration (Maybe (a, b)) deriving Functor

-- | 'Nothing' if unchanged; @'Just' (novel, st')@ otherwise, where @novel@ is
-- 'True' iff the EB was not present before. Only bumps the slot when strictly
-- greater than the one recorded.
insertAcquiredLeiosEb ::
  LeiosPoint -> AcquiredLeiosEbs -> Maybe (Bool, AcquiredLeiosEbs)
insertAcquiredLeiosEb (MkLeiosPoint slot eb) (AcquiredLeiosEbs youngest bySlot) =
  case mbAltered of
    Nothing -> Nothing
    Just ((novel, bySlot'), youngest') ->
      Just (novel, AcquiredLeiosEbs youngest' bySlot')
 where
  MkAlteration mbAltered =
    Map.alterF (fmap Just . MkAlteration . alteration) eb youngest
  alteration = \case
    Nothing -> Just ((True, insertBucket slot eb bySlot), slot)
    Just prevSlot
      | slot <= prevSlot -> Nothing
      | otherwise ->
          Just ((False, insertBucket slot eb $ deleteBucket prevSlot eb bySlot), slot)

  insertBucket s e = Map.insertWith NESet.union s (NESet.singleton e)
  deleteBucket s e = Map.update (NESet.nonEmptySet . NESet.delete e) s

acquiredLeiosEbsFromList :: [LeiosPoint] -> AcquiredLeiosEbs
acquiredLeiosEbsFromList =
  F.foldl' (\st p -> maybe st snd (insertAcquiredLeiosEb p st)) emptyAcquiredLeiosEbs

-- | Drop every EB whose youngest announcement slot is strictly older than the
-- given (immutable tip) slot.
pruneAcquiredLeiosEbs ::
  WithOrigin SlotNo -> AcquiredLeiosEbs -> Maybe AcquiredLeiosEbs
pruneAcquiredLeiosEbs immTip (AcquiredLeiosEbs youngest bySlot) =
  withOrigin Nothing prune immTip
 where
  prune immTipSlot
    | Map.null prunedSlots = Nothing
    | otherwise = Just (AcquiredLeiosEbs youngest' bySlot')
   where
    (prunedSlots, bySlot') = Map.spanAntitone (< immTipSlot) bySlot
    prunedEbHashes = foldMap NESet.toSet prunedSlots
    youngest' = youngest `Map.withoutKeys` prunedEbHashes

-- | Types used in Praos headers
data EbAnnouncement = EbAnnouncement
  { ebAnnouncementHash :: EbHash
  , ebAnnouncementSize :: BytesSize
  }
  deriving stock (Generic, Show, Eq, Ord)
  deriving anyclass NoThunks

encodeEbAnnouncement :: EbAnnouncement -> Encoding
encodeEbAnnouncement ebAnn =
  CBOR.encodeListLen 2
    <> encode (ebAnnouncementHash ebAnn)
    <> encode (ebAnnouncementSize ebAnn)

decodeEbAnnouncement :: Decoder s EbAnnouncement
decodeEbAnnouncement = do
  enforceSize "EbAnnouncement" 2
  EbAnnouncement <$> decode <*> decode

-- * Fetch logic types

type BytesSize = Word32

data LeiosFetchRequest
  = LeiosBlockRequest LeiosBlockRequest
  | LeiosBlockTxsRequest LeiosBlockTxsRequest

data LeiosBlockRequest
  = -- |
    --
    -- The size isn't sent to the peer, but it's used to validate the reponse
    -- when it arrives.
    MkLeiosBlockRequest
      !LeiosPoint
      !BytesSize

data LeiosBlockTxsRequest
  = -- | A request for some of an EB's txs: its point and the 'Jobs.LeiosJob's it
    -- covers, keyed by job id (so a request can't list a job twice), each with its
    -- full commitment. Everything needed to validate the response is carried here,
    -- so the reply handler needs no lookup into the (body-less) job pool -- and a
    -- redundant response for a job that has since completed still validates. On
    -- the wire only the offset bitmap goes to the peer; it is derived from the
    -- union of the jobs' offsets (see the LeiosFetch client), not stored.
    MkLeiosBlockTxsRequest
      !LeiosPoint
      !(NEIntMap Jobs.LeiosJob)

prettyLeiosBlockTxsRequest :: LeiosBlockTxsRequest -> String
prettyLeiosBlockTxsRequest (MkLeiosBlockTxsRequest p jobs) =
  unwords
    [ "MsgLeiosBlockTxs"
    , prettyLeiosPoint p
    , "jobs=" <> show (toList (NEIntMap.keys jobs))
    ]

prettyBitmap :: (Word16, Word64) -> String
prettyBitmap (idx, bitmap) =
  show idx ++ ":0x" ++ padding ++ Numeric.showHex bitmap ""
 where
  n = Bits.countLeadingZeros bitmap

  padding = replicate (n `div` 4) '0'

--
-- Compare the following data types to the @LeiosFetchDynamicEnv@ and
-- @LeiosFetchState@ types in the Leios model exe
--
-- These data types are organized differently because they are organized by the
-- patterns of access to the "Ouroboros.Consensus.NodeKernel"'s shared state.
--

-- | Whether an EB offer also implies its tx-closure is on offer. A CertRB does
-- (it certifies the whole EB); a bare 'MsgLeiosBlockOffer' does not -- the closure
-- is offered separately, as a 'MsgLeiosBlockTxsOffer'. This is also the value we
-- store per offered point: 'TxsClosureAlsoOffered' means the peer can serve the
-- body /and/ the closure (a closure offer implies the body), while
-- 'TxsClosureNotAlsoOffered' is body-only.
data AlsoOfferedTxsClosure = TxsClosureAlsoOffered | TxsClosureNotAlsoOffered
  deriving (Eq, Show)

-- | Merge two offers for one point: the closure is on offer if either says so.
mergeOffer :: AlsoOfferedTxsClosure -> AlsoOfferedTxsClosure -> AlsoOfferedTxsClosure
mergeOffer TxsClosureAlsoOffered _ = TxsClosureAlsoOffered
mergeOffer _ TxsClosureAlsoOffered = TxsClosureAlsoOffered
mergeOffer _ _ = TxsClosureNotAlsoOffered

data LeiosPeerVars m = MkLeiosPeerVars
  { whetherBigLedgerPeer :: !IsBigLedgerPeer
  -- ^ fixed for the connection's lifetime; the fetch logic fetches more
  -- aggressively from a big-ledger peer (see 'leiosFetchLogicIteration')
  , offerings :: !(MVar m (Map LeiosPoint AlsoOfferedTxsClosure))
  -- ^ the peer's current offers, keyed by point -- so the map is already in slot
  -- order (freshest-first via 'Map.toDescList'), no dedup by EB hash needed
  -- (honest announcements don't reuse a hash, and an adversary defeats such
  -- dedup anyway). Written to only by the LeiosNotify client and eviction.
  , requestsToSend :: !(StrictTVar m (Seq LeiosFetchRequest))
  -- ^ written to by the fetch logic and the LeiosFetch client
  --
  -- These are the requests the fetch logic assumes will be sent, but have
  -- not already been sent.
  --
  -- Each client also maintains its own queue of requests that were
  -- actually sent (ie dequeued from this sequence but their reply
  -- hasn't yet arrived).
  --
  -- Note that @requestedPerPeer@ is the list maintained per client,
  -- whereas this list is not present in the model exe.
  --
  -- This is a 'TVar' so that the LeiosFetch client can wait on either it or
  -- the Diffusion Layer's control message to be actionable.
  }

newLeiosPeerVars :: IOLike m => IsBigLedgerPeer -> m (LeiosPeerVars m)
newLeiosPeerVars whetherBigLedgerPeer = do
  offerings <- MVar.newMVar Map.empty
  requestsToSend <- StrictSTM.newTVarIO Seq.empty
  pure MkLeiosPeerVars{whetherBigLedgerPeer, offerings, requestsToSend}

-- | Main data structure used in the Leios fetching logic.
--
-- Tracks both EB-level state (what EBs we have/need) and TX-level state
-- (what TXs we need for each EB), along with request tracking for bandwidth
-- management.
--
-- TODO: Potential simplifications once we have better test coverage:
--
-- 1. Consider separating "offer tracking" from "request tracking" into distinct
--    data structures for clarity.
data LeiosOutstanding pid = MkLeiosOutstanding
  { -- EB-level tracking
    ebState :: !(Map EbHash EbState)
  -- ^ Per-EB state for every EB we have seen announced (or offered)
  --
  -- TODO once offers are only valid if preceded by an announcement, then
  -- 'ebState' and the @selfPeer@ field of
  -- 'LeiosDemoLogic.Announcements.CentralState' are partially redundant
  , ebsPerMaxAnnouncementSlot :: !(Map SlotNo (NESet EbHash))
  -- ^ Slot-keyed reverse index of 'ebStateMaxSlot' on 'ebState'
  --
  -- Used to accelerate pruning.
  --
  -- TODO will also be redundant with by 'CentralState.selfPeer.live' once
  -- offers are no longer trusted.
  , acquiredEbBodiesPrunedSlot :: !SlotNo
  -- ^ The slot 'ebState' has most recently been pruned up to (see
  -- 'pruneOutstandingToImmTip').
  --
  -- Used to robustly prevent re-inserting what has already been pruned out.
  , missingEbBodies :: !(Map LeiosPoint BytesSize)
  -- ^ EB bodies still needed to be fetched (indexed by point and size)
  , reverseSlotIndexByEbHash :: !(Map EbHash (NESet SlotNo))
  -- ^ Inverse of 'missingEbBodies' grouped by content hash: for each EbHash
  -- listed there, the slots of the 'LeiosPoint's listing it. An EbHash is not
  -- 1-to-1 with slots, so one body can be listed at several points; on acquiring
  -- the body (keyed by hash) 'processLeiosBlock' must clear every such point, and
  -- this index makes that a direct lookup rather than a scan of 'missingEbBodies'
  -- (it likewise backs the "already listed?" check on the offer/announcement
  -- paths). Kept in step with 'missingEbBodies' at every insert and delete.

  -- Request tracking
  , requestedEbPeers :: !(Map EbHash (Set (PeerId pid)))
  -- ^ Which peers we've requested each EB from
  --
  -- TODO add requestedEbsPerPeer :: !(Map (PeerId pid) (NESet EbHash)) to avoid
  -- linear scan
  , requestedBytesSizePerPeer :: !(Map (PeerId pid) BytesSize)
  -- ^ Running total of bytes requested from each peer. This is the only
  -- outstanding-byte limit: there is no global cap (the global bound falls out
  -- as this per-peer cap times the peer count).
  , requestedJobsPerPeer :: !(Map (PeerId pid) (Map EbHash NEIntSet))
  -- ^ Per peer, per EB, the job ids it currently has in flight -- for
  -- decrementing those multiplicities on disconnect
  }

-- | The empty outstanding state, given the slot it has already been pruned up
-- to. The caller supplies the immutable-tip slot at startup so that a body at
-- or below it reads as too old from the outset (see
-- 'acquiredEbBodiesPrunedSlot' / 'pruneOutstandingToImmTip').
emptyLeiosOutstanding :: SlotNo -> LeiosOutstanding pid
emptyLeiosOutstanding prunedSlot =
  MkLeiosOutstanding
    { ebState = Map.empty
    , ebsPerMaxAnnouncementSlot = Map.empty
    , acquiredEbBodiesPrunedSlot = prunedSlot
    , missingEbBodies = Map.empty
    , reverseSlotIndexByEbHash = Map.empty
    , requestedEbPeers = Map.empty
    , requestedBytesSizePerPeer = Map.empty
    , requestedJobsPerPeer = Map.empty
    }

-- | Per-EB state tracked in 'ebState'
data EbState =
  -- | The greatest slot at which the EB has been announced (TODO or, for now,
  -- offered); the wall-clock onset of its /oldest/ announcement slot (kept as the
  -- minimum, so the body\/closure arrival handlers can report how old the EB was
  -- when we first held it; 'SNothing' for an unheralded offer-only or self-forged
  -- EB); and the current progress of fetching it.
  MkEbState !SlotNo !(StrictMaybe RelativeTime) !EbFetchState
  deriving (Eq, Show)

-- | Whether we hold an EB's body, plus the forge's imminent case.
data EbFetchState
  = -- | Our own forge is producing this EB
    --
    -- Its body and closure are in our store already or will be imminently. The
    -- fetch logic issues no requests for it and treats any peer offer as dead.
    --
    -- Distinct from 'NoBody' so we never fetch it, yet (like 'NoBody') reports
    -- no body held, so the forged body is still recognised as novel and
    -- persisted when it arrives; it becomes an ordinary 'BodyAcquired' at that
    -- point. This lets "EB arriving from forge" and "EB arriving from peer" be
    -- treated mostly the same way.
    BodyImminent
  | -- | We've only ever received an announcement, and our forge hasn't issued
    -- this EB (though it potentially could in the future!)
    NoBody
  | -- | The job pool: the jobs not yet /requested/ (NB this can be empty even
    -- before jobs have /arrived/).
    --
    -- The body itself is /not/ retained -- it lives in the LeiosDb, and each job
    -- carries a 'Jobs.JobRootHash' commitment sufficient to validate its
    -- response. Retaining up to ~10k bodies (each up to ~512 kB) would cost
    -- gigabytes.
    --
    -- TODO the 'Jobs.LeiosJobPool' could be an 'MVar m LeiosJobPool' for per-EB
    -- locking, at the cost of an 'm' parameter on
    -- EbFetchState\/EbState\/LeiosOutstanding and a monadic body-acquire;
    -- deferred.
    BodyAcquired !Jobs.LeiosJobPool
  deriving (Eq, Show)

ebStateMaxSlot :: EbState -> SlotNo
ebStateMaxSlot (MkEbState slot _onset _fetchState) = slot

-- | The recorded onset of the EB's oldest announcement slot, if any (see
-- 'MkEbState'); the arrival handlers use it to report the EB's age on arrival.
ebStateOnset :: EbState -> StrictMaybe RelativeTime
ebStateOnset (MkEbState _slot onset _fetchState) = onset

-- | Whether we already hold the EB's body (the "do we have it?" test that the
-- offer/announcement/arrival paths consult before fetching).
ebStateHasBody :: EbState -> Bool
ebStateHasBody (MkEbState _slot _onset fetchState) = case fetchState of
  NoBody -> False
  BodyImminent -> False
  BodyAcquired{} -> True

-- | A size summary of the LeiosFetch decision loop's working set
--
-- The stats are rather coarse because we forbid their calculation work to scale
-- with EBs; it's either O(1) or scales based on the number of peers.
data LeiosOutstandingStats = MkLeiosOutstandingStats
  { losTracked :: !Int
  -- ^ Total EBs in 'ebState' (should stay bounded by the pruning window; a
  -- persistent climb signals a pruning leak).
  , losMissingBodies :: !Int
  -- ^ Size of 'missingEbBodies' (EB body points still to fetch) -- the body-fetch
  -- backlog.
  , losPeersInflight :: !Int
  -- ^ Peers tracked in the outstanding-request byte map.
  , losInflightBytesDesc :: !(Vector Int)
  -- ^ Per-peer outstanding requested bytes, sorted descending -- the whole
  -- distribution, so budget concentration across peers is visible. The peer
  -- count is low, so materializing and sorting this is cheap.
  , losOffersDesc :: !(Vector Int)
  -- ^ Per-peer offer-set sizes, sorted descending -- the whole distribution.
  -- Offers are not byte-budgeted, so a peer can pile them up between prunes; a
  -- lone climbing head is the per-peer flood signal.
  }
  deriving (Eq, Show, Generic)

-- | @offerSizes@ is each peer's current offer-set size (e.g.
-- @map Map.size (Map.elems offerings)@ in the decision loop) and @numOfferingPeers@
-- its length, passed separately as an O(1) 'Map.size' hint so the sorted vector
-- is allocated exactly.
leiosOutstandingStats :: Int -> [Int] -> LeiosOutstanding pid -> LeiosOutstandingStats
leiosOutstandingStats numOfferingPeers offerSizes o =
  MkLeiosOutstandingStats
    { losTracked = Map.size (ebState o)
    , losMissingBodies = Map.size (missingEbBodies o)
    , losPeersInflight = Map.size inflightMap
    , losInflightBytesDesc = inflightDesc
    , losOffersDesc = offersDesc
    }
 where
  inflightMap = requestedBytesSizePerPeer o
  inflightDesc =
    V.fromListN
      (Map.size inflightMap)
      (sortOn Down (map fromIntegral (Map.elems inflightMap)))
  offersDesc = V.fromListN numOfferingPeers (sortOn Down offerSizes)

-- | Summary order-statistics of a distribution across peers, given as a
-- /descending-sorted, non-negative/ 'Vector' (as 'losInflightBytesDesc' /
-- 'losOffersDesc' are). All are cheap to derive, so the vector stays the source
-- of truth and these are computed only when emitting telemetry.
--
-- The median is taken over the /non-zero/ values only, so peers currently
-- holding nothing don't drag it toward zero.
data PeerDistSummary = MkPeerDistSummary
  { pdsNonzeroCount :: !Int
  , pdsTotal :: !Int
  , pdsTop1 :: !Int
  , pdsTop2 :: !Int
  , pdsTop3 :: !Int
  , pdsTop4 :: !Int
  , pdsTop5 :: !Int
  , pdsNonzeroMedian :: !Int
  }

summarizePeerDist :: Vector Int -> PeerDistSummary
summarizePeerDist desc =
  MkPeerDistSummary
    { pdsNonzeroCount = nz
    , pdsTotal = V.sum desc
    , pdsTop1 = nth 0
    , pdsTop2 = nth 1
    , pdsTop3 = nth 2
    , pdsTop4 = nth 3
    , pdsTop5 = nth 4
    , pdsNonzeroMedian = median
    }
 where
  n = V.length desc
  nth i = if i < n then desc V.! i else 0
  -- Descending-sorted and non-negative, so the non-zero values are the leading
  -- prefix and the median index lands inside it.
  nz = V.length (V.takeWhile (> 0) desc)
  median
    | nz == 0 = 0
    | odd nz = desc V.! (nz `div` 2)
    | otherwise = (desc V.! (nz `div` 2 - 1) + desc V.! (nz `div` 2)) `div` 2

-- | Simple counts describing what one decision iteration issued -- bounded by the
-- number of requests issued that iteration, never by the outstanding state. See
-- 'TraceLeiosFetchDecision'.
data LeiosDecisionStats = MkLeiosDecisionStats
  { ldsPeers :: !Int
  -- ^ Peers issued at least one request this iteration.
  , ldsRequests :: !Int
  -- ^ Total fetch requests issued.
  , ldsBodyRequests :: !Int
  -- ^ Of those, EB-body ('MsgLeiosBlock') requests; the rest are tx-batch
  -- ('MsgLeiosBlockTxs') requests.
  , ldsJobs :: !Int
  -- ^ Total jobs across the tx-batch requests.
  , ldsBodyBytes :: !Int
  -- ^ Requested EB-body bytes (sum of the body requests' sizes).
  , ldsTxBytes :: !Int
  -- ^ Requested tx bytes (sum of the covered jobs' on-the-wire sizes).
  }
  deriving (Eq, Show, Generic)

summarizeDecisions ::
  Foldable t => Map k (t LeiosFetchRequest) -> LeiosDecisionStats
summarizeDecisions decs =
  MkLeiosDecisionStats
    { ldsPeers = Map.size decs
    , ldsRequests = length reqs
    , ldsBodyRequests = length [() | LeiosBlockRequest{} <- reqs]
    , ldsJobs = sum [NEIntMap.size jobs | LeiosBlockTxsRequest (MkLeiosBlockTxsRequest _ jobs) <- reqs]
    , ldsBodyBytes = sum [fromIntegral sz | LeiosBlockRequest (MkLeiosBlockRequest _ sz) <- reqs]
    , ldsTxBytes =
        sum
          [ fromIntegral b
          | LeiosBlockTxsRequest (MkLeiosBlockTxsRequest _ jobs) <- reqs
          , Jobs.MkLeiosJob _ b _ <- F.toList jobs
          ]
    }
 where
  reqs = concatMap toList (Map.elems decs)

insertAcquiredEbBody ::
  EbHash -> Jobs.LeiosJobPool -> LeiosOutstanding pid -> LeiosOutstanding pid
insertAcquiredEbBody ebHash jobPool =
  alterEbState ebHash $ \case
    Nothing ->
      -- The state must have been pruned before the MsgLeiosBlock
      -- arrived. (Because we couldn't have sent a MsgLeiosBlockRequest if no
      -- announcement had arrived.)
      --
      -- Because it was previously pruned, it should simply be ignored now.
      Nothing
    Just (MkEbState slot onset fetchState) -> case fetchState of
      BodyAcquired{} -> Nothing
      NoBody -> Just $ MkEbState slot onset (BodyAcquired jobPool)
      BodyImminent ->
        -- note that we ignore the given jobPool here
        Just $ MkEbState slot onset (BodyAcquired Jobs.emptyLeiosJobPool)

-- | Record that our own forge is producing this EB
markBodyImminent ::
  EbHash -> SlotNo -> LeiosOutstanding pid -> LeiosOutstanding pid
markBodyImminent ebHash slot =
  alterEbState ebHash $ \case
    -- A self-forged EB records no onset: we produced it, so its arrival age is a
    -- trivial ~0, not a diffusion-latency data point.
    Nothing -> Just $ MkEbState slot SNothing BodyImminent
    Just (MkEbState oldSlot onset fetchState) -> case fetchState of
      NoBody -> Just $ MkEbState oldSlot onset BodyImminent
      BodyImminent -> Nothing
      BodyAcquired{} -> Just $ MkEbState oldSlot onset (BodyAcquired Jobs.emptyLeiosJobPool)

-- | Record that the EB with this hash is referenced (announced or offered) at this
-- slot, along with that slot's wall-clock onset if known.
--
-- The same EB (hash) can be referenced by several points; we keep the
-- /greatest/ such slot, so the EB's state isn't pruned prematurely. The onset,
-- in contrast, is kept as the /minimum/ (oldest announcement), so the arrival
-- handlers report the age since the EB was first heralded. An offer carries no
-- onset ('SNothing') and so never overrides one already recorded by an
-- announcement.
recordMaxAnnouncementSlot ::
  EbHash -> SlotNo -> StrictMaybe RelativeTime -> LeiosOutstanding pid -> LeiosOutstanding pid
recordMaxAnnouncementSlot ebHash slot onset =
  alterEbState ebHash $ \mbOld -> case mbOld of
    Nothing -> Just $ MkEbState slot onset NoBody
    Just (MkEbState oldSlot oldOnset fetchState) ->
      let newSlot = max slot oldSlot
          newOnset = minOnset onset oldOnset
       in if newSlot == oldSlot && newOnset == oldOnset
            then Nothing
            else Just $ MkEbState newSlot newOnset fetchState

-- | Combine two onsets, keeping the earlier and treating 'SNothing' as absent.
minOnset :: StrictMaybe RelativeTime -> StrictMaybe RelativeTime -> StrictMaybe RelativeTime
minOnset SNothing y = y
minOnset x SNothing = x
minOnset (SJust a) (SJust b) = SJust (min a b)

-- | Initialize the outstanding state
--
-- Its contents are only @'BodyAcquired' 'emptyLeiosJobPool'@ for the EBs the
-- LeiosDb already holds /in full/: a complete tx closure whose announcer is no
-- older than the immutable tip. Each is marked 'BodyAcquired' with an empty job
-- pool so we neither re-fetch nor re-process (eg emit 'AcquiredEb') an EB whose
-- closure we already have at start-up time.
--
-- Everything else in the LeiosDb is deliberately ignored here, and it's
-- safe to do so:
--
--   * A merely body-held EB with a /partial/ closure is not seeded: an
--     empty pool would strand its missing txs. Left absent, it is
--     re-derived from a fresh announcement/offer, and the redundant body
--     re-fetch + re-insert is idempotent (INSERT-OR-IGNORE / no-op on
--     duplicate).
--
--   * Complete closures at or below the immutable tip are already final;
--     they would read as @tooOld@ anyway, so there is nothing to track.
--
--   * For the "points" and "txs" SQL tables, the control paths that
--     actually depend on them read the LeiosDb directly, never via this
--     outstanding state.
--
-- The per-peer request-tracking fields must start empty regardless: there are
-- no connections yet and nothing is in flight.
--
-- The 'cdbAcquiredLeiosEbs' field of the ChainDB (which gates ChainSel for
-- CertRBs) is initialized in the exact same way: from
-- 'LeiosDemoDb.leiosDbScanCompleteEbClosuresNotOlderThanSlot', already
-- restricted to announcers no older than the immutable tip. And, it's
-- necessarily initialized earlier, as part of the ChainDB. But for the sake of
-- modularity/independence (see the TODO below), we're not reusing it to
-- initialize 'LeiosOutstanding'.
--
-- TODO At the cost of more complexity here, we could initialize 'ebState'
-- /and/ the LeiosTxCache to perfectly reflect the state of the LeiosDb on
-- start-up. It's not clear that that's worthwhile for the MVP; /healthy/
-- nodes shouldn't be frequently restarting.
initializeLeiosOutstanding :: [LeiosPoint] -> SlotNo -> LeiosOutstanding pid
initializeLeiosOutstanding points immTipSlot =
  F.foldl' (flip seed1) (emptyLeiosOutstanding immTipSlot) points
 where
  seed1 (MkLeiosPoint slot ebHash) =
    insertAcquiredEbBody ebHash Jobs.emptyLeiosJobPool
      . recordMaxAnnouncementSlot ebHash slot SNothing

-- | Upsert an EB's 'ebState' entry, keeping 'ebsPerMaxAnnouncementSlot' in step
-- whenever the entry's max slot moves. The supplied function must be
-- slot-monotonic (never lower the greatest slot), which both callers are.
alterEbState ::
  EbHash ->
  (Maybe EbState -> Maybe EbState) ->
  -- ^ REQUIREMENT: must not reduce 'ebStateMaxSlot'
  LeiosOutstanding pid ->
  LeiosOutstanding pid
alterEbState ebHash f outstanding =
  case Map.alterF upsert1 ebHash (ebState outstanding) of
    (Nothing, _) -> outstanding
    (Just (mbOldSlot, newSlot), ebState') ->
      outstanding
        { ebState = ebState'
        , ebsPerMaxAnnouncementSlot =
            if mbOldSlot == Just newSlot
              then ebsPerMaxAnnouncementSlot outstanding -- max slot unchanged
              else
                Map.insertWith NESet.union newSlot (NESet.singleton ebHash) $
                  case mbOldSlot of
                    Nothing ->
                      ebsPerMaxAnnouncementSlot outstanding
                    Just oldSlot ->
                      Map.update
                        (NESet.nonEmptySet . NESet.delete ebHash)
                        oldSlot
                        (ebsPerMaxAnnouncementSlot outstanding)
        }
 where
  -- One traversal of 'ebState': the pair functor carries whether the entry
  -- changed at all and, if so, the prior and new greatest slots for the
  -- reverse-index update.
  upsert1 mbOld = case f mbOld of
     Nothing -> (Nothing, mbOld)
     Just new -> (Just (ebStateMaxSlot <$> mbOld, ebStateMaxSlot new), Just new)

-- | Prune 'Outstanding' to the immutable tip, returning the EB hashes it dropped
-- (so the caller can drop those same hashes from the peers' offers).
--
-- Uses the 'ebsPerMaxAnnouncementSlot' reverse index to drop the below-tip prefix
-- directly (@spanAntitone@), rather than scanning the whole map.
--
-- TODO still more it could prune (e.g. abandoned in-flight EB requests).
pruneOutstandingToImmTip :: SlotNo -> LeiosOutstanding pid -> (Set EbHash, LeiosOutstanding pid)
pruneOutstandingToImmTip immTipSlot outstanding =
  ( prunedHashes
  , outstanding
      { ebState = ebState outstanding `Map.withoutKeys` prunedHashes
      , ebsPerMaxAnnouncementSlot = atOrAbove
      , acquiredEbBodiesPrunedSlot = max (acquiredEbBodiesPrunedSlot outstanding) immTipSlot
      , missingEbBodies = missingEbBodiesAtOrAbove
      , reverseSlotIndexByEbHash = reverseSlotIndexByEbHash'
      }
  )
 where
  (below, atOrAbove) =
    Map.spanAntitone (< immTipSlot) (ebsPerMaxAnnouncementSlot outstanding)
  prunedHashes = Set.unions (map NESet.toSet (Map.elems below))

  -- 'LeiosPoint' orders slot-first, so the below-tip points are a prefix.
  (belowBodies, missingEbBodiesAtOrAbove) =
    Map.spanAntitone
      (\(MkLeiosPoint slot _ebHash) -> slot < immTipSlot)
      (missingEbBodies outstanding)
  -- Remove each dropped point's slot from its hash's reverse-index entry (which
  -- exists, since the index is the exact inverse of 'missingEbBodies').
  reverseSlotIndexByEbHash' =
    foldr
      (\(MkLeiosPoint slot ebHash) -> Map.update (NESet.nonEmptySet . NESet.delete slot) ebHash)
      (reverseSlotIndexByEbHash outstanding)
      (Map.keys belowBodies)

-- | Pretty-print the per-peer 'offerings' map: for each peer, its offered points
-- freshest-first, each tagged with the strongest kind offered. Hashes truncated.
prettyOfferings :: Show pid => Map (PeerId pid) (Map LeiosPoint AlsoOfferedTxsClosure) -> String
prettyOfferings m =
  unlines $
    map ("    [leios] " ++) $
      [ show peer ++ " " ++ shortOffers offers
      | (peer, offers) <- Map.toList m
      ]
 where
  shortOffers offers = case Map.toDescList offers of
    [] -> "{}"
    points ->
      "{"
        ++ unwords
          [ show slot ++ ":" ++ take 8 (prettyEbHash h) ++ kindTag k
          | (MkLeiosPoint slot h, k) <- points
          ]
        ++ "}"
  kindTag = \case
    TxsClosureNotAlsoOffered -> "b"
    TxsClosureAlsoOffered -> "c"

prettyLeiosOutstanding :: LeiosOutstanding pid -> String
prettyLeiosOutstanding x =
  unlines $
    map ("    [leios] " ++) $
      [ "ebState = " ++ show (Map.size ebState)
      , "missingEbBodies = " ++ show (Map.size missingEbBodies)
      , "reverseSlotIndexByEbHash = " ++ show (Map.size reverseSlotIndexByEbHash)
      , "requestedEbPeers = " ++ unwords (map prettyEbHash (Map.keys requestedEbPeers))
      , "requestedBytesSizePerPeer = " ++ show (Map.elems requestedBytesSizePerPeer)
      , ""
      ]
 where
  MkLeiosOutstanding
    { ebState
    , missingEbBodies
    , reverseSlotIndexByEbHash
    , requestedEbPeers
    , requestedBytesSizePerPeer
    } = x

-- TODO which of these limits are allowed to be exceeded by at most one
-- request?
data LeiosFetchStaticEnv = MkLeiosFetchStaticEnv
  { maxRequestedBytesSizePerPeer :: BytesSize
  -- ^ At most this many outstanding bytes requested from each non-big-ledger
  -- peer
  , maxRequestBytesSize :: BytesSize
  -- ^ At most this many outstanding bytes per request
  , maxJobBytesSize :: BytesSize
  -- ^ At most this many bytes of txs per job
  , maxJobTxCount :: Int
  -- ^ At most this many txs per job
  , fetchPriorityWindowSlots :: Word64
  -- ^ @L = 3*L_hdr + L_vote + L_diff@ (in slots): the window, ending at the
  -- current slot, of EBs still worth voting on. Fetch prioritisation inverts to
  -- oldest-first within it (see @fetchPriorityTiers@). ~14s on mainnet, ~10 on
  -- the testnet.
  --
  -- TODO these are Leios protocol parameters (@L_hdr@/@L_vote@/@L_diff@) that
  -- should be read from the ledger state and can change via on-chain governance;
  -- static stub for now.
  , maxLeiosNotifyIngressQueue :: BytesSize
  -- ^ @maximumIngressQueue@ for LeiosNotify
  , maxLeiosFetchIngressQueue :: BytesSize
  -- ^ @maximumIngressQueue@ for LeiosFetch. This is the concrete bound from
  -- which 'maxRequestedBytesSizePerBigLedgerPeer' is derived: the scheduler
  -- must never leave more requested-but-unconsumed response bytes outstanding
  -- than this queue can hold, else the mux tears the connection down (see
  -- @Network.Mux.Ingress@'s @IngressQueueOverRun@).
  }

demoLeiosFetchStaticEnv :: LeiosFetchStaticEnv
demoLeiosFetchStaticEnv =
  MkLeiosFetchStaticEnv
    { maxRequestedBytesSizePerPeer = 5 * million
    , maxRequestBytesSize = 500 * thousand
    , maxJobBytesSize = 64 * thousandBase2
    , maxJobTxCount = 20000   -- TODO do we want this to be low enough to matter?
    , fetchPriorityWindowSlots = 10   -- TODO read dynamically from ledger state
    , maxLeiosNotifyIngressQueue = 1 * millionBase2
    , maxLeiosFetchIngressQueue = 5 * 12 * millionBase2
    }
 where
  million :: Num a => a
  million = 10 ^ (6 :: Int)
  millionBase2 :: Num a => a
  millionBase2 = 2 ^ (20 :: Int)
  thousand :: Num a => a
  thousand = 10 ^ (3 :: Int)
  thousandBase2 :: Num a => a
  thousandBase2 = 2 ^ (10 :: Int)

-- | At most this many outstanding bytes requested from each big-ledger peer.
--
-- Derived from the concrete lower-level bound 'maxLeiosFetchIngressQueue': the
-- scheduler may leave outstanding as many requested tx bytes as the LeiosFetch
-- ingress queue can hold. The on-the-wire message framing that sits atop those
-- tx bytes is covered by the +10% @addSafetyMargin@ the mux wiring applies to
-- 'maxLeiosFetchIngressQueue' when it sets the actual @maximumIngressQueue@, so
-- a full budget's worth of tx bytes plus framing still fits.
--
-- Larger than 'maxRequestedBytesSizePerPeer' so a high-stake peer can be asked
-- for multiple whole EB closures at once, but still bounded (by the ingress
-- queue) so an adversarial peer can't drown us.
maxRequestedBytesSizePerBigLedgerPeer :: LeiosFetchStaticEnv -> BytesSize
maxRequestedBytesSizePerBigLedgerPeer = maxLeiosFetchIngressQueue

-- * LeiosTx newtype

-- | A wrapper around transaction bytes for the simple purpose of serving them.
-- This typically contains a CBOR-encoded 'Tx era'.
newtype LeiosTx = MkLeiosTx {cbor :: ByteString}
  deriving Show

instance ShowProxy LeiosTx where showProxy _ = "LeiosTx"

-- | Uses cbor-in-cbor to allow for not needing to decode into a 'Tx era'.
encodeLeiosTx :: LeiosTx -> Encoding
encodeLeiosTx MkLeiosTx{cbor} =
  CBOR.encodeBytes cbor

-- | Relies on cbor-in-cbor to allow for not needing to decode into a 'Tx era'.
decodeLeiosTx :: Decoder s LeiosTx
decodeLeiosTx =
  MkLeiosTx <$> CBOR.decodeBytes

hashLeiosTx :: LeiosTx -> TxHash
hashLeiosTx =
  MkTxHash . Hash.hashToBytes . Hash.hashWith @HASH cbor

-- * Endorser Block

-- | An Endorser Block as it is submitted through the network.
-- TODO: Keep track of the slot of an EB?
data LeiosEb = MkLeiosEb
  { leiosEbTxs :: !(Vector (TxHash, BytesSize))
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NoThunks

-- | A newly forged 'LeiosEb' that includes the whole closure of endorsed
-- transactions.
data ForgedLeiosEb = ForgedLeiosEb
  { point :: !LeiosPoint
  , body :: !LeiosEb
  , txClosure :: ![(TxHash, ByteString)]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NoThunks

instance ShowProxy LeiosEb where showProxy _ = "LeiosEb"

forgeLeiosEb :: EraTx era => SlotNo -> NonEmpty (Tx TopTx era) -> ForgedLeiosEb
forgeLeiosEb slot txs =
  ForgedLeiosEb{point, body, txClosure}
 where
  point = MkLeiosPoint slot (hashLeiosEb body)

  body =
    serializedTxs
      & map (\(hash, size, _) -> (hash, size))
      & V.fromList
      & MkLeiosEb

  txClosure =
    serializedTxs
      & map (\(hash, _, bytes) -> (hash, bytes))
      & toList

  hashTx =
    MkTxHash . Hash.hashToBytes . Hash.hashWithSerialiser @HASH toCBOR

  serializedTxs =
    [ (hashTx tx, byteSize, bytes)
    | tx <- toList txs
    , let bytes = serialize' tx
    , let byteSize = fromIntegral $ BS.length bytes
    ]

leiosEbBodyItems :: LeiosEb -> [(Int, TxHash, BytesSize)]
leiosEbBodyItems eb =
  leiosEbTxs eb
    & V.imap (\ix (txh, size) -> (ix, txh, size))
    & toList

leiosEbBytesSize :: LeiosEb -> BytesSize
leiosEbBytesSize (MkLeiosEb items) =
  cborIntBytesSize (length items) + sum (fmap (each . snd) items)
 where
  each sz = cborBytesSize 32 + cborIntBytesSize sz

  cborBytesSize len = cborIntBytesSize len + len

-- | Length of a unsigned integer if it were encoded in a "flattened format".
-- See 'encodeInteger'.
cborIntBytesSize :: Integral i => i -> BytesSize
cborIntBytesSize n
  | n < 24 = 1
  | n < 0x100 = 2
  | n < 0x10000 = 3
  | otherwise = 5

hashLeiosEb :: LeiosEb -> EbHash
hashLeiosEb =
  MkEbHash . Hash.hashToBytes . Hash.hashWith @HASH id . serialize' . encodeLeiosEb

encodeLeiosEb :: LeiosEb -> Encoding
encodeLeiosEb (MkLeiosEb v) =
  foldl
    ( \acc (MkTxHash bytes, txBytesSize) ->
        acc <> CBOR.encodeBytes bytes <> CBOR.encodeWord32 txBytesSize
    )
    (CBOR.encodeMapLen $ fromIntegral $ length v)
    v

decodeLeiosEb :: Decoder s LeiosEb
decodeLeiosEb = do
  n <- CBOR.decodeMapLen
  -- TODO does V.generateM allocate exacly one buffer, via the hint?
  --
  -- If not, we could do so manually by relying on the fact that Decoder is
  -- ultimate in ST.
  fmap MkLeiosEb $ V.generateM n $ \_i -> do
    (,) <$> (fmap MkTxHash CBOR.decodeBytes) <*> CBOR.decodeWord32

-- | An EB body as its canonical CBOR bytes: the @b@ stored in the
-- 'LeiosTxCache' index. Its 'LeiosTxCache.API.ReferencesTxsByHash' instance
-- (defined alongside the class in "LeiosTxCache.API") decodes it to enumerate the
-- referenced txs.
newtype SerializedEbBody = MkSerializedEbBody SBS.ShortByteString

serializeEbBody :: LeiosEb -> SerializedEbBody
serializeEbBody = MkSerializedEbBody . SBS.toShort . toStrictByteString . encodeLeiosEb

-- * Voting

-- | Select the voting committee from a stake (weight) distribution per CIP-164:
-- order by stake descending and take the shortest prefix whose cumulative stake
-- reaches @target@ (σ_c).
--
-- CIP-164 breaks equal-stake ties by ascending pool id, which we satisfy only
-- because 'sortOn' is stable and callers pass 'Data.Map.elems' (already in
-- ascending key order); nothing here enforces it.
--
-- TODO: make the tie-break explicit by threading the pool id through and sorting
-- on @(Down w, k)@ under an @Ord a@ constraint, so an unstable sort or a caller
-- switching to an unordered container cannot silently renumber seats.
selectCommitteeByStake ::
  -- | The target coverage of weights / stake.
  Weight ->
  -- | All available voters weights.
  [(a, Weight)] ->
  -- | The selected committee weights.
  [(a, Weight)]
selectCommitteeByStake target = go 0 . sortOn (Down . snd)
 where
  go _ [] = []
  go acc (p : ps)
    | acc >= target = []
    | otherwise = p : go (acc + snd p) ps

-- ** Vote

-- | A vote in the Leios protocol.
data LeiosVote = MkLeiosVote
  { announcingRbHash :: RbHash
  -- ^ The message that gets signed, the hash of the ranking block
  --   that announced an endorser block.
  , voterId :: LeiosSeatId
  -- ^ Identity within a 'LeiosCommittee' who signed this vote.
  , voteSignature :: LeiosSignature
  -- ^ The cryptographic signature of the vote.
  }
  deriving (Generic, Eq, Show)

instance Ord LeiosVote where
  compare v1 v2 =
    compare v1.announcingRbHash v2.announcingRbHash
      <> compare v1.voterId v2.voterId

instance ShowProxy LeiosVote where showProxy _ = "LeiosVote"

-- | Encode a 'LeiosVote' into CBOR.
-- NOTE: Encodes points flat into the vote for smaller votes.
encodeLeiosVote :: LeiosVote -> Encoding
encodeLeiosVote MkLeiosVote{announcingRbHash, voterId, voteSignature} =
  CBOR.encodeListLen 3
    <> encodeRbHash announcingRbHash
    <> CBOR.encodeWord16 voterId.leiosSeatIndex
    <> encodeFixedSized voteSignature

-- | Dedoe a 'LeiosVote' from CBOR.
decodeLeiosVote :: Decoder s LeiosVote
decodeLeiosVote = do
  enforceSize (fromString "LeiosVote") 3
  pointRbHash <- decodeRbHash
  voterId <- LeiosSeatId <$> CBOR.decodeWord16
  voteSignature <- decodeFixedSized
  pure
    MkLeiosVote
      { announcingRbHash = pointRbHash
      , voterId
      , voteSignature
      }

voteToObject :: LeiosVote -> Aeson.Object
voteToObject MkLeiosVote{announcingRbHash, voterId} =
  mconcat
    [ "rbHash" .= prettyRbHash announcingRbHash
    , "voterId" .= voterId.leiosSeatIndex
    ]

-- | Create a vote for given 'LeiosPoint' and signing key.
signLeiosVote :: LeiosSigningKey -> LeiosSeatId -> RbHash -> LeiosVote
signLeiosVote sk voterId announcingRbHash =
  MkLeiosVote
    { announcingRbHash
    , voterId
    , voteSignature = signDSIGN leiosSignContext announcingRbHash sk
    }

-- | Validate a 'LeiosVote' against a selected 'Commitee'.
validateLeiosVote :: LeiosCommittee -> LeiosVote -> Either VoteInvalid Weight
validateLeiosVote committee MkLeiosVote{announcingRbHash, voterId, voteSignature} =
  case resolveLeiosSeat committee voterId of
    Nothing -> Left SignerNotInCommittee
    Just seat ->
      case seat.seatVKey of
        SNothing -> Left SignerHasNoKey
        SJust vk ->
          case verifyDSIGN leiosSignContext vk announcingRbHash voteSignature of
            Left _ -> Left InvalidSignature
            Right () -> Right seat.seatWeight

data VoteInvalid
  = InvalidSignature
  | SignerNotInCommittee
  | SignerHasNoKey
  deriving (Eq, Show)

-- | Why a CertRB was rejected during ledger validation of its Leios
-- certificate.
--
-- Enumerated rather than rendered to a 'String' so each case is a distinct,
-- monitorable outcome (e.g. for Grafana) and carries enough context to diagnose
-- without extra forensics. Temporary prototype stand-in: once the ledger
-- interface is Leios-aware an invalid cert becomes expressible as an ordinary
-- 'LedgerError', at which point this type (and the 'ExtValidationErrorLeios'
-- constructor that wraps it) can be removed.
data LeiosExtValidationError
  = -- | The block carries a Leios cert but its predecessor announced no EB, so
    -- there is nothing for the cert to certify.
    LeiosCertificateWithoutAnnouncement !LeiosCert
  | -- | A CertRB reached ledger validation in an era/state with no Leios
    -- committee to verify the cert against.
    LeiosMissingCommittee !LeiosPoint !LeiosCert
  | -- | A CertRB whose announcing ranking block could not be determined; it
    -- would be certifying against genesis.
    LeiosCertificateAfterGenesis !LeiosCert !LeiosPoint
  | -- | The certificate failed committee / threshold / signature verification.
    LeiosInvalidCertificate !LeiosCert !LeiosPoint !RbHash !VerificationError
  deriving stock (Eq, Show, Generic)

deriving via
  OnlyCheckWhnfNamed "LeiosExtValidationError" LeiosExtValidationError
  instance
    NoThunks LeiosExtValidationError

-- * Era-level Leios dispatch

-- | Per-era hooks for Leios voting and CertRB admission. Default
-- methods make this a no-op for non-Leios eras.
--
-- Lives here rather than in 'LeiosVoting' so it can be referenced from
-- the LedgerDB layer ('applyBlock') without pulling 'ChainDB' (which
-- 'runLeiosVoting' depends on) into scope.
class HasLeiosVoting blk where
  -- | The voting committee for the given (pre-tick) ledger state, or
  -- 'Nothing' if the era does not participate in Leios voting.
  getLeiosCommittee :: LedgerState blk EmptyMK -> Maybe LeiosCommittee
  getLeiosCommittee _ = Nothing

-- * Tracing

messageLeiosNotifyToObject ::
  -- | Extracts the announced EB point and body size from the relayed RB
  -- header, so an announcement's diffusion can be correlated with its EB.
  (announcement -> Maybe (LeiosPoint, BytesSize)) ->
  Message (LeiosNotify LeiosPoint announcement LeiosVote) st st' ->
  Aeson.Object
messageLeiosNotifyToObject announcedEb = \case
  MsgLeiosNotificationRequestNext ->
    mconcat
      [ "kind" .= Aeson.String "MsgLeiosNotificationRequestNext"
      ]
  MsgLeiosBlockAnnouncement announcement ->
    mconcat $
      "kind" .= Aeson.String "MsgLeiosBlockAnnouncement"
        : case announcedEb announcement of
          Nothing -> []
          Just (MkLeiosPoint ebSlot ebHash, ebBodySize) ->
            [ "ebSlot" .= ebSlot
            , "ebHash" .= prettyEbHash ebHash
            , "ebBodySize" .= ebBodySize
            ]
  MsgLeiosBlockOffer (MkLeiosPoint ebSlot ebHash) ebBytesSize ->
    mconcat
      [ "kind" .= Aeson.String "MsgLeiosBlockOffer"
      , "ebSlot" .= ebSlot
      , "ebHash" .= prettyEbHash ebHash
      , "ebBytesSize" .= ebBytesSize
      ]
  MsgLeiosBlockTxsOffer (MkLeiosPoint ebSlot ebHash) ->
    mconcat
      [ "kind" .= Aeson.String "MsgLeiosBlockTxsOffer"
      , "ebSlot" .= ebSlot
      , "ebHash" .= prettyEbHash ebHash
      ]
  MsgLeiosVotes votes ->
    mconcat
      [ "kind" .= Aeson.String "MsgLeiosVotes"
      , "votes" .= fmap voteToObject votes
      ]
  LeiosNotify.MsgDone ->
    mconcat
      [ "kind" .= Aeson.String "MsgDone"
      ]

messageLeiosFetchToObject ::
  Message (LeiosFetch LeiosPoint LeiosEb LeiosTx) st st' ->
  Aeson.Object
messageLeiosFetchToObject = \case
  MsgLeiosBlockRequest (MkLeiosPoint ebSlot ebHash) ->
    mconcat
      [ "kind" .= Aeson.String "MsgLeiosBlockRequest"
      , "ebSlot" .= ebSlot
      , "ebHash" .= prettyEbHash ebHash
      ]
  MsgLeiosBlock eb ->
    mconcat
      [ "kind" .= Aeson.String "MsgLeiosBlock"
      , "ebHash" .= prettyEbHash (hashLeiosEb eb)
      , "ebBytesSize" .= Aeson.Number (fromIntegral $ leiosEbBytesSize eb)
      ]
  MsgLeiosBlockTxsRequest (MkLeiosPoint ebSlot ebHash) bitmaps ->
    mconcat
      [ "kind" .= Aeson.String "MsgLeiosBlockTxsRequest"
      , "ebSlot" .= ebSlot
      , "ebHash" .= prettyEbHash ebHash
      , "numTxs" .= Aeson.Number (fromIntegral $ sum $ map (Bits.popCount . snd) bitmaps)
      , "bitmaps" .= map prettyBitmap bitmaps
      ]
  MsgLeiosBlockTxs (MkLeiosPoint ebSlot ebHash) bitmaps txs ->
    mconcat
      [ "kind" .= Aeson.String "MsgLeiosBlockTxs"
      , "numTxs" .= Aeson.Number (fromIntegral (length txs))
      , "txsBytesSize" .= Aeson.Number (fromIntegral $ sum $ fmap (BS.length . cbor) txs)
      , "ebSlot" .= ebSlot
      , "ebHash" .= prettyEbHash ebHash
      , "bitmaps" .= map prettyBitmap bitmaps
      ]
  LeiosFetch.MsgDone ->
    "kind" .= Aeson.String "MsgDone"

-- | Summary of an EB body inserted into the LeiosTxCache, for observability (see
-- 'TraceLeiosBodyHits'). The counts nest:
-- @ibsTxsInEb >= ibsTracked >= ibsAcquired >= ibsValidated@.
data LeiosTxCacheInsertBodySummary = MkLeiosTxCacheInsertBodySummary
  { ibsTxsInEb :: !Int
  -- ^ txs the EB body references
  , ibsTracked :: !Int
  -- ^ of those, how many the cache already tracked
  , ibsAcquired :: !Int
  -- ^ of those tracked, how many were already acquired (inserted or validated)
  , ibsValidated :: !Int
  -- ^ of those acquired, how many were already validated
  , ibsCacheTxCount :: !Int
  -- ^ total txs the cache tracks after this insert
  , ibsCacheLoad :: !Double
  -- ^ 'ibsCacheTxCount' as a fraction of the worst-case cache capacity
  }
  deriving (Eq, Show)

data TraceLeiosKernel
  = MkTraceLeiosKernel String
  | -- | An EB body was first acquired
    --
    -- Carries how old the EB was on arrival, if it was preceded by an
    -- announcement and not forged locally.
    TraceLeiosBlockAcquired LeiosPoint (Maybe NominalDiffTime)
  | -- | The EB body was received but the point was not in the database. This is
    -- unexpected as the point should have been inserted during announcement handling.
    TraceLeiosBlockPointMissing LeiosPoint
  | -- | An EB's tx closure was first completed. Carries the EB's age on arrival,
    -- as for 'TraceLeiosBlockAcquired'.
    TraceLeiosBlockTxsAcquired LeiosPoint (Maybe NominalDiffTime)
  | -- | An EB body was inserted into the LeiosTxCache
    --
    -- Carries the LeiosTxCache summary (cache hits), how many of its txs we found
    -- in our local mempool (the /full/ mempool-resident count, which may overlap
    -- the cache hits), and how many were in /neither/ and so must be fetched. The
    -- combined cache+mempool hit rate is thus @(txsInEb - missedBoth) \/ txsInEb@;
    -- using @missedBoth@ avoids double-counting the mempool\/cache overlap. The two
    -- 'Int's are the mempool count then @missedBoth@.
    TraceLeiosBodyHits LeiosPoint LeiosTxCacheInsertBodySummary Int Int
  | forall m. (Show m, TxMeasureMetrics m) => TraceLeiosBlockForged
      { slot :: SlotNo
      , eb :: LeiosEb
      , ebMeasure :: m
      , mempoolRestMeasure :: m
      }
  | TraceLeiosBlockStored {slot :: SlotNo, eb :: LeiosEb}
  | -- | An RB header announces a freshly-forged EB on this chain.
    -- Lets downstream consumers (e.g. the visualizer) attach the EB to
    -- the announcing RB without having to correlate by timing.
    TraceLeiosBlockAnnounced
      { announcingRbHashBytes :: ByteString
      , announcedEbPoint :: LeiosPoint
      }
  | -- NOTE: We avoid 'Header blk' or 'Point blk' here and a slot should be
    -- sufficient because it the certying block must be directly succeeding the
    -- forging/announcing anyways.
    TraceLeiosBlockCertified {atSlot :: SlotNo, certifiedPoint :: LeiosPoint}
  | TraceLeiosVoted {vote :: LeiosVote, weight :: Weight}
  | TraceLeiosVoteAcquired {vote :: LeiosVote}
  | TraceLeiosCertified {rbHash :: RbHash}
  | -- | An 'AcquiredEbTxs' notification arrived but 'runLeiosVoting' chose
    -- not to cast a vote; the reason identifies which precondition failed.
    TraceLeiosNotVoted {ebPoint :: LeiosPoint, reason :: LeiosNotVotedReason}
  | TraceLeiosDbException LeiosDbException
  | TraceLeiosDb TraceLeiosDb
  | -- | A forged RB both certifies an EB and announce a new one
    TraceLeiosCertifiedAndAnnounced {atSlot :: SlotNo, rbHash :: RbHash}
  | -- | The node accepted a new EB announcement, deduplicated across all peers
    -- and its own block forging (see 'AnnouncementSource').
    TraceLeiosAnnouncementAccepted
      !AnnouncementSource
      !AnnouncementEquivocation
      !AnnouncementFields
      -- | How late the announcement was — seconds from the election slot's
      -- wall-clock onset to when this node counted it — when known (relayed
      -- announcements carry it; a locally-forged one does not).
      !(Maybe NominalDiffTime)
  | -- | An arriving 'MsgLeiosBlock' (EB body) from an upstream peer
    TraceLeiosFetchBodyArrival !FetchArrivalBytes
  | -- | An arriving 'MsgLeiosBlockTxs' (tx batch) from an upstream peer
    TraceLeiosFetchTxsArrival !FetchArrivalBytes
  | -- | One completed iteration of the LeiosFetch decision logic: its wall-clock
    -- duration and a size sample of the resulting 'LeiosOutstanding' state.
    TraceLeiosFetchDecision !NominalDiffTime !LeiosOutstandingStats !LeiosDecisionStats

-- | The data of a relayed EB announcement, shared by 'TraceLeiosPeerAnnouncement'
-- and 'TraceLeiosAnnouncementAccepted'. A separate record so its selectors are
-- total rather than partial over the trace sum types.
data AnnouncementFields = MkAnnouncementFields
  { announcementElection :: !ElId
  , announcementEbHash :: !EbHash
  , announcementEbBodySize :: !BytesSize
  }
  deriving (Eq, Show)

-- | The bytes of one LeiosFetch arrival ('MsgLeiosBlock' or 'MsgLeiosBlockTxs'),
-- partitioned by the arriving item's /prior/ state in the LeiosTxCache. The four
-- fields sum to the message's total size.
--
-- See 'TraceLeiosFetchBodyArrival' and 'TraceLeiosFetchTxsArrival'.
data FetchArrivalBytes = MkFetchArrivalBytes
  { fabInvalid :: !BytesSize
  -- ^ Bytes of an invalid message (the whole message).
  , fabEvicted :: !BytesSize
  -- ^ Bytes whose prior cache state was absent (assumed present once, since evicted).
  , fabGood :: !BytesSize
  -- ^ Bytes in the expected not-yet-inserted state.
  , fabExtra :: !BytesSize
  -- ^ Bytes already inserted (redundant).
  }
  deriving (Eq, Show)

instance Semigroup FetchArrivalBytes where
  MkFetchArrivalBytes a1 b1 c1 d1 <> MkFetchArrivalBytes a2 b2 c2 d2 =
    MkFetchArrivalBytes (a1 + a2) (b1 + b2) (c1 + c2) (d1 + d2)

instance Monoid FetchArrivalBytes where
  mempty = MkFetchArrivalBytes 0 0 0 0

-- | The message's whole size attributed to a single bucket, the rest zero.
fetchArrivalInvalid, fetchArrivalEvicted, fetchArrivalGood, fetchArrivalExtra ::
  BytesSize -> FetchArrivalBytes
fetchArrivalInvalid n = mempty{fabInvalid = n}
fetchArrivalEvicted n = mempty{fabEvicted = n}
fetchArrivalGood n = mempty{fabGood = n}
fetchArrivalExtra n = mempty{fabExtra = n}

-- | Whether the accepted announcement equivocates: a second, distinct header
-- announcing an election that a prior header already announced. (The two
-- headers can even announce the same EB hash and size and still equivocate,
-- since it is the header, not the EB, that carries the election.) The flag
-- spares a log consumer from statefully correlating the two announcements.
data AnnouncementEquivocation
  = NoEquivocation
  | Equivocation
  deriving (Eq, Show)

-- | How the node came to accept an EB announcement.
data AnnouncementSource
  = -- | The node forged the EB itself.
    ForgedLocally
  | -- | An upstream peer relayed it over the LeiosNotify mini-protocol.
    ReceivedViaLeiosNotify
  | -- | It rode in on a ChainSync 'MsgRollForward' header (the announcing RB).
    ReceivedViaChainSync
  deriving (Eq, Show)

-- | Reasons 'runLeiosVoting' may decline to cast a vote after acquiring an
-- EB closure. See 'TraceLeiosNotVoted'.
data LeiosNotVotedReason
  = -- | The tip of the currently selected chain does not announce this EB.
    -- Either our chain hasn't caught up to the announcing RB yet, or the
    -- chain has extended past it, or the tip announces a different EB.
    ChainTipDoesNotAnnounce
  | -- | The vote deadline ('announcedSlot + 3 * L_hdr + L_vote') has
    -- already passed by the time we became eligible.
    TooLate
  | -- | We are not part of the current voting committee.
    NotOnCommittee
  deriving Show

deriving instance Show TraceLeiosKernel

traceLeiosKernelToObject :: TraceLeiosKernel -> Aeson.Object
traceLeiosKernelToObject = \case
  TraceLeiosFetchBodyArrival fab ->
    mconcat
      [ "kind" .= Aeson.String "LeiosFetchBodyArrival"
      , fabObject fab
      ]
  TraceLeiosFetchTxsArrival fab ->
    mconcat
      [ "kind" .= Aeson.String "LeiosFetchTxsArrival"
      , fabObject fab
      ]
  TraceLeiosFetchDecision d stats dec ->
    let inflight = summarizePeerDist (losInflightBytesDesc stats)
        offers = summarizePeerDist (losOffersDesc stats)
     in mconcat
          [ "kind" .= Aeson.String "LeiosFetchDecision"
          , "durationSeconds" .= (realToFrac d :: Double)
          , "durationMillis" .= (realToFrac d * 1000 :: Double)
          , "decisionPeers" .= ldsPeers dec
          , "decisionRequests" .= ldsRequests dec
          , "decisionBodyRequests" .= ldsBodyRequests dec
          , "decisionJobs" .= ldsJobs dec
          , "decisionBodyBytes" .= ldsBodyBytes dec
          , "decisionTxBytes" .= ldsTxBytes dec
          , "tracked" .= losTracked stats
          , "missingBodies" .= losMissingBodies stats
          , "peersInflight" .= losPeersInflight stats
          , "inflightNonzeroCount" .= pdsNonzeroCount inflight
          , "inflightTotal" .= pdsTotal inflight
          , "inflightTop1" .= pdsTop1 inflight
          , "inflightTop2" .= pdsTop2 inflight
          , "inflightTop3" .= pdsTop3 inflight
          , "inflightTop4" .= pdsTop4 inflight
          , "inflightTop5" .= pdsTop5 inflight
          , "inflightNonzeroMedian" .= pdsNonzeroMedian inflight
          , "inflightDesc" .= V.toList (losInflightBytesDesc stats)
          , "offersNonzeroCount" .= pdsNonzeroCount offers
          , "offersTotal" .= pdsTotal offers
          , "offersTop1" .= pdsTop1 offers
          , "offersTop2" .= pdsTop2 offers
          , "offersTop3" .= pdsTop3 offers
          , "offersTop4" .= pdsTop4 offers
          , "offersTop5" .= pdsTop5 offers
          , "offersNonzeroMedian" .= pdsNonzeroMedian offers
          , "offersDesc" .= V.toList (losOffersDesc stats)
          ]
  MkTraceLeiosKernel s ->
    mconcat
      [ "kind" .= Aeson.String "LeiosKernelMsg"
      , "msg" .= s
      ]
  TraceLeiosBlockAcquired (MkLeiosPoint (SlotNo ebSlot) ebHash) mbAge ->
    mconcat $
      [ "kind" .= Aeson.String "LeiosBlockAcquired"
      , "ebHash" .= prettyEbHash ebHash
      , "ebSlot" .= ebSlot
      ]
        ++ foldMap (\age -> ["bodyAgeSeconds" .= (realToFrac age :: Double)]) mbAge
  TraceLeiosBlockPointMissing (MkLeiosPoint (SlotNo ebSlot) ebHash) ->
    mconcat
      [ "kind" .= Aeson.String "LeiosBlockPointMissing"
      , "ebHash" .= prettyEbHash ebHash
      , "ebSlot" .= ebSlot
      ]
  TraceLeiosBlockTxsAcquired (MkLeiosPoint (SlotNo ebSlot) ebHash) mbAge ->
    mconcat $
      [ "kind" .= Aeson.String "LeiosBlockTxsAcquired"
      , "ebHash" .= prettyEbHash ebHash
      , "ebSlot" .= ebSlot
      ]
        ++ foldMap (\age -> ["closureAgeSeconds" .= (realToFrac age :: Double)]) mbAge
  TraceLeiosBodyHits (MkLeiosPoint (SlotNo ebSlot) ebHash) ibs mempoolHits missedBoth ->
    mconcat
      [ "kind" .= Aeson.String "LeiosBodyHits"
      , "ebHash" .= prettyEbHash ebHash
      , "ebSlot" .= ebSlot
      , "txsInEb" .= ibsTxsInEb ibs
      , "tracked" .= ibsTracked ibs
      , "acquired" .= ibsAcquired ibs
      , "validated" .= ibsValidated ibs
      , "mempoolHits" .= mempoolHits
      , "missedBoth" .= missedBoth
      , "cacheTxCount" .= ibsCacheTxCount ibs
      , "cacheLoad" .= ibsCacheLoad ibs
      ]
  TraceLeiosBlockForged{slot, eb, ebMeasure, mempoolRestMeasure} ->
    mconcat
      [ "kind" .= Aeson.String "LeiosBlockForged"
      , "slot" .= slot
      , "hash" .= prettyEbHash (hashLeiosEb eb)
      , "numTxs" .= length (leiosEbTxs eb)
      , "ebSize" .= leiosEbBytesSize eb
      , "closureSize" .= unByteSize32 (txMeasureMetricTxSizeBytes ebMeasure)
      , "mempoolRestSize" .= unByteSize32 (txMeasureMetricTxSizeBytes mempoolRestMeasure)
      ]
  TraceLeiosBlockStored{slot, eb} ->
    mconcat
      [ "kind" .= Aeson.String "LeiosBlockStored"
      , "slot" .= slot
      , "hash" .= prettyEbHash (hashLeiosEb eb)
      ]
  TraceLeiosBlockAnnounced{announcingRbHashBytes, announcedEbPoint} ->
    mconcat
      [ "kind" .= Aeson.String "LeiosBlockAnnounced"
      , "rbHash" .= BS8.unpack (BS16.encode announcingRbHashBytes)
      , "ebSlot" .= announcedEbPoint.pointSlotNo
      , "ebHash" .= prettyEbHash announcedEbPoint.pointEbHash
      ]
  TraceLeiosBlockCertified{atSlot, certifiedPoint} ->
    mconcat
      [ "kind" .= Aeson.String "LeiosBlockCertified"
      , "atSlot" .= atSlot
      , "ebSlot" .= certifiedPoint.pointSlotNo
      , "ebHash" .= prettyEbHash certifiedPoint.pointEbHash
      ]
  TraceLeiosVoted{vote, weight} ->
    mconcat
      [ "kind" .= Aeson.String "LeiosVoted"
      , "vote" .= voteToObject vote
      , -- NOTE: 1 ADA delegation is 2.2 × 10^-11 of the total stake. So 10^-12
        -- is reasonable precision here.
        "weight" .= fromRational @Pico weight
      ]
  TraceLeiosVoteAcquired{vote} ->
    mconcat
      [ "kind" .= Aeson.String "LeiosVoteAcquired"
      , "vote" .= voteToObject vote
      ]
  TraceLeiosCertified{rbHash = announcingRbHash} ->
    mconcat
      [ "kind" .= Aeson.String "LeiosCertified"
      , "rbHash" .= prettyRbHash announcingRbHash
      ]
  TraceLeiosNotVoted{ebPoint = MkLeiosPoint (SlotNo ebSlot) ebHash, reason} ->
    mconcat
      [ "kind" .= Aeson.String "LeiosNotVoted"
      , "ebHash" .= prettyEbHash ebHash
      , "ebSlot" .= ebSlot
      , "reason" .= notVotedReasonText reason
      ]
  TraceLeiosDbException e ->
    jsonLeiosDbException e
  TraceLeiosDb (TraceLeiosDbInsertCollision table key) ->
    mconcat
      [ "kind" .= Aeson.String "LeiosDbInsertCollision"
      , "table" .= table
      , "key" .= key
      ]
  TraceLeiosCertifiedAndAnnounced slotNo rbHash ->
    mconcat
      [ "kind" .= Aeson.String "LeiosCertifiedAndAnnounced"
      , "slotNo" .= slotNo
      , "rbHash" .= prettyRbHash rbHash
      ]
  TraceLeiosAnnouncementAccepted announcementSource equivocation acc mbAge ->
    mconcat $
      [ "kind" .= Aeson.String "LeiosAnnouncementAccepted"
      , "source" .= announcementSourceText announcementSource
      , announcementFieldsToObject acc
      , announcementEquivocationToObject equivocation
      ]
        ++ foldMap (\age -> ["announcementAgeSeconds" .= (realToFrac age :: Double)]) mbAge
  where
    fabObject fab =
      mconcat
        [ "invalidBytes" .= fabInvalid fab
        , "evictedBytes" .= fabEvicted fab
        , "goodBytes" .= fabGood fab
        , "extraBytes" .= fabExtra fab
        ]

announcementFieldsToObject :: AnnouncementFields -> Aeson.Object
announcementFieldsToObject
  (MkAnnouncementFields (MkElId (SlotNo electionSlot) poolId) ebHash ebBodySize) =
    mconcat
      [ "electionSlot" .= electionSlot
      , "electionPool" .= BS8.unpack (BS16.encode (SBS.fromShort poolId))
      , "ebHash" .= prettyEbHash ebHash
      , "ebBodySize" .= ebBodySize
      ]

announcementEquivocationToObject :: AnnouncementEquivocation -> Aeson.Object
announcementEquivocationToObject = \case
  NoEquivocation -> "equivocation" .= False
  Equivocation -> "equivocation" .= True

announcementSourceText :: AnnouncementSource -> Aeson.Value
announcementSourceText = \case
  ForgedLocally -> Aeson.String "forgedLocally"
  ReceivedViaLeiosNotify -> Aeson.String "receivedViaLeiosNotify"
  ReceivedViaChainSync -> Aeson.String "receivedViaChainSync"

notVotedReasonText :: LeiosNotVotedReason -> Aeson.Value
notVotedReasonText = \case
  ChainTipDoesNotAnnounce -> Aeson.String "chainTipDoesNotAnnounce"
  TooLate -> Aeson.String "tooLate"
  NotOnCommittee -> Aeson.String "notOnCommittee"

data TraceLeiosPeer
  = MkTraceLeiosPeer String
  | TraceLeiosPeerDbException LeiosDbException
  | -- | This upstream peer relayed a valid, newly-counted EB announcement.
    TraceLeiosPeerAnnouncement !AnnouncementEquivocation !AnnouncementFields
  deriving Show

traceLeiosPeerToObject :: TraceLeiosPeer -> Aeson.Object
traceLeiosPeerToObject = \case
  MkTraceLeiosPeer s -> fromString "msg" .= Aeson.String (fromString s)
  TraceLeiosPeerDbException e -> jsonLeiosDbException e
  TraceLeiosPeerAnnouncement equivocation acc ->
    mconcat
      [ fromString "kind" .= Aeson.String "LeiosPeerAnnouncement"
      , announcementFieldsToObject acc
      , announcementEquivocationToObject equivocation
      ]

-- * Protocol parameters

maxMsgLeiosBlockBytesSize :: BytesSize
maxMsgLeiosBlockBytesSize = 500 * 10 ^ (3 :: Int) -- from CIP-0164's recommendations

minEbItemBytesSize :: BytesSize
minEbItemBytesSize = 32 + hashOverhead + minSizeOverhead
 where
  hashOverhead = 1 + 1 -- bytestring major byte + a length = 32
  minSizeOverhead = 1 + 1 -- int major byte + a value at low as 55

maxTxsPerEb :: Int
maxTxsPerEb =
  fromIntegral $
    (maxMsgLeiosBlockBytesSize - msgOverhead - sequenceOverhead)
      `div` minEbItemBytesSize
 where
  msgOverhead = 1 + 1 -- short list len + small word
  sequenceOverhead = 1 + 2 -- sequence major byte + a length > 255

maxEBClosureSize :: ByteSize32
maxEBClosureSize = ByteSize32 12_000_000

-- FIXME: This should actually be 14 if we follow the CIP-164 recommended
-- values.
minCertificationGap :: Word64
minCertificationGap = 10

-- | Minimum fraction of stake to create a valid 'LeiosCertificate'.
minCertificationThreshold :: Rational
minCertificationThreshold = 3 % 4

-- | Stake to be covered when selecting the committee.
-- TODO: Switch to a committee size parameter followin the CIP-164 discussions.
committeeStakeCoverage :: Weight
committeeStakeCoverage = 99 % 100

-- * Utilities for prototyping

-- | Like 'traceShow', but with pretty printing of the value.
{-# WARNING spy "Use for debugging purposes only" #-}
spy :: Show a => a -> a
spy a = trace (toString $ pShow a) a

-- | Like 'spy' but prefixed with a label.
{-# WARNING spy' "Use for debugging purposes only" #-}
spy' :: Show a => String -> a -> a
spy' msg a = trace (msg <> ": " <> toString (pShow a)) a
