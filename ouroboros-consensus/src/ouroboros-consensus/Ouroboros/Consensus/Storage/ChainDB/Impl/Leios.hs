{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | What the ChainDB knows about Leios endorser blocks: which closures it has
-- acquired, and so which blocks certifying an endorser block ChainSel may
-- select; and which claims some valid certificate has established.
module Ouroboros.Consensus.Storage.ChainDB.Impl.Leios
  ( -- * The Leios facts of a block
    leiosBlockInfo

    -- * Acquired closures
  , AcquiredLeiosEbs
  , mkAcquiredLeiosEbs
  , insertAcquiredLeiosEb
  , isAcquiredLeiosEb
  , pruneAcquiredLeiosEbs

    -- * Parking
  , ignoreParkedCertRB
  , ignoreParkedCertRBSuc

    -- * Valid claims
  , LeiosValidClaims
  , emptyLeiosValidClaims
  , insertLeiosValidClaim
  , isLeiosValidClaim
  , leiosCertifiedEbs
  , pruneLeiosValidClaims
  ) where

import Control.Monad (guard)
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..), maybeToStrictMaybe, strictMaybeToMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Set.NonEmpty (NESet)
import qualified Data.Set.NonEmpty as NESet
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Leios.Cert (LeiosClaim (..))
import Ouroboros.Consensus.Leios.Protocol
  ( LeiosAnnouncement (..)
  , LeiosProtocol (..)
  )
import Ouroboros.Consensus.Leios.Types
  ( ElectionId (..)
  , EndorserBlockHash
  , LeiosPoint (..)
  , RankingBlockHash
  )
import Ouroboros.Consensus.Storage.VolatileDB
  ( BlockInfo (..)
  , LeiosBlockInfo (..)
  )

{-------------------------------------------------------------------------------
  The Leios facts of a block
-------------------------------------------------------------------------------}

-- | What the block says about Leios endorser blocks, read off its header.
leiosBlockInfo ::
  forall blk.
  BlockSupportsProtocol blk =>
  BlockConfig blk ->
  blk ->
  LeiosBlockInfo
leiosBlockInfo bcfg blk =
  LeiosBlockInfo
    { lbiHasCert = getContainsLeiosCert p view
    , lbiAnnouncement = maybeToStrictMaybe $ getLeiosAnnouncement p view
    }
 where
  p = Proxy @(BlockProtocol blk)
  view = validateView bcfg (getHeader blk)

{-------------------------------------------------------------------------------
  Acquired closures
-------------------------------------------------------------------------------}

-- | The endorser block points whose closure the LeiosDB holds: those of the
-- @AcquiredEbTxs@ notifications (see @LeiosEbNotification@).
data AcquiredLeiosEbs = MkAcquiredLeiosEbs
  { alePoints :: !(Set LeiosPoint)
  , alePrunedSlot :: !SlotNo
  -- ^ The slot this has most recently been pruned to, so that a notification
  -- that arrives after the pruning cannot undo it.
  }
  deriving stock Generic
  deriving anyclass NoThunks

mkAcquiredLeiosEbs :: [LeiosPoint] -> AcquiredLeiosEbs
mkAcquiredLeiosEbs =
  List.foldl'
    (flip insertAcquiredLeiosEb)
    (MkAcquiredLeiosEbs Set.empty (SlotNo 0))

-- | Record that the closure of the endorser block at this point is acquired,
-- unless the point is older than what has been pruned.
insertAcquiredLeiosEb :: LeiosPoint -> AcquiredLeiosEbs -> AcquiredLeiosEbs
insertAcquiredLeiosEb point acquired@(MkAcquiredLeiosEbs points prunedSlot)
  | pointSlotNo point < prunedSlot = acquired
  | otherwise = MkAcquiredLeiosEbs (Set.insert point points) prunedSlot

isAcquiredLeiosEb :: LeiosPoint -> AcquiredLeiosEbs -> Bool
isAcquiredLeiosEb point = Set.member point . alePoints

-- | Forget every point older than the slot.
pruneAcquiredLeiosEbs :: SlotNo -> AcquiredLeiosEbs -> AcquiredLeiosEbs
pruneAcquiredLeiosEbs slot (MkAcquiredLeiosEbs points prunedSlot) =
  MkAcquiredLeiosEbs
    (Set.dropWhileAntitone ((< slot) . pointSlotNo) points)
    (max slot prunedSlot)

{-------------------------------------------------------------------------------
  Parking
-------------------------------------------------------------------------------}

-- | Whether the block contains a certificate of an endorser block whose closure
-- is not acquired. ChainSel must not select such a block until it is, so it
-- treats the block as absent: the block is parked.
--
-- The certified endorser block is the one the block's predecessor announces.
isParkedCertRB ::
  AcquiredLeiosEbs ->
  (HeaderHash blk -> Maybe (BlockInfo blk)) ->
  BlockInfo blk ->
  Bool
isParkedCertRB acquired lookupBlockInfo info
  | not (lbiHasCert (biLeios info)) = False
  | otherwise = case certified of
      Just point -> not (isAcquiredLeiosEb point acquired)
      -- Either the VolatileDB lacks the predecessor, so the block cannot
      -- be selected before it arrives anyway, or the predecessor (perhaps
      -- genesis) announces nothing, so the block can never be valid and
      -- staying parked for good is harmless.
      Nothing -> True
 where
  certified = case biPrevHash info of
    GenesisHash -> Nothing
    BlockHash prevHash -> do
      prev <- lookupBlockInfo prevHash
      announcement <- strictMaybeToMaybe (lbiAnnouncement (biLeios prev))
      pure $ MkLeiosPoint (biSlotNo prev) (announcementEbHash announcement)

-- | Hide parked blocks ('isParkedCertRB') from a @getBlockInfo@. The given one
-- must be the VolatileDB's own, so that it still finds every predecessor.
ignoreParkedCertRB ::
  AcquiredLeiosEbs ->
  (HeaderHash blk -> Maybe (BlockInfo blk)) ->
  (HeaderHash blk -> Maybe (BlockInfo blk))
ignoreParkedCertRB acquired lookupBlockInfo hash = do
  info <- lookupBlockInfo hash
  guard (not (isParkedCertRB acquired lookupBlockInfo info))
  pure info

-- | Hide parked blocks ('isParkedCertRB') from a @filterByPredecessor@. The
-- given @getBlockInfo@ must be the VolatileDB's own.
ignoreParkedCertRBSuc ::
  AcquiredLeiosEbs ->
  (HeaderHash blk -> Maybe (BlockInfo blk)) ->
  (ChainHash blk -> Set (HeaderHash blk)) ->
  (ChainHash blk -> Set (HeaderHash blk))
ignoreParkedCertRBSuc acquired lookupBlockInfo succsOf =
  Set.filter (not . isParked) . succsOf
 where
  isParked = maybe False (isParkedCertRB acquired lookupBlockInfo) . lookupBlockInfo

{-------------------------------------------------------------------------------
  Valid claims
-------------------------------------------------------------------------------}

-- | The blocks known to announce an endorser block that some valid certificate
-- certifies, each with its slot.
--
-- A block containing a certificate claims that the endorser block its
-- predecessor announced is certified. The claims that valid certificates have
-- established are how LeiosFetch learns which endorser blocks are certified,
-- for its Recovery Path. A block making an already established claim need not
-- have its certificate verified.
data LeiosValidClaims = LeiosValidClaims
  { lvcSlot :: !(Map RankingBlockHash SlotNo)
  , lvcBySlot :: !(Map SlotNo (NESet RankingBlockHash))
  -- ^ The inverse of 'lvcSlot', so that pruning costs only what it prunes.
  , lvcCertified :: !(Map ElectionId EndorserBlockHash)
  -- ^ The endorser block certified in each election whose claim was
  -- established with its announcement known.
  }
  deriving stock Generic
  deriving anyclass NoThunks

emptyLeiosValidClaims :: LeiosValidClaims
emptyLeiosValidClaims = LeiosValidClaims Map.empty Map.empty Map.empty

-- | Record that some valid certificate established the claim, whose announcing
-- block made this announcement, if known.
insertLeiosValidClaim ::
  LeiosClaim -> StrictMaybe LeiosAnnouncement -> LeiosValidClaims -> LeiosValidClaims
insertLeiosValidClaim (LeiosClaim slot rbHash) mbAnnouncement claims@(LeiosValidClaims slots bySlot certified)
  | Map.member rbHash slots = claims
  | otherwise =
      LeiosValidClaims
        (Map.insert rbHash slot slots)
        (Map.insertWith NESet.union slot (NESet.singleton rbHash) bySlot)
        ( case mbAnnouncement of
            SNothing -> certified
            SJust ann ->
              Map.insert
                (ElectionId slot (announcementElectee ann))
                (announcementEbHash ann)
                certified
        )

-- | The endorser block each election has had certified, as far as the
-- established claims tell.
leiosCertifiedEbs :: LeiosValidClaims -> Map ElectionId EndorserBlockHash
leiosCertifiedEbs = lvcCertified

isLeiosValidClaim :: RankingBlockHash -> LeiosValidClaims -> Bool
isLeiosValidClaim rbHash = Map.member rbHash . lvcSlot

-- | Forget every claim whose announcing block is older than the slot: once the
-- immutable tip has passed it, no block that ChainSel has yet to select can
-- make the claim.
pruneLeiosValidClaims :: SlotNo -> LeiosValidClaims -> LeiosValidClaims
pruneLeiosValidClaims slot (LeiosValidClaims slots bySlot certified) =
  LeiosValidClaims
    (slots `Map.withoutKeys` foldMap NESet.toSet pruned)
    kept
    -- 'ElectionId' orders by slot first.
    (Map.dropWhileAntitone ((< slot) . electionSlot) certified)
 where
  (pruned, kept) = Map.spanAntitone (< slot) bySlot
