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
  , pruneLeiosValidClaims
  ) where

import Control.Monad (guard)
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (maybeToStrictMaybe, strictMaybeToMaybe)
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
  ( EndorserBlockHash
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
    , lbiAnnouncedEb =
        maybeToStrictMaybe $ announcementEbHash <$> getLeiosAnnouncement p view
    }
 where
  p = Proxy @(BlockProtocol blk)
  view = validateView bcfg (getHeader blk)

{-------------------------------------------------------------------------------
  Acquired closures
-------------------------------------------------------------------------------}

-- | The endorser blocks whose closure the LeiosDB holds, each with the slot of
-- its youngest focus (see @LeiosEbNotification@).
--
-- That slot is the youngest slot of any 'LeiosPoint' in an @AcquiredEbTxs@
-- notification for the endorser block, since the LeiosDB sends that
-- notification at every focus not older than the ImmutableDB tip.
--
-- 'UnsafeAcquiredLeiosEbs' does not ensure that 'aleBySlot' is the inverse of
-- 'aleYoungestSlot'; build one with 'mkAcquiredLeiosEbs' instead.
data AcquiredLeiosEbs = UnsafeAcquiredLeiosEbs
  { aleYoungestSlot :: !(Map EndorserBlockHash SlotNo)
  , aleBySlot :: !(Map SlotNo (NESet EndorserBlockHash))
  -- ^ The inverse of 'aleYoungestSlot', so that pruning costs only what it
  -- prunes.
  }
  deriving stock Generic
  deriving anyclass NoThunks

mkAcquiredLeiosEbs :: [LeiosPoint] -> AcquiredLeiosEbs
mkAcquiredLeiosEbs =
  List.foldl' (flip insertAcquiredLeiosEb) (UnsafeAcquiredLeiosEbs Map.empty Map.empty)

-- | Record that the endorser block's closure is acquired, and that the given
-- point announces it.
insertAcquiredLeiosEb :: LeiosPoint -> AcquiredLeiosEbs -> AcquiredLeiosEbs
insertAcquiredLeiosEb (MkLeiosPoint slot ebHash) acquired@(UnsafeAcquiredLeiosEbs youngest bySlot) =
  case Map.lookup ebHash youngest of
    Nothing -> record bySlot
    Just prevSlot
      | slot <= prevSlot -> acquired
      | otherwise -> record (Map.update (NESet.nonEmptySet . NESet.delete ebHash) prevSlot bySlot)
 where
  record bySlot' =
    UnsafeAcquiredLeiosEbs
      (Map.insert ebHash slot youngest)
      (Map.insertWith NESet.union slot (NESet.singleton ebHash) bySlot')

isAcquiredLeiosEb :: EndorserBlockHash -> AcquiredLeiosEbs -> Bool
isAcquiredLeiosEb ebHash = Map.member ebHash . aleYoungestSlot

-- | Forget every endorser block all of whose foci are older than the slot. A
-- block that ChainSel has yet to select can then certify it only at another
-- announcement, and verifying that certificate makes the announcement a focus,
-- which records the endorser block here again.
--
-- See 'AcquiredLeiosEbs' for why its slots are the youngest foci.
pruneAcquiredLeiosEbs :: SlotNo -> AcquiredLeiosEbs -> AcquiredLeiosEbs
pruneAcquiredLeiosEbs slot (UnsafeAcquiredLeiosEbs youngest bySlot) =
  UnsafeAcquiredLeiosEbs
    (youngest `Map.withoutKeys` foldMap NESet.toSet pruned)
    kept
 where
  (pruned, kept) = Map.spanAntitone (< slot) bySlot

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
  | otherwise = case certifiedEb of
      Just ebHash -> not (isAcquiredLeiosEb ebHash acquired)
      -- Either the VolatileDB lacks the predecessor, so the block cannot
      -- be selected before it arrives anyway, or the predecessor (perhaps
      -- genesis) announces nothing, so the block can never be valid and
      -- staying parked for good is harmless.
      Nothing -> True
 where
  certifiedEb = case biPrevHash info of
    GenesisHash -> Nothing
    BlockHash prevHash ->
      lookupBlockInfo prevHash >>= strictMaybeToMaybe . lbiAnnouncedEb . biLeios

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
-- established are how LeiosFetch will learn which endorser blocks are
-- certified, for its Recovery Path; that part is not ported yet. Meanwhile, a
-- block making an already established claim need not have its certificate
-- verified.
data LeiosValidClaims = LeiosValidClaims
  { lvcSlot :: !(Map RankingBlockHash SlotNo)
  , lvcBySlot :: !(Map SlotNo (NESet RankingBlockHash))
  -- ^ The inverse of 'lvcSlot', so that pruning costs only what it prunes.
  }
  deriving stock Generic
  deriving anyclass NoThunks

emptyLeiosValidClaims :: LeiosValidClaims
emptyLeiosValidClaims = LeiosValidClaims Map.empty Map.empty

-- | Record that some valid certificate established the claim.
insertLeiosValidClaim :: LeiosClaim -> LeiosValidClaims -> LeiosValidClaims
insertLeiosValidClaim (LeiosClaim slot rbHash) claims@(LeiosValidClaims slots bySlot)
  | Map.member rbHash slots = claims
  | otherwise =
      LeiosValidClaims
        (Map.insert rbHash slot slots)
        (Map.insertWith NESet.union slot (NESet.singleton rbHash) bySlot)

isLeiosValidClaim :: RankingBlockHash -> LeiosValidClaims -> Bool
isLeiosValidClaim rbHash = Map.member rbHash . lvcSlot

-- | Forget every claim whose announcing block is older than the slot: once the
-- immutable tip has passed it, no block that ChainSel has yet to select can
-- make the claim.
pruneLeiosValidClaims :: SlotNo -> LeiosValidClaims -> LeiosValidClaims
pruneLeiosValidClaims slot (LeiosValidClaims slots bySlot) =
  LeiosValidClaims
    (slots `Map.withoutKeys` foldMap NESet.toSet pruned)
    kept
 where
  (pruned, kept) = Map.spanAntitone (< slot) bySlot
