{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | What the ChainDB knows about Leios endorser blocks: which closures it has
-- acquired, and so which blocks certifying an endorser block ChainSel may
-- select; which claims some valid certificate has established; and whether
-- the ImmutableDB's blocks have the endorser blocks they certify.
module Ouroboros.Consensus.Storage.ChainDB.Impl.Leios
  ( -- * The Leios facts of a block
    leiosBlockInfo
  , leiosHeaderInfo

    -- * Acquired closures
  , AcquiredEbs
  , mkAcquiredEbs
  , insertAcquiredEb
  , isAcquiredEb
  , pruneAcquiredEbs

    -- * Parking
  , ignoreParkedCertRB
  , ignoreParkedCertRBSuc

    -- * Valid claims
  , LeiosValidClaims
  , emptyLeiosValidClaims
  , insertLeiosValidClaim
  , isLeiosValidClaim
  , pruneLeiosValidClaims

    -- * Checking the ImmutableDB
  , certifiesPromotedEb
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
  ( EndorserBlockHash
  , LeiosPoint (..)
  , RankingBlockHash
  )
import Ouroboros.Consensus.Storage.ImmutableDB (Parent (..))
import Ouroboros.Consensus.Storage.LeiosDB.API (LeiosDbReader (..))
import Ouroboros.Consensus.Storage.VolatileDB
  ( BlockInfo (..)
  , LeiosBlockInfo (..)
  )

{-------------------------------------------------------------------------------
  The Leios facts of a block
-------------------------------------------------------------------------------}

-- | What the block says about Leios endorser blocks, read off its header.
leiosBlockInfo ::
  BlockSupportsProtocol blk =>
  BlockConfig blk ->
  blk ->
  LeiosBlockInfo
leiosBlockInfo bcfg = leiosHeaderInfo bcfg . getHeader

-- | What the header says about Leios endorser blocks.
leiosHeaderInfo ::
  forall blk.
  BlockSupportsProtocol blk =>
  BlockConfig blk ->
  Header blk ->
  LeiosBlockInfo
leiosHeaderInfo bcfg hdr =
  LeiosBlockInfo
    { lbiHasCert = getContainsLeiosCert p view
    , lbiAnnouncedEb =
        maybeToStrictMaybe $ announcementEbHash <$> getLeiosAnnouncement p view
    }
 where
  p = Proxy @(BlockProtocol blk)
  view = validateView bcfg hdr

{-------------------------------------------------------------------------------
  Acquired closures
-------------------------------------------------------------------------------}

-- | The endorser blocks whose closure the LeiosDB holds.
--
-- 'UnsafeAcquiredEbs' does not ensure that 'aleBySlot' is the inverse of
-- 'aleYoungestSlot'; build one with 'mkAcquiredEbs' instead.
data AcquiredEbs = UnsafeAcquiredEbs
  { aleYoungestSlot :: !(Map EndorserBlockHash SlotNo)
  -- ^ INVARIANT: the slot of the youngest 'LeiosPoint' with that EB hash in
  -- 'aleBySlot'.
  --
  -- The domain is used by ChainSel when checking which CertRBs are
  -- parked/not-selectable.
  --
  -- The codomain is used instead of a refcount when pruning.
  , aleBySlot :: !(Map SlotNo (NESet EndorserBlockHash))
  -- ^ Each 'LeiosPoint' the LeiosDB has acquired is inserted into this map;
  -- see @launchBgTasks@.
  }
  deriving stock Generic
  deriving anyclass NoThunks

mkAcquiredEbs :: [LeiosPoint] -> AcquiredEbs
mkAcquiredEbs =
  List.foldl' (flip insertAcquiredEb) (UnsafeAcquiredEbs Map.empty Map.empty)

-- | Record that the endorser block's closure is acquired, and that the given
-- point announces it.
insertAcquiredEb :: LeiosPoint -> AcquiredEbs -> AcquiredEbs
insertAcquiredEb (MkLeiosPoint slot ebHash) acquired@(UnsafeAcquiredEbs youngest bySlot) =
  case Map.lookup ebHash youngest of
    Nothing -> record bySlot
    Just prevSlot
      | slot <= prevSlot -> acquired
      | otherwise -> record (Map.update (NESet.nonEmptySet . NESet.delete ebHash) prevSlot bySlot)
 where
  record bySlot' =
    UnsafeAcquiredEbs
      (Map.insert ebHash slot youngest)
      (Map.insertWith NESet.union slot (NESet.singleton ebHash) bySlot')

isAcquiredEb :: EndorserBlockHash -> AcquiredEbs -> Bool
isAcquiredEb ebHash = Map.member ebHash . aleYoungestSlot

-- | Forget every endorser block all of whose foci are older than the slot. A
-- block that ChainSel has yet to select can then certify it only at another
-- announcement, and verifying that certificate makes the announcement a focus,
-- which records the endorser block here again.
--
-- See 'AcquiredEbs' for why its slots are the youngest foci.
pruneAcquiredEbs :: SlotNo -> AcquiredEbs -> AcquiredEbs
pruneAcquiredEbs slot (UnsafeAcquiredEbs youngest bySlot) =
  UnsafeAcquiredEbs
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
  AcquiredEbs ->
  (HeaderHash blk -> Maybe (BlockInfo blk)) ->
  BlockInfo blk ->
  Bool
isParkedCertRB acquired lookupBlockInfo info
  | not (lbiHasCert (biLeios info)) = False
  | otherwise = case certifiedEb of
      Just ebHash -> not (isAcquiredEb ebHash acquired)
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
  AcquiredEbs ->
  (HeaderHash blk -> Maybe (BlockInfo blk)) ->
  (HeaderHash blk -> Maybe (BlockInfo blk))
ignoreParkedCertRB acquired lookupBlockInfo hash = do
  info <- lookupBlockInfo hash
  guard (not (isParkedCertRB acquired lookupBlockInfo info))
  pure info

-- | Hide parked blocks ('isParkedCertRB') from a @filterByPredecessor@. The
-- given @getBlockInfo@ must be the VolatileDB's own.
ignoreParkedCertRBSuc ::
  AcquiredEbs ->
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

{-------------------------------------------------------------------------------
  Checking the ImmutableDB
-------------------------------------------------------------------------------}

-- | Whether the endorser block that the block certifies, if any, is in the
-- LeiosDB's immutable partition, given the header of the block's predecessor,
-- which announces it.
--
-- An unclean shutdown can lose the newest writes of the ImmutableDB and of the
-- LeiosDB, as neither syncs them to disk. The ImmutableDB checks this on each
-- block it validates when it opens, and truncates before the first that fails,
-- so that every block it holds has the endorser block it certifies there.
certifiesPromotedEb ::
  (Monad m, BlockSupportsProtocol blk) =>
  BlockConfig blk ->
  LeiosDbReader m ->
  Parent (Header blk) ->
  Header blk ->
  m Bool
certifiesPromotedEb bcfg reader (Parent prev) hdr
  | not (lbiHasCert (leiosHeaderInfo bcfg hdr)) = pure True
  | otherwise = case lbiAnnouncedEb (leiosHeaderInfo bcfg prev) of
      SJust ebHash ->
        isImmutable reader (MkLeiosPoint (blockSlot prev) ebHash)
      -- The block cannot be valid.
      SNothing -> pure False
