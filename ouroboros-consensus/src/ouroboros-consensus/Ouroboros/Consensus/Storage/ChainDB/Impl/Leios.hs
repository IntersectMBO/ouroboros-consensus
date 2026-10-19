{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | What the ChainDB knows about Leios endorser blocks: which closures it has
-- acquired, and so which blocks certifying an endorser block ChainSel may
-- select.
module Ouroboros.Consensus.Storage.ChainDB.Impl.Leios
  ( -- * The Leios facts of a block
    leiosBlockInfo

    -- * Acquired closures
  , AcquiredLeiosEbs
  , acquiredLeiosEbsFromList
  , insertAcquiredLeiosEb
  , isAcquiredLeiosEb
  , pruneAcquiredLeiosEbs

    -- * Parking
  , ignoreParkedCertRB
  , ignoreParkedCertRBSuc
  ) where

import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..), maybeToStrictMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Set.NonEmpty (NESet)
import qualified Data.Set.NonEmpty as NESet
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Leios.Protocol
  ( LeiosAnnouncement (..)
  , LeiosProtocol (..)
  )
import Ouroboros.Consensus.Leios.Types (EbHash, LeiosPoint (..))
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
  ( BlockSupportsProtocol blk
  , LeiosProtocol (BlockProtocol blk)
  ) =>
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
-- its youngest announcement.
data AcquiredLeiosEbs = AcquiredLeiosEbs
  { aleYoungestSlot :: !(Map EbHash SlotNo)
  , aleBySlot :: !(Map SlotNo (NESet EbHash))
  -- ^ The inverse of 'aleYoungestSlot', so that pruning costs only what it
  -- prunes.
  }
  deriving stock Generic
  deriving anyclass NoThunks

acquiredLeiosEbsFromList :: [LeiosPoint] -> AcquiredLeiosEbs
acquiredLeiosEbsFromList =
  List.foldl' (flip insertAcquiredLeiosEb) (AcquiredLeiosEbs Map.empty Map.empty)

-- | Record that the endorser block's closure is acquired, and that the given
-- point announces it.
insertAcquiredLeiosEb :: LeiosPoint -> AcquiredLeiosEbs -> AcquiredLeiosEbs
insertAcquiredLeiosEb (MkLeiosPoint slot ebHash) acquired@(AcquiredLeiosEbs youngest bySlot) =
  case Map.lookup ebHash youngest of
    Nothing -> record bySlot
    Just prevSlot
      | slot <= prevSlot -> acquired
      | otherwise -> record (Map.update (NESet.nonEmptySet . NESet.delete ebHash) prevSlot bySlot)
 where
  record bySlot' =
    AcquiredLeiosEbs
      (Map.insert ebHash slot youngest)
      (Map.insertWith NESet.union slot (NESet.singleton ebHash) bySlot')

isAcquiredLeiosEb :: EbHash -> AcquiredLeiosEbs -> Bool
isAcquiredLeiosEb ebHash = Map.member ebHash . aleYoungestSlot

-- | Forget every endorser block all of whose announcements are older than the
-- slot: once the immutable tip has passed them, no block that ChainSel has yet
-- to select can certify it.
pruneAcquiredLeiosEbs :: SlotNo -> AcquiredLeiosEbs -> AcquiredLeiosEbs
pruneAcquiredLeiosEbs slot (AcquiredLeiosEbs youngest bySlot) =
  AcquiredLeiosEbs
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
-- The certified endorser block is the one the block's predecessor announces. A
-- predecessor missing from the VolatileDB leaves it unknown, so the block is
-- parked too.
isParkedCertRB ::
  AcquiredLeiosEbs ->
  (HeaderHash blk -> Maybe (BlockInfo blk)) ->
  HeaderHash blk ->
  Bool
isParkedCertRB acquired lookupBlockInfo hash =
  case lookupBlockInfo hash of
    Nothing -> False
    Just info ->
      lbiHasCert (biLeios info) && case biPrevHash info of
        GenesisHash -> True
        BlockHash prevHash ->
          case lbiAnnouncedEb . biLeios <$> lookupBlockInfo prevHash of
            Just (SJust ebHash) -> not (isAcquiredLeiosEb ebHash acquired)
            Just SNothing -> True
            Nothing -> True

-- | Hide parked blocks ('isParkedCertRB') from a @getBlockInfo@. The given one
-- must be the VolatileDB's own, so that it still finds every predecessor.
ignoreParkedCertRB ::
  AcquiredLeiosEbs ->
  (HeaderHash blk -> Maybe (BlockInfo blk)) ->
  (HeaderHash blk -> Maybe (BlockInfo blk))
ignoreParkedCertRB acquired lookupBlockInfo hash
  | isParkedCertRB acquired lookupBlockInfo hash = Nothing
  | otherwise = lookupBlockInfo hash

-- | Hide parked blocks ('isParkedCertRB') from a @filterByPredecessor@. The
-- given @getBlockInfo@ must be the VolatileDB's own.
ignoreParkedCertRBSuc ::
  AcquiredLeiosEbs ->
  (HeaderHash blk -> Maybe (BlockInfo blk)) ->
  (ChainHash blk -> Set (HeaderHash blk)) ->
  (ChainHash blk -> Set (HeaderHash blk))
ignoreParkedCertRBSuc acquired lookupBlockInfo succsOf =
  Set.filter (not . isParkedCertRB acquired lookupBlockInfo) . succsOf
