{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Model implementation of the chain DB
--
-- Intended for qualified import
module Test.Ouroboros.Storage.ChainDB.Model
  ( Model
  -- opaque
  , CPS.FollowerId
  , IteratorId

    -- * Construction
  , addBlock
  , addBlockPromise
  , addBlocks
  , addPerasCert
  , empty

    -- * Queries
  , currentChain
  , currentLedger
  , getBlock
  , getBlockByPoint
  , getBlockComponentByPoint
  , getIsValid
  , getLoEFragment
  , getMaxSlotNo
  , hasBlock
  , hasBlockByPoint
  , immutableBlockNo
  , immutableChain
  , immutableSlotNo
  , invalid
  , isOpen
  , isValid
  , maxPerasRoundNo
  , tipBlock
  , tipPoint
  , volatileChain

    -- * Iterators
  , iteratorClose
  , iteratorNext
  , stream

    -- * Followers
  , followerClose
  , followerForward
  , followerInstruction
  , newFollower

    -- * ModelSupportsBlock
  , ModelSupportsBlock

    -- * Exported for testing purposes
  , ShouldGarbageCollect (GarbageCollect, DoNotGarbageCollect)
  , between
  , blocks
  , chains
  , closeDB
  , copyToImmutableDB
  , garbageCollectable
  , garbageCollectableIteratorNext
  , garbageCollectablePoint
  , getFragmentBetween
  , immutableDbChain
  , initLedger
  , reopen
  , updateLoE
  , validChains
  , volatileDbBlocks
  , wipeVolatileDB
  ) where

import Cardano.Ledger.BaseTypes
  ( unNonZero
  )
import Codec.Serialise (Serialise, serialise)
import Control.Monad (unless)
import Control.Monad.Except (runExcept)
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as Lazy
import Data.Containers.ListUtils (nubOrdOn)
import Data.Foldable (foldMap')
import Data.Function (on, (&))
import Data.Functor (($>), (<&>))
import Data.List (isInfixOf, isPrefixOf, sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Proxy
import Data.Set (Set)
import qualified Data.Set as Set
import Data.TreeDiff
import GHC.Generics (Generic)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.HeaderValidation
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Ledger.SupportsProtocol
import Ouroboros.Consensus.Peras.SelectView
import Ouroboros.Consensus.Peras.Weight
import Ouroboros.Consensus.Protocol.Abstract
import Ouroboros.Consensus.Protocol.MockChainSel
import Ouroboros.Consensus.Storage.ChainDB.API
  ( AddBlockPromise (..)
  , AddBlockResult (..)
  , BlockComponent (..)
  , ChainDbError (..)
  , IteratorResult (..)
  , LoE (..)
  , StreamFrom (..)
  , StreamTo (..)
  , UnknownRange (..)
  , validBounds
  )
import Ouroboros.Consensus.Storage.ChainDB.Impl.ChainSel (olderThanImmTip)
import Ouroboros.Consensus.Storage.Common ()
import Ouroboros.Consensus.Util (repeatedly)
import qualified Ouroboros.Consensus.Util.AnchoredFragment as Fragment
import Ouroboros.Consensus.Util.IOLike (MonadSTM)
import Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as Fragment
import Ouroboros.Network.Block (MaxSlotNo (..))
import Ouroboros.Network.Mock.Chain (Chain (..), ChainUpdate)
import qualified Ouroboros.Network.Mock.Chain as Chain
import Ouroboros.Network.Mock.ProducerState (ChainProducerState)
import qualified Ouroboros.Network.Mock.ProducerState as CPS
import Test.Cardano.Slotting.TreeDiff ()
import Test.Util.Orphans.ToExpr ()

type IteratorId = Int

-- | Model of the chain DB
data Model blk = Model
  { volatileDbBlocks :: Map (HeaderHash blk) blk
  -- ^ The VolatileDB
  , immutableDbChain :: Chain blk
  -- ^ The ImmutableDB
  , perasCerts :: Map PerasRoundNo (PerasCert blk)
  , cps :: CPS.ChainProducerState blk
  , currentLedger :: ExtLedgerState blk EmptyMK
  , initLedger :: ExtLedgerState blk EmptyMK
  , iterators :: Map IteratorId [blk]
  , valid :: Set (HeaderHash blk)
  , invalid :: InvalidBlocks blk
  , loeFragment :: LoE (AnchoredFragment blk)
  , isOpen :: Bool
  -- ^ While the model tracks whether it is closed or not, the queries and
  -- other functions in this module ignore this for simplicity. The mock
  -- ChainDB that wraps this model will throw a 'ClosedDBError' whenever
  -- it is used while closed.
  }
  deriving Generic

deriving instance
  ( ToExpr blk
  , ToExpr (HeaderHash blk)
  , ToExpr (ChainDepState (BlockProtocol blk))
  , ToExpr (TipInfo blk)
  , ToExpr (LedgerState blk EmptyMK)
  , ToExpr (ExtValidationError blk)
  , ToExpr (Chain blk)
  , ToExpr (ChainProducerState blk)
  , ToExpr (ExtLedgerState blk EmptyMK)
  ) =>
  ToExpr (Model blk)

deriving instance (LedgerSupportsProtocol blk, Show blk) => Show (Model blk)

{-------------------------------------------------------------------------------
  Queries
-------------------------------------------------------------------------------}

immutableDbBlocks :: HasHeader blk => Model blk -> Map (HeaderHash blk) blk
immutableDbBlocks Model{immutableDbChain} =
  Map.fromList $
    [ (blockHash blk, blk)
    | blk <- Chain.toOldestFirst immutableDbChain
    ]

blocks :: HasHeader blk => Model blk -> Map (HeaderHash blk) blk
blocks m = volatileDbBlocks m <> immutableDbBlocks m

currentChain :: Model blk -> Chain blk
currentChain = CPS.producerChain . cps

getBlock :: HasHeader blk => HeaderHash blk -> Model blk -> Maybe blk
getBlock hash m = Map.lookup hash (blocks m)

hasBlock :: HasHeader blk => HeaderHash blk -> Model blk -> Bool
hasBlock hash = isJust . getBlock hash

getBlockByPoint ::
  HasHeader blk =>
  RealPoint blk ->
  Model blk ->
  Maybe blk
getBlockByPoint (RealPoint _ hash) = getBlock hash

getBlockComponentByPoint ::
  ModelSupportsBlock blk =>
  BlockComponent blk b ->
  RealPoint blk ->
  Model blk ->
  Either (ChainDbError blk) (Maybe b) -- Just to satify the API
getBlockComponentByPoint blockComponent pt m =
  Right $
    (`getBlockComponent` blockComponent) <$> getBlockByPoint pt m

hasBlockByPoint ::
  HasHeader blk =>
  Point blk -> Model blk -> Bool
hasBlockByPoint pt = case pointHash pt of
  GenesisHash -> const False
  BlockHash hash -> hasBlock hash

tipBlock :: Model blk -> Maybe blk
tipBlock = Chain.head . currentChain

tipPoint :: HasHeader blk => Model blk -> Point blk
tipPoint = maybe GenesisPoint blockPoint . tipBlock

getMaxSlotNo :: HasHeader blk => Model blk -> MaxSlotNo
getMaxSlotNo = foldMap (MaxSlotNo . blockSlot) . blocks

-- | Actual amount of weight that can be rolled back. This can non-trivially
-- smaller than @k@ in the following cases:
--
-- * Near genesis, the chain might not have grown sufficiently yet.
-- * After VolatileDB corruption, the whole chain might have more than weight
--   @k@, but the tip of the ImmutableDB might be buried under significantly
--   less than weight @k@ worth of blocks.
maxActualRollback :: HasHeader blk => SecurityParam -> Model blk -> PerasWeight
maxActualRollback k m =
  foldMap' (weightBoostOfPoint weights)
    . takeWhile (/= immutableTipPoint)
    . map blockPoint
    . Chain.toNewestFirst
    . currentChain
    $ m
 where
  weights = perasWeights m

  immutableTipPoint = Chain.headPoint (immutableChain k m)

-- | Return the immutable prefix of the current chain.
--
-- This is the longest of the given two chains:
--
-- 1. The current chain with the longest suffix of weight at most @k@ dropped.
-- 2. The chain formed by the blocks in 'immutableDbChain', i.e., the
--    \"ImmutableDB\". We need to take this case in consideration because the
--    VolatileDB might have been wiped.
--
-- We need this because we do not allow rolling back more than weight @k@, but
-- the background thread copying blocks from the VolatileDB to the ImmutableDB
-- might not have caught up yet. This means we cannot use the tip of the
-- ImmutableDB to know the most recent \"immutable\" block.
immutableChain ::
  forall blk.
  HasHeader blk =>
  SecurityParam ->
  Model blk ->
  Chain blk
immutableChain k m =
  maxBy
    -- As one of the two chains is a prefix of the other, Peras weight doesn't
    -- matter here.
    Chain.length
    (dropAtMostWeight (maxRollbackWeight k) (currentChain m))
    (immutableDbChain m)
 where
  maxBy f a b
    | f a >= f b = a
    | otherwise = b

  weights = perasWeights m

  -- Drop the longest suffix with at most the given weight.
  dropAtMostWeight :: PerasWeight -> Chain blk -> Chain blk
  dropAtMostWeight budget = go mempty
   where
    go w = \case
      Genesis -> Genesis
      c@(c' :> b)
        | w' <= budget -> go w' c'
        | otherwise -> c
       where
        w' = w <> PerasWeight 1 <> weightBoostOfPoint weights (blockPoint b)

-- | Return the volatile suffix of the current chain.
--
-- The opposite of 'immutableChain'.
--
-- This is the shortest of the given two chain fragments:
--
-- 1. The longest suffix of the current chain with weight at most @k@.
-- 2. The suffix of the current chain not part of the 'immutableDbChain', i.e.,
--    the \"ImmutableDB\".
volatileChain ::
  (HasHeader a, HasHeader blk) =>
  SecurityParam ->
  -- | Provided since 'AnchoredFragment' is not a functor
  (blk -> a) ->
  Model blk ->
  AnchoredFragment a
volatileChain k f m =
  Fragment.fromNewestFirst anchor
    . map f
    . takeWhile ((/= immutableTipPoint) . blockPoint)
    . Chain.toNewestFirst
    . currentChain
    $ m
 where
  (immutableTipPoint, anchor) = case Chain.head (immutableChain k m) of
    Nothing -> (GenesisPoint, Fragment.AnchorGenesis)
    Just b -> (blockPoint b, Fragment.anchorFromBlock (f b))

-- | The block number of the most recent \"immutable\" block, i.e. the oldest
-- block we can roll back to. We cannot roll back the block itself.
--
-- Note that this is not necessarily the block at the tip of the ImmutableDB,
-- because the background thread copying blocks to the ImmutableDB might not
-- have caught up.
immutableBlockNo ::
  HasHeader blk =>
  SecurityParam -> Model blk -> WithOrigin BlockNo
immutableBlockNo k = Chain.headBlockNo . immutableChain k

-- | The slot number of the most recent \"immutable\" block (see
-- 'immutableBlockNo').
--
-- This is used for garbage collection of the VolatileDB, which is done in
-- terms of slot numbers, not in terms of block numbers.
immutableSlotNo ::
  HasHeader blk =>
  SecurityParam ->
  Model blk ->
  WithOrigin SlotNo
immutableSlotNo k = Chain.headSlot . immutableChain k

getIsValid ::
  forall blk.
  LedgerSupportsProtocol blk =>
  Model blk ->
  (RealPoint blk -> Maybe Bool)
getIsValid m = \(RealPoint _ hash) ->
  if
    -- Note that we are not checking whether the block is in the VolatileDB.
    -- This is justified as we already assume that the model knows more about
    -- valid blocks (see 'IsValidResult') and garbage collection of invalid
    -- blocks differs between the model and the SUT (see the "Invalid blocks"
    -- note in @./StateMachine.hs@).
    | Set.member hash (valid m) -> Just True
    | Map.member hash (invalid m) -> Just False
    | otherwise -> Nothing

isValid ::
  forall blk.
  LedgerSupportsProtocol blk =>
  RealPoint blk ->
  Model blk ->
  Maybe Bool
isValid = flip getIsValid

getLoEFragment :: Model blk -> LoE (AnchoredFragment blk)
getLoEFragment = loeFragment

perasWeights :: StandardHash blk => Model blk -> PerasWeightSnapshot blk
perasWeights =
  mkPerasWeightSnapshot
    -- TODO make boost per cert configurable
    . fmap (\c -> (perasCertBoostedBlock c, boostPerCert))
    . Map.elems
    . perasCerts

maxPerasRoundNo :: Model blk -> Maybe PerasRoundNo
maxPerasRoundNo m = fst <$> Map.lookupMax (perasCerts m)

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

empty ::
  HasHeader blk =>
  LoE () ->
  ExtLedgerState blk EmptyMK ->
  Model blk
empty loe initLedger =
  Model
    { volatileDbBlocks = Map.empty
    , immutableDbChain = Chain.Genesis
    , perasCerts = Map.empty
    , cps = CPS.initChainProducerState Chain.Genesis
    , currentLedger = initLedger
    , initLedger = initLedger
    , iterators = Map.empty
    , valid = Set.empty
    , invalid = Map.empty
    , isOpen = True
    , loeFragment = loe $> Fragment.Empty Fragment.AnchorGenesis
    }

addBlock ::
  forall blk.
  (LedgerSupportsProtocol blk, LedgerTablesAreTrivial (ExtLedgerState blk)) =>
  TopLevelConfig blk ->
  blk ->
  Model blk ->
  Model blk
addBlock cfg blk m
  | ignoreBlock = m
  | otherwise =
      chainSelection
        cfg
        m
          { volatileDbBlocks = Map.insert (blockHash blk) blk (volatileDbBlocks m)
          }
 where
  secParam = configSecurityParam cfg
  immBlockNo = immutableBlockNo secParam m

  hdr = getHeader blk

  ignoreBlock =
    -- If the block is as old as the tip of the ImmutableDB, i.e. older
    -- than @k@, we ignore it, as we can never switch to it.
    olderThanImmTip hdr immBlockNo
      ||
      -- If it's an invalid block we've seen before, ignore it.
      Map.member (blockHash blk) (invalid m)

addPerasCert ::
  forall blk.
  (LedgerSupportsProtocol blk, LedgerTablesAreTrivial (ExtLedgerState blk)) =>
  TopLevelConfig blk ->
  PerasCert blk ->
  Model blk ->
  Model blk
addPerasCert cfg cert m
  -- Do not alter the model when a certificate for that round already exists.
  | Map.member certRound (perasCerts m) = m
  | otherwise =
      chainSelection
        cfg
        m{perasCerts = Map.insert certRound cert (perasCerts m)}
 where
  certRound = perasCertRound cert

chainSelection ::
  forall blk.
  ( LedgerTablesAreTrivial (ExtLedgerState blk)
  , LedgerSupportsProtocol blk
  ) =>
  TopLevelConfig blk ->
  Model blk ->
  Model blk
chainSelection cfg m =
  Model
    { volatileDbBlocks = volatileDbBlocks m
    , immutableDbChain = immutableDbChain m
    , perasCerts = perasCerts m
    , cps = CPS.switchFork newChain (cps m)
    , currentLedger = newLedger
    , initLedger = initLedger m
    , iterators = iterators m
    , valid = valid'
    , invalid = invalid'
    , isOpen = True
    , loeFragment = loeFragment m
    }
 where
  secParam = configSecurityParam cfg

  -- @invalid'@ will be a (non-strict) superset of the previous value of
  -- @invalid@, see 'validChains', thus no need to union.
  invalid' :: InvalidBlocks blk
  candidates :: [(Chain blk, ExtLedgerState blk EmptyMK)]
  (invalid', candidates) = validChains cfg m (blocks m)

  immutableChainHashes =
    map blockHash
      . Chain.toOldestFirst
      $ immutableChain'

  immutableChain' = immutableChain secParam m

  extendsImmutableChain :: Chain blk -> Bool
  extendsImmutableChain fork =
    immutableChainHashes
      `isPrefixOf` map blockHash (Chain.toOldestFirst fork)

  -- Note that this includes the currently selected chain, but that does not
  -- influence chain selection via 'selectChain'. Note that duplicates might
  -- be introduced by `trimToLoE` so we deduplicate explicitly here.
  consideredCandidates =
    candidates
      & filter (extendsImmutableChain . fst)
      & map (first trimToLoE)
      & nubOrdOn (Chain.headPoint . fst)

  currentChain' = currentChain m

  -- \| Trim a candidate fragment to the LoE fragment.
  --
  -- - A (sanitized) LoE fragment @loe@ is some fragment containing the
  --   immutable tip.
  --
  -- - A candidate fragment @cf@ is valid according to the LoE in one of two
  --   cases:
  --   - @loe@ is an extension of @cf@.
  --   - @cf@ is an extension of @loe@, and @cf@ has at most @k@ blocks after
  --     the tip of loe.
  --
  -- - Trimming a candidate fragment according to the LoE is defined to be the
  --   longest prefix that is valid according to the LoE.
  --
  -- NOTE: It is possible that `trimToLoE a == trimToLoE b` even though `a /=
  -- b` if the longest prefix is the same.
  trimToLoE :: Chain blk -> Chain blk
  trimToLoE candidate =
    case loeChain of
      LoEDisabled -> candidate
      LoEEnabled loeChain' ->
        Chain.fromOldestFirst $ go (Chain.toOldestFirst candidate) loePoints
       where
        loePoints = blockPoint <$> Chain.toOldestFirst loeChain'
   where
    SecurityParam k = secParam

    go :: [blk] -> [Point blk] -> [blk]
    -- The LoE chain is an extension of the candidate, return the candidate.
    go [] _loePoints = []
    -- The candidate is an extension of the LoE chain, return at most the
    -- next k blocks on the candidate.
    go blks [] = take (fromIntegral $ unNonZero k) blks
    go (blk : blks) (pt : loePoints)
      -- The candidate and the LoE chain agree on the next point, continue
      -- recursively.
      | blockPoint blk == pt = blk : go blks loePoints
      -- The candidate forks off from the LoE chain; stop here.
      | otherwise = []

  -- If the LoE fragment does not intersect with the current volatile chain,
  -- then we use the immutable chain instead.
  loeChain =
    loeFragment m <&> \loeFragment' -> fromMaybe immutableChain' $ do
      _ <- Fragment.intersect volatileFrag loeFragment'
      (_, loeChain') <- Fragment.cross currentFrag loeFragment'
      Chain.fromAnchoredFragment loeChain'
   where
    currentFrag = Chain.toAnchoredFragment currentChain'
    volatileFrag = volatileChain secParam id m

  newChain :: Chain blk
  newLedger :: ExtLedgerState blk EmptyMK
  (newChain, newLedger) =
    fromMaybe (currentChain m, currentLedger m)
      . selectChain
        (Proxy @(BlockProtocol blk))
        (projectChainOrderConfig (configBlock cfg))
        ( weightedSelectView (configBlock cfg) (perasWeights m)
            . Chain.toAnchoredFragment
            . fmap getHeader
        )
        (currentChain m)
      $ consideredCandidates

  -- We update the set of valid blocks with all valid blocks on all candidate
  -- chains that are considered by the modeled chain selection. This ensures
  -- that the model always knows about more valid blocks than the system under
  -- test. See 'IsValidResult' for more context.
  valid' =
    valid m
      <> foldMap
        (Set.fromList . map blockHash . Chain.toOldestFirst . fst)
        consideredCandidates

addBlocks ::
  (LedgerSupportsProtocol blk, LedgerTablesAreTrivial (ExtLedgerState blk)) =>
  TopLevelConfig blk ->
  [blk] ->
  Model blk ->
  Model blk
addBlocks cfg = repeatedly (addBlock cfg)

-- | Wrapper around 'addBlock' that returns an 'AddBlockPromise'.
addBlockPromise ::
  forall m blk.
  (LedgerSupportsProtocol blk, MonadSTM m, LedgerTablesAreTrivial (ExtLedgerState blk)) =>
  TopLevelConfig blk ->
  blk ->
  Model blk ->
  (AddBlockPromise m blk, Model blk)
addBlockPromise cfg blk m = (result, m')
 where
  m' = addBlock cfg blk m
  blockWritten =
    Map.notMember (blockHash blk) (blocks m)
      && Map.member (blockHash blk) (blocks m')
  result =
    AddBlockPromise
      { blockWrittenToDisk = return blockWritten
      , blockProcessed = return $ SuccesfullyAddedBlock $ tipPoint m'
      }

-- | Update the LoE fragment, trigger chain selection and return the new tip
-- point.
updateLoE ::
  forall blk.
  ( LedgerTablesAreTrivial (ExtLedgerState blk)
  , LedgerSupportsProtocol blk
  ) =>
  TopLevelConfig blk ->
  AnchoredFragment blk ->
  Model blk ->
  (Point blk, Model blk)
updateLoE cfg f m = (tipPoint m', m')
 where
  m' = chainSelection cfg $ m{loeFragment = loeFragment m $> f}

{-------------------------------------------------------------------------------
  Iterators
-------------------------------------------------------------------------------}

stream ::
  GetPrevHash blk =>
  SecurityParam ->
  StreamFrom blk ->
  StreamTo blk ->
  Model blk ->
  Either
    (ChainDbError blk)
    (Either (UnknownRange blk) IteratorId, Model blk)
stream securityParam from to m = do
  unless (validBounds from to) $ Left (InvalidIteratorRange from to)
  case between securityParam from to m of
    Left e -> return (Left e, m)
    Right blks ->
      return
        ( Right itrId
        , m
            { iterators = Map.insert itrId blks (iterators m)
            }
        )
 where
  itrId :: IteratorId
  itrId = Map.size (iterators m) -- we never delete iterators

iteratorNext ::
  ModelSupportsBlock blk =>
  IteratorId ->
  BlockComponent blk b ->
  Model blk ->
  (IteratorResult blk b, Model blk)
iteratorNext itrId blockComponent m =
  case Map.lookup itrId (iterators m) of
    Just [] ->
      (IteratorExhausted, m)
    Just (b : bs)
      | blockHash b `Map.member` blocks m ->
          (IteratorResult $ getBlockComponent b blockComponent, updateIter bs)
    -- The next block `b` was part of a dead fork and has been garbage
    -- collected.  The system-under-test then closes the iterator, and we set
    -- the state of the iterator to the empty list to mimic that behaviour.
    Just (b : _) ->
      (IteratorBlockGCed $ blockRealPoint b, updateIter [])
    Nothing ->
      error "iteratorNext: unknown iterator ID"
 where
  updateIter bs = m{iterators = Map.insert itrId bs (iterators m)}

getBlockComponent ::
  forall blk b.
  ModelSupportsBlock blk =>
  blk -> BlockComponent blk b -> b
getBlockComponent blk = \case
  GetVerifiedBlock -> blk -- We don't verify it
  GetBlock -> blk
  GetRawBlock -> serialise blk
  GetHeader -> getHeader blk
  GetRawHeader -> serialise $ getHeader blk
  GetHash -> blockHash blk
  GetSlot -> blockSlot blk
  GetIsEBB -> headerToIsEBB (getHeader blk)
  GetBlockSize -> fromIntegral $ Lazy.length $ serialise blk
  GetHeaderSize -> fromIntegral $ Lazy.length $ serialise $ getHeader blk
  GetNestedCtxt -> case unnest (getHeader blk) of
    DepPair nestedCtxt _ -> SomeSecond nestedCtxt
  GetPure a -> a
  GetApply f bc -> getBlockComponent blk f $ getBlockComponent blk bc

-- We never delete iterators such that we can use the size of the map as the
-- next iterator id.
iteratorClose :: IteratorId -> Model blk -> Model blk
iteratorClose itrId m = m{iterators = Map.insert itrId [] (iterators m)}

{-------------------------------------------------------------------------------
  Followers
-------------------------------------------------------------------------------}

followerExists :: CPS.FollowerId -> Model blk -> Bool
followerExists flrId = CPS.followerExists flrId . cps

checkIfFollowerExists ::
  CPS.FollowerId ->
  Model blk ->
  a ->
  Either (ChainDbError blk) a
checkIfFollowerExists flrId m a
  | followerExists flrId m =
      Right a
  | otherwise =
      Left ClosedFollowerError

newFollower :: HasHeader blk => Model blk -> (CPS.FollowerId, Model blk)
newFollower m = (flrId, m{cps = cps'})
 where
  (cps', flrId) = CPS.initFollower GenesisPoint (cps m)

followerInstruction ::
  forall blk b.
  ModelSupportsBlock blk =>
  CPS.FollowerId ->
  BlockComponent blk b ->
  Model blk ->
  Either
    (ChainDbError blk)
    (Maybe (ChainUpdate blk b), Model blk)
followerInstruction flrId blockComponent m =
  checkIfFollowerExists flrId m $
    rewrap $
      CPS.followerInstruction flrId (cps m)
 where
  toB :: blk -> b
  toB blk = getBlockComponent blk blockComponent

  rewrap ::
    Maybe (ChainUpdate blk blk, CPS.ChainProducerState blk) ->
    (Maybe (ChainUpdate blk b), Model blk)
  rewrap Nothing = (Nothing, m)
  rewrap (Just (upd, cps')) = (Just (toB <$> upd), m{cps = cps'})

followerForward ::
  HasHeader blk =>
  CPS.FollowerId ->
  [Point blk] ->
  Model blk ->
  Either (ChainDbError blk) (Maybe (Point blk), Model blk)
followerForward flrId points m = checkIfFollowerExists flrId m $
  case CPS.findFirstPoint points (cps m) of
    Nothing -> (Nothing, m)
    Just ipoint -> (Just ipoint, m{cps = cps'})
     where
      cps' = CPS.updateFollower flrId ipoint (cps m)

followerClose ::
  CPS.FollowerId ->
  Model blk ->
  Model blk
followerClose flrId m
  | followerExists flrId m =
      m{cps = CPS.deleteFollower flrId (cps m)}
  | otherwise =
      m

{-------------------------------------------------------------------------------
  ModelSupportsBlock
-------------------------------------------------------------------------------}

-- | Functionality the block needs to support so that it can be used in the
-- 'Model'.
class
  ( HasHeader blk
  , GetHeader blk
  , HasHeader (Header blk)
  , Serialise blk
  , Serialise (Header blk)
  , HasNestedContent Header blk
  ) =>
  ModelSupportsBlock blk

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

type InvalidBlocks blk = Map (HeaderHash blk) (ExtValidationError blk, SlotNo)

-- | Result of 'validate', also used internally.
data ValidatedChain blk
  = ValidatedChain
      -- | Valid prefix
      (Chain blk)
      -- | Corresponds to the tip of the valid prefix
      (ExtLedgerState blk EmptyMK)
      -- | Invalid blocks encountered while validating
      -- the candidate chain.
      (InvalidBlocks blk)

-- | Validate the given 'Chain'.
--
-- The 'InvalidBlocks' in the returned 'ValidatedChain' will be >= the
-- 'invalid' of the given 'Model'.
validate ::
  forall blk.
  (LedgerSupportsProtocol blk, LedgerTablesAreTrivial (ExtLedgerState blk)) =>
  TopLevelConfig blk ->
  Model blk ->
  Chain blk ->
  ValidatedChain blk
validate cfg Model{initLedger, invalid} chain =
  go initLedger Genesis (Chain.toOldestFirst chain)
 where
  mkInvalid :: blk -> ExtValidationError blk -> InvalidBlocks blk
  mkInvalid b reason =
    Map.singleton (blockHash b) (reason, blockSlot b)

  go ::
    ExtLedgerState blk EmptyMK ->
    -- \^ Corresponds to the tip of the valid prefix
    Chain blk ->
    -- \^ Valid prefix
    [blk] ->
    -- \^ Remaining blocks to validate
    ValidatedChain blk
  go ledger validPrefix = \case
    -- Return 'mbFinal' if it contains an "earlier" result
    [] -> ValidatedChain validPrefix ledger invalid
    b : bs' -> case runExcept (tickThenApply OmitLedgerEvents (ExtLedgerCfg cfg) b (convertMapKind ledger)) of
      -- Invalid block according to the ledger
      Left e ->
        ValidatedChain
          validPrefix
          ledger
          (invalid <> mkInvalid b e)
      -- Valid block according to the ledger
      Right ledger' -> go (convertMapKind ledger') (validPrefix :> b) bs'

chains ::
  forall blk.
  GetPrevHash blk =>
  Map (HeaderHash blk) blk -> [Chain blk]
chains bs = go Chain.Genesis
 where
  -- Construct chains,
  go :: Chain blk -> [Chain blk]
  go ch
    | null extensions = [ch]
    | otherwise = extensions
   where
    -- If we can extend the chain, don't include the chain itself. See
    -- the property "Always Extend".

    extensions :: [Chain blk]
    extensions = concat [go (ch :> b) | b <- succs]

    succs :: [blk]
    succs =
      Map.elems $
        Map.findWithDefault Map.empty (Chain.headHash ch) fwd

  fwd :: Map (ChainHash blk) (Map (HeaderHash blk) blk)
  fwd = successors (Map.elems bs)

validChains ::
  forall blk.
  (LedgerSupportsProtocol blk, LedgerTablesAreTrivial (ExtLedgerState blk)) =>
  TopLevelConfig blk ->
  Model blk ->
  Map (HeaderHash blk) blk ->
  (InvalidBlocks blk, [(Chain blk, ExtLedgerState blk EmptyMK)])
validChains cfg m bs =
  foldMap (classify . validate cfg m) $
    -- Note that we sort here to make sure we pick the same chain as the real
    -- chain selection in case there are multiple equally preferable chains
    -- after detecting invalid blocks. For example:
    --
    -- We add the following blocks: B, B', C', A where C' is invalid. Without
    -- sorting here (in the model), this results in the following two
    -- unvalidated chains: A->B and A->B'->C'. After validation, this results
    -- in the following two validated chains: A->B and A->B'. The first of
    -- these two will be chosen.
    --
    -- In the real implementation, we sort the candidate chains before
    -- validation so that in the best case (no invalid blocks) we only have to
    -- validate the most preferable candidate chain. So A->B'->C' is validated
    -- first, which results in the valid chain A->B', which is then chosen
    -- over the equally preferable A->B as it will be the first in the list
    -- after a stable sort.
    sortChains $
      chains bs
 where
  sortChains :: [Chain blk] -> [Chain blk]
  sortChains =
    sortBy $
      flip
        ( Fragment.compareAnchoredFragments (configBlock cfg) (perasWeights m)
            `on` (Chain.toAnchoredFragment . fmap getHeader)
        )

  classify ::
    ValidatedChain blk ->
    (InvalidBlocks blk, [(Chain blk, ExtLedgerState blk EmptyMK)])
  classify (ValidatedChain chain ledger invalid) =
    (invalid, [(chain, ledger)])

-- Map (HeaderHash blk) blk maps a block's hash to the block itself
successors ::
  forall blk.
  GetPrevHash blk =>
  [blk] ->
  Map (ChainHash blk) (Map (HeaderHash blk) blk)
successors = Map.unionsWith Map.union . map single
 where
  single :: blk -> Map (ChainHash blk) (Map (HeaderHash blk) blk)
  single b =
    Map.singleton
      (blockPrevHash b)
      (Map.singleton (blockHash b) b)

between ::
  forall blk.
  GetPrevHash blk =>
  SecurityParam ->
  StreamFrom blk ->
  StreamTo blk ->
  Model blk ->
  Either (UnknownRange blk) [blk]
between k from to m = do
  fork <- errFork
  -- See #871.
  if partOfCurrentChain fork
    || Fragment.forksAtMostKWeight
      (perasWeights m)
      (maxActualRollback k m)
      currentFrag
      fork
    then return $ Fragment.toOldestFirst fork
    -- We cannot stream from an old fork
    else Left $ ForkTooOld from
 where
  currentFrag :: AnchoredFragment blk
  currentFrag = Chain.toAnchoredFragment (currentChain m)

  partOfCurrentChain :: AnchoredFragment blk -> Bool
  partOfCurrentChain fork =
    map blockPoint (Fragment.toOldestFirst fork)
      `isInfixOf` map blockPoint (Chain.toOldestFirst (currentChain m))

  -- A fragment for each possible chain in the database
  fragments :: [AnchoredFragment blk]
  fragments =
    map Chain.toAnchoredFragment
      . chains
      . blocks
      $ m

  -- The fork that contained the start and end point, i.e. the fork to
  -- stream from. This relies on the fact that each block uniquely
  -- determines its prefix.
  errFork :: Either (UnknownRange blk) (AnchoredFragment blk)
  errFork = do
    -- The error refers to @to@, because if the list is empty, @to@ was not
    -- found on any chain
    let err = MissingBlock $ case to of
          StreamToInclusive p -> p
    -- Note that any chain that contained @to@, must have an identical
    -- prefix because the hashes of the blocks enforce this. So we can just
    -- pick any fork.
    afterTo <- anyFork (map cutOffAfterTo fragments) err
    cutOffBeforeFrom afterTo

  -- Select the first 'Right' in the list, otherwise return the last 'Left'.
  -- If the list is empty, return the error given as second argument.
  --
  -- See 'errFork' for why it doesn't matter which fork we return.
  anyFork ::
    [Either (UnknownRange blk) (AnchoredFragment blk)] ->
    UnknownRange blk ->
    Either (UnknownRange blk) (AnchoredFragment blk)
  anyFork (Right f : _) _ = Right f
  anyFork (Left u : []) _ = Left u
  anyFork (Left _ : fs) e = anyFork fs e
  anyFork [] e = Left e

  -- If @to@ is on the fragment, remove all blocks after it. If it is not on
  -- the fragment, return a 'MissingBlock' error.
  cutOffAfterTo ::
    AnchoredFragment blk ->
    Either (UnknownRange blk) (AnchoredFragment blk)
  cutOffAfterTo frag = case to of
    StreamToInclusive p
      | Just frag' <- fst <$> Fragment.splitAfterPoint frag (realPointToPoint p) ->
          return frag'
      | otherwise ->
          Left $ MissingBlock p

  -- If @from@ is on the fragment, remove all blocks before it, including
  -- @from@ itself in case of 'StreamFromExclusive'. It it is not on the
  -- fragment, return a 'MissingBlock' error.
  cutOffBeforeFrom ::
    AnchoredFragment blk ->
    Either (UnknownRange blk) (AnchoredFragment blk)
  cutOffBeforeFrom frag = case from of
    StreamFromInclusive p
      | Just frag' <- snd <$> Fragment.splitBeforePoint frag (realPointToPoint p) ->
          return frag'
      | otherwise ->
          Left $ MissingBlock p
    StreamFromExclusive p@(BlockPoint s h)
      | Just frag' <- snd <$> Fragment.splitAfterPoint frag p ->
          return frag'
      | otherwise ->
          Left $ MissingBlock (RealPoint s h)
    StreamFromExclusive GenesisPoint ->
      return frag

-- | Should the given block be garbage collected from the VolatileDB?
--
-- Blocks can be garbage collected when their slot number is older than the
-- slot number of the immutable block (the block @k@ blocks after the current
-- tip).
garbageCollectable ::
  forall blk.
  HasHeader blk =>
  SecurityParam -> Model blk -> blk -> Bool
garbageCollectable secParam m b =
  -- Note: we don't use the block number but the slot number, as the
  -- VolatileDB's garbage collection is in terms of slot numbers.
  NotOrigin (blockSlot b) < immutableSlotNo secParam m

-- | Return 'True' when the model contains the block corresponding to the point
-- and the block itself is eligible for garbage collection, i.e. the real
-- implementation might have garbage collected it.
--
-- If the block is not in the model, return 'True', as it has likely been
-- garbage-collected from the model too. Note that we cannot distinguish this
-- case from a block that was never added to the model in the first place.
garbageCollectablePoint ::
  forall blk.
  HasHeader blk =>
  SecurityParam -> Model blk -> RealPoint blk -> Bool
garbageCollectablePoint secParam m pt
  | Just blk <- getBlock (realPointHash pt) m =
      garbageCollectable secParam m blk
  | otherwise =
      True

-- | Return 'True' when the next block the given iterator would produced is
-- eligible for garbage collection, i.e. the real implementation might have
-- garbage collected it.
garbageCollectableIteratorNext ::
  forall blk.
  ModelSupportsBlock blk =>
  SecurityParam -> Model blk -> IteratorId -> Bool
garbageCollectableIteratorNext secParam m itId =
  case fst (iteratorNext itId GetBlock m) of
    IteratorExhausted -> True -- TODO
    IteratorBlockGCed{} -> True
    IteratorResult blk -> garbageCollectable secParam m blk

-- | Delete blocks that are older than the security parameter from the volatile
-- DB. This function assumes that the blocks that will be deleted are copied to
-- the immutable DB.
--
-- If this function collects blocks that are not yet copied to the immutable DB
-- the volatile fragment of the current chain will not be connected to the
-- immutable part of the chain. For this reason, this function should not be
-- used in isolation and is not exported.
garbageCollect ::
  forall blk.
  HasHeader blk =>
  SecurityParam -> Model blk -> Model blk
garbageCollect secParam m@Model{..} =
  m
    { volatileDbBlocks = Map.filter (not . collectable) volatileDbBlocks
    -- TODO garbage collection Peras certs?
    }
 where
  -- TODO what about iterators that will stream garbage collected blocks?

  collectable :: blk -> Bool
  collectable = garbageCollectable secParam m

data ShouldGarbageCollect = GarbageCollect | DoNotGarbageCollect
  deriving (Eq, Show)

-- | Copy all blocks on the current chain older than @k@ to the \"mock
-- ImmutableDB\" ('immutableDbChain').
--
-- The 'ShouldGarbageCollect' parameter determines if garbage collection should
-- be performed __after__ copying.
--
-- Idempotent.
copyToImmutableDB ::
  forall blk.
  HasHeader blk =>
  SecurityParam -> ShouldGarbageCollect -> Model blk -> Model blk
copyToImmutableDB secParam shouldCollectGarbage m =
  garbageCollectIf shouldCollectGarbage $
    m{immutableDbChain = immutableChain secParam m}
 where
  garbageCollectIf GarbageCollect = garbageCollect secParam
  garbageCollectIf DoNotGarbageCollect = id

closeDB :: Model blk -> Model blk
closeDB m@Model{..} =
  m
    { isOpen = False
    , cps = cps{CPS.chainFollowers = Map.empty}
    , iterators = Map.empty
    }

reopen :: Model blk -> Model blk
reopen m = m{isOpen = True}

wipeVolatileDB ::
  forall blk.
  (LedgerSupportsProtocol blk, LedgerTablesAreTrivial (ExtLedgerState blk)) =>
  TopLevelConfig blk ->
  Model blk ->
  (Point blk, Model blk)
wipeVolatileDB cfg m =
  (tipPoint m', reopen m')
 where
  m' =
    (closeDB m)
      { volatileDbBlocks = Map.empty
      , -- TODO: Currently, the SUT has no persistence of Peras certs across
        -- restarts, but this will change. There are at least two options:
        --
        --  * Change this command to mean "wipe volatile state" (including
        --    volatile certificates)
        --
        --  * Add a separate "Wipe volatile certs".
        perasCerts = Map.empty
      , cps = CPS.switchFork newChain (cps m)
      , currentLedger = newLedger
      , invalid = Map.empty
      , -- The LoE fragment must be anchored in an immutable point. Wiping the
        -- VolDB can invalidate this when some immutable blocks have not yet
        -- been persisted.
        loeFragment = Fragment.Empty Fragment.AnchorGenesis <$ loeFragment m
      }

  -- Get the chain ending at the ImmutableDB by doing chain selection on the
  -- sole candidate (or none) in the ImmutableDB.
  newChain :: Chain blk
  newLedger :: ExtLedgerState blk EmptyMK
  (newChain, newLedger) =
    isSameAsImmutableDbChain
      $ selectChain
        (Proxy @(BlockProtocol blk))
        (projectChainOrderConfig (configBlock cfg))
        -- Weight is inconsequential as there is only a single candidate.
        ( weightedSelectView (configBlock cfg) emptyPerasWeightSnapshot
            . Chain.toAnchoredFragment
            . fmap getHeader
        )
        Chain.genesis
      $ snd
      $ validChains cfg m (immutableDbBlocks m)

  isSameAsImmutableDbChain = \case
    Nothing
      | Chain.null (immutableDbChain m) ->
          (Chain.Genesis, initLedger m)
      | otherwise ->
          error "Did not select any chain"
    Just res@(chain, _ledger)
      | toHashes chain == toHashes (immutableDbChain m) ->
          res
      | otherwise ->
          error "Did not select the ImmutableDB's chain"

  toHashes = map blockHash . Chain.toOldestFirst

-- | Look in the given blocks database for a fragment spanning from the given
-- anchor to the given hash, and return the fragment in question, or 'Nothing'.
getFragmentBetween ::
  forall blk.
  GetPrevHash blk =>
  -- | A map of blocks; usually the 'volatileDbBlocks' of a 'Model'.
  Map (HeaderHash blk) blk ->
  -- | The anchor of the fragment to get.
  Fragment.Anchor blk ->
  -- | The hash of the block to get the fragment up to.
  ChainHash blk ->
  Maybe (AnchoredFragment blk)
getFragmentBetween bs anchor = go
 where
  go :: ChainHash blk -> Maybe (AnchoredFragment blk)
  go hash
    | hash == Fragment.anchorToHash anchor =
        Just $ Fragment.Empty anchor
  go GenesisHash =
    Nothing
  go (BlockHash hash) = do
    block <- Map.lookup hash bs
    prevFragment <- go $ blockPrevHash block
    Just $ prevFragment Fragment.:> block
