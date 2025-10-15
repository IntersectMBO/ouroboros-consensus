{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Main tests for the chain DB.
--
-- These are the main tests for the chain DB. Commands include
--
-- * Add a block
-- * Get the current chain and/or ledger state
-- * Create a new iterator and use it to stream blocks
-- * Create a new follower and use it to follow the chain
-- * (Limited) disk corruption (the chain DB relies on the immutable DB and
--   volatile DB for the storage proper and /they/ have extensive disk corruption
--   tests, so we don't need to repeat that here).
module Test.Ouroboros.Storage.ChainDB.StateMachine
  ( -- * Commands
    At (..)
  , Cmd (..)
  , FollowerRef
  , IterRef
  , IteratorResult (..)
  , IteratorResultGCed (..)

    -- * Responses
  , Resp (..)
  , Success (..)

    -- * Model
  , Model
  , ShouldGarbageCollect (..)

    -- * Running the model
  , runCmdsLockstep

    -- * System under test
  , ChainDBEnv (..)
  , ChainDBState (..)
  , close
  , mkTestCfg
  , open
  , persistBlks

    -- * Specifying block components
  , AllComponents
  , allComponents

    -- * Constraints
  , TestConstraints

    -- * Tracing
  , traceEventName

    -- * Entry point to the tests
  , tests
  ) where

import Cardano.Ledger.BaseTypes (NonZero (..), unsafeNonZero)
import Codec.Serialise (Serialise)
import Control.Monad (replicateM, void)
import Control.ResourceRegistry
import Control.Tracer as CT
import Data.Bifoldable
import Data.Bifunctor
import qualified Data.Bifunctor.TH as TH
import Data.Bitraversable
import Data.ByteString.Lazy (ByteString)
import Data.Function (on)
import Data.Functor (($>))
import Data.Functor.Classes (Eq1, Show1)
import Data.Functor.Identity (Identity)
import Data.List (sortOn)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..))
import Data.Proxy
import Data.TreeDiff
import Data.Typeable
import Data.Void (Void)
import Data.Word (Word16, Word64)
import GHC.Generics (Generic)
import qualified Generics.SOP as SOP
import NoThunks.Class (AllowThunk (..))
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
  ( RelativeTime (..)
  , WithArrivalTime (..)
  )
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.HardFork.Abstract
import Ouroboros.Consensus.HardFork.Combinator.Abstract
  ( ImmutableEraParams
  )
import Ouroboros.Consensus.HeaderValidation
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Ledger.Inspect
import Ouroboros.Consensus.Ledger.SupportsProtocol
import Ouroboros.Consensus.Ledger.Tables.Utils
import Ouroboros.Consensus.Protocol.Abstract
import Ouroboros.Consensus.Storage.ChainDB hiding
  ( TraceFollowerEvent (..)
  )
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import qualified Ouroboros.Consensus.Storage.ChainDB.API.Types.InvalidBlockPunishment as InvalidBlockPunishment
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.Args as ChainDB
import Ouroboros.Consensus.Storage.Common (SizeInBytes)
import qualified Ouroboros.Consensus.Storage.ImmutableDB as ImmutableDB
import Ouroboros.Consensus.Storage.ImmutableDB.Chunks.Internal
  ( unsafeChunkNoToEpochNo
  )
import Ouroboros.Consensus.Storage.LedgerDB (LedgerSupportsLedgerDB)
import qualified Ouroboros.Consensus.Storage.LedgerDB.TraceEvent as LedgerDB
import qualified Ouroboros.Consensus.Storage.LedgerDB.V1.DbChangelog as DbChangelog
import qualified Ouroboros.Consensus.Storage.PerasCertDB as PerasCertDB
import qualified Ouroboros.Consensus.Storage.VolatileDB as VolatileDB
import Ouroboros.Consensus.Util (split)
import Ouroboros.Consensus.Util.CallStack
import Ouroboros.Consensus.Util.Condense (condense)
import Ouroboros.Consensus.Util.Enclose
import Ouroboros.Consensus.Util.IOLike hiding (invariant)
import Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF
import Ouroboros.Network.Block (ChainUpdate, MaxSlotNo)
import qualified Ouroboros.Network.Mock.Chain as Chain
import System.FS.Sim.MockFS (MockFS)
import qualified System.FS.Sim.MockFS as Mock
import Test.Ouroboros.Storage.ChainDB.Model
  ( FollowerId
  , IteratorId
  , ModelSupportsBlock
  , ShouldGarbageCollect (DoNotGarbageCollect, GarbageCollect)
  )
import qualified Test.Ouroboros.Storage.ChainDB.Model as Model
import Test.Ouroboros.Storage.Orphans ()
import Test.Ouroboros.Storage.TestBlock
import Test.QuickCheck hiding (forAll)
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Monadic as QC
import Test.StateMachine
import qualified Test.StateMachine.Labelling as C
import qualified Test.StateMachine.Sequential as QSM
import qualified Test.StateMachine.Types as QSM
import qualified Test.StateMachine.Types.Rank2 as Rank2
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.Util.ChainDB
import Test.Util.ChunkInfo
import Test.Util.Header (attachSlotTimeToFragment)
import Test.Util.Orphans.Arbitrary ()
import Test.Util.Orphans.ToExpr ()
import Test.Util.QuickCheck
import Test.Util.RefEnv (RefEnv)
import qualified Test.Util.RefEnv as RE
import Test.Util.SOP
import Test.Util.TestEnv (adjustQuickCheckTests)
import Test.Util.ToExpr ()
import Test.Util.Tracer (recordingTracerIORef)
import Test.Util.WithEq

{-------------------------------------------------------------------------------
  Abstract model
-------------------------------------------------------------------------------}

-- | A randomly generated value that gets persisted between steps, so that we
-- more details.
newtype Persistent a = Persistent {unPersistent :: a}
  deriving (Eq, Show, Functor)

-- | Commands
data Cmd blk it flr
  = -- | Add a block, with (possibly) some gap blocks leading to it.
    -- For more information about gap blocks, refer to 'GenState' below.
    AddBlock blk (Persistent [blk])
  | -- | Add a Peras cert for a block, with (possibly) some gap blocks leading to it.
    -- For more information about gap blocks, refer to 'GenState' below.
    AddPerasCert (WithArrivalTime (ValidatedPerasCert blk)) (Persistent [blk])
  | GetCurrentChain
  | GetTipBlock
  | GetTipHeader
  | GetTipPoint
  | GetBlockComponent (RealPoint blk)
  | -- | Only for blocks that may have been garbage collected.
    GetGCedBlockComponent (RealPoint blk)
  | GetMaxSlotNo
  | GetIsValid (RealPoint blk)
  | Stream (StreamFrom blk) (StreamTo blk)
  | -- | Update the LoE fragment and run chain selection.
    UpdateLoE (AnchoredFragment blk)
  | IteratorNext it
  | -- | Only for blocks that may have been garbage collected.
    IteratorNextGCed it
  | IteratorClose it
  | NewFollower ChainType
  | -- | 'followerInstructionBlocking' is excluded, as it requires multiple
    -- threads. Its code path is pretty much the same as 'followerInstruction'
    -- anyway.
    FollowerInstruction flr
  | FollowerForward flr [Point blk]
  | FollowerClose flr
  | Close
  | Reopen
  | -- Internal

    -- | Copy the blocks older than @k@ from the Volatile DB to the Immutable
    -- DB.
    PersistBlks
  | -- | Copy the blocks older than @k@ from the Volatile DB to the Immutable
    -- DB __and then__ perform garbage colllection.
    --
    -- The garbage collection procedure of the Chain DB (our system under test)
    -- removes the blocks from the volatile DB __without__ caring about whether
    -- the removed blocks were persisted. Therefore, this part of the Chain DB
    -- logic assumes that copy to the immutable DB took place __before__
    -- garbage collection. The model uses this assumption as well. As a result,
    -- we cannot perform garbage collection in isolation, since this will break
    -- the model's 'invariant'.
    PersistBlksThenGC
  | -- | Write a new 'LedgerDB' snapshot to disk and remove the oldest ones.
    UpdateLedgerSnapshots
  | -- Corruption
    WipeVolatileDB
  deriving (Generic, Show, Functor, Foldable, Traversable)

-- = Invalid blocks
--
-- We don't test 'getIsInvalidBlock' because the simple chain selection in the
-- model and the incremental chain selection in the real implementation differ
-- non-trivially.
--
-- In the real chain selection, if a block is invalid, no chains containing
-- that block will be validated again. So if a successor of the block is added
-- afterwards, it will never be validated, as any chain it would be part of
-- would also contain the block we know to be invalid. So if the successor is
-- also invalid, we would never discover and record it (as there is no need
-- from the point of chain selection, as we'll never use it in a candidate
-- chain anyway). In the model implementation of chain selection, all possible
-- chains are (re)validated each time, including previously invalid chains, so
-- new invalid blocks that are successors of known invalid blocks /are/ being
-- validated and recorded as invalid blocks.
--
-- Further complicating this is the fact that the recorded invalid blocks are
-- also garbage-collected. We can work around this, just like for 'getBlock'.
--
-- While it is certainly possible to overcome the issues described above,
-- e.g., we could change the model to closer match the real implementation
-- (but at the cost of more complexity), it is not worth the effort. The whole
-- point of recording invalid blocks is to avoid constructing candidates
-- containing known invalid blocks and needlessly validating them, which is
-- something we are testing in 'prop_trace', see
-- 'invalidBlockNeverValidatedAgain'.

deriving instance SOP.Generic (Cmd blk it flr)
deriving instance SOP.HasDatatypeInfo (Cmd blk it flr)

-- | Return type for successful database operations.
data Success blk it flr
  = Unit ()
  | Chain (AnchoredFragment (Header blk))
  | LedgerDB (DbChangelog.DbChangelog' blk)
  | MbBlock (Maybe blk)
  | MbAllComponents (Maybe (AllComponents blk))
  | MbGCedAllComponents (MaybeGCedBlock (AllComponents blk))
  | MbHeader (Maybe (Header blk))
  | Point (Point blk)
  | IsValid IsValidResult
  | UnknownRange (UnknownRange blk)
  | Iter it
  | IterResult (IteratorResult blk (AllComponents blk))
  | IterResultGCed (IteratorResultGCed blk)
  | Flr flr
  | MbChainUpdate (Maybe (ChainUpdate blk (AllComponents blk)))
  | MbPoint (Maybe (Point blk))
  | MaxSlot MaxSlotNo
  deriving (Functor, Foldable, Traversable)

-- | Product of all 'BlockComponent's. As this is a GADT, generating random
-- values of it (and combinations!) is not so simple. Therefore, we just
-- always request all block components.
allComponents :: BlockComponent blk (AllComponents blk)
allComponents =
  (,,,,,,,,,,)
    <$> GetVerifiedBlock
    <*> GetBlock
    <*> GetHeader
    <*> GetRawBlock
    <*> GetRawHeader
    <*> GetHash
    <*> GetSlot
    <*> GetIsEBB
    <*> GetBlockSize
    <*> GetHeaderSize
    <*> GetNestedCtxt

-- | A list of all the 'BlockComponent' indices (@b@) we are interested in.
type AllComponents blk =
  ( blk
  , blk
  , Header blk
  , ByteString
  , ByteString
  , HeaderHash blk
  , SlotNo
  , IsEBB
  , SizeInBytes
  , Word16
  , SomeSecond (NestedCtxt Header) blk
  )

type TestConstraints blk =
  ( ConsensusProtocol (BlockProtocol blk)
  , LedgerSupportsProtocol blk
  , BlockSupportsDiffusionPipelining blk
  , InspectLedger blk
  , Eq (ChainDepState (BlockProtocol blk))
  , Eq (LedgerState blk EmptyMK)
  , Eq blk
  , Show blk
  , HasHeader blk
  , StandardHash blk
  , Serialise blk
  , ModelSupportsBlock blk
  , Eq (Header blk)
  , Show (Header blk)
  , ConvertRawHash blk
  , HasHardForkHistory blk
  , SerialiseDiskConstraints blk
  , Show (LedgerState blk EmptyMK)
  , LedgerTablesAreTrivial (LedgerState blk)
  , LedgerSupportsLedgerDB blk
  , ImmutableEraParams blk
  )

deriving instance
  (TestConstraints blk, Eq it, Eq flr) =>
  Eq (Success blk it flr)
deriving instance
  (TestConstraints blk, Show it, Show flr) =>
  Show (Success blk it flr)

instance ModelSupportsBlock TestBlock

-- | Short-hand
type TestIterator m blk = WithEq (Iterator m blk (AllComponents blk))

-- | Short-hand
type TestFollower m blk = WithEq (Follower m blk (AllComponents blk))

-- | The current ChainDB instance and things related to it.
--
-- When closing and reopening the ChainDB, this record will be replaced in the
-- 'varDB' field of 'ChainDBEnv' with a new one.
data ChainDBState m blk = ChainDBState
  { chainDB :: ChainDB m blk
  , internal :: ChainDB.Internal m blk
  , addBlockAsync :: Async m Void
  -- ^ Background thread that adds blocks to the ChainDB
  }
  deriving NoThunks via AllowThunk (ChainDBState m blk)

-- | Environment to run commands against the real ChainDB implementation.
data ChainDBEnv m blk = ChainDBEnv
  { varDB :: StrictTVar m (ChainDBState m blk)
  , registry :: ResourceRegistry m
  , varNextId :: StrictTVar m Id
  , varVolatileDbFs :: StrictTMVar m MockFS
  , args :: ChainDbArgs Identity m blk
  -- ^ Needed to reopen a ChainDB, i.e., open a new one.
  , varLoEFragment :: StrictTVar m (AnchoredFragment (HeaderWithTime blk))
  }

open ::
  (IOLike m, TestConstraints blk) =>
  ChainDbArgs Identity m blk -> m (ChainDBState m blk)
open args = do
  (chainDB, internal) <- openDBInternal args False
  addBlockAsync <- async (intAddBlockRunner internal)
  link addBlockAsync
  return ChainDBState{chainDB, internal, addBlockAsync}

-- PRECONDITION: the ChainDB is closed
reopen ::
  (IOLike m, TestConstraints blk) =>
  ChainDBEnv m blk -> m ()
reopen ChainDBEnv{varDB, args} = do
  chainDBState <- open args
  void $ atomically $ swapTVar varDB chainDBState

close :: IOLike m => ChainDBState m blk -> m ()
close ChainDBState{chainDB, addBlockAsync} = do
  cancel addBlockAsync
  closeDB chainDB

run ::
  forall m blk.
  (IOLike m, TestConstraints blk) =>
  TopLevelConfig blk ->
  ChainDBEnv m blk ->
  Cmd blk (TestIterator m blk) (TestFollower m blk) ->
  m (Success blk (TestIterator m blk) (TestFollower m blk))
run cfg env@ChainDBEnv{varDB, ..} cmd =
  readTVarIO varDB >>= \st@ChainDBState{chainDB = chainDB@ChainDB{..}, internal} -> case cmd of
    AddBlock blk _ -> Point <$> advanceAndAdd st blk
    AddPerasCert cert _ -> Unit <$> addPerasCertSync chainDB cert
    GetCurrentChain -> Chain <$> atomically getCurrentChain
    GetTipBlock -> MbBlock <$> getTipBlock
    GetTipHeader -> MbHeader <$> getTipHeader
    GetTipPoint -> Point <$> atomically getTipPoint
    GetBlockComponent pt -> MbAllComponents <$> getBlockComponent allComponents pt
    GetGCedBlockComponent pt -> mbGCedAllComponents <$> getBlockComponent allComponents pt
    GetIsValid pt -> isValidResult <$> ($ pt) <$> atomically getIsValid
    GetMaxSlotNo -> MaxSlot <$> atomically getMaxSlotNo
    UpdateLoE frag -> Point <$> updateLoE st frag
    Stream from to -> iter =<< stream registry allComponents from to
    IteratorNext it -> IterResult <$> iteratorNext (unWithEq it)
    IteratorNextGCed it -> iterResultGCed <$> iteratorNext (unWithEq it)
    IteratorClose it -> Unit <$> iteratorClose (unWithEq it)
    NewFollower ct -> follower =<< newFollower registry ct allComponents
    FollowerInstruction flr -> MbChainUpdate <$> followerInstruction (unWithEq flr)
    FollowerForward flr pts -> MbPoint <$> followerForward (unWithEq flr) pts
    FollowerClose flr -> Unit <$> followerClose (unWithEq flr)
    Close -> Unit <$> close st
    Reopen -> Unit <$> reopen env
    PersistBlks -> ignore <$> persistBlks DoNotGarbageCollect internal
    PersistBlksThenGC -> ignore <$> persistBlks GarbageCollect internal
    UpdateLedgerSnapshots -> ignore <$> intTryTakeSnapshot internal
    WipeVolatileDB -> Point <$> wipeVolatileDB st
 where
  mbGCedAllComponents = MbGCedAllComponents . MaybeGCedBlock True
  isValidResult = IsValid . IsValidResult True
  iterResultGCed = IterResultGCed . IteratorResultGCed True
  iter = either (return . UnknownRange) (fmap Iter . giveWithEq)
  follower = fmap Flr . giveWithEq
  ignore _ = Unit ()

  advanceAndAdd :: ChainDBState m blk -> blk -> m (Point blk)
  advanceAndAdd ChainDBState{chainDB} blk = do
    -- `blockProcessed` always returns 'Just'
    res <- addBlock chainDB InvalidBlockPunishment.noPunishment blk
    return $ case res of
      FailedToAddBlock f -> error $ "advanceAndAdd: block not added - " ++ f
      SuccesfullyAddedBlock pt -> pt

  updateLoE :: ChainDBState m blk -> AnchoredFragment blk -> m (Point blk)
  updateLoE ChainDBState{chainDB} frag = do
    let headersFrag = AF.mapAnchoredFragment getHeader frag
    atomically $ writeTVar varLoEFragment $ attachSlotTimeToFragment cfg headersFrag
    ChainDB.triggerChainSelection chainDB
    atomically $ getTipPoint chainDB

  wipeVolatileDB :: ChainDBState m blk -> m (Point blk)
  wipeVolatileDB st = do
    close st
    atomically $ do
      writeTMVar varVolatileDbFs Mock.empty
      -- The LoE fragment must be anchored in an immutable point. Wiping the
      -- VolDB can invalidate this when some immutable blocks have not yet
      -- been persisted.
      writeTVar varLoEFragment $ AF.Empty AF.AnchorGenesis
    reopen env
    ChainDB{getTipPoint} <- chainDB <$> readTVarIO varDB
    atomically getTipPoint

  giveWithEq :: a -> m (WithEq a)
  giveWithEq a =
    fmap (`WithEq` a) $ atomically $ stateTVar varNextId $ \i -> (i, succ i)

persistBlks :: IOLike m => ShouldGarbageCollect -> ChainDB.Internal m blk -> m ()
persistBlks collectGarbage ChainDB.Internal{..} = do
  mSlotNo <- intCopyToImmutableDB
  case (collectGarbage, mSlotNo) of
    (DoNotGarbageCollect, _) -> pure ()
    (GarbageCollect, Origin) -> pure ()
    (GarbageCollect, NotOrigin slotNo) -> intGarbageCollect slotNo

-- | Result type for 'getBlock'. Note that the real implementation of
-- 'getBlock' is non-deterministic: if the requested block is older than @k@
-- and not part of the current chain, it might have been garbage collected.
--
-- The first source of non-determinism is whether or not the background thread
-- that performs garbage collection has been run yet. We disable this thread in
-- the state machine tests and instead generate the 'PersistBlksThenGC'
-- command that triggers the garbage collection explicitly, after persisting
-- the blocks older than @k@ from the current chain's tip. So this source of
-- non-determinism is not a problem in the tests.
--
-- However, there is a second source of non-determinism: if a garbage
-- collection has been performed and the block was eligible for collection, it
-- might still not have been removed because it was part of a file that
-- contained other blocks that cannot be garbage collected yet. So the block
-- is still left in the VolatileDB. We do not try to imitate this behaviour,
-- which would couple the test too tightly to the actual implementation.
-- Instead, we accept this source of non-determinism and are more lenient when
-- comparing the results of 'getBlock' when the block may have been garbage
-- collected.
--
-- Equality of two 'MaybeGCedBlock' is determined as follows:
-- * If both are produced by a model implementation, then the @Maybe blk@s must
--   be equal, as the these results are deterministic.
-- * If at least one of them is produced by a real implementation, then:
--   * If either is 'Nothing', which means the block might have been
--     garbage-collected, then they are equal (even if the other is 'Just',
--     which means it was not yet garbage-collected).
--   * If both are 'Just's, then the blocks must be equal.
--
-- In practice, this equality is used when comparing the result of the real
-- implementation with the result of the model implementation.
data MaybeGCedBlock blk = MaybeGCedBlock
  { real :: Bool
  -- ^ 'True':  result of calling 'getBlock' on the real implementation
  -- ^ 'False': result of calling 'getBlock' on the model implementation
  , mbBlock :: Maybe blk
  -- ^ A value of 'Nothing' in this field indicates that the block might have
  -- been garbage collected.
  }
  deriving Show

instance Eq blk => Eq (MaybeGCedBlock blk) where
  MaybeGCedBlock real1 mbBlock1 == MaybeGCedBlock real2 mbBlock2 =
    -- Two @MaybeGCedBlock@s are equal iff either:
    --
    -- - they are both produced by the model and contain the same result, or
    -- - at least one of them was garbage collected, or
    -- - none of them were garbage collected and they contain the same block.
    --
    -- See the comments on 'MaybeGCedBlock' for a justification on why we
    -- implemented this form of lenient equality.
    case (real1, real2) of
      (False, False) -> mbBlock1 == mbBlock2
      (True, _) -> eqIfJust
      (_, True) -> eqIfJust
   where
    eqIfJust = case (mbBlock1, mbBlock2) of
      (Just b1, Just b2) -> b1 == b2
      _ -> True

-- | Similar to 'MaybeGCedBlock', but for the block returned by
-- 'iteratorNext'. A garbage-collected block could result in
-- 'IteratorBlockGCed' instead of 'IteratorResult'.
data IteratorResultGCed blk = IteratorResultGCed
  { real :: Bool
  -- ^ 'True':  result of calling 'getBlock' on the real implementation
  -- ^ 'False': result of calling 'getBlock' on the model implementation
  , iterResult :: IteratorResult blk (AllComponents blk)
  }

deriving instance
  ( Show blk
  , Show (Header blk)
  , StandardHash blk
  , HasNestedContent Header blk
  ) =>
  Show (IteratorResultGCed blk)

instance
  (Eq blk, Eq (Header blk), StandardHash blk, HasNestedContent Header blk) =>
  Eq (IteratorResultGCed blk)
  where
  IteratorResultGCed real1 iterResult1 == IteratorResultGCed real2 iterResult2 =
    case (real1, real2) of
      (False, False) -> iterResult1 == iterResult2
      (True, _) -> eqIfNotGCed
      (_, True) -> eqIfNotGCed
   where
    eqIfNotGCed = case (iterResult1, iterResult2) of
      (IteratorBlockGCed{}, _) -> True
      (_, IteratorBlockGCed{}) -> True
      (IteratorResult b1, IteratorResult b2) -> b1 == b2
      (IteratorExhausted, IteratorExhausted) -> True
      _ -> False

-- | The model knows about all valid blocks whereas the real implementation
-- only knows about blocks that have been validated in the VolatileDB if they
-- were part of a chain selected by chain selection.
--
-- 'Nothing' means the validity of the block is unknown.
--
-- When the real implementation returned 'Nothing', we ignore the result of
-- the model. If the model returned 'Nothing', the real implementation must
-- too. In the 'Just' case, the result of the implementation and the model
-- must match.
data IsValidResult = IsValidResult
  { real :: Bool
  -- ^ 'True':  result of calling 'getIsValid' on the real implementation
  -- ^ 'False': result of calling 'getIsValid' on the model implementation
  , isValid :: Maybe Bool
  }
  deriving Show

instance Eq IsValidResult where
  IsValidResult real1 isValid1 == IsValidResult real2 isValid2 =
    case (real1, real2) of
      (False, False) -> isValid1 == isValid2
      (True, False) -> realMatchesModel isValid1 isValid2
      (False, True) -> realMatchesModel isValid2 isValid1
      (True, True) -> eqIfJust
   where
    eqIfJust = case (isValid1, isValid2) of
      (Just x1, Just x2) -> x1 == x2
      _ -> True

    realMatchesModel real model = case (real, model) of
      (Just x1, Just x2) -> x1 == x2
      (Nothing, Nothing) -> True
      (Nothing, Just _) -> True
      (Just _, Nothing) -> False

{-------------------------------------------------------------------------------
  Responses
-------------------------------------------------------------------------------}

-- | Responses are either successful termination or an error.
newtype Resp blk it flr = Resp
  {getResp :: Either (ChainDbError blk) (Success blk it flr)}
  deriving (Functor, Foldable, Traversable)

deriving instance
  (TestConstraints blk, Show it, Show flr) =>
  Show (Resp blk it flr)

instance (TestConstraints blk, Eq it, Eq flr) => Eq (Resp blk it flr) where
  Resp (Left e) == Resp (Left e') = e == e'
  Resp (Right a) == Resp (Right a') = a == a'
  _ == _ = False

{-------------------------------------------------------------------------------
  Bitraversable instances
-------------------------------------------------------------------------------}

TH.deriveBifunctor ''Cmd
TH.deriveBifoldable ''Cmd
TH.deriveBitraversable ''Cmd

TH.deriveBifunctor ''Success
TH.deriveBifoldable ''Success
TH.deriveBitraversable ''Success

TH.deriveBifunctor ''Resp
TH.deriveBifoldable ''Resp
TH.deriveBitraversable ''Resp

{-------------------------------------------------------------------------------
  Instantiating the semantics
-------------------------------------------------------------------------------}

type DBModel blk = Model.Model blk

-- We can't reuse 'run' because the 'ChainDB' API uses 'STM'. Instead, we call
-- the model directly.
runPure ::
  forall blk.
  TestConstraints blk =>
  TopLevelConfig blk ->
  Cmd blk IteratorId FollowerId ->
  DBModel blk ->
  (Resp blk IteratorId FollowerId, DBModel blk)
runPure cfg = \case
  AddBlock blk _ -> ok Point $ update (add blk)
  AddPerasCert cert _ -> ok Unit $ ((),) . update (Model.addPerasCert cfg cert)
  GetCurrentChain -> ok Chain $ query (Model.volatileChain k getHeader)
  GetTipBlock -> ok MbBlock $ query Model.tipBlock
  GetTipHeader -> ok MbHeader $ query (fmap getHeader . Model.tipBlock)
  GetTipPoint -> ok Point $ query Model.tipPoint
  GetBlockComponent pt -> err MbAllComponents $ query (Model.getBlockComponentByPoint allComponents pt)
  GetGCedBlockComponent pt -> err mbGCedAllComponents $ query (Model.getBlockComponentByPoint allComponents pt)
  GetMaxSlotNo -> ok MaxSlot $ query Model.getMaxSlotNo
  GetIsValid pt -> ok isValidResult $ query (Model.isValid pt)
  UpdateLoE frag -> ok Point $ update (Model.updateLoE cfg frag)
  Stream from to -> err iter $ updateE (Model.stream k from to)
  IteratorNext it -> ok IterResult $ update (Model.iteratorNext it allComponents)
  IteratorNextGCed it -> ok iterResultGCed $ update (Model.iteratorNext it allComponents)
  IteratorClose it -> ok Unit $ update_ (Model.iteratorClose it)
  -- As tentative followers differ from normal followers only during chain
  -- selection, this test can not distinguish between them due to its
  -- sequential nature. Hence, we don't add a pure model for tentative
  -- followers.
  NewFollower _ -> ok Flr $ update Model.newFollower
  FollowerInstruction flr -> err MbChainUpdate $ updateE (Model.followerInstruction flr allComponents)
  FollowerForward flr pts -> err MbPoint $ updateE (Model.followerForward flr pts)
  FollowerClose flr -> ok Unit $ update_ (Model.followerClose flr)
  PersistBlks -> ok Unit $ update_ (Model.copyToImmutableDB k DoNotGarbageCollect)
  PersistBlksThenGC -> ok Unit $ update_ (Model.copyToImmutableDB k GarbageCollect)
  -- TODO: The model does not capture the notion of ledger snapshots,
  -- therefore we ignore this command here. This introduces an assymetry in
  -- the way the 'UpdateLedgerSnapshots' command is handled in the model and
  -- in the system under test. It would be better if we modelled the
  -- snapshots so that this aspect of the system would be explicitly
  -- specified. See https://github.com/IntersectMBO/ouroboros-network/issues/3375
  --
  UpdateLedgerSnapshots -> ok Unit $ ((),)
  Close -> openOrClosed $ update_ Model.closeDB
  Reopen -> openOrClosed $ update_ Model.reopen
  WipeVolatileDB -> ok Point $ update (Model.wipeVolatileDB cfg)
 where
  k = configSecurityParam cfg

  add blk m = (Model.tipPoint m', m')
   where
    m' = Model.addBlock cfg blk m

  iter = either UnknownRange Iter
  mbGCedAllComponents = MbGCedAllComponents . MaybeGCedBlock False
  iterResultGCed = IterResultGCed . IteratorResultGCed False
  isValidResult = IsValid . IsValidResult False

  query f m = (f m, m)

  update f m = f m
  update_ f m = ((), f m)
  updateE f m = case f m of
    Left e -> (Left e, m)
    Right (a, m') -> (Right a, m')

  -- Only executed when the ChainDB is open, otherwise a 'ClosedDBError' is
  -- returned.
  ok toSuccess f = err toSuccess (first Right . f)
  err toSuccess f m
    | Model.isOpen m =
        first (Resp . fmap toSuccess) (f m)
    | otherwise =
        (Resp (Left (ClosedDBError prettyCallStack)), m)

  -- Executed whether the ChainDB is open or closed.
  openOrClosed f = first (Resp . Right . Unit) . f

runIO ::
  TestConstraints blk =>
  TopLevelConfig blk ->
  ChainDBEnv IO blk ->
  Cmd blk (TestIterator IO blk) (TestFollower IO blk) ->
  IO (Resp blk (TestIterator IO blk) (TestFollower IO blk))
runIO cfg env cmd = Resp <$> try (run cfg env cmd)

{-------------------------------------------------------------------------------
  Collect arguments
-------------------------------------------------------------------------------}

-- | Collect all iterators created.
iters :: Bitraversable t => t it flr -> [it]
iters = bifoldMap (: []) (const [])

-- | Collect all followers created.
flrs :: Bitraversable t => t it flr -> [flr]
flrs = bifoldMap (const []) (: [])

{-------------------------------------------------------------------------------
  Model
-------------------------------------------------------------------------------}

-- | Concrete or symbolic references to a real iterator
type IterRef blk m r = Reference (Opaque (TestIterator m blk)) r

-- | Mapping between iterator references and mocked iterators
type KnownIters blk m r = RefEnv (Opaque (TestIterator m blk)) IteratorId r

-- | Concrete or symbolic references to a real follower
type FollowerRef blk m r = Reference (Opaque (TestFollower m blk)) r

-- | Mapping between iterator references and mocked followers
type KnownFollowers blk m r = RefEnv (Opaque (TestFollower m blk)) FollowerId r

-- | Generator state to be carried around between commands
--
-- The main idea here is to keep intermediate blocks generated by 'genBlk'
-- around, so that we no longer need to pray for them to be (re)generated from
-- scratch in subsequent 'AddBlock' and 'AddPerasCert' invocations.
--
-- To illustrate this more concretely, suppose the test has so far built the
-- following chain:
--
-- @
--  A -> B -> C
-- @
--
-- From here, one of the options 'genBlk' has is to generate a dangling block
-- via 'genBlockAfterGap'. For this, it first builds a random fork and then
-- returns its tip. For instance, let's assume that 'genBlockAfterGap' branches
-- from @B@, and generates a block @F@, that fits on top of @B@ via some
-- (hypothetical) intermediate blocks @D@ and @E@:
--
-- @
-- A -> B -> C
--      |
--      + -> (D) -> (E) -> F
-- @
--
-- At this point, the state machine then runs @AddBlock F@, leaving the gap
-- between @B@ and @F@ to be potentially filled by other random invocations to
-- @AddBlock@ later on. This mechanism allows the state machine to generate
-- invocations leading to interesting forks from time to time.
--
-- However, for that to happen, we really want to keep @D@ and @E@ around, so
-- that they can easily be reused to fill the gap leading to @F@. Otherwise,
-- 'genBlk' would depend entirely on randomness to regenerate these (or
-- compatible) intermediate blocks leading to @F@ from scratch, which is in
-- turn very unlikely for any non-trivial fork, as new random blocks can only
-- be added _on top_ of existing ones (i.e. with no way of backtracking from 'F'
-- to 'B' to fill the gap).
--
-- To tackle this problem, the model carries around an evolving 'GenState' that
-- keeps track of all intermediate blocks generated so far. To do this, note
-- that some block generators return not only a block, but also a collection of
-- intermediate blocks that were generated along the way and that lead to the
-- final block being returned. We then update the 'GenState' accordingly after
-- each command execution (see 'updateGenState' below). Later, 'genBlk' can also
-- sample previosuly seen blocks directly from the 'GenState' (via the
-- 'genGapBlock' sub-generator).
--
-- NOTE: 'quickcheck-state-machine' does not provide much support for this, so
-- we need to manually carry it around as part of the evolving SUT's model--even
-- if it's technically not part of the actual model we are trying to test
-- against.
--
-- TODO: Explore if this can be improved by tweaking the API of
-- 'quickcheck-state-machine' to allow for the same functionality to exist
-- under the hood.
data GenState blk
  = GenState
  { seenBlocks :: Map (HeaderHash blk) blk
  -- ^ Blocks that have been generated but not yet added to the ChainDB, e.g.,
  -- gap blocks generated by 'genBlockAfterGap', or boosted blocks generated by
  -- 'genAddPerasCert'. We don't want to discard these because they can be used
  -- to fill gaps between existing blocks added via 'AddBlock', simulating
  -- blocks and certificates arriving out of order.
  }
  deriving Generic

deriving instance
  ( ToExpr blk
  , ToExpr (HeaderHash blk)
  ) =>
  ToExpr (GenState blk)

deriving instance (Show blk, Show (HeaderHash blk)) => Show (GenState blk)

emptyGenState :: GenState blk
emptyGenState =
  GenState
    { seenBlocks = Map.empty
    }

-- | Use the extra state stored in a generated command to update a model's
-- 'GenState' accordingly.
updateGenState ::
  HasHeader blk =>
  At Cmd blk m r ->
  GenState blk ->
  GenState blk
updateGenState cmd gs =
  case unAt cmd of
    AddBlock _ (Persistent blks) -> saveSeenBlocks blks gs
    AddPerasCert _ (Persistent blks) -> saveSeenBlocks blks gs
    _ -> gs
 where
  saveSeenBlocks blks gs' =
    gs'
      { seenBlocks =
          Map.union
            (Map.fromList [(blockHash blk, blk) | blk <- blks])
            (seenBlocks gs')
      }

-- | Execution model
data Model blk m r = Model
  { dbModel :: DBModel blk
  , knownIters :: KnownIters blk m r
  , knownFollowers :: KnownFollowers blk m r
  , modelConfig :: Opaque (TopLevelConfig blk)
  , genState :: GenState blk
  -- ^ Generator state that evolves in tandem with the model.
  -- See the definition of 'GenState' above for more details.
  }
  deriving Generic

deriving instance (TestConstraints blk, Show1 r) => Show (Model blk m r)

-- | Initial model
initModel ::
  HasHeader blk =>
  LoE () ->
  TopLevelConfig blk ->
  ExtLedgerState blk EmptyMK ->
  Model blk m r
initModel loe cfg initLedger =
  Model
    { dbModel = Model.empty loe initLedger
    , knownIters = RE.empty
    , knownFollowers = RE.empty
    , modelConfig = QSM.Opaque cfg
    , genState = emptyGenState
    }

-- | Key property of the model is that we can go from real to mock responses
toMock ::
  (Bifunctor (t blk), Eq1 r) =>
  Model blk m r -> At t blk m r -> t blk IteratorId FollowerId
toMock Model{..} (At t) = bimap (knownIters RE.!) (knownFollowers RE.!) t

-- | Step the mock semantics
--
-- We cannot step the whole Model here (see 'event', below)
step ::
  (TestConstraints blk, Eq1 r) =>
  Model blk m r ->
  At Cmd blk m r ->
  (Resp blk IteratorId FollowerId, DBModel blk)
step model@Model{dbModel, modelConfig} cmd =
  runPure (QSM.unOpaque modelConfig) (toMock model cmd) dbModel

{-------------------------------------------------------------------------------
  Wrapping in quickcheck-state-machine references
-------------------------------------------------------------------------------}

-- | Instantiate functor @t blk@ to
-- @t blk ('IterRef' blk m r) ('FollowerRef' blk m r)@.
--
-- Needed because we need to (partially) apply @'At' t blk flr m@ to @r@.
newtype At t blk m r = At {unAt :: t blk (IterRef blk m r) (FollowerRef blk m r)}
  deriving Generic

deriving newtype instance
  Show (t blk (IterRef blk m r) (FollowerRef blk m r)) =>
  Show (At t blk m r)

deriving instance (TestConstraints blk, Eq1 r) => Eq (At Resp blk m r)

instance Bifunctor (t blk) => Rank2.Functor (At t blk m) where
  fmap = \f (At x) -> At (bimap (app f) (app f) x)
   where
    app :: (r x -> r' x) -> QSM.Reference x r -> QSM.Reference x r'
    app f (QSM.Reference x) = QSM.Reference (f x)

instance Bifoldable (t blk) => Rank2.Foldable (At t blk m) where
  foldMap = \f (At x) -> bifoldMap (app f) (app f) x
   where
    app :: (r x -> n) -> QSM.Reference x r -> n
    app f (QSM.Reference x) = f x

instance Bitraversable (t blk) => Rank2.Traversable (At t blk m) where
  traverse = \f (At x) -> At <$> bitraverse (app f) (app f) x
   where
    app ::
      Functor f =>
      (r x -> f (r' x)) -> QSM.Reference x r -> f (QSM.Reference x r')
    app f (QSM.Reference x) = QSM.Reference <$> f x

{-------------------------------------------------------------------------------
  Events
-------------------------------------------------------------------------------}

-- | An event records the model before and after a command along with the
-- command itself, and a mocked version of the response.
data Event blk m r = Event
  { eventBefore :: Model blk m r
  , eventCmd :: At Cmd blk m r
  , eventAfter :: Model blk m r
  , eventMockResp :: Resp blk IteratorId FollowerId
  }

deriving instance (TestConstraints blk, Show1 r) => Show (Event blk m r)

-- | Construct an event
lockstep ::
  (TestConstraints blk, Eq1 r, Show1 r) =>
  Model blk m r ->
  At Cmd blk m r ->
  At Resp blk m r ->
  Event blk m r
lockstep model@Model{..} cmd (At resp) =
  Event
    { eventBefore = model
    , eventCmd = cmd
    , eventAfter = model'
    , eventMockResp = mockResp
    }
 where
  (mockResp, dbModel') = step model cmd
  genState' = updateGenState cmd genState
  newIters = RE.fromList $ zip (iters resp) (iters mockResp)
  newFollowers = RE.fromList $ zip (flrs resp) (flrs mockResp)
  model' = case unAt cmd of
    -- When closing the database, all open iterators and followers are closed
    -- too, so forget them.
    Close ->
      model
        { dbModel = dbModel'
        , genState = genState'
        , knownIters = RE.empty
        , knownFollowers = RE.empty
        }
    WipeVolatileDB ->
      model
        { dbModel = dbModel'
        , genState = genState'
        , knownIters = RE.empty
        , knownFollowers = RE.empty
        }
    _ ->
      model
        { dbModel = dbModel'
        , genState = genState'
        , knownIters = knownIters `RE.union` newIters
        , knownFollowers = knownFollowers `RE.union` newFollowers
        }

{-------------------------------------------------------------------------------
  Generator
-------------------------------------------------------------------------------}

-- | Generate a 'Cmd'
--
-- NOTE: the frequencies for generating blocks and Peras certificates are
-- substantially higher than for other commands. This is to ensure that the
-- ChainDB is populated with enough blocks to exhibit interesting weighted chain
-- selection behaviour, e.g., switching to a different (possibly shorter) fork.
--
-- Keep this in mind when adjusting the frequencies below, making sure not to
-- degrade the probability of triggering the more uncommon events too much. For
-- this, see the output of the @Tags@ table generated by QuickCheck.
generator ::
  forall blk m.
  TestConstraints blk =>
  LoE () ->
  (Model blk m Symbolic -> Gen (blk, Persistent [blk])) ->
  Model blk m Symbolic ->
  Gen (At Cmd blk m Symbolic)
generator loe genBlock m@Model{..} =
  At
    <$> frequency
      [ (100, genAddBlock)
      , let freq = case loe of
              LoEDisabled -> 100
              -- The LoE does not yet support Peras.
              LoEEnabled () -> 0
         in (freq, genAddPerasCert)
      , (if empty then 1 else 10, return GetCurrentChain)
      , --    , (if empty then 1 else 10, return GetLedgerDB)
        (if empty then 1 else 10, return GetTipBlock)
      , -- To check that we're on the right chain
        (if empty then 1 else 10, return GetTipPoint)
      , (10, genGetBlockComponent)
      , (if empty then 1 else 10, return GetMaxSlotNo)
      , (if empty then 1 else 10, genGetIsValid)
      , let freq = case loe of
              LoEDisabled -> 0
              LoEEnabled () -> if empty then 1 else 10
         in (freq, UpdateLoE <$> genLoEFragment)
      , -- Iterators
        (if empty then 1 else 10, uncurry Stream <$> genBounds)
      , (if null iterators then 0 else 20, genIteratorNext)
      , -- Use a lower frequency for closing, so that the chance increases that
        -- we can stream multiple blocks from an iterator.
        (if null iterators then 0 else 2, genIteratorClose)
      , -- Followers
        (10, genNewFollower)
      , (if null followers then 0 else 10, genFollowerInstruction)
      , (if null followers then 0 else 10, genFollowerForward)
      , -- Use a lower frequency for closing, so that the chance increases that
        -- we can read multiple blocks from a follower
        (if null followers then 0 else 2, genFollowerClose)
      , (if empty then 1 else 10, return Close)
      ,
        ( if Model.isOpen dbModel
            then
              (if empty then 1 else 10)
            else 0
        , return Reopen
        )
      , -- Internal
        (if empty then 1 else 10, return PersistBlks)
      , (if empty then 1 else 10, return PersistBlksThenGC)
      , (if empty then 1 else 10, return UpdateLedgerSnapshots)
      , (if empty then 1 else 10, return WipeVolatileDB)
      ]
 where
  -- TODO adjust the frequencies after labelling

  cfg :: TopLevelConfig blk
  cfg = unOpaque modelConfig

  secParam :: SecurityParam
  secParam = configSecurityParam cfg

  iterators :: [Reference (Opaque (TestIterator m blk)) Symbolic]
  iterators = RE.keys knownIters

  followers :: [Reference (Opaque (TestFollower m blk)) Symbolic]
  followers = RE.keys knownFollowers

  genRandomPoint :: Gen (RealPoint blk)
  genRandomPoint = blockRealPoint . fst <$> genBlock m

  blocksInDB :: Map.Map (HeaderHash blk) blk
  blocksInDB = Model.blocks dbModel

  pointsInDB :: [RealPoint blk]
  pointsInDB = blockRealPoint <$> Map.elems blocksInDB

  genLoEFragment :: Gen (AnchoredFragment blk)
  genLoEFragment =
    frequency
      [ (1, return $ AF.Empty AF.AnchorGenesis)
      ,
        ( 20
        , flip suchThatMap id $ do
            -- Generate a fragment between an anchor in the ImmutableDB and a
            -- tip corresponding to either the immutable tip, a volatile block
            -- or a block not yet in the ChainDB.
            anchor <-
              elements $
                AF.AnchorGenesis : fmap AF.anchorFromBlock immutableBlocks
            (blk, _) <- genBlock m
            tip <-
              frequency
                [ (1, pure $ Chain.headHash immutableChain)
                , (5, pure $ BlockHash (blockHash blk))
                ,
                  ( if null volatileBlocks then 0 else 5
                  , elements $ BlockHash . blockHash <$> volatileBlocks
                  )
                ]
            let blks = Map.insert (blockHash blk) blk blocksInDB
            pure $ Model.getFragmentBetween blks anchor tip
        )
      ]
   where
    immutableChain = Model.immutableChain secParam dbModel
    immutableBlocks = Chain.toNewestFirst immutableChain
    volatileBlocks = Map.elems $ Model.volatileDbBlocks dbModel

  empty :: Bool
  empty = null pointsInDB

  genRelativeTime :: Gen RelativeTime
  genRelativeTime = RelativeTime . fromIntegral <$> arbitrary @Word64

  genRealPoint :: Gen (RealPoint blk)
  genRealPoint =
    frequency
      [ (1, genRandomPoint)
      , (if empty then 0 else 7, elements pointsInDB)
      ]

  genPoint :: Gen (Point blk)
  genPoint =
    frequency
      [ (1, return GenesisPoint)
      , (9, realPointToPoint <$> genRealPoint)
      ]

  genGetIsValid :: Gen (Cmd blk it flr)
  genGetIsValid =
    GetIsValid <$> genRealPoint

  genGetBlockComponent :: Gen (Cmd blk it flr)
  genGetBlockComponent = do
    pt <- genRealPoint
    return $
      if Model.garbageCollectablePoint secParam dbModel pt
        then GetGCedBlockComponent pt
        else GetBlockComponent pt

  genAddBlock :: Gen (Cmd blk it flr)
  genAddBlock = do
    (blk, gapBlks) <- genBlock m
    pure $ AddBlock blk gapBlks

  genAddPerasCert :: Gen (Cmd blk it flr)
  genAddPerasCert = do
    -- TODO should we be more strict on which blocks we add certs to?
    -- see https://github.com/tweag/cardano-peras/issues/124
    (blk, gapBlks) <- genBlock m
    let roundNo = case Model.maxPerasRoundNo dbModel of
          Nothing -> PerasRoundNo 0
          Just (PerasRoundNo r) -> PerasRoundNo (r + 1)
    -- Generate an almost-always-valid boost, i.e., below the maximum rollback
    let k = unPerasWeight (maxRollbackWeight secParam)
    boost <-
      PerasWeight
        <$> frequency
          [ (10, choose (1, k - 1))
          , (1, choose (k, k + 1))
          ]
    -- Include the boosted block itself in the persisted seenBlocks
    let seenBlks = fmap (blk :) gapBlks
    -- Build the certificate
    now <- genRelativeTime
    let certWithTime =
          WithArrivalTime now $
            ValidatedPerasCert
              { vpcCert =
                  PerasCert
                    { pcCertRound = roundNo
                    , pcCertBoostedBlock = blockPoint blk
                    }
              , vpcCertBoost = boost
              }
    pure $ AddPerasCert certWithTime seenBlks

  genBounds :: Gen (StreamFrom blk, StreamTo blk)
  genBounds =
    frequency
      [ (1, genRandomBounds)
      , (if empty then 0 else 3, genExistingBounds)
      ]

  genRandomBounds :: Gen (StreamFrom blk, StreamTo blk)
  genRandomBounds =
    (,)
      <$> ( do
              inEx <- genFromInEx
              case inEx of
                Left inc -> inc <$> genRealPoint
                Right exc -> exc <$> genPoint
          )
      <*> (StreamToInclusive <$> genRealPoint)

  genFromInEx ::
    Gen
      ( Either
          (RealPoint blk -> StreamFrom blk)
          (Point blk -> StreamFrom blk)
      )
  genFromInEx = elements [Left StreamFromInclusive, Right StreamFromExclusive]

  genFromInEx' :: Gen (RealPoint blk -> StreamFrom blk)
  genFromInEx' = either id (. realPointToPoint) <$> genFromInEx

  -- Generate bounds that correspond to existing blocks in the DB. Make sure
  -- that the start bound is older than the end bound.
  -- NOTE: this does not mean that these bounds are on the same chain.
  genExistingBounds :: Gen (StreamFrom blk, StreamTo blk)
  genExistingBounds = do
    start <- elements pointsInDB
    end <-
      elements pointsInDB
        `suchThat` ( (>= realPointSlot start)
                       . realPointSlot
                   )
    (,)
      <$> (genFromInEx' <*> return start)
      <*> (return $ StreamToInclusive end)

  genIteratorClose = IteratorClose <$> elements iterators
  genIteratorNext = do
    it <- elements iterators
    let blockCanBeGCed =
          Model.garbageCollectableIteratorNext
            secParam
            dbModel
            (knownIters RE.! it)
    return $
      if blockCanBeGCed
        then IteratorNextGCed it
        else IteratorNext it

  genNewFollower = NewFollower <$> elements [SelectedChain, TentativeChain]

  genFollowerInstruction = FollowerInstruction <$> elements followers
  genFollowerForward =
    FollowerForward
      <$> elements followers
      <*> genFollowerForwardPoints

  genFollowerForwardPoints :: Gen [Point blk]
  genFollowerForwardPoints =
    choose (1, 3) >>= \n ->
      sortOn (Down . pointSlot) <$> replicateM n genFollowerForwardPoint

  genFollowerForwardPoint :: Gen (Point blk)
  genFollowerForwardPoint = genPoint

  genFollowerClose = FollowerClose <$> elements followers

chooseSlot :: SlotNo -> SlotNo -> Gen SlotNo
chooseSlot (SlotNo start) (SlotNo end) = SlotNo <$> choose (start, end)

{-------------------------------------------------------------------------------
  Shrinking
-------------------------------------------------------------------------------}

-- | Shrinker
shrinker ::
  Model blk m Symbolic ->
  At Cmd blk m Symbolic ->
  [At Cmd blk m Symbolic]
shrinker _ = const [] -- TODO: implement the shrinker. Command
-- 'PersistBlksThenGC' should be shrunk to
-- ['PersistBlks']

{-------------------------------------------------------------------------------
  The final state machine
-------------------------------------------------------------------------------}

-- | Mock a response
--
-- We do this by running the pure semantics and then generating mock
-- references for any new handles.
mock ::
  (TestConstraints blk, Typeable m) =>
  Model blk m Symbolic ->
  At Cmd blk m Symbolic ->
  GenSym (At Resp blk m Symbolic)
mock model cmd = At <$> bitraverse (const genSym) (const genSym) resp
 where
  (resp, _dbm) = step model cmd

precondition ::
  forall m blk.
  TestConstraints blk =>
  Model blk m Symbolic -> At Cmd blk m Symbolic -> Logic
precondition Model{..} (At cmd) =
  forAll (iters cmd) (`member` RE.keys knownIters)
    .&& forAll (flrs cmd) (`member` RE.keys knownFollowers)
    .&& case cmd of
      -- Even though we ensure this in the generator, shrinking might change
      -- it.
      GetBlockComponent pt -> Not $ garbageCollectable pt
      GetGCedBlockComponent pt -> garbageCollectable pt
      IteratorNext it -> Not $ garbageCollectableIteratorNext it
      IteratorNextGCed it -> garbageCollectableIteratorNext it
      -- TODO The real implementation allows streaming blocks from the
      -- VolatileDB that have no path to the current chain. The model
      -- implementation disallows this, as it only allows streaming from one of
      -- the possible forks, each starting at genesis. Temporarily only test
      -- with iterators that the model allows. So we only test a subset of the
      -- functionality, which does not include error paths.
      Stream from to -> isValidIterator from to
      Reopen -> Not $ Boolean (Model.isOpen dbModel)
      WipeVolatileDB -> Boolean $ Model.isOpen dbModel
      UpdateLoE frag -> Boolean $ Chain.pointOnChain (AF.anchorPoint frag) immChain
       where
        immChain = Model.immutableChain (configSecurityParam cfg) dbModel
      _ -> Top
 where
  garbageCollectable :: RealPoint blk -> Logic
  garbageCollectable =
    Boolean . Model.garbageCollectablePoint secParam dbModel

  garbageCollectableIteratorNext :: IterRef blk m Symbolic -> Logic
  garbageCollectableIteratorNext it =
    Boolean $
      Model.garbageCollectableIteratorNext secParam dbModel (knownIters RE.! it)

  cfg :: TopLevelConfig blk
  cfg = unOpaque modelConfig

  secParam :: SecurityParam
  secParam = configSecurityParam cfg

  -- TODO #871
  isValidIterator :: StreamFrom blk -> StreamTo blk -> Logic
  isValidIterator from to =
    case Model.between secParam from to dbModel of
      Left _ -> Bot
      -- All blocks must be valid
      Right blks -> forAll blks $ \blk ->
        Boolean $
          Map.notMember (blockHash blk) $
            Model.invalid dbModel

transition ::
  (TestConstraints blk, Show1 r, Eq1 r) =>
  Model blk m r ->
  At Cmd blk m r ->
  At Resp blk m r ->
  Model blk m r
transition model cmd = eventAfter . lockstep model cmd

invariant ::
  forall m blk.
  TestConstraints blk =>
  TopLevelConfig blk ->
  Model blk m Concrete ->
  Logic
invariant cfg Model{..} =
  forAll ptsOnCurChain (Boolean . fromMaybe False . Model.getIsValid dbModel)
    .&& loeHasImmutableAnchor
 where
  -- \| The blocks occurring on the current volatile chain fragment
  ptsOnCurChain :: [RealPoint blk]
  ptsOnCurChain =
    map blockRealPoint
      . AF.toOldestFirst
      . Model.volatileChain (configSecurityParam cfg) id
      $ dbModel

  loeHasImmutableAnchor :: Logic
  loeHasImmutableAnchor = case Model.getLoEFragment dbModel of
    LoEEnabled frag ->
      Boolean $ Chain.pointOnChain (AF.anchorPoint frag) immChain
    LoEDisabled -> Top
   where
    immChain = Model.immutableChain (configSecurityParam cfg) dbModel

postcondition ::
  TestConstraints blk =>
  TopLevelConfig blk ->
  Model blk m Concrete ->
  At Cmd blk m Concrete ->
  At Resp blk m Concrete ->
  Logic
postcondition cfg model cmd resp =
  (toMock (eventAfter ev) resp .== eventMockResp ev)
    .// "real response didn't match model response"
    .&& immutableTipMonotonicity
 where
  ev = lockstep model cmd resp

  immutableTipMonotonicity = case unAt cmd of
    -- When we wipe the VolatileDB (and haven't persisted all immutable blocks),
    -- the immutable tip can recede.
    WipeVolatileDB -> Top
    _ ->
      Annotate ("Immutable tip non-monotonicity: " <> show before <> " > " <> show after) $
        Boolean (before <= after)
   where
    before = immTipBlockNo $ eventBefore ev
    after = immTipBlockNo $ eventAfter ev
    immTipBlockNo =
      Chain.headBlockNo
        . Model.immutableChain (configSecurityParam cfg)
        . dbModel

semantics ::
  forall blk.
  TestConstraints blk =>
  TopLevelConfig blk ->
  ChainDBEnv IO blk ->
  At Cmd blk IO Concrete ->
  IO (At Resp blk IO Concrete)
semantics cfg env (At cmd) =
  At . (bimap (QSM.reference . QSM.Opaque) (QSM.reference . QSM.Opaque))
    <$> runIO cfg env (bimap QSM.opaque QSM.opaque cmd)

-- | The state machine proper
sm ::
  TestConstraints blk =>
  LoE () ->
  ChainDBEnv IO blk ->
  (Model blk IO Symbolic -> Gen (blk, Persistent [blk])) ->
  TopLevelConfig blk ->
  ExtLedgerState blk EmptyMK ->
  StateMachine
    (Model blk IO)
    (At Cmd blk IO)
    IO
    (At Resp blk IO)
sm loe env genBlock cfg initLedger =
  StateMachine
    { initModel = initModel loe cfg initLedger
    , transition = transition
    , precondition = precondition
    , postcondition = postcondition cfg
    , generator = Just . generator loe genBlock
    , shrinker = shrinker
    , semantics = semantics cfg env
    , mock = mock
    , invariant = Just $ invariant cfg
    , cleanup = noCleanup
    }

{-------------------------------------------------------------------------------
  Required instances

  The 'ToExpr' constraints come from "Data.TreeDiff".
-------------------------------------------------------------------------------}

instance CommandNames (At Cmd blk m) where
  cmdName (At cmd) = constrName cmd
  cmdNames (_ :: Proxy (At Cmd blk m r)) =
    constrNames (Proxy @(Cmd blk () ()))

deriving instance
  ( ToExpr blk
  , ToExpr (HeaderHash blk)
  , ToExpr (ChainDepState (BlockProtocol blk))
  , ToExpr (TipInfo blk)
  , ToExpr (LedgerState blk EmptyMK)
  , ToExpr (ExtValidationError blk)
  ) =>
  ToExpr (Model blk IO Concrete)

{-------------------------------------------------------------------------------
  Labelling
-------------------------------------------------------------------------------}

deriving instance SOP.Generic (TraceEvent blk)
deriving instance SOP.HasDatatypeInfo (TraceEvent blk)
deriving instance SOP.Generic (TraceAddBlockEvent blk)
deriving instance SOP.HasDatatypeInfo (TraceAddBlockEvent blk)
deriving instance SOP.Generic (ChainDB.TraceFollowerEvent blk)
deriving instance SOP.HasDatatypeInfo (ChainDB.TraceFollowerEvent blk)
deriving instance SOP.Generic (TraceCopyToImmutableDBEvent blk)
deriving instance SOP.HasDatatypeInfo (TraceCopyToImmutableDBEvent blk)
deriving instance SOP.Generic (TraceValidationEvent blk)
deriving instance SOP.HasDatatypeInfo (TraceValidationEvent blk)
deriving instance SOP.Generic (TraceInitChainSelEvent blk)
deriving instance SOP.HasDatatypeInfo (TraceInitChainSelEvent blk)
deriving instance SOP.Generic (TraceOpenEvent blk)
deriving instance SOP.HasDatatypeInfo (TraceOpenEvent blk)
deriving instance SOP.Generic (TraceGCEvent blk)
deriving instance SOP.HasDatatypeInfo (TraceGCEvent blk)
deriving instance SOP.Generic (TraceIteratorEvent blk)
deriving instance SOP.HasDatatypeInfo (TraceIteratorEvent blk)
deriving instance SOP.Generic (LedgerDB.TraceEvent blk)
deriving instance SOP.HasDatatypeInfo (LedgerDB.TraceEvent blk)
deriving instance SOP.Generic (ImmutableDB.TraceEvent blk)
deriving instance SOP.HasDatatypeInfo (ImmutableDB.TraceEvent blk)
deriving instance SOP.Generic (VolatileDB.TraceEvent blk)
deriving instance SOP.HasDatatypeInfo (VolatileDB.TraceEvent blk)
deriving instance SOP.Generic (PerasCertDB.TraceEvent blk)
deriving instance SOP.HasDatatypeInfo (PerasCertDB.TraceEvent blk)
deriving anyclass instance SOP.Generic (TraceChainSelStarvationEvent blk)
deriving anyclass instance SOP.HasDatatypeInfo (TraceChainSelStarvationEvent blk)
deriving anyclass instance SOP.Generic (TraceAddPerasCertEvent blk)
deriving anyclass instance SOP.HasDatatypeInfo (TraceAddPerasCertEvent blk)

data Tag
  = TagGetIsValidJust
  | TagGetIsValidNothing
  | TagChainSelReprocessChangedSelection
  | TagChainSelReprocessKeptSelection
  | TagSwitchedToShorterChain
  deriving (Show, Eq)

-- | Predicate on events
type EventPred m = C.Predicate (Event Blk m Symbolic) Tag

-- | Convenience combinator for creating classifiers for successful commands
successful ::
  ( Event Blk m Symbolic ->
    Success Blk IteratorId FollowerId ->
    Either Tag (EventPred m)
  ) ->
  EventPred m
successful f = C.predicate $ \ev -> case eventMockResp ev of
  Resp (Left _) -> Right $ successful f
  Resp (Right ok) -> f ev ok

-- | Tag commands
--
-- Tagging works on symbolic events, so that we can tag without doing real IO.
tag :: forall m. [Event Blk m Symbolic] -> [Tag]
tag =
  C.classify
    [ tagGetIsValidJust
    , tagGetIsValidNothing
    , tagChainSelReprocess TagChainSelReprocessChangedSelection (/=)
    , tagChainSelReprocess TagChainSelReprocessKeptSelection (==)
    , tagSwitchedToShorterChain
    ]
 where
  tagGetIsValidJust :: EventPred m
  tagGetIsValidJust = successful $ \ev r -> case r of
    IsValid (IsValidResult{isValid = Just _})
      | GetIsValid{} <- unAt $ eventCmd ev ->
          Left TagGetIsValidJust
    _ -> Right tagGetIsValidJust

  tagGetIsValidNothing :: EventPred m
  tagGetIsValidNothing = successful $ \ev r -> case r of
    IsValid (IsValidResult{isValid = Nothing})
      | GetIsValid{} <- unAt $ eventCmd ev ->
          Left TagGetIsValidNothing
    _ -> Right tagGetIsValidNothing

  tagChainSelReprocess ::
    Tag -> (Point TestBlock -> Point TestBlock -> Bool) -> EventPred m
  tagChainSelReprocess t test = successful $ \ev _r -> case unAt $ eventCmd ev of
    UpdateLoE{}
      | (test `on` Model.tipPoint . dbModel) (eventBefore ev) (eventAfter ev) ->
          Left t
    _ -> Right $ tagChainSelReprocess t test

  -- Tag this test case if we ever switch from a longer to a shorter chain in a
  -- non-degenerate case.
  tagSwitchedToShorterChain :: EventPred m
  tagSwitchedToShorterChain = C.predicate $ \case
    ev
      | case unAt $ eventCmd ev of
          -- Wiping the VolatileDB is not interesting here.
          WipeVolatileDB{} -> False
          _ -> True
      , ((>) `on` curChainLength) (eventBefore ev) (eventAfter ev) ->
          Left TagSwitchedToShorterChain
      | otherwise -> Right tagSwitchedToShorterChain
   where
    curChainLength = Chain.length . Model.currentChain . dbModel

-- | Step the model using a 'QSM.Command' (i.e., a command associated with
-- an explicit set of variables)
execCmd ::
  Model Blk m Symbolic ->
  QSM.Command (At Cmd Blk m) (At Resp Blk m) ->
  Event Blk m Symbolic
execCmd model (QSM.Command cmdErr resp _vars) = lockstep model cmdErr resp

-- | 'execCmds' is just the repeated form of 'execCmd'
execCmds ::
  forall m.
  Model Blk m Symbolic ->
  QSM.Commands (At Cmd Blk m) (At Resp Blk m) ->
  [Event Blk m Symbolic]
execCmds model = \(QSM.Commands cs) -> go model cs
 where
  go ::
    Model Blk m Symbolic ->
    [QSM.Command (At Cmd Blk m) (At Resp Blk m)] ->
    [Event Blk m Symbolic]
  go _ [] = []
  go m (c : cs) = let ev = execCmd m c in ev : go (eventAfter ev) cs

{-------------------------------------------------------------------------------
  Generator for TestBlock
-------------------------------------------------------------------------------}

type Blk = TestBlock

-- | Note that the 'Blk = TestBlock' is general enough to be used by both the
-- ChainDB /and/ the ImmutableDB, its generators cannot. For example, in the
-- ChainDB, blocks are added /out of order/, while in the ImmutableDB, they
-- must be added /in order/. This generator can thus not be reused for the
-- ImmutableDB.
genBlk :: ImmutableDB.ChunkInfo -> Model Blk m r -> Gen (TestBlock, Persistent [TestBlock])
genBlk chunkInfo Model{..} =
  frequency
    [ (if noBlocksInChainDB then 0 else 1, withoutGapBlocks genAlreadyInChain)
    , (if noSavedGapBlocks then 0 else 20, withoutGapBlocks genGapBlock)
    , (5, withoutGapBlocks genAppendToCurrentChain)
    , (5, withoutGapBlocks genFitsOnSomewhere)
    , (3, genBlockAfterGap)
    ]
 where
  blocksInChainDB = Model.blocks dbModel

  k = unNonZero (maxRollbacks (configSecurityParam (unOpaque modelConfig)))

  modelSupportsEBBs =
    ImmutableDB.chunkInfoSupportsEBBs chunkInfo
      -- NOTE: we disable the generation of EBBs entirely when k>2 to avoid
      -- triggering an edge case caused by a mismatch between the model and
      -- actual the implementation. For more information, see:
      -- https://github.com/IntersectMBO/ouroboros-consensus/issues/1745
      && k <= 2

  noBlocksInChainDB = Map.null blocksInChainDB

  savedGapBlocks = seenBlocks genState
  noSavedGapBlocks = Map.null savedGapBlocks
  withoutGapBlocks = fmap (,Persistent [])

  canContainEBB = const modelSupportsEBBs -- TODO: we could be more precise
  genBody :: Gen TestBody
  genBody = do
    isValid <-
      frequency
        [ (4, return True)
        , (1, return False)
        ]
    forkNo <- choose (1, 3)
    return
      TestBody
        { tbForkNo = forkNo
        , tbIsValid = isValid
        }

  -- A block that already exists in the ChainDB
  genAlreadyInChain :: Gen TestBlock
  genAlreadyInChain = elements $ Map.elems blocksInChainDB

  -- A block that fits onto the current chain
  genAppendToCurrentChain :: Gen TestBlock
  genAppendToCurrentChain = case Model.tipBlock dbModel of
    Nothing -> genFirstBlock
    Just b -> genFitsOn b

  -- A block that fits onto some block @b@ in the ChainDB. The block @b@
  -- could be at the tip of the chain and the generated block might already
  -- be present in the ChainDB.
  genFitsOnSomewhere :: Gen TestBlock
  genFitsOnSomewhere = case Model.tipBlock dbModel of
    Nothing -> genFirstBlock
    Just _ -> genAlreadyInChain >>= genFitsOn

  -- A block that doesn't fit onto a block in the ChainDB, but it creates a gap
  -- of a couple of blocks between genesis or an existing block in the ChainDB.
  -- We generate it by generating a few intermediary blocks first, which we
  -- don't add just yet. These are in turn returned and stored as seen blocks
  -- in the generator state of the model. We can sample from these later on to
  -- (hopefully) fill the gaps.
  genBlockAfterGap :: Gen (TestBlock, Persistent [TestBlock])
  genBlockAfterGap = do
    gapSize <- choose (1, 3)
    start <- genFitsOnSomewhere
    go gapSize start []
   where
    go :: Int -> TestBlock -> [TestBlock] -> Gen (TestBlock, Persistent [TestBlock])
    go 0 tip gapBlks = return (tip, Persistent gapBlks)
    go n tip gapBlks = do
      tip' <- genFitsOn tip
      go (n - 1) tip' (tip : gapBlks)

  -- An intermediate gap block that was generated by 'genBlockAfterGap' but
  -- saved for later in the model's generator state. See 'GenState' for details.
  genGapBlock :: Gen TestBlock
  genGapBlock = elements (Map.elems savedGapBlocks)

  -- Generate a block or EBB fitting on genesis
  genFirstBlock :: Gen TestBlock
  genFirstBlock =
    frequency
      [
        ( 1
        , firstBlock <$> chooseSlot 0 2 <*> genBody
        )
      ,
        ( if modelSupportsEBBs then 1 else 0
        , firstEBB canContainEBB <$> genBody
        )
      ]

  -- Helper that generates a block that fits onto the given block.
  genFitsOn :: TestBlock -> Gen TestBlock
  genFitsOn b =
    frequency
      [
        ( 4
        , do
            slotNo <-
              if fromIsEBB (testBlockIsEBB b)
                then chooseSlot (blockSlot b) (blockSlot b + 2)
                else chooseSlot (blockSlot b + 1) (blockSlot b + 3)
            body <- genBody
            return $ mkNextBlock b slotNo body
        )
      , -- An EBB is never followed directly by another EBB, otherwise they
        -- would have the same 'BlockNo', as the EBB has the same 'BlockNo' of
        -- the block before it.

        ( if fromIsEBB (testBlockIsEBB b) || not modelSupportsEBBs then 0 else 1
        , do
            let prevSlotNo = blockSlot b
                prevChunk =
                  ImmutableDB.chunkIndexOfSlot
                    chunkInfo
                    prevSlotNo
                prevEpoch = unsafeChunkNoToEpochNo prevChunk
                nextEBB =
                  ImmutableDB.chunkSlotForBoundaryBlock
                    chunkInfo
                    (prevEpoch + 1)
                nextNextEBB =
                  ImmutableDB.chunkSlotForBoundaryBlock
                    chunkInfo
                    (prevEpoch + 2)
            (slotNo, epoch) <-
              first (ImmutableDB.chunkSlotToSlot chunkInfo)
                <$> frequency
                  [ (7, return (nextEBB, prevEpoch + 1))
                  , (1, return (nextNextEBB, prevEpoch + 2))
                  ]
            body <- genBody
            return $ mkNextEBB canContainEBB b slotNo epoch body
        )
      ]

-- | Generate a random security parameter (k)
--
-- NOTE: after analysing the effect of varying the security parameter, we have
-- observed a tension between:
--
-- 1) generating enough tests to trigger interesting weighted chain selection
--    behavior such as switching to a shorter but heavier chain (note that the
--    certificate boost is derived from k and must be large enough to overcome
--    the weight of a longer chain). This corresponds to the
--    'TagSwitchedToShorterChain' event from the @Tags@ table.
--
-- 2) generating enough tests exercising the ImmutableDB logic (the chain
--    must have at least k blocks). This corresponds to the 'True' event from
--    the @Chain length >= k@ table.
--
-- Here are some empirical results using fixed values of k:
--
-- @
--  k    -> P(switching to shorter chain), P(building a chain with >= k blocks)
--  k=2  -> ~1.3%, ~40%
--  k=3  -> ~1.9%, ~20%
--  k=4  -> ~2.4%, ~9%
--  k=5  -> ~2.5%, ~3%
--  k=10 -> ~3%,   ~0.05%
-- @
--
-- From these results, we can see observe that the sweet spot between both
-- desiderata appears to be around @k=2@ and @k=4@. So, we use a geometric
-- distribution to bias the generation towards smaller security parameters,
-- while still allowing arbitrary large ones to appear with decresing
-- probability. Concretely, by instantiating a geometric distribution with
-- parameter @p=0.5@ and shifting it to the right by 2, we obtain the following
-- distribution:
--
-- @
--  k    -> P(k)
--  k=2  -> 50%
--  k=3  -> 25%
--  k=4  -> 12.5%
--  k=5  -> 6.25%
--  ...
-- @
--
-- This way, roughly 87.5% of the generated security parameters will be between
-- 2 and 4, while still allowing for larger values to appear from time to time.
genSecurityParam :: Gen SecurityParam
genSecurityParam =
  SecurityParam
    . unsafeNonZero
    . fromIntegral
    . (+ 2) -- shift to the right to avoid degenerate cases
    <$> geometric 0.5 -- range in [0, +inf); mean = 1/p = 2
 where
  geometric :: Double -> Gen Int
  geometric p
    | p <= 0 || p > 1 = error "p must be in (0,1]"
    | otherwise = do
        u <- choose (0.0, 1.0)
        let k = floor (log u / log (1 - p))
        return k

{-------------------------------------------------------------------------------
  Top-level tests
-------------------------------------------------------------------------------}

mkTestCfg :: SecurityParam -> ImmutableDB.ChunkInfo -> TopLevelConfig TestBlock
mkTestCfg k (ImmutableDB.UniformChunkSize chunkSize) =
  mkTestConfig k chunkSize

envUnused :: ChainDBEnv m blk
envUnused = error "ChainDBEnv used during command generation"

smUnused ::
  LoE () ->
  SecurityParam ->
  ImmutableDB.ChunkInfo ->
  StateMachine (Model Blk IO) (At Cmd Blk IO) IO (At Resp Blk IO)
smUnused loe k chunkInfo =
  sm
    loe
    envUnused
    (genBlk chunkInfo)
    (mkTestCfg k chunkInfo)
    testInitExtLedger

prop_sequential :: LoE () -> SmallChunkInfo -> Property
prop_sequential loe smallChunkInfo@(SmallChunkInfo chunkInfo) =
  QC.forAll genSecurityParam $ \k ->
    forAllCommands (smUnused loe k chunkInfo) Nothing $
      runCmdsLockstep loe k smallChunkInfo

runCmdsLockstep ::
  LoE () ->
  SecurityParam ->
  SmallChunkInfo ->
  QSM.Commands (At Cmd Blk IO) (At Resp Blk IO) ->
  Property
runCmdsLockstep loe k (SmallChunkInfo chunkInfo) cmds =
  QC.monadicIO $ do
    let
      -- Current test case command names.
      ctcCmdNames :: [String]
      ctcCmdNames = fmap (show . cmdName . QSM.getCommand) $ QSM.unCommands cmds

    (hist, prop) <- QC.run $ test cmds
    prettyCommands (smUnused loe k chunkInfo) hist
      $ tabulate
        "Tags"
        (map show $ tag (execCmds (QSM.initModel (smUnused loe k chunkInfo)) cmds))
      $ tabulate "Command sequence length" [show $ length ctcCmdNames]
      $ tabulate "Commands" ctcCmdNames
      $ prop
 where
  testCfg = mkTestCfg k chunkInfo

  test ::
    QSM.Commands (At Cmd Blk IO) (At Resp Blk IO) ->
    IO
      ( QSM.History (At Cmd Blk IO) (At Resp Blk IO)
      , Property
      )
  test cmds' = do
    threadRegistry <- unsafeNewRegistry
    iteratorRegistry <- unsafeNewRegistry
    (tracer, getTrace) <- recordingTracerIORef
    varNextId <- uncheckedNewTVarM 0
    nodeDBs <- emptyNodeDBs
    varLoEFragment <- newTVarIO $ AF.Empty AF.AnchorGenesis
    let args =
          mkArgs
            testCfg
            chunkInfo
            (testInitExtLedger `withLedgerTables` emptyLedgerTables)
            threadRegistry
            nodeDBs
            tracer
            (loe $> varLoEFragment)

    (hist, model, res, trace) <- bracket
      (open args >>= newTVarIO)
      -- Note: we might be closing a different ChainDB than the one we
      -- opened, as we can reopen it the ChainDB, swapping the ChainDB in
      -- the TVar.
      (\varDB -> readTVarIO varDB >>= close)
      $ \varDB -> do
        let env =
              ChainDBEnv
                { varDB
                , registry = iteratorRegistry
                , varNextId
                , varVolatileDbFs = nodeDBsVol nodeDBs
                , varLoEFragment
                , args
                }
            sm' = sm loe env (genBlk chunkInfo) testCfg testInitExtLedger
        (hist, model, res) <- QSM.runCommands' sm' cmds'
        trace <- getTrace
        return (hist, model, res, trace)

    closeRegistry threadRegistry

    -- 'closeDB' should have closed all open 'Follower's and 'Iterator's,
    -- freeing up all resources, so there should be no more clean-up
    -- actions left.
    --
    -- Note that this is only true because we're not simulating exceptions
    -- (yet), in which case there /will be/ clean-up actions left. This is
    -- exactly the reason for introducing the 'ResourceRegistry' in the
    -- first place: to clean up resources in case exceptions get thrown.
    remainingCleanups <- countResources iteratorRegistry
    closeRegistry iteratorRegistry

    -- Read the final MockFS of each database
    fses <- atomically $ traverse readTMVar nodeDBs
    let
      modelChain = Model.currentChain $ dbModel model
      secParam = unNonZero (maxRollbacks (configSecurityParam testCfg))
      prop =
        counterexample (show (configSecurityParam testCfg)) $
          counterexample ("Model chain: " <> condense modelChain) $
            counterexample ("TraceEvents: " <> unlines (map show trace)) $
              tabulate "Chain length" [show (Chain.length modelChain)] $
                tabulate "Security Parameter (k)" [show secParam] $
                  tabulate "Chain length >= k" [show (Chain.length modelChain >= fromIntegral secParam)] $
                    tabulate "TraceEvents" (map traceEventName trace) $
                      res
                        === Ok
                        .&&. prop_trace testCfg (dbModel model) trace
                        .&&. counterexample
                          "ImmutableDB is leaking file handles"
                          (Mock.numOpenHandles (nodeDBsImm fses) === 0)
                        .&&. counterexample
                          "VolatileDB is leaking file handles"
                          (Mock.numOpenHandles (nodeDBsVol fses) === 0)
                        .&&. counterexample
                          "LedgerDB is leaking file handles"
                          (Mock.numOpenHandles (nodeDBsLgr fses) === 0)
                        .&&. counterexample
                          "There were registered clean-up actions"
                          (remainingCleanups === 0)
    return (hist, prop)

prop_trace :: TopLevelConfig Blk -> DBModel Blk -> [TraceEvent Blk] -> Property
prop_trace cfg dbModel trace =
  invalidBlockNeverValidatedAgain
    .&&. tentativeHeaderMonotonicity
 where
  -- Whenever we validate a block that turns out to be invalid, check that
  -- we never again validate the same block.
  invalidBlockNeverValidatedAgain =
    whenOccurs trace invalidBlock $ \trace' invalidPoint ->
      whenOccurs trace' invalidBlock $ \_ invalidPoint' ->
        -- If the database was reopened in the meantime, we have forgotten
        -- about the invalid block and might validate it again, that's fine
        if any isOpened trace'
          then
            property True
          else
            counterexample "An invalid block is validated twice" $
              invalidPoint =/= invalidPoint'

  invalidBlock :: TraceEvent blk -> Maybe (RealPoint blk)
  invalidBlock = \case
    TraceAddBlockEvent (AddBlockValidation ev) -> extract ev
    TraceInitChainSelEvent (InitChainSelValidation ev) -> extract ev
    _ -> Nothing
   where
    extract (ChainDB.InvalidBlock _ pt) = Just pt
    extract _ = Nothing

  isOpened :: TraceEvent blk -> Bool
  isOpened (TraceOpenEvent (OpenedDB{})) = True
  isOpened _ = False

  tentativeHeaderMonotonicity =
    counterexample "Trap tentative headers did not improve monotonically" $
      conjoin (strictlyIncreasing <$> trapTentativeSelectViews)
   where
    trapTentativeSelectViews :: [[SelectView (BlockProtocol Blk)]]
    trapTentativeSelectViews =
      [ [ selectView (configBlock cfg) hdr
        | TraceAddBlockEvent (PipeliningEvent ev) <- trace'
        , SetTentativeHeader hdr FallingEdge <- [ev]
        , Map.member (headerHash hdr) (Model.invalid dbModel)
        ]
      | -- Check the property between DB reopenings
      trace' <- NE.toList $ split isOpened trace
      ]

-- | Given a trace of events, for each event in the trace for which the
-- predicate yields a @Just a@, call the continuation function with the
-- remaining events and @a@.
whenOccurs :: [ev] -> (ev -> Maybe a) -> ([ev] -> a -> Property) -> Property
whenOccurs evs occurs k = go evs
 where
  go [] = property True
  go (ev : evs')
    | Just a <- occurs ev =
        k evs' a .&&. go evs'
    | otherwise =
        go evs'

traceEventName :: TraceEvent blk -> String
traceEventName = \case
  TraceAddBlockEvent ev ->
    "AddBlock." <> case ev of
      AddBlockValidation ev' -> constrName ev'
      _ -> constrName ev
  TraceFollowerEvent ev -> "Follower." <> constrName ev
  TraceCopyToImmutableDBEvent ev -> "CopyToImmutableDB." <> constrName ev
  TraceInitChainSelEvent ev ->
    "InitChainSel." <> case ev of
      InitChainSelValidation ev' -> constrName ev'
      StartedInitChainSelection -> "StartedInitChainSelection"
      InitialChainSelected -> "InitialChainSelected"
  TraceOpenEvent ev -> "Open." <> constrName ev
  TraceGCEvent ev -> "GC." <> constrName ev
  TraceIteratorEvent ev -> "Iterator." <> constrName ev
  TraceLedgerDBEvent ev -> "Ledger." <> constrName ev
  TraceImmutableDBEvent ev -> "ImmutableDB." <> constrName ev
  TraceVolatileDBEvent ev -> "VolatileDB." <> constrName ev
  TracePerasCertDbEvent ev -> "PerasCertDB." <> constrName ev
  TraceLastShutdownUnclean -> "LastShutdownUnclean"
  TraceChainSelStarvationEvent ev -> "ChainSelStarvation." <> constrName ev
  TraceAddPerasCertEvent ev -> "AddPerasCert." <> constrName ev

mkArgs ::
  IOLike m =>
  TopLevelConfig Blk ->
  ImmutableDB.ChunkInfo ->
  ExtLedgerState Blk ValuesMK ->
  ResourceRegistry m ->
  NodeDBs (StrictTMVar m MockFS) ->
  CT.Tracer m (TraceEvent Blk) ->
  LoE (StrictTVar m (AnchoredFragment (HeaderWithTime Blk))) ->
  ChainDbArgs Identity m Blk
mkArgs cfg chunkInfo initLedger registry nodeDBs tracer varLoEFragment =
  let args =
        fromMinimalChainDbArgs
          MinimalChainDbArgs
            { mcdbTopLevelConfig = cfg
            , mcdbChunkInfo = chunkInfo
            , mcdbInitLedger = initLedger
            , mcdbRegistry = registry
            , mcdbNodeDBs = nodeDBs
            }
   in ChainDB.updateTracer tracer $
        args
          { cdbsArgs =
              (cdbsArgs args)
                { ChainDB.cdbsBlocksToAddSize = 2
                , ChainDB.cdbsLoE = traverse (atomically . readTVar) varLoEFragment
                }
          , cdbImmDbArgs =
              (cdbImmDbArgs args)
                { ImmutableDB.immCheckIntegrity = testBlockIsValid
                }
          , cdbVolDbArgs =
              (cdbVolDbArgs args)
                { VolatileDB.volCheckIntegrity = testBlockIsValid
                }
          }

tests :: TestTree
tests =
  testGroup
    "ChainDB q-s-m"
    [ adjustQuickCheckTests (* 100) $ testProperty "sequential" prop_sequential
    ]
