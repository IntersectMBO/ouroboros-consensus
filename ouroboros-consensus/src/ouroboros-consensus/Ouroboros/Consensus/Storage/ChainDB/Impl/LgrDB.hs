{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Thin wrapper around the LedgerDB
module Ouroboros.Consensus.Storage.ChainDB.Impl.LgrDB (
    LgrDB
    -- opaque
  , LedgerDB'
  , LgrDbSerialiseConstraints
    -- * Initialization
  , LgrDbArgs (..)
  , defaultArgs
  , openDB
    -- * 'TraceReplayEvent' decorator
  , LedgerDB.decorateReplayTracerWithGoal
    -- * Wrappers
  , currentPoint
  , getCurrent
  , getDiskPolicy
  , setCurrent
  , takeSnapshot
  , trimSnapshots
    -- * Validation
  , ValidateResult (..)
  , validate
    -- * Previously applied blocks
  , garbageCollectPrevApplied
  , getPrevApplied
    -- * Re-exports
  , LedgerDB.AnnLedgerError (..)
  , LedgerDB.DiskPolicy (..)
  , LedgerDB.DiskSnapshot
  , LedgerDB.ExceededRollback (..)
  , LedgerDB.TraceReplayEvent (..)
  , LedgerDB.TraceSnapshotEvent (..)
  , LedgerDB.ledgerDbCurrent
    -- * Exported for testing purposes
  , mkLgrDB
  ) where

import           Codec.Serialise (Serialise (decode))
import           Control.Monad.Trans.Class
import           Control.Tracer
import           Data.Foldable as Foldable (foldl')
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Inspect
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Storage.ChainDB.API (ChainDbFailure (..))
import           Ouroboros.Consensus.Storage.ChainDB.Impl.BlockCache
                     (BlockCache)
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.BlockCache as BlockCache
import           Ouroboros.Consensus.Storage.ImmutableDB (ImmutableDB)
import           Ouroboros.Consensus.Storage.ImmutableDB.Stream
import           Ouroboros.Consensus.Storage.LedgerDB (LedgerDB')
import qualified Ouroboros.Consensus.Storage.LedgerDB as LedgerDB
import           Ouroboros.Consensus.Storage.Serialisation
import           Ouroboros.Consensus.Util.Args
import           Ouroboros.Consensus.Util.IOLike
import           System.FS.API (SomeHasFS (..), createDirectoryIfMissing)
import           System.FS.API.Types (FsError, mkFsPath)

-- | Thin wrapper around the ledger database
data LgrDB m blk = LgrDB {
      varDB          :: !(StrictTVar m (LedgerDB' blk))
      -- ^ INVARIANT: the tip of the 'LedgerDB' is always in sync with the tip
      -- of the current chain of the ChainDB.
    , varPrevApplied :: !(StrictTVar m (Set (RealPoint blk)))
      -- ^ INVARIANT: this set contains only points that are in the
      -- VolatileDB.
      --
      -- INVARIANT: all points on the current chain fragment are in this set.
      --
      -- The VolatileDB might contain invalid blocks, these will not be in
      -- this set.
      --
      -- When a garbage-collection is performed on the VolatileDB, the points
      -- of the blocks eligible for garbage-collection should be removed from
      -- this set.
    , resolveBlock   :: !(RealPoint blk -> m blk)
      -- ^ Read a block from disk
    , cfg            :: !(LedgerDB.LedgerDbCfg (ExtLedgerState blk))
    , diskPolicy     :: !LedgerDB.DiskPolicy
    , hasFS          :: !(SomeHasFS m)
    , tracer         :: !(Tracer m (LedgerDB.TraceSnapshotEvent blk))
    } deriving (Generic)

deriving instance (IOLike m, LedgerSupportsProtocol blk)
               => NoThunks (LgrDB m blk)
  -- use generic instance

-- | 'EncodeDisk' and 'DecodeDisk' constraints needed for the LgrDB.
type LgrDbSerialiseConstraints blk =
  ( Serialise      (HeaderHash  blk)
  , EncodeDisk blk (LedgerState blk)
  , DecodeDisk blk (LedgerState blk)
  , EncodeDisk blk (AnnTip      blk)
  , DecodeDisk blk (AnnTip      blk)
  , EncodeDisk blk (ChainDepState (BlockProtocol blk))
  , DecodeDisk blk (ChainDepState (BlockProtocol blk))
  )

{-------------------------------------------------------------------------------
  Initialization
-------------------------------------------------------------------------------}

data LgrDbArgs f m blk = LgrDbArgs {
      lgrDiskPolicyArgs :: LedgerDB.DiskPolicyArgs
    , lgrGenesis        :: HKD f (m (ExtLedgerState blk))
    , lgrHasFS          :: HKD f (SomeHasFS m)
    , lgrConfig         :: LedgerDB.LedgerDbCfgF f (ExtLedgerState blk)
    , lgrTracer         :: Tracer m (LedgerDB.TraceSnapshotEvent blk)
    }

-- | Default arguments
defaultArgs :: Applicative m => Incomplete LgrDbArgs m blk
defaultArgs = LgrDbArgs {
      lgrDiskPolicyArgs = LedgerDB.defaultDiskPolicyArgs
    , lgrGenesis        = noDefault
    , lgrHasFS          = noDefault
    , lgrConfig         = LedgerDB.LedgerDbCfg noDefault noDefault OmitLedgerEvents
    , lgrTracer         = nullTracer
    }

-- | Open the ledger DB
--
-- In addition to the ledger DB also returns the number of immutable blocks
-- that were replayed.
openDB :: forall m blk.
          ( IOLike m
          , LedgerSupportsProtocol blk
          , LgrDbSerialiseConstraints blk
          , InspectLedger blk
          , HasCallStack
          )
       => Complete LgrDbArgs m blk
       -- ^ Stateless initializaton arguments
       -> Tracer m (LedgerDB.ReplayGoal blk -> LedgerDB.TraceReplayEvent blk)
       -- ^ Used to trace the progress while replaying blocks against the
       -- ledger.
       -> ImmutableDB m blk
       -- ^ Reference to the immutable DB
       --
       -- After reading a snapshot from disk, the ledger DB will be brought
       -- up to date with tip of the immutable DB. The corresponding ledger
       -- state can then be used as the starting point for chain selection in
       -- the ChainDB driver.
       -> (RealPoint blk -> m blk)
       -- ^ Read a block from disk
       --
       -- The block may be in the immutable DB or in the volatile DB; the ledger
       -- DB does not know where the boundary is at any given point.
       -> m (LgrDB m blk, Word64)
openDB args@LgrDbArgs { lgrHasFS = lgrHasFS@(SomeHasFS hasFS), .. } replayTracer immutableDB getBlock = do
    createDirectoryIfMissing hasFS True (mkFsPath [])
    (db, replayed) <- initFromDisk args replayTracer immutableDB
    -- When initializing the ledger DB from disk we:
    --
    -- - Look for the newest valid snapshot, say 'Lbs', which corresponds to the
    --   application of a block in the immutable DB, say 'b'.
    --
    -- - Push onto the ledger DB all the ledger states that result from applying
    --   blocks found in the on-disk immutable DB, starting from the successor
    --   of 'b'.
    --
    -- The anchor of 'LedgerDB' must be the oldest point we can rollback to. So
    -- if we follow the procedure described above (that 'initFromDisk'
    -- implements), the newest ledger state in 'db', say 'Lbn' corresponds to
    -- the most recent block in the immutable DB. If this block is in the
    -- immutable DB, it means that at some point it was part of a chain that was
    -- >k blocks long. Thus 'Lbn' is the oldest point we can roll back to.
    -- Therefore, we need to make the newest state (current) of the ledger DB
    -- the anchor.
    let dbPrunedToImmDBTip = LedgerDB.ledgerDbPrune LedgerDB.LedgerDbPruneAll db
    (varDB, varPrevApplied) <-
      (,) <$> newTVarIO dbPrunedToImmDBTip <*> newTVarIO Set.empty
    return (
        LgrDB {
            varDB          = varDB
          , varPrevApplied = varPrevApplied
          , resolveBlock   = getBlock
          , cfg            = lgrConfig
          , diskPolicy     = let k = LedgerDB.ledgerDbCfgSecParam lgrConfig
            in  LedgerDB.mkDiskPolicy k lgrDiskPolicyArgs
          , hasFS          = lgrHasFS
          , tracer         = lgrTracer
          }
      , replayed
      )

initFromDisk ::
     forall blk m.
     ( IOLike m
     , LedgerSupportsProtocol blk
     , LgrDbSerialiseConstraints blk
     , InspectLedger blk
     , HasCallStack
     )
  => Complete LgrDbArgs m blk
  -> Tracer m (LedgerDB.ReplayGoal blk -> LedgerDB.TraceReplayEvent blk)
  -> ImmutableDB m blk
  -> m (LedgerDB' blk, Word64)
initFromDisk LgrDbArgs { lgrHasFS = hasFS, .. }
             replayTracer
             immutableDB = wrapFailure (Proxy @blk) $ do
    (_initLog, db, replayed) <-
      LedgerDB.initLedgerDB
        replayTracer
        lgrTracer
        hasFS
        (decodeDiskExtLedgerState ccfg)
        decode
        lgrConfig
        lgrGenesis
        (streamAPI immutableDB)
        doDiskSnapshotChecksum
    return (db, replayed)
  where
    ccfg = configCodec $ getExtLedgerCfg $ LedgerDB.ledgerDbCfg lgrConfig
    LedgerDB.DiskPolicyArgs _ _ doDiskSnapshotChecksum = lgrDiskPolicyArgs

-- | For testing purposes
mkLgrDB :: StrictTVar m (LedgerDB' blk)
        -> StrictTVar m (Set (RealPoint blk))
        -> (RealPoint blk -> m blk)
        -> Complete LgrDbArgs m blk
        -> SecurityParam
        -> LgrDB m blk
mkLgrDB varDB varPrevApplied resolveBlock args k = LgrDB {..}
  where
    LgrDbArgs {
        lgrConfig         = cfg
      , lgrDiskPolicyArgs = diskPolicyArgs
      , lgrHasFS          = hasFS
      , lgrTracer         = tracer
      } = args
    diskPolicy = LedgerDB.mkDiskPolicy k diskPolicyArgs

{-------------------------------------------------------------------------------
  Wrappers
-------------------------------------------------------------------------------}

getCurrent :: IOLike m => LgrDB m blk -> STM m (LedgerDB' blk)
getCurrent LgrDB{..} = readTVar varDB

-- | PRECONDITION: The new 'LedgerDB' must be the result of calling either
-- 'LedgerDB.ledgerDbSwitch' or 'LedgerDB.ledgerDbPushMany' on the current
-- 'LedgerDB'.
setCurrent :: IOLike m => LgrDB m blk -> LedgerDB' blk -> STM m ()
setCurrent LgrDB{..} = writeTVar $! varDB

currentPoint :: forall blk. UpdateLedger blk => LedgerDB' blk -> Point blk
currentPoint = castPoint
             . ledgerTipPoint
             . ledgerState
             . LedgerDB.ledgerDbCurrent

takeSnapshot ::
     forall m blk.
     ( IOLike m
     , LgrDbSerialiseConstraints blk
     , HasHeader blk
     , IsLedger (LedgerState blk)
     )
  => LgrDB m blk -> m (Maybe (LedgerDB.DiskSnapshot, RealPoint blk))
takeSnapshot lgrDB@LgrDB{ cfg, tracer, hasFS, diskPolicy } = wrapFailure (Proxy @blk) $ do
    ledgerDB <- LedgerDB.ledgerDbAnchor <$> atomically (getCurrent lgrDB)
    LedgerDB.takeSnapshot
      tracer
      hasFS
      (LedgerDB.onDiskShouldChecksumSnapshots diskPolicy)
      (encodeDiskExtLedgerState ccfg)
      ledgerDB
  where
    ccfg = configCodec $ getExtLedgerCfg $ LedgerDB.ledgerDbCfg cfg

trimSnapshots ::
     forall m blk. (MonadCatch m, HasHeader blk)
  => LgrDB m blk
  -> m [LedgerDB.DiskSnapshot]
trimSnapshots LgrDB { diskPolicy, tracer, hasFS } = wrapFailure (Proxy @blk) $
    LedgerDB.trimSnapshots tracer hasFS diskPolicy

getDiskPolicy :: LgrDB m blk -> LedgerDB.DiskPolicy
getDiskPolicy = diskPolicy

{-------------------------------------------------------------------------------
  Validation
-------------------------------------------------------------------------------}

data ValidateResult blk =
    ValidateSuccessful       (LedgerDB'       blk)
  | ValidateLedgerError      (LedgerDB.AnnLedgerError' blk)
  | ValidateExceededRollBack LedgerDB.ExceededRollback

validate :: forall m blk. (IOLike m, LedgerSupportsProtocol blk, HasCallStack)
         => LgrDB m blk
         -> LedgerDB' blk
            -- ^ This is used as the starting point for validation, not the one
            -- in the 'LgrDB'.
         -> BlockCache blk
         -> Word64  -- ^ How many blocks to roll back
         -> (LedgerDB.UpdateLedgerDbTraceEvent blk -> m ())
         -> [Header blk]
         -> m (ValidateResult blk)
validate LgrDB{..} ledgerDB blockCache numRollbacks trace = \hdrs -> do
    aps <- mkAps hdrs <$> atomically (readTVar varPrevApplied)
    res <- fmap rewrap $ LedgerDB.defaultResolveWithErrors resolveBlock $
             LedgerDB.ledgerDbSwitch
               cfg
               numRollbacks
               (lift . lift . trace)
               aps
               ledgerDB
    atomically $ modifyTVar varPrevApplied $
      addPoints (validBlockPoints res (map headerRealPoint hdrs))
    return res
  where
    rewrap :: Either (LedgerDB.AnnLedgerError' blk) (Either LedgerDB.ExceededRollback (LedgerDB' blk))
           -> ValidateResult blk
    rewrap (Left         e)  = ValidateLedgerError      e
    rewrap (Right (Left  e)) = ValidateExceededRollBack e
    rewrap (Right (Right l)) = ValidateSuccessful       l

    mkAps :: forall n l. l ~ ExtLedgerState blk
          => [Header blk]
          -> Set (RealPoint blk)
          -> [LedgerDB.Ap n l blk ( LedgerDB.ResolvesBlocks    n   blk
                                  , LedgerDB.ThrowsLedgerError n l blk
                                  )]
    mkAps hdrs prevApplied =
      [ case ( Set.member (headerRealPoint hdr) prevApplied
             , BlockCache.lookup (headerHash hdr) blockCache
             ) of
          (False, Nothing)  ->                   LedgerDB.ApplyRef   (headerRealPoint hdr)
          (True,  Nothing)  -> LedgerDB.Weaken $ LedgerDB.ReapplyRef (headerRealPoint hdr)
          (False, Just blk) -> LedgerDB.Weaken $ LedgerDB.ApplyVal   blk
          (True,  Just blk) -> LedgerDB.Weaken $ LedgerDB.ReapplyVal blk
      | hdr <- hdrs
      ]

    -- | Based on the 'ValidateResult', return the hashes corresponding to
    -- valid blocks.
    validBlockPoints :: ValidateResult blk -> [RealPoint blk] -> [RealPoint blk]
    validBlockPoints = \case
      ValidateExceededRollBack _ -> const []
      ValidateSuccessful       _ -> id
      ValidateLedgerError      e -> takeWhile (/= LedgerDB.annLedgerErrRef e)

    addPoints :: [RealPoint blk]
              -> Set (RealPoint blk) -> Set (RealPoint blk)
    addPoints hs set = Foldable.foldl' (flip Set.insert) set hs

{-------------------------------------------------------------------------------
  Previously applied blocks
-------------------------------------------------------------------------------}

getPrevApplied :: IOLike m => LgrDB m blk -> STM m (Set (RealPoint blk))
getPrevApplied LgrDB{..} = readTVar varPrevApplied

-- | Remove all points with a slot older than the given slot from the set of
-- previously applied points.
garbageCollectPrevApplied :: IOLike m => LgrDB m blk -> SlotNo -> STM m ()
garbageCollectPrevApplied LgrDB{..} slotNo = modifyTVar varPrevApplied $
    Set.dropWhileAntitone ((< slotNo) . realPointSlot)

{-------------------------------------------------------------------------------
  Error handling
-------------------------------------------------------------------------------}

-- | Wrap exceptions that may indicate disk failure in a 'ChainDbFailure'
-- exception using the 'LgrDbFailure' constructor.
wrapFailure ::
     forall m x blk. (MonadCatch m, HasHeader blk)
  => Proxy blk
  -> m x
  -> m x
wrapFailure _ k = catch k rethrow
  where
    rethrow :: FsError -> m x
    rethrow err = throwIO $ LgrDbFailure @blk err
