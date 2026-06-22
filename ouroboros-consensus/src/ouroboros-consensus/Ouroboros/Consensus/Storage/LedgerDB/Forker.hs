{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Storage.LedgerDB.Forker
  ( -- * Forker API
    ExceededRollback (..)
  , Forker (..)
  , Forker'
  , ForkerKey (..)
  , GetForkerError (..)
  , RangeQuery (..)
  , RangeQueryPrevious (..)
  , Statistics (..)
  , forkerCurrentPoint
  , castRangeQueryPrevious
  , ledgerStateReadOnlyForker

    -- ** Read only
  , ReadOnlyForker (..)
  , ReadOnlyForker'
  , readOnlyForker

    -- ** Tracing
  , TraceForkerEvent (..)
  , TraceForkerEventWithKey (..)
  , ForkerWasCommitted (..)

    -- * Validation
  , AnnLedgerError (..)
  , AnnLedgerError'
  , ResolveBlock
  , ResolveLeiosBlock (..)
  , resolveLeiosBlock
  , SuccessForkerAction (..)
  , ValidateArgs (..)
  , ValidateResult (..)
  , validate

    -- ** Tracing
  , PushGoal (..)
  , PushStart (..)
  , Pushing (..)
  , TraceValidateEvent (..)
  ) where

import Control.Monad.Except
  ( runExcept
  )
import Data.Bifunctor (first)
import Data.Functor ((<&>))
import Data.Kind
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word
import GHC.Generics
import LeiosDemoDb (LeiosDbConnection)
import LeiosDemoTypes
  ( BytesSize
  , HasLeiosVoting (..)
  , LeiosCert
  , LeiosPoint
  , minCertificationThreshold
  , verifyLeiosCert
  )
import NoThunks.Class
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Config (configLedger)
import Ouroboros.Consensus.HeaderValidation (headerStateChainDep)
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Ledger.SupportsMempool (GenTx)
import Ouroboros.Consensus.Ledger.Tables.Utils
  ( calculateDifference
  , prependDiffs
  , trackingToDiffs
  )
import Ouroboros.Consensus.Protocol.Abstract (ChainDepState)
import Ouroboros.Consensus.Storage.ChainDB.Impl.BlockCache
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.BlockCache as BlockCache
import Ouroboros.Consensus.Util.CallStack
import Ouroboros.Consensus.Util.Enclose
import Ouroboros.Consensus.Util.IOLike

{-------------------------------------------------------------------------------
  Forker
-------------------------------------------------------------------------------}

-- | An independent handle to a point in the LedgerDB, which can be advanced to
-- evaluate forks in the chain.
type Forker :: (Type -> Type) -> LedgerStateKind -> Type
data Forker m l = Forker
  { forkerClose :: !(m ())
  -- ^ Close the current forker (idempotent).
  --
  -- Other functions on forkers should throw a 'ClosedForkError' once the
  -- forker is closed.
  --
  -- Note: always use this functions before the forker is forgotten!
  -- Otherwise, cleanup of (on-disk) state might not be prompt or guaranteed.
  --
  -- This function should release any resources that are held by the forker,
  -- and not by the LedgerDB.
  , -- Queries

    forkerReadTables :: !(LedgerTables l KeysMK -> m (LedgerTables l ValuesMK))
  -- ^ Read ledger tables from disk.
  , forkerRangeReadTables :: !(RangeQueryPrevious l -> m (LedgerTables l ValuesMK, Maybe (TxIn l)))
  -- ^ Range-read ledger tables from disk.
  --
  -- This range read will return as many values as the 'QueryBatchSize' that was
  -- passed when opening the LedgerDB.
  --
  -- The second component of the returned tuple is the maximal key found by the
  -- forker. This is only necessary because some backends have a different
  -- sorting for the keys than the order defined in Haskell.
  --
  -- The last key retrieved is part of the map too. It is intended to be fed
  -- back into the next iteration of the range read. If the function returns
  -- Nothing, it means the read returned no results, or in other words, we
  -- reached the end of the ledger tables.
  , forkerGetLedgerState :: !(STM m (l EmptyMK))
  -- ^ Get the full ledger state without tables.
  --
  -- If an empty ledger state is all you need, use 'getVolatileTip',
  -- 'getImmutableTip', or 'getPastLedgerState' instead of using a 'Forker'.
  , forkerReadStatistics :: !(m Statistics)
  -- ^ Get statistics about the current state of the handle if possible.
  --
  -- Returns 'Nothing' if the implementation is backed by @lsm-tree@.
  , -- Updates

    forkerPush :: !(l DiffMK -> m ())
  -- ^ Advance the fork handle by pushing a new ledger state to the tip of the
  -- current fork.
  , forkerCommit :: !(STM m (m ()))
  -- ^ Commit the fork, which was constructed using 'forkerPush', as the
  -- current version of the LedgerDB.
  --
  -- Returns an IO action that has to be run on cleanup. It closes the orphaned
  -- resources from the LedgerDB.
  }
  deriving Generic
  deriving NoThunks via OnlyCheckWhnf (Forker m l)

-- | An identifier for a 'Forker'. See 'ldbForkers'.
newtype ForkerKey = ForkerKey Word16
  deriving stock (Show, Eq, Ord)
  deriving newtype (Enum, NoThunks, Num)

type instance HeaderHash (Forker m l) = HeaderHash l

type Forker' m blk = Forker m (ExtLedgerState blk)

instance
  (GetTip l, MonadSTM m) =>
  GetTipSTM m (Forker m l)
  where
  getTipSTM forker = castPoint . getTip <$> forkerGetLedgerState forker

data RangeQueryPrevious l = NoPreviousQuery | PreviousQueryWasFinal | PreviousQueryWasUpTo (TxIn l)

castRangeQueryPrevious :: TxIn l ~ TxIn l' => RangeQueryPrevious l -> RangeQueryPrevious l'
castRangeQueryPrevious NoPreviousQuery = NoPreviousQuery
castRangeQueryPrevious PreviousQueryWasFinal = PreviousQueryWasFinal
castRangeQueryPrevious (PreviousQueryWasUpTo txin) = PreviousQueryWasUpTo txin

data RangeQuery l = RangeQuery
  { rqPrev :: !(RangeQueryPrevious l)
  , rqCount :: !Int
  }

-- | This type captures the size of the ledger tables at a particular point in
-- the LedgerDB.
--
-- This is for now the only metric that was requested from other components, but
-- this type might be augmented in the future with more statistics.
newtype Statistics = Statistics
  { ledgerTableSize :: Int
  }

-- | Errors that can be thrown while acquiring forkers.
data GetForkerError
  = -- | The requested point was not found in the LedgerDB, but the point is
    -- recent enough that the point is not in the immutable part of the chain,
    -- i.e. it belongs to an unselected fork.
    PointNotOnChain
  | -- | The requested point was not found in the LedgerDB because the point
    -- older than the immutable tip.
    PointTooOld !(Maybe ExceededRollback)
  deriving (Show, Eq)

-- | Exceeded maximum rollback supported by the current ledger DB state
--
-- Under normal circumstances this will not arise. It can really only happen
-- in the presence of data corruption (or when switching to a shorter fork,
-- but that is disallowed by all currently known Ouroboros protocols).
--
-- Records both the supported and the requested rollback.
data ExceededRollback = ExceededRollback
  { rollbackMaximum :: Word64
  , rollbackRequested :: Word64
  }
  deriving (Show, Eq)

forkerCurrentPoint ::
  (GetTip l, HeaderHash l ~ HeaderHash blk, Functor (STM m)) =>
  Proxy blk ->
  Forker m l ->
  STM m (Point blk)
forkerCurrentPoint _ forker =
  castPoint
    . getTip
    <$> forkerGetLedgerState forker

ledgerStateReadOnlyForker ::
  IOLike m => ReadOnlyForker m (ExtLedgerState blk) -> ReadOnlyForker m (LedgerState blk)
ledgerStateReadOnlyForker frk =
  ReadOnlyForker
    { roforkerClose = roforkerClose
    , roforkerReadTables = fmap castLedgerTables . roforkerReadTables . castLedgerTables
    , roforkerRangeReadTables =
        fmap (first castLedgerTables) . roforkerRangeReadTables . castRangeQueryPrevious
    , roforkerGetLedgerState = ledgerState <$> roforkerGetLedgerState
    , roforkerReadStatistics = roforkerReadStatistics
    }
 where
  ReadOnlyForker
    { roforkerClose
    , roforkerReadTables
    , roforkerRangeReadTables
    , roforkerGetLedgerState
    , roforkerReadStatistics
    } = frk

{-------------------------------------------------------------------------------
  Read-only forkers
-------------------------------------------------------------------------------}

-- | Read-only 'Forker'.
--
-- These forkers are not allowed to commit. They are used everywhere except in
-- Chain Selection. In particular they are now used in:
--
-- - LocalStateQuery server, via 'getReadOnlyForkerAtPoint'
--
-- - Forging loop.
--
-- - Mempool.
type ReadOnlyForker :: (Type -> Type) -> LedgerStateKind -> Type
data ReadOnlyForker m l = ReadOnlyForker
  { roforkerClose :: !(m ())
  -- ^ See 'forkerClose'
  , roforkerReadTables :: !(LedgerTables l KeysMK -> m (LedgerTables l ValuesMK))
  -- ^ See 'forkerReadTables'
  , roforkerRangeReadTables :: !(RangeQueryPrevious l -> m (LedgerTables l ValuesMK, Maybe (TxIn l)))
  -- ^ See 'forkerRangeReadTables'.
  , roforkerGetLedgerState :: !(STM m (l EmptyMK))
  -- ^ See 'forkerGetLedgerState'
  , roforkerReadStatistics :: !(m Statistics)
  -- ^ See 'forkerReadStatistics'
  }
  deriving Generic

instance NoThunks (ReadOnlyForker m l) where
  wNoThunks _ _ = pure Nothing
  showTypeOf _ = "ReadOnlyForker"

type instance HeaderHash (ReadOnlyForker m l) = HeaderHash l

type ReadOnlyForker' m blk = ReadOnlyForker m (ExtLedgerState blk)

readOnlyForker :: Forker m l -> ReadOnlyForker m l
readOnlyForker forker =
  ReadOnlyForker
    { roforkerClose = forkerClose forker
    , roforkerReadTables = forkerReadTables forker
    , roforkerRangeReadTables = forkerRangeReadTables forker
    , roforkerGetLedgerState = forkerGetLedgerState forker
    , roforkerReadStatistics = forkerReadStatistics forker
    }

{-------------------------------------------------------------------------------
  Validation
-------------------------------------------------------------------------------}

data ValidateArgs m l blk = ValidateArgs
  { resolve :: !(ResolveBlock m blk)
  -- ^ How to retrieve blocks from headers
  , validateConfig :: !(LedgerCfg l)
  -- ^ The config
  , addPrevApplied :: !([RealPoint blk] -> STM m ())
  -- ^ How to add a previously applied block to the set of known blocks
  , prevApplied :: !(STM m (Set (RealPoint blk)))
  -- ^ Get the current set of previously applied blocks
  , withForkerAtFromTip :: !(forall r. Word64 -> (Forker m l -> m r) -> m (Either GetForkerError r))
  -- ^ Create a forker from the tip
  , onSuccess :: !(SuccessForkerAction m l)
  -- ^ Continuation to run when the validation was successful
  , trace :: !(TraceValidateEvent blk -> m ())
  -- ^ A tracer for validate events
  , blockCache :: BlockCache blk
  -- ^ The block cache
  , numRollbacks :: Word64
  -- ^ How many blocks to roll back before applying the blocks
  , hdrs :: NonEmpty (Header blk)
  -- ^ The headers we want to apply
  , leiosDB :: !(LeiosDbConnection m)
  -- ^ Leios demo DB connection: 'applyBlock' calls 'resolveLeiosBlock'
  -- with this connection before each ledger application, so that
  -- Dijkstra blocks carrying a 'Maybe LeiosCert' can have the EB
  -- closure spliced into the body.
  }

validate ::
  forall m l blk.
  ( IOLike m
  , HasCallStack
  , ApplyBlock l blk
  , ResolveLeiosBlock blk
  , HasLeiosVoting blk
  , l ~ ExtLedgerState blk
  ) =>
  ComputeLedgerEvents ->
  ValidateArgs m l blk ->
  m (ValidateResult l blk)
validate evs args = do
  aps <- mkAps <$> atomically prevApplied
  res <-
    rewrap
      <$> switch
        leiosDB
        withForkerAtFromTip
        evs
        validateConfig
        numRollbacks
        trace
        aps
        resolve
        onSuccess
  atomically $ addPrevApplied (validBlockPoints res (map headerRealPoint $ NE.toList hdrs))
  pure res
 where
  ValidateArgs
    { resolve
    , validateConfig
    , addPrevApplied
    , prevApplied
    , withForkerAtFromTip
    , trace
    , blockCache
    , numRollbacks
    , hdrs
    , onSuccess
    , leiosDB
    } = args

  rewrap ::
    Either GetForkerError (Either (AnnLedgerError l blk) ()) ->
    ValidateResult l blk
  rewrap (Right (Left e)) = ValidateLedgerError e
  rewrap (Left (PointTooOld (Just e))) = ValidateExceededRollBack e
  rewrap (Left _) = error "Unreachable, validating will always rollback from the tip"
  rewrap (Right (Right ())) = ValidateSuccessful

  mkAps ::
    Set (RealPoint blk) ->
    NonEmpty (Ap m l blk)
  mkAps prev =
    NE.map
      ( \hdr -> case ( Set.member (headerRealPoint hdr) prev
                     , BlockCache.lookup (headerHash hdr) blockCache
                     ) of
          (False, Nothing) -> ApplyRef (headerRealPoint hdr)
          (True, Nothing) -> ReapplyRef (headerRealPoint hdr)
          (False, Just blk) -> ApplyVal blk
          (True, Just blk) -> ReapplyVal blk
      )
      hdrs

  -- \| Based on the 'ValidateResult', return the hashes corresponding to
  -- valid blocks.
  validBlockPoints :: ValidateResult l blk -> [RealPoint blk] -> [RealPoint blk]
  validBlockPoints = \case
    ValidateExceededRollBack _ -> const []
    ValidateSuccessful -> id
    ValidateLedgerError e -> takeWhile (/= annLedgerErrRef e)

-- | Switch to a fork by rolling back a number of blocks and then pushing the
-- new blocks.
switch ::
  ( ApplyBlock l blk
  , MonadSTM m
  , ResolveLeiosBlock blk
  , HasLeiosVoting blk
  , l ~ ExtLedgerState blk
  ) =>
  LeiosDbConnection m ->
  (forall r. Word64 -> (Forker m l -> m r) -> m (Either GetForkerError r)) ->
  ComputeLedgerEvents ->
  LedgerCfg l ->
  -- | How many blocks to roll back
  Word64 ->
  (TraceValidateEvent blk -> m ()) ->
  -- | New blocks to apply
  NonEmpty (Ap m l blk) ->
  ResolveBlock m blk ->
  SuccessForkerAction m l ->
  m (Either GetForkerError (Either (AnnLedgerError l blk) ()))
switch leiosDb withForkerAtFromTip evs cfg numRollbacks trace newBlocks doResolve onSuccess = do
  withForkerAtFromTip numRollbacks $ \fo -> do
    let start = PushStart . toRealPoint . NE.head $ newBlocks
        goal = PushGoal . toRealPoint . NE.last $ newBlocks
    ePush <-
      applyThenPushMany
        leiosDb
        (trace . StartedPushingBlockToTheLedgerDb start goal)
        evs
        cfg
        (NE.toList newBlocks)
        fo
        doResolve
    case ePush of
      Left err -> pure (Left err)
      Right () -> fmap Right $ applySuccessForkerAction onSuccess fo

{-------------------------------------------------------------------------------
  Apply blocks
-------------------------------------------------------------------------------}

-- | 'Ap' is used to pass information about blocks to ledger DB updates
--
-- The constructors provide answers to two questions:
--
--     1. Are we passing the block by value or by reference?
--
--     2. Are we applying or reapplying the block?
type Ap :: (Type -> Type) -> LedgerStateKind -> Type -> Type
data Ap m l blk where
  ReapplyVal :: blk -> Ap m l blk
  ApplyVal :: blk -> Ap m l blk
  ReapplyRef :: RealPoint blk -> Ap m l blk
  ApplyRef :: RealPoint blk -> Ap m l blk

toRealPoint :: HasHeader blk => Ap m l blk -> RealPoint blk
toRealPoint (ReapplyVal blk) = blockRealPoint blk
toRealPoint (ApplyVal blk) = blockRealPoint blk
toRealPoint (ReapplyRef rp) = rp
toRealPoint (ApplyRef rp) = rp

-- | Apply blocks to the given forker
applyBlock ::
  forall m l blk.
  ( ApplyBlock l blk
  , MonadSTM m
  , ResolveLeiosBlock blk
  , HasLeiosVoting blk
  , l ~ ExtLedgerState blk
  ) =>
  LeiosDbConnection m ->
  ComputeLedgerEvents ->
  LedgerCfg l ->
  Ap m l blk ->
  Forker m l ->
  ResolveBlock m blk ->
  m (Either (AnnLedgerError l blk) (l DiffMK))
applyBlock leiosDb evs cfg ap fo doResolveBlock = case ap of
  ReapplyVal b -> do
    cds <- headerStateChainDep . headerState <$> atomically (forkerGetLedgerState fo)
    b' <- resolveLeiosBlock leiosDb cds b
    withValues b' (return . Right . tickThenReapply evs cfg b')
  ApplyVal b -> do
    extSt <- atomically (forkerGetLedgerState fo)
    let cds = headerStateChainDep (headerState extSt)
    case blockLeiosCert b of
      Just cert -> do
        -- CertRB apply path. The block's body on the wire is empty (it
        -- carries only the Leios certificate, not the EB's txs). The
        -- sequence:
        --
        --   1. 'verifyLeiosCert' against the current committee and the
        --      announced (expected signed) EB.
        --   2. 'resolveLeiosClosure' to fetch the EB's txs from the
        --      LeiosDB.
        --   3. Load utxos (ledger tables) for the closure txs.
        --   4. 'applyLeiosClosure' folds those txs onto the /unticked/
        --      parent ledger via the per-era ledger 'ApplyTx' class
        --      with 'ValidateNone' (txs were validated upstream when
        --      inserted into the LeiosDb).
        --   5. 'tickThenApply' the CertRB on the post-closure state;
        --      BBODY's 'hbBodyHash' check matches because the on-wire
        --      body is genuinely empty.
        --
        -- Steps 1 and 2 happen here rather than as Dijkstra ledger
        -- rules because step 2 involves an IO read of the LeiosDB
        -- which can't be interleaved with the pure STS evaluation.
        case protocolStateLeiosAnnouncement @blk cds of
          Nothing ->
            -- TODO: make this less fatal or impossible to reach
            error $ "applyBlock: nothing announced!?"
          Just (announcedPoint, _) -> do
            cm <- case getLeiosCommittee (ledgerState extSt) of
              Just c -> pure c
              Nothing ->
                -- CertRB on an era without a Leios committee is itself a protocol
                -- violation: the era machinery shouldn't have let one through.
                -- TODO: make this less fatal
                error "applyBlock: CertRB seen but no Leios committee for this era"
            -- FIXME: This should not be about a LeiosPoint, but an RbHash
            case verifyLeiosCert cm minCertificationThreshold announcedPoint cert of
              Left invalid ->
                -- TODO: make this less fatal. This is like a ledger error.
                error $ "applyBlock: invalid Leios cert: " <> show invalid
              Right _weight -> do
                -- Load EB txs from disk
                closureTxs <- resolveLeiosClosure leiosDb announcedPoint b
                -- UTXO-HD of the whole closure
                let blkKeys = getBlockKeySets b
                    closureKeys = foldMap (castLedgerTables . leiosClosureTxKeySets) closureTxs
                lsBeforeEB <- withLedgerTables extSt <$> forkerReadTables fo (closureKeys <> blkKeys)
                let tip = castPoint $ getTip lsBeforeEB
                -- FIXME: Use the announcing block slot for txs
                case applyLeiosClosure
                  (configLedger (getExtLedgerCfg cfg))
                  (blockSlot b)
                  closureTxs
                  (ledgerState lsBeforeEB) of
                  Left lerr ->
                    -- REVIEW: Better annotation than CertRB point possible?
                    pure
                      ( Left
                          ( AnnLedgerError
                              tip
                              (blockRealPoint b)
                              (ExtValidationErrorLedger lerr)
                          )
                      )
                  Right newLst ->
                    let lsAfterEB = lsBeforeEB{ledgerState = newLst}
                     in case runExcept $ tickThenApply evs cfg b lsAfterEB of
                          Left lerr ->
                            pure (Left (AnnLedgerError tip (blockRealPoint b) lerr))
                          Right blockDiff ->
                            -- The closure's table modifications happened
                            -- between 'lsBeforeEB' and 'lsAfterEB' and are
                            -- /not/ in 'blockDiff' (which is a diff relative
                            -- to 'lsAfterEB'). 'forkerPush' interprets the
                            -- pushed diff as being on top of 'extSt' (the
                            -- LedgerDB anchor), so without composition the
                            -- closure inputs/outputs would be silently
                            -- dropped from the changelog and unavailable to
                            -- the next block's 'forkerReadTables'.
                            let closureDiff =
                                  trackingToDiffs
                                    (calculateDifference lsBeforeEB lsAfterEB)
                             in pure (Right (prependDiffs closureDiff blockDiff))
      Nothing ->
        -- Not a CertRB: ordinary Praos block
        withValues b $ \v ->
          case runExcept $ tickThenApply evs cfg b v of
            Left lerr -> pure (Left (AnnLedgerError (castPoint $ getTip v) (blockRealPoint b) lerr))
            Right st -> pure (Right st)
  ReapplyRef r -> do
    b <- doResolveBlock r
    applyBlock leiosDb evs cfg (ReapplyVal b) fo doResolveBlock
  ApplyRef r -> do
    b <- doResolveBlock r
    applyBlock leiosDb evs cfg (ApplyVal b) fo doResolveBlock
 where
  withValues ::
    blk ->
    (l ValuesMK -> m (Either (AnnLedgerError l blk) (l DiffMK))) ->
    m (Either (AnnLedgerError l blk) (l DiffMK))
  withValues blk f = do
    l <- atomically $ forkerGetLedgerState fo
    vs <- withLedgerTables l <$> forkerReadTables fo (getBlockKeySets blk)
    f vs

-- | If applying a block on top of the ledger state at the tip is succesful,
-- push the resulting ledger state to the forker.
applyThenPush ::
  ( ApplyBlock l blk
  , MonadSTM m
  , ResolveLeiosBlock blk
  , HasLeiosVoting blk
  , l ~ ExtLedgerState blk
  ) =>
  LeiosDbConnection m ->
  ComputeLedgerEvents ->
  LedgerCfg l ->
  Ap m l blk ->
  Forker m l ->
  ResolveBlock m blk ->
  m (Either (AnnLedgerError l blk) ())
applyThenPush leiosDb evs cfg ap fo doResolve = do
  eLerr <- applyBlock leiosDb evs cfg ap fo doResolve
  case eLerr of
    Left err -> pure (Left err)
    Right st -> Right <$> forkerPush fo st

-- | Apply and push a sequence of blocks (oldest first).
applyThenPushMany ::
  ( ApplyBlock l blk
  , MonadSTM m
  , ResolveLeiosBlock blk
  , HasLeiosVoting blk
  , l ~ ExtLedgerState blk
  ) =>
  LeiosDbConnection m ->
  (Pushing blk -> m ()) ->
  ComputeLedgerEvents ->
  LedgerCfg l ->
  [Ap m l blk] ->
  Forker m l ->
  ResolveBlock m blk ->
  m (Either (AnnLedgerError l blk) ())
applyThenPushMany leiosDb trace evs cfg aps fo doResolveBlock = pushAndTrace aps
 where
  pushAndTrace [] = pure $ Right ()
  pushAndTrace (ap : aps') = do
    trace $ Pushing . toRealPoint $ ap
    res <- applyThenPush leiosDb evs cfg ap fo doResolveBlock
    case res of
      Left err -> pure (Left err)
      Right () -> pushAndTrace aps'

{-------------------------------------------------------------------------------
  Finding blocks
-------------------------------------------------------------------------------}

-- | Resolve a block
--
-- Resolving a block reference to the actual block lives in @m@ because
-- it might need to read the block from disk (and can therefore not be
-- done inside an STM transaction).
--
-- NOTE: The ledger DB will only ask the 'ChainDB' for blocks it knows
-- must exist. If the 'ChainDB' is unable to fulfill the request, data
-- corruption must have happened and the 'ChainDB' should trigger
-- validation mode.
type ResolveBlock m blk = RealPoint blk -> m blk

-- | Resolve a block before it is applied to the ledger.
--
-- In Leios, a Dijkstra-era block may carry only a 'LeiosCert' on its body
-- in place of the regular tx list: the actual transactions to apply live
-- in the EB's stored closure ('LeiosDbConnection'). 'resolveLeiosBlock'
-- splices the EB closure back into the block before validation. For
-- block types that do not carry such certificates, the default 'return
-- blk' is correct.
class ResolveLeiosBlock blk where
  -- | For a CertRB, look up the EB closure that the cert in this block's
  -- body attests to and return it as a list of transactions (in the order
  -- they appear in the EB). 'Nothing' for non-CertRB blocks.
  --
  -- This is the apply-path variant: the caller applies the closure txs
  -- onto the parent ledger state via 'applyLeiosClosure', then applies
  -- the CertRB itself (empty body) via 'tickThenApply'. The wire body is
  -- genuinely empty (so 'hbBodyHash' matches) and the closure txs are
  -- folded into the unticked ledger state, mirroring how a non-CertRB
  -- Praos block's body txs would be applied — but sourced from the
  -- LeiosDB rather than the block body.
  resolveLeiosClosure ::
    Monad m =>
    LeiosDbConnection m ->
    LeiosPoint ->
    blk ->
    m [GenTx blk]
  resolveLeiosClosure _ _ _ = pure []

  -- | The ledger keys read by a closure tx — what 'forkerReadTables' needs
  -- to load before 'applyLeiosClosure' can run. Leios-enabled instances
  -- should override with their 'LedgerSupportsMempool.getTransactionKeySets'.
  --
  -- The default panics: a non-Leios block has @'resolveLeiosClosure' = pure
  -- []@ so this method is never reached on the apply path. We can't return
  -- 'emptyLedgerTables' as a defensive default because building one needs
  -- 'LedgerTableConstraints' which isn't in scope at the class-default site.
  -- TODO: could be avoided if we move this into resolveLeiosClosure?
  leiosClosureTxKeySets :: GenTx blk -> LedgerTables (LedgerState blk) KeysMK
  leiosClosureTxKeySets _ =
    error "leiosClosureTxKeySets: not Leios-enabled for this block type"

  -- | Apply an EB closure's transactions onto an /unticked/ ledger state,
  -- without validation. The closure has already been individually
  -- validated when each tx was inserted into the LeiosDb, so we trust it
  -- here (era-level @ApplyTxValidation ValidateNone@). Returns the
  -- unticked post-closure ledger state, ready to feed into
  -- 'tickThenApply' for the CertRB itself.
  --
  -- Sidesteps the consensus' Ticked-state mempool API by dropping down to
  -- the per-era ledger 'ApplyTx' class, which works directly on the pure
  -- per-era @LedgerState era@.
  applyLeiosClosure ::
    LedgerCfg (LedgerState blk) ->
    SlotNo ->
    [GenTx blk] ->
    LedgerState blk ValuesMK ->
    Either (LedgerErr (LedgerState blk)) (LedgerState blk ValuesMK)
  applyLeiosClosure _ _ _ st = Right st

  -- | Inline transactions of an EB closure into a 'blk'. The returned 'blk' may be deemed invalid, but
  -- this is useful nonetheless for some use cases. Returns 'Nothing' when no
  -- resolution was needed so the caller can reuse the original blk directly.
  inlineLeiosClosure :: blk -> [GenTx blk] -> blk
  inlineLeiosClosure blk _ = blk

  -- | Get the 'LeiosCert' of this block if it carries one (is a CertRB).
  -- 'Nothing' default implementation for eras that don't have Leios certs.
  blockLeiosCert :: blk -> Maybe LeiosCert
  blockLeiosCert _ = Nothing

  -- | The EB announcement carried by this header (point + on-the-wire
  -- body size), if any. 'Nothing' for headers in eras that don't carry
  -- Leios announcements.
  headerLeiosAnnouncement :: Header blk -> Maybe (LeiosPoint, BytesSize)
  headerLeiosAnnouncement _ = Nothing

  -- | The EB most recent announcement in the 'HeaderState', if any. 'Nothing'
  -- for headers in eras that don't carry Leios announcements.
  protocolStateLeiosAnnouncement ::
    ChainDepState (BlockProtocol blk) -> Maybe (LeiosPoint, BytesSize)
  protocolStateLeiosAnnouncement _ = Nothing

-- | Resolve and inline EB closure transactions as announced on the previous
-- header. NOTE: This produces a block that would fail full validation.
resolveLeiosBlock ::
  forall blk m.
  Monad m =>
  ResolveLeiosBlock blk =>
  LeiosDbConnection m ->
  ChainDepState (BlockProtocol blk) ->
  blk ->
  m blk
resolveLeiosBlock leiosDb cds b =
  case protocolStateLeiosAnnouncement @blk cds of
    Nothing -> pure b
    Just (announcedPoint, _) ->
      -- NOTE: This produces a block that would fail full validation.
      resolveLeiosClosure leiosDb announcedPoint b
        <&> inlineLeiosClosure b

{-------------------------------------------------------------------------------
  Validation
-------------------------------------------------------------------------------}

-- | A helpful type for a callback of the validation logic
--
-- The latest iteration of the maintenance of backend resources held
-- by 'Forker's relies heavily on 'bracket'. For that reason, we end
-- up passing a "success continuation" through several layers of
-- interface, which runs inside of those brackets.
--
-- This type makes that continuation easier to recognize. In
-- particular, any continuation that ends with @res -> m ()@ is
-- commonly used as "how to close a @res@", which is *NOT* the case
-- here. So it's preferable to use this more perspicious type in
-- signatures.
newtype SuccessForkerAction m l = MkSuccessForkerAction
  { applySuccessForkerAction :: Forker m l -> m ()
  }

-- | When validating a sequence of blocks, these are the possible outcomes.
data ValidateResult l blk
  = ValidateSuccessful
  | ValidateLedgerError (AnnLedgerError l blk)
  | ValidateExceededRollBack ExceededRollback

{-------------------------------------------------------------------------------
  An annotated ledger error
-------------------------------------------------------------------------------}

-- | Annotated ledger errors
data AnnLedgerError l blk = AnnLedgerError
  { annLedgerBaseRef :: Point blk
  -- ^ The last block that was valid
  , annLedgerErrRef :: RealPoint blk
  -- ^ Reference to the block that had the error
  , annLedgerErr :: LedgerErr l
  -- ^ The ledger error itself
  }

type AnnLedgerError' blk = AnnLedgerError (ExtLedgerState blk) blk

{-------------------------------------------------------------------------------
  Trace validation events
-------------------------------------------------------------------------------}

newtype PushStart blk = PushStart {unPushStart :: RealPoint blk}
  deriving (Show, Eq)

newtype PushGoal blk = PushGoal {unPushGoal :: RealPoint blk}
  deriving (Show, Eq)

newtype Pushing blk = Pushing {unPushing :: RealPoint blk}
  deriving (Show, Eq)

data TraceValidateEvent blk
  = -- | Event fired when we are about to push a block to a forker
    StartedPushingBlockToTheLedgerDb
      -- | Point from which we started pushing new blocks
      !(PushStart blk)
      -- | Point to which we are updating the ledger, the last event
      -- StartedPushingBlockToTheLedgerDb will have Pushing and PushGoal
      -- wrapping over the same RealPoint
      (PushGoal blk)
      -- | Point which block we are about to push
      !(Pushing blk)
  deriving (Show, Eq, Generic)

{-------------------------------------------------------------------------------
  Forker events
-------------------------------------------------------------------------------}

data TraceForkerEventWithKey
  = TraceForkerEventWithKey ForkerKey TraceForkerEvent
  deriving (Show, Eq)

data TraceForkerEvent
  = ForkerOpen
  | ForkerReadTables EnclosingTimed
  | ForkerRangeReadTables EnclosingTimed
  | ForkerReadStatistics
  | ForkerPush EnclosingTimed
  | ForkerClose ForkerWasCommitted
  deriving (Show, Eq)

data ForkerWasCommitted
  = ForkerWasCommitted
  | ForkerWasUncommitted
  deriving (Eq, Show)
