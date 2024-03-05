{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE QuantifiedConstraints    #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE UndecidableInstances     #-}

module Ouroboros.Consensus.Storage.LedgerDB.Impl.Validate (
    -- * Find blocks
    ResolveBlock
  , ResolvesBlocks (..)
    -- * Validation
  , ValidLedgerState (..)
  , validate
    -- * Testing
  , defaultResolveWithErrors
  , defaultThrowLedgerErrors
  ) where

import           Control.Monad (void)
import           Control.Monad.Base
import           Control.Monad.Except (ExceptT (..), MonadError (..), runExcept,
                     runExceptT)
import           Control.Monad.Reader (ReaderT (..))
import           Control.Monad.Trans (MonadTrans (..))
import           Data.Kind
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Storage.ChainDB.Impl.BlockCache
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.BlockCache as BlockCache
import           Ouroboros.Consensus.Storage.LedgerDB.API hiding (validate)
import           Ouroboros.Consensus.Util.CallStack
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry

{-------------------------------------------------------------------------------
  Validation
-------------------------------------------------------------------------------}

validate ::
     forall m blk. (
       IOLike m
     , LedgerSupportsProtocol blk
     , HasCallStack
     , MonadBase m m
     )
  => ResolveBlock m blk
  -> TopLevelConfig blk
  -> ([RealPoint blk] -> STM m ())
  -> STM m (Set (RealPoint blk))
  -> (ResourceRegistry m -> Word64 -> m (Either ExceededRollback (Forker' m blk)))
  -> ResourceRegistry m
  -> (TraceValidateEvent blk -> m ())
  -> BlockCache blk
  -> Word64          -- ^ How many blocks to roll back
  -> [Header blk]
  -> m (ValidateResult' m blk)
validate resolve config addPrevApplied prevApplied forkerAtFromTip rr trace blockCache numRollbacks hdrs = do
    aps <- mkAps <$> atomically prevApplied
    res <- fmap rewrap $ defaultResolveWithErrors resolve $
             switch
               forkerAtFromTip
               rr
               (ExtLedgerCfg config)
               numRollbacks
               (lift . lift . trace)
               aps
    liftBase $ atomically $ addPrevApplied (validBlockPoints res (map headerRealPoint hdrs))
    return res
  where
    rewrap :: Either (AnnLedgerError' n blk) (Either ExceededRollback (Forker' n blk))
           -> ValidateResult' n blk
    rewrap (Left         e)  = ValidateLedgerError      e
    rewrap (Right (Left  e)) = ValidateExceededRollBack e
    rewrap (Right (Right l)) = ValidateSuccessful       l

    mkAps :: forall bn n l. l ~ ExtLedgerState blk
          => Set (RealPoint blk)
          -> [Ap bn n l blk ( ResolvesBlocks       n   blk
                            , ThrowsLedgerError bn n l blk
                            )]
    mkAps prev =
      [ case ( Set.member (headerRealPoint hdr) prev
             , BlockCache.lookup (headerHash hdr) blockCache
             ) of
          (False, Nothing)  ->          ApplyRef   (headerRealPoint hdr)
          (True,  Nothing)  -> Weaken $ ReapplyRef (headerRealPoint hdr)
          (False, Just blk) -> Weaken $ ApplyVal   blk
          (True,  Just blk) -> Weaken $ ReapplyVal blk
      | hdr <- hdrs
      ]

    -- | Based on the 'ValidateResult', return the hashes corresponding to
    -- valid blocks.
    validBlockPoints :: forall n. ValidateResult' n blk -> [RealPoint blk] -> [RealPoint blk]
    validBlockPoints = \case
      ValidateExceededRollBack _ -> const []
      ValidateSuccessful       _ -> id
      ValidateLedgerError      e -> takeWhile (/= annLedgerErrRef e)

-- | Switch to a fork by rolling back a number of blocks and then pushing the
-- new blocks.
switch ::
     (ApplyBlock l blk, MonadBase bm m, c, MonadSTM bm)
  => (ResourceRegistry bm -> Word64 -> bm (Either ExceededRollback (Forker bm l blk)))
  -> ResourceRegistry bm
  -> LedgerCfg l
  -> Word64          -- ^ How many blocks to roll back
  -> (TraceValidateEvent blk -> m ())
  -> [Ap bm m l blk c]  -- ^ New blocks to apply
  -> m (Either ExceededRollback (Forker bm l blk))
switch forkerAtFromTip rr cfg numRollbacks trace newBlocks = do
  foEith <- liftBase $ forkerAtFromTip rr numRollbacks
  case foEith of
    Left rbExceeded -> pure $ Left rbExceeded
    Right fo -> do
      case newBlocks of
        [] -> pure ()
        -- no blocks to apply to ledger state, return the forker
        (firstBlock:_) -> do
          let start   = PushStart . toRealPoint $ firstBlock
              goal    = PushGoal  . toRealPoint . last $ newBlocks
          void $ applyThenPushMany
                    (trace . StartedPushingBlockToTheLedgerDb start goal)
                    cfg
                    newBlocks
                    fo
      pure $ Right fo

{-------------------------------------------------------------------------------
  Apply blocks
-------------------------------------------------------------------------------}

newtype ValidLedgerState l = ValidLedgerState { getValidLedgerState :: l }

-- | 'Ap' is used to pass information about blocks to ledger DB updates
--
-- The constructors serve two purposes:
--
-- * Specify the various parameters
--
--     1. Are we passing the block by value or by reference?
--
--     2. Are we applying or reapplying the block?
--
-- * Compute the constraint @c@ on the monad @m@ in order to run the query:
--
--     1. If we are passing a block by reference, we must be able to resolve it.
--
--     2. If we are applying rather than reapplying, we might have ledger errors.
type Ap :: (Type -> Type) -> (Type -> Type) -> LedgerStateKind -> Type -> Constraint -> Type
data Ap bm m l blk c where
  ReapplyVal ::           blk -> Ap bm m l blk ()
  ApplyVal   ::           blk -> Ap bm m l blk ( ThrowsLedgerError bm m l blk )
  ReapplyRef :: RealPoint blk -> Ap bm m l blk ( ResolvesBlocks       m   blk )
  ApplyRef   :: RealPoint blk -> Ap bm m l blk ( ResolvesBlocks       m   blk
                                               , ThrowsLedgerError bm m l blk )

  -- | 'Weaken' increases the constraint on the monad @m@.
  --
  -- This is primarily useful when combining multiple 'Ap's in a single
  -- homogeneous structure.
  Weaken :: (c' => c) => Ap bm m l blk c -> Ap bm m l blk c'

toRealPoint :: HasHeader blk => Ap bm m l blk c -> RealPoint blk
toRealPoint (ReapplyVal blk) = blockRealPoint blk
toRealPoint (ApplyVal blk)   = blockRealPoint blk
toRealPoint (ReapplyRef rp)  = rp
toRealPoint (ApplyRef rp)    = rp
toRealPoint (Weaken ap)      = toRealPoint ap

-- | Apply blocks to the given forker
applyBlock :: forall m bm c l blk. (ApplyBlock l blk, MonadBase bm m, c, MonadSTM bm)
           => LedgerCfg l
           -> Ap bm m l blk c
           -> Forker bm l blk
           -> m (ValidLedgerState (l DiffMK))
applyBlock cfg ap fo = case ap of
    ReapplyVal b ->
          ValidLedgerState
      <$> withValues b (return . tickThenReapply cfg b)
    ApplyVal b ->
          ValidLedgerState
      <$> withValues b
          ( either (throwLedgerError fo (blockRealPoint b)) return
            . runExcept
            . tickThenApply cfg b
          )
    ReapplyRef r  -> do
      b <- doResolveBlock r
      applyBlock cfg (ReapplyVal b) fo
    ApplyRef r -> do
      b <- doResolveBlock r
      applyBlock cfg (ApplyVal b) fo
    Weaken ap' ->
      applyBlock cfg ap' fo
  where
    withValues :: blk -> (l ValuesMK -> m (l DiffMK)) -> m (l DiffMK)
    withValues blk f = do
        l <- liftBase $ atomically $ forkerGetLedgerState fo
        vs <- withLedgerTables l
              <$> liftBase (forkerReadTables fo (getBlockKeySets blk))
        f vs

-- | If applying a block on top of the ledger state at the tip is succesful,
-- push the resulting ledger state to the forker.
--
-- Note that we require @c@ (from the particular choice of @Ap m l blk c@) so
-- this sometimes can throw ledger errors.
applyThenPush :: (ApplyBlock l blk, MonadBase bm m, c, MonadSTM bm)
              => LedgerCfg l
              -> Ap bm m l blk c
              -> Forker bm l blk
              -> m ()
applyThenPush cfg ap fo =
    liftBase . forkerPush fo . getValidLedgerState =<<
      applyBlock cfg ap fo

-- | Apply and push a sequence of blocks (oldest first).
applyThenPushMany :: (ApplyBlock l blk, MonadBase bm m, c, MonadSTM bm)
                  => (Pushing blk -> m ())
                  -> LedgerCfg l
                  -> [Ap bm m l blk c]
                  -> Forker bm l blk
                  -> m ()
applyThenPushMany trace cfg aps fo = mapM_ pushAndTrace aps
  where
    pushAndTrace ap = do
      trace $ Pushing . toRealPoint $ ap
      applyThenPush cfg ap fo

{-------------------------------------------------------------------------------
  Annotated ledger errors
-------------------------------------------------------------------------------}

class Monad m => ThrowsLedgerError bm m l blk where
  throwLedgerError :: Forker bm l blk -> RealPoint blk -> LedgerErr l -> m a

instance Monad m => ThrowsLedgerError bm (ExceptT (AnnLedgerError bm l blk) m) l blk where
  throwLedgerError f l r = throwError $ AnnLedgerError f l r

defaultThrowLedgerErrors :: ExceptT (AnnLedgerError bm l blk) m a
                         -> m (Either (AnnLedgerError bm l blk) a)
defaultThrowLedgerErrors = runExceptT

defaultResolveWithErrors :: ResolveBlock m blk
                         -> ExceptT (AnnLedgerError bm l blk)
                                    (ReaderT (ResolveBlock m blk) m)
                                    a
                         -> m (Either (AnnLedgerError bm l blk) a)
defaultResolveWithErrors resolve =
      defaultResolveBlocks resolve
    . defaultThrowLedgerErrors

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

-- | Monads in which we can resolve blocks
--
-- To guide type inference, we insist that we must be able to infer the type
-- of the block we are resolving from the type of the monad.
class Monad m => ResolvesBlocks m blk | m -> blk where
  doResolveBlock :: ResolveBlock m blk

instance Monad m => ResolvesBlocks (ReaderT (ResolveBlock m blk) m) blk where
  doResolveBlock r = ReaderT $ \f -> f r

defaultResolveBlocks :: ResolveBlock m blk
                     -> ReaderT (ResolveBlock m blk) m a
                     -> m a
defaultResolveBlocks = flip runReaderT

-- Quite a specific instance so we can satisfy the fundep
instance Monad m
      => ResolvesBlocks (ExceptT e (ReaderT (ResolveBlock m blk) m)) blk where
  doResolveBlock = lift . doResolveBlock
