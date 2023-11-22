{-# LANGUAGE DeriveAnyClass           #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE QuantifiedConstraints    #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UndecidableInstances     #-}

-- | Pieces needed for any LedgerDB implementation
module Ouroboros.Consensus.Storage.LedgerDB.Common (
    -- * Configuration
    LedgerDbCfg (..)
  , configLedgerDb
    -- * Ledger errors
  , AnnLedgerError (..)
  , AnnLedgerError'
  , ThrowsLedgerError (..)
  , defaultResolveWithErrors
  , defaultThrowLedgerErrors
    -- * Finding blocks
  , ResolveBlock
  , ResolvesBlocks (..)
  , defaultResolveBlocks
    -- * Applying blocks
  , Ap (..)
  , ExceededRollback (..)
  , ValidLedgerState (..)
  , toRealPoint
    -- * Tracing
  , PushGoal (..)
  , PushStart (..)
  , Pushing (..)
  , UpdateLedgerDbTraceEvent (..)
  ) where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Kind
import           Data.Word
import           GHC.Generics (Generic)
import           NoThunks.Class
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Basics
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Protocol.Abstract

{-------------------------------------------------------------------------------
 Configuration
-------------------------------------------------------------------------------}

data LedgerDbCfg l = LedgerDbCfg {
      ledgerDbCfgSecParam :: !SecurityParam
    , ledgerDbCfg         :: !(LedgerCfg l)
    }
  deriving (Generic)

deriving instance NoThunks (LedgerCfg l) => NoThunks (LedgerDbCfg l)

configLedgerDb ::
     ConsensusProtocol (BlockProtocol blk)
  => TopLevelConfig blk
  -> LedgerDbCfg (ExtLedgerState blk)
configLedgerDb config = LedgerDbCfg {
      ledgerDbCfgSecParam    = configSecurityParam config
    , ledgerDbCfg            = ExtLedgerCfg config
    }

{-------------------------------------------------------------------------------
  A ledger error annotated with the block
-------------------------------------------------------------------------------}

-- | Annotated ledger errors
data AnnLedgerError ldb l blk = AnnLedgerError {
      annLedgerState  :: ldb l
      -- | Reference to the block that had the error
    , annLedgerErrRef :: RealPoint blk

      -- | The ledger error itself
    , annLedgerErr    :: LedgerErr l
    }

class Monad m => ThrowsLedgerError m ldb l blk where
  throwLedgerError :: ldb l -> RealPoint blk -> LedgerErr l -> m a

instance Monad m
      => ThrowsLedgerError (ExceptT (AnnLedgerError ldb l blk) m) ldb l blk where
  throwLedgerError ldb r e = throwError $ AnnLedgerError ldb r e

type AnnLedgerError' ldb blk = AnnLedgerError ldb (ExtLedgerState blk) blk

defaultThrowLedgerErrors :: ExceptT (AnnLedgerError ldb l blk) m a
                         -> m (Either (AnnLedgerError ldb l blk) a)
defaultThrowLedgerErrors = runExceptT

defaultResolveWithErrors :: ResolveBlock m blk
                         -> ExceptT (AnnLedgerError ldb l blk)
                                    (ReaderT (ResolveBlock m blk) m)
                                    a
                         -> m (Either (AnnLedgerError ldb l blk) a)
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

{-------------------------------------------------------------------------------
  Applying blocks
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
type Ap :: (LedgerStateKind -> Type) -> (Type -> Type) -> LedgerStateKind -> Type -> Constraint -> Type
data Ap ldb m l blk c where
  ReapplyVal ::           blk -> Ap ldb m l blk ()
  ApplyVal   ::           blk -> Ap ldb m l blk ( ThrowsLedgerError m ldb l blk )
  ReapplyRef :: RealPoint blk -> Ap ldb m l blk ( ResolvesBlocks    m   blk )
  ApplyRef   :: RealPoint blk -> Ap ldb m l blk ( ResolvesBlocks    m   blk
                                                , ThrowsLedgerError m ldb l blk )

  -- | 'Weaken' increases the constraint on the monad @m@.
  --
  -- This is primarily useful when combining multiple 'Ap's in a single
  -- homogeneous structure.
  Weaken :: (c' => c) => Ap ldb m l blk c -> Ap ldb m l blk c'

toRealPoint :: HasHeader blk => Ap ldb m l blk c -> RealPoint blk
toRealPoint (ReapplyVal blk) = blockRealPoint blk
toRealPoint (ApplyVal blk)   = blockRealPoint blk
toRealPoint (ReapplyRef rp)  = rp
toRealPoint (ApplyRef rp)    = rp
toRealPoint (Weaken ap)      = toRealPoint ap

-- | Exceeded maximum rollback supported by the current ledger DB state
--
-- Under normal circumstances this will not arise. It can really only happen
-- in the presence of data corruption (or when switching to a shorter fork,
-- but that is disallowed by all currently known Ouroboros protocols).
--
-- Records both the supported and the requested rollback.
data ExceededRollback = ExceededRollback {
      rollbackMaximum   :: Word64
    , rollbackRequested :: Word64
    }

{-------------------------------------------------------------------------------
  Trace events
-------------------------------------------------------------------------------}

newtype PushStart blk = PushStart { unPushStart :: RealPoint blk }
  deriving (Show, Eq)

newtype PushGoal blk = PushGoal { unPushGoal :: RealPoint blk }
  deriving (Show, Eq)

newtype Pushing blk = Pushing { unPushing :: RealPoint blk }
  deriving (Show, Eq)

data UpdateLedgerDbTraceEvent blk =
    -- | Event fired when we are about to push a block to the LedgerDB
      StartedPushingBlockToTheLedgerDb
        !(PushStart blk)
        -- ^ Point from which we started pushing new blocks
        (PushGoal blk)
        -- ^ Point to which we are updating the ledger, the last event
        -- StartedPushingBlockToTheLedgerDb will have Pushing and PushGoal
        -- wrapping over the same RealPoint
        !(Pushing blk)
        -- ^ Point which block we are about to push
  deriving (Show, Eq, Generic)
