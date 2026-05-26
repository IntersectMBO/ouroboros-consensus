{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | A pure tick-then-apply for 'ExtLedgerState', usable from the
-- 'Test.Ouroboros.Storage.ChainDB.Model' state-machine model.
--
-- The new 'tickThenApply' (in "Ouroboros.Consensus.Ledger.Abstract") is
-- monadic over a 'Handle' and requires 'MonadThrow' on the monad. For
-- the test blocks shipped in this testlib the 'StateHandle' /
-- 'TickedStateHandle' / 'LedgerTablesHandle' machinery is purely
-- structural (newtype wrappers, '()'), so we can run 'tickThenApply' at
-- @m = 'IOSim' s@ — a 'MonadThrow' instance that never actually throws
-- for these blocks — via 'runSim', and unwrap the result back to a pure
-- 'ExtLedgerState'.
module Test.Util.PureApplyBlock
  ( PureExtApplyBlock (..)
  , pureExtTickThenApply
  ) where

import Control.Monad.Except (Except, throwError)
import qualified Control.Monad.Except as Except
import Control.Monad.IOSim (IOSim, runSim)
import Data.Typeable (Typeable)
import Ouroboros.Consensus.Ledger.Abstract
  ( BlockSupportsLedgerHD
  , ComputeLedgerEvents (OmitLedgerEvents)
  , tickThenApply
  )
import Ouroboros.Consensus.Ledger.Extended
  ( ExtLedgerCfg
  , ExtLedgerState (..)
  , ExtStateHandle (..)
  , ExtValidationError
  , extLedgerState
  )
import Ouroboros.Consensus.Ledger.SupportsProtocol (LedgerSupportsProtocol)
import qualified Test.Ouroboros.Storage.TestBlock as Storage
import Test.Util.TestBlock
  ( PayloadSemantics
  , StateHandle (TestStateHandle)
  , TestBlockWith
  )

-- | Blocks whose 'StateHandle' can be constructed from a pure
-- 'LedgerState' at @m = IOSim s@ for any @s@. The default function
-- 'pureExtTickThenApply' uses this to run the monadic 'tickThenApply'
-- in pure code via 'runSim'.
class
  ( LedgerSupportsProtocol blk
  , forall s. BlockSupportsLedgerHD (IOSim s) blk
  ) =>
  PureExtApplyBlock blk
  where
  wrapExtStateHandle ::
    ExtLedgerState blk -> ExtStateHandle (IOSim s) blk

-- | Pure analogue of 'tickThenApply' on 'ExtLedgerState'. Routes through
-- the monadic 'tickThenApply' at @m = IOSim s@; for trivial-handle
-- blocks the simulation is fully deterministic and never blocks, so
-- 'runSim' always succeeds with a 'Right'.
pureExtTickThenApply ::
  forall blk.
  PureExtApplyBlock blk =>
  ExtLedgerCfg blk ->
  blk ->
  ExtLedgerState blk ->
  Except (ExtValidationError blk) (ExtLedgerState blk)
pureExtTickThenApply cfg blk extSt =
  case runSim go of
    Left simErr ->
      error $
        "Test.Util.PureApplyBlock.pureExtTickThenApply: IOSim failure on a "
          <> "trivial-handle block (impossible): "
          <> show simErr
    Right (Left validationErr) -> throwError validationErr
    Right (Right ext') -> pure ext'
 where
  go ::
    forall s.
    IOSim s (Either (ExtValidationError blk) (ExtLedgerState blk))
  go = do
    let handle :: ExtStateHandle (IOSim s) blk
        handle = wrapExtStateHandle extSt
    result <-
      Except.runExceptT
        (tickThenApply OmitLedgerEvents cfg blk handle)
    pure (fmap extLedgerState result)

{-------------------------------------------------------------------------------
  Instances for the consensus 'Test.Util.TestBlock'
-------------------------------------------------------------------------------}

instance
  (PayloadSemantics ptype, Typeable ptype) =>
  PureExtApplyBlock (TestBlockWith ptype)
  where
  wrapExtStateHandle ExtLedgerState{ledgerState, headerState} =
    ExtStateHandle (TestStateHandle ledgerState) headerState

{-------------------------------------------------------------------------------
  Instances for the storage 'Test.Ouroboros.Storage.TestBlock'
-------------------------------------------------------------------------------}

instance PureExtApplyBlock Storage.TestBlock where
  wrapExtStateHandle ExtLedgerState{ledgerState, headerState} =
    ExtStateHandle (Storage.TestStateHandle ledgerState) headerState
