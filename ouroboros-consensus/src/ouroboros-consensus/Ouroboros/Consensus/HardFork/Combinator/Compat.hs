{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Ouroboros.Consensus.HardFork.Combinator.Compat (
    HardForkCompatQuery (..)
    -- * Convenience constructors
  , compatGetEraStart
  , compatGetInterpreter
  , compatIfCurrent
    -- * Wrappers
  , forwardCompatQuery
  , singleEraCompatQuery
  ) where

import           Data.Kind (Type)
import           Data.SOP.BasicFunctors
import           Data.SOP.NonEmpty
import           Data.SOP.Strict
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.HardFork.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.Abstract.SingleEraBlock
import           Ouroboros.Consensus.HardFork.Combinator.Basics
import           Ouroboros.Consensus.HardFork.Combinator.Ledger.Query
import qualified Ouroboros.Consensus.HardFork.History.Qry as Qry
import           Ouroboros.Consensus.HardFork.History.Summary (Bound, Summary,
                     initBound, neverForksSummary)
import           Ouroboros.Consensus.Ledger.Query

{-------------------------------------------------------------------------------
  Query language
-------------------------------------------------------------------------------}

-- | Version of @Query (HardForkBlock xs)@ without the restriction to have
-- at least two eras
type HardForkCompatQuery :: Type -> QueryFootprint -> Type -> Type
data HardForkCompatQuery blk fp result where
  CompatIfCurrent ::
       BlockQuery blk fp result
    -> HardForkCompatQuery blk fp result

  CompatAnytime ::
       QueryAnytime result
    -> EraIndex (HardForkIndices blk)
    -> HardForkCompatQuery blk QFNoTables result

  CompatHardFork ::
       QueryHardFork (HardForkIndices blk) result
    -> HardForkCompatQuery blk QFNoTables result

{-------------------------------------------------------------------------------
  Convenience constructors for 'HardForkCompatQuery'
-------------------------------------------------------------------------------}

-- | Submit query to underlying ledger
compatIfCurrent ::
     BlockQuery fp blk result
  -> HardForkCompatQuery fp blk result
compatIfCurrent = CompatIfCurrent

-- | Get the start of the specified era, if known
compatGetEraStart ::
     EraIndex (HardForkIndices blk)
  -> HardForkCompatQuery blk QFNoTables (Maybe Bound)
compatGetEraStart = CompatAnytime GetEraStart

-- | Get an interpreter for history queries
--
-- I.e., this can be used for slot/epoch/time conversions.
compatGetInterpreter ::
    HardForkCompatQuery blk QFNoTables (Qry.Interpreter (HardForkIndices blk))
compatGetInterpreter = CompatHardFork GetInterpreter

{-------------------------------------------------------------------------------
  Wrappers
-------------------------------------------------------------------------------}

-- | Wrapper used when connecting to a server that's running the HFC with
-- at least two eras
forwardCompatQuery ::
       forall m x xs fp. IsNonEmpty xs
    => (forall result. BlockQuery (HardForkBlock (x ': xs)) fp  result -> m result)
    -- ^ Submit a query through the LocalStateQuery protocol.
    -> (forall result. HardForkCompatQuery (HardForkBlock (x ': xs)) fp result -> m result)
forwardCompatQuery f = go
  where
    go :: HardForkCompatQuery (HardForkBlock (x ': xs)) fp result -> m result
    go (CompatIfCurrent qry)    = f qry
    go (CompatAnytime   qry ix) = f (QueryAnytime qry ix)
    go (CompatHardFork  qry)    = f (QueryHardFork qry)

-- | Wrapper used when connecting to a server that's not using the HFC, or
-- is using the HFC but with a single era only.
singleEraCompatQuery ::
       forall m blk era fp. (Monad m, HardForkIndices blk ~ '[era])
    => EpochSize
    -> SlotLength
    -> GenesisWindow
    -> (forall result. BlockQuery blk fp result -> m result)
    -- ^ Submit a query through the LocalStateQuery protocol.
    -> (forall result. HardForkCompatQuery blk fp result -> m result)
singleEraCompatQuery epochSize slotLen genesisWindow f = go
  where
    go :: HardForkCompatQuery blk fp result -> m result
    go (CompatIfCurrent qry)    = f qry
    go (CompatAnytime   qry ix) = const (goAnytime qry) (trivialIndex ix)
    go (CompatHardFork  qry)    = goHardFork qry

    goAnytime :: QueryAnytime result -> m result
    goAnytime GetEraStart = return $ Just initBound

    goHardFork :: QueryHardFork '[era] result -> m result
    goHardFork GetInterpreter = return $ Qry.mkInterpreter summary
    goHardFork GetCurrentEra  = return eraIndexZero

    summary :: Summary '[era]
    summary = neverForksSummary epochSize slotLen genesisWindow

    trivialIndex :: EraIndex '[era] -> ()
    trivialIndex (EraIndex (Z (K ()))) = ()
