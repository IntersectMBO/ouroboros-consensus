{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Cardano.QueryHF () where

import Data.Coerce
import Data.Functor.Product
import Data.SOP.BasicFunctors
import Data.SOP.Constraint
import Data.SOP.Functors (Flip (..))
import Data.SOP.Index
import Data.SOP.Strict
import qualified Data.SOP.Telescope as Telescope
import Data.Singletons
import Data.Type.Equality ((:~:) (..))
import NoThunks.Class
import Ouroboros.Consensus.Byron.Ledger
import Ouroboros.Consensus.Byron.Node ()
import Ouroboros.Consensus.Cardano.Block
import Ouroboros.Consensus.Cardano.CanHardFork
import Ouroboros.Consensus.Cardano.Ledger
import Ouroboros.Consensus.HardFork.Combinator
import Ouroboros.Consensus.HardFork.Combinator.State.Types (currentState)
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Ledger.Query
import Ouroboros.Consensus.Ledger.Tables
import Ouroboros.Consensus.Shelley.HFEras ()
import Ouroboros.Consensus.Shelley.Ledger
import Ouroboros.Consensus.Shelley.Node ()
import Ouroboros.Consensus.Shelley.Protocol.Praos ()
import Ouroboros.Consensus.Storage.LedgerDB
import Ouroboros.Consensus.TypeFamilyWrappers
import Ouroboros.Consensus.Util.IOLike (MonadSTM (atomically))

-- | Just to have the @x@ as the last type variable
newtype FlipBlockQuery footprint result x
  = FlipBlockQuery (BlockQuery x footprint result)

answerCardanoQueryHF ::
  forall x xs c footprint result m.
  ( xs ~ CardanoEras c
  , CardanoHardForkConstraints c
  , All (Compose NoThunks WrapTxOut) xs
  , SingI footprint
  ) =>
  ( forall blk.
    IsShelleyBlock blk =>
    Index xs blk ->
    ExtLedgerCfg blk ->
    BlockQuery blk footprint result ->
    ReadOnlyForker' m (HardForkBlock xs) ->
    m result
  ) ->
  Index xs x ->
  ExtLedgerCfg x ->
  BlockQuery x footprint result ->
  ReadOnlyForker' m (HardForkBlock xs) ->
  m result
answerCardanoQueryHF f idx cfg q dlv =
  case sing :: Sing footprint of
    SQFNoTables ->
      error "answerCardanoQueryHF: unreachable, this was called with a QFNoTables query"
    _ ->
      hcollapse $
        hap
          ( (Fn $ \(Pair _ (FlipBlockQuery q')) -> case q' of {})
              :* hcmap
                (Proxy @(IsShelleyBlock))
                (\idx' -> Fn $ \(Pair cfg' (FlipBlockQuery q')) -> K $ f (IS idx') cfg' q' dlv)
                indices
          )
          (injectNS idx (Pair cfg (FlipBlockQuery q)))

shelleyCardanoFilter ::
  forall proto era c result.
  ( CardanoHardForkConstraints c
  , ShelleyCompatible proto era
  ) =>
  BlockQuery (ShelleyBlock proto era) QFTraverseTables result ->
  TxOut (LedgerState (HardForkBlock (CardanoEras c))) ->
  Bool
shelleyCardanoFilter q = eliminateCardanoTxOut (\_ -> shelleyQFTraverseTablesPredicate q)

instance CardanoHardForkConstraints c => BlockSupportsHFLedgerQuery (CardanoEras c) where
  answerBlockQueryHFLookup =
    answerCardanoQueryHF
      ( \idx cfg q forker ->
          answerShelleyLookupQueries
            (injectLedgerTables idx)
            (ejectHardForkTxOut idx)
            (coerce . ejectCanonicalTxIn idx)
            (getPerEraShelleyLedgerState idx forker)
            cfg q forker
      )
  answerBlockQueryHFTraverse =
    answerCardanoQueryHF
      ( \idx ->
          answerShelleyTraversingQueries
            (ejectHardForkTxOut idx)
            (coerce . ejectCanonicalTxIn idx)
            (queryLedgerGetTraversingFilter idx)
      )

  queryLedgerGetTraversingFilter idx q = case idx of
    -- Byron
    IZ -> byronCardanoFilter q
    -- Shelley based
    IS IZ -> shelleyCardanoFilter q
    IS (IS IZ) -> shelleyCardanoFilter q
    IS (IS (IS IZ)) -> shelleyCardanoFilter q
    IS (IS (IS (IS IZ))) -> shelleyCardanoFilter q
    IS (IS (IS (IS (IS IZ)))) -> shelleyCardanoFilter q
    IS (IS (IS (IS (IS (IS IZ))))) -> shelleyCardanoFilter q
    IS (IS (IS (IS (IS (IS (IS IZ)))))) -> shelleyCardanoFilter q
    IS (IS (IS (IS (IS (IS (IS (IS idx'))))))) -> case idx' of {}

byronCardanoFilter ::
  BlockQuery ByronBlock QFTraverseTables result ->
  TxOut (LedgerState (HardForkBlock (CardanoEras c))) ->
  Bool
byronCardanoFilter = \case {}

getPerEraShelleyLedgerState ::
  forall blk xs m.
  (MonadSTM m, All SingleEraBlock xs) =>
  Index xs blk ->
  ReadOnlyForker' m (HardForkBlock xs) ->
  m (LedgerState blk EmptyMK)
getPerEraShelleyLedgerState idx forker = do
  ext <- atomically (roforkerGetLedgerState forker)
  let tipNS = Telescope.tip $ getHardForkState $ hardForkLedgerStatePerEra $ ledgerState ext
  pure $ case projectNS idx tipNS of
    Just cur -> unFlip (currentState cur)
    Nothing -> error "getPerEraShelleyLedgerState: era mismatch"

projectNS :: Index xs x -> NS f xs -> Maybe (f x)
projectNS idx ns = go (getIndex idx) ns
  where
    go :: NS ((:~:) x) ys -> NS f ys -> Maybe (f x)
    go (Z Refl) (Z fx) = Just fx
    go (S i)    (S s)  = go i s
    go _        _      = Nothing
