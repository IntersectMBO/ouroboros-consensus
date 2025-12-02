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
import Data.SOP.Index
import Data.SOP.Strict
import Data.Singletons
import NoThunks.Class
import Ouroboros.Consensus.Byron.Ledger
import Ouroboros.Consensus.Byron.Node ()
import Ouroboros.Consensus.Cardano.Block
import Ouroboros.Consensus.Cardano.CanHardFork
import Ouroboros.Consensus.Cardano.Ledger
import Ouroboros.Consensus.HardFork.Combinator
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Ledger.Query
import Ouroboros.Consensus.Ledger.Tables
import Ouroboros.Consensus.Shelley.HFEras ()
import Ouroboros.Consensus.Shelley.Ledger
import Ouroboros.Consensus.Shelley.Node ()
import Ouroboros.Consensus.Shelley.Protocol.Praos ()
import Ouroboros.Consensus.Storage.LedgerDB
import Ouroboros.Consensus.TypeFamilyWrappers

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
  TxOut (HardForkBlock (CardanoEras c)) ->
  Bool
shelleyCardanoFilter q = eliminateCardanoTxOut (\_ -> shelleyQFTraverseTablesPredicate q)

instance CardanoHardForkConstraints c => BlockSupportsHFLedgerQuery (CardanoEras c) where
  answerBlockQueryHFLookup =
    answerCardanoQueryHF
      ( \idx ->
          answerShelleyLookupQueries
            (undefined) -- injectLedgerTables idx)
            (ejectHardForkTxOut idx)
      )
  answerBlockQueryHFTraverse =
    answerCardanoQueryHF
      ( \idx ->
          answerShelleyTraversingQueries
            (ejectHardForkTxOut idx)
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
  TxOut (HardForkBlock (CardanoEras c)) ->
  Bool
byronCardanoFilter = \case {}
