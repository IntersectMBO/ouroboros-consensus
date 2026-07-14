{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Cardano.QueryHF () where

import Data.Functor.Product
import Data.SOP.BasicFunctors
import Data.SOP.Constraint
import Data.SOP.Index
import Data.SOP.Strict
import Data.Singletons
import NoThunks.Class
import Ouroboros.Consensus.Byron.Ledger ()
import Ouroboros.Consensus.Byron.Node ()
import Ouroboros.Consensus.Cardano.Block
import Ouroboros.Consensus.Cardano.CanHardFork
import Ouroboros.Consensus.HardFork.Combinator
import Ouroboros.Consensus.Ledger.HD (Values)
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Ledger.Query
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

-- | Project the current era's values out of the hard-fork @NS@. The forker read
-- returns values tagged with the era we injected the keys at (the current era),
-- so the requested arm is guaranteed present.
projectShelleyValues ::
  forall c blk.
  Index (CardanoEras c) blk ->
  Values (HardForkBlock (CardanoEras c)) ->
  Values blk
projectShelleyValues idx0 =
  maybe (error "answerCardanoQueryHF: values in unexpected era") unwrapValues
    . go idx0
 where
  go :: Index xs x -> NS WrapValues xs -> Maybe (WrapValues x)
  go IZ (Z x) = Just x
  go (IS idx) (S ns) = go idx ns
  go _ _ = Nothing

instance CardanoHardForkConstraints c => BlockSupportsHFLedgerQuery (CardanoEras c) where
  answerBlockQueryHFLookup idx cfg q forker =
    answerCardanoQueryHF
      ( \idx' cfg' q' forker' ->
          answerShelleyLookupQueries
            (injectNS idx' . WrapKeys)
            (projectShelleyValues idx')
            cfg'
            q'
            forker'
      )
      idx
      cfg
      q
      forker

  -- The traverse loop reads through the era-level range reader obtained from the
  -- provider, so the forker itself is unused here. The filter is era-typed: the
  -- per-era predicate applied to the current era's @TxOut@.
  answerBlockQueryHFTraverse idx cfg q provider forker =
    answerCardanoQueryHF
      ( \idx' cfg' q' _forker' ->
          answerShelleyTraversingQueries
            (projectShelleyValues idx')
            shelleyQFTraverseTablesPredicate
            cfg'
            q'
            provider
      )
      idx
      cfg
      q
      forker
