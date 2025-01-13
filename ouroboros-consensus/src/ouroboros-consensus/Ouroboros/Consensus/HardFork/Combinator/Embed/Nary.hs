{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.HardFork.Combinator.Embed.Nary (
    Inject (..)
  , inject'
    -- * Defaults
  , injectHardForkState
  , injectNestedCtxt_
  , injectQuery
    -- * Initial 'ExtLedgerState'
  , Translate
  , injectInitialExtLedgerState
  , mkInitialStateViaTranslation
  ) where

import           Data.Bifunctor (first)
import           Data.Coerce (Coercible, coerce)
import           Data.SOP.BasicFunctors
import           Data.SOP.Counting (Exactly (..))
import           Data.SOP.Dict (Dict (..))
import           Data.SOP.Index
import           Data.SOP.Strict
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.State
import qualified Ouroboros.Consensus.HardFork.Combinator.State as State
import qualified Ouroboros.Consensus.HardFork.History as History
import           Ouroboros.Consensus.HeaderValidation (AnnTip, HeaderState (..))
import           Ouroboros.Consensus.Ledger.Extended (ExtLedgerState (..))
import           Ouroboros.Consensus.Storage.Serialisation
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Consensus.Util ((.:))

{-------------------------------------------------------------------------------
  Injection for a single block into a HardForkBlock
-------------------------------------------------------------------------------}

class Inject f where
  inject ::
       forall x xs. CanHardFork xs
    => Exactly xs History.Bound
       -- ^ Start bound of each era
    -> Index xs x
    -> f x
    -> f (HardForkBlock xs)

inject' ::
     forall f a b x xs.
     ( Inject f
     , CanHardFork xs
     , Coercible a (f x)
     , Coercible b (f (HardForkBlock xs))
     )
  => Proxy f -> Exactly xs History.Bound -> Index xs x -> a -> b
inject' _ startBounds idx = coerce . inject @f startBounds idx . coerce

{-------------------------------------------------------------------------------
  Defaults (to ease implementation)
-------------------------------------------------------------------------------}

injectNestedCtxt_ ::
     forall f x xs a.
     Index xs x
  -> NestedCtxt_ x f a
  -> NestedCtxt_ (HardForkBlock xs) f a
injectNestedCtxt_ idx nc = case idx of
    IZ      -> NCZ nc
    IS idx' -> NCS (injectNestedCtxt_ idx' nc)

injectQuery ::
     forall x xs result.
     Index xs x
  -> BlockQuery x result
  -> QueryIfCurrent xs result
injectQuery idx q = case idx of
    IZ      -> QZ q
    IS idx' -> QS (injectQuery idx' q)

injectHardForkState ::
     forall f x xs.
     Exactly xs History.Bound
     -- ^ Start bound of each era
  -> Index xs x
  -> f x
  -> HardForkState f xs
injectHardForkState startBounds idx x =
    HardForkState $ go startBounds idx
  where
    go ::
         Exactly xs' History.Bound
      -> Index xs' x
      -> Telescope (K State.Past) (State.Current f) xs'
    go (ExactlyCons start _) IZ =
        TZ (State.Current { currentStart = start, currentState = x })
    go (ExactlyCons start startBounds'@(ExactlyCons nextStart _)) (IS idx') =
        TS (K State.Past { pastStart = start, pastEnd = nextStart })
           (go startBounds' idx')
    go (ExactlyCons _ ExactlyNil) (IS idx') = case idx' of {}
    go ExactlyNil idx' = case idx' of {}

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

instance Inject I where
  inject _ = injectNS' (Proxy @I)

instance Inject Header where
  inject _ = injectNS' (Proxy @Header)

instance Inject SerialisedHeader where
  inject _ idx =
        serialisedHeaderFromPair
      . first (mapSomeNestedCtxt (injectNestedCtxt_ idx))
      . serialisedHeaderToPair

instance Inject WrapHeaderHash where
  inject _ (idx :: Index xs x) =
    case dictIndexAll (Proxy @SingleEraBlock) idx of
      Dict ->
          WrapHeaderHash
        . OneEraHash
        . toShortRawHash (Proxy @x)
        . unwrapHeaderHash

instance Inject GenTx where
  inject _ = injectNS' (Proxy @GenTx)

instance Inject WrapGenTxId where
  inject _ = injectNS' (Proxy @WrapGenTxId)

instance Inject WrapApplyTxErr where
  inject _ =
      (WrapApplyTxErr . HardForkApplyTxErrFromEra)
        .: injectNS' (Proxy @WrapApplyTxErr)

instance Inject (SomeSecond BlockQuery) where
  inject _ idx (SomeSecond q) = SomeSecond (QueryIfCurrent (injectQuery idx q))

instance Inject AnnTip where
  inject _ = undistribAnnTip .: injectNS' (Proxy @AnnTip)

instance Inject LedgerState where
  inject startBounds idx =
      HardForkLedgerState . injectHardForkState startBounds idx

instance Inject WrapChainDepState where
  inject startBounds idx =
      coerce . injectHardForkState startBounds idx

instance Inject HeaderState where
  inject startBounds idx HeaderState {..} = HeaderState {
        headerStateTip      = inject startBounds idx <$> headerStateTip
      , headerStateChainDep = unwrapChainDepState
                            $ inject startBounds idx
                            $ WrapChainDepState headerStateChainDep
      }

instance Inject ExtLedgerState where
  inject startBounds idx ExtLedgerState {..} = ExtLedgerState {
        ledgerState = inject startBounds idx ledgerState
      , headerState = inject startBounds idx headerState
      }

{-------------------------------------------------------------------------------
  Initial ExtLedgerState
-------------------------------------------------------------------------------}

-- | Inject the genesis 'ExtLedgerState' of some era into an 'ExtLedgerState'
-- for a 'HardForkBlock'. All eras before @x@ (if any) are empty.
injectInitialExtLedgerState ::
     forall xs. CanHardFork xs
  => NS ExtLedgerState xs
  -> ExtLedgerState (HardForkBlock xs)
injectInitialExtLedgerState =
      hcollapse
    . himap (K .: inject initBounds)
  where
    initBounds :: Exactly xs History.Bound
    initBounds = Exactly $ hpure $ K History.initBound

type Translate f = CrossEra f Proxy f

-- | Translate the given @f x@ until it has the same index as the n-ary sum. The
-- translations happen at 'History.initBound'.
mkInitialStateViaTranslation ::
     InPairs (Translate f) (x : xs)
  -> f x
  -> NS g (x : xs)
  -> NS f (x : xs)
mkInitialStateViaTranslation = go
  where
    go ::
         InPairs (Translate f) (x : xs)
      -> f x
      -> NS g (x : xs)
      -> NS f (x : xs)
    go _            fx Z{}     = Z fx
    go (PCons t ts) fx (S gxs) = S $ go ts fx' gxs
      where
        fx' = crossEra t (Current History.initBound Proxy) fx
