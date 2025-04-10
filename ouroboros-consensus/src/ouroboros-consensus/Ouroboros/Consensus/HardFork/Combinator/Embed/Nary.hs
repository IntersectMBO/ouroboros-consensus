{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Ouroboros.Consensus.HardFork.Combinator.Embed.Nary (
    Inject (..)
  , InjectionIndex (InjectionIndex)
  , inject'
    -- * Defaults
  , injectHardForkState
  , injectNestedCtxt_
  , injectQuery
    -- * Initial 'ExtLedgerState'
  , injectInitialExtLedgerState
    -- * Convenience
  , forgetInjectionIndex
  , oracularInjectionIndex
  ) where

import           Data.Bifunctor (first)
import           Data.Coerce (Coercible, coerce)
import           Data.SOP.BasicFunctors
import           Data.SOP.Constraint
import           Data.SOP.Counting (Exactly (..))
import           Data.SOP.Dict (Dict (..))
import           Data.SOP.Functors (Flip (..))
import           Data.SOP.Index
import qualified Data.SOP.InPairs as InPairs
import           Data.SOP.Strict
import qualified Data.SOP.Telescope as Telescope
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.HardFork.Combinator
import qualified Ouroboros.Consensus.HardFork.Combinator.State as State
import qualified Ouroboros.Consensus.HardFork.History as History
import           Ouroboros.Consensus.HeaderValidation (AnnTip, HeaderState (..),
                     genesisHeaderState)
import           Ouroboros.Consensus.Ledger.Basics
import           Ouroboros.Consensus.Ledger.Extended (ExtLedgerState (..))
import           Ouroboros.Consensus.Ledger.Query
import           Ouroboros.Consensus.Ledger.Tables.Utils
import           Ouroboros.Consensus.Storage.Serialisation
import           Ouroboros.Consensus.TypeFamilyWrappers

{-------------------------------------------------------------------------------
  Injection for a single block into a HardForkBlock
-------------------------------------------------------------------------------}

class Inject f where
  inject ::
       forall x xs. (CanHardFork xs, HasCanonicalTxIn xs, HasHardForkTxOut xs)
    => InjectionIndex xs x
    -> f x
    -> f (HardForkBlock xs)

inject' ::
     forall f a b x xs.
     ( Inject f
     , CanHardFork xs
     , HasCanonicalTxIn xs
     , HasHardForkTxOut xs
     , Coercible a (f x)
     , Coercible b (f (HardForkBlock xs))
     )
  => Proxy f -> InjectionIndex xs x -> a -> b
inject' _ iidx = coerce . inject @f iidx . coerce

{-------------------------------------------------------------------------------
  InjectionIndex
-------------------------------------------------------------------------------}

-- | This data type is isomorphic to an 'Index' that additionally provides a
-- 'History.Bound' for every era up to and including that index, but none of
-- the subsequent eras.
newtype InjectionIndex xs x =
    InjectionIndex (Telescope (K History.Bound) (State.Current ((:~:) x)) xs)

-- | Many instances of 'Inject' do not need the 'History.Bound's, eg those that
-- do not construct 'HardForkState's
forgetInjectionIndex :: SListI xs => InjectionIndex xs x -> Index xs x
forgetInjectionIndex (InjectionIndex tele) =
  Index $ hmap State.currentState $ Telescope.tip tele

-- | Build an 'InjectionIndex' from oracular 'History.Bound's and an 'Index'
--
-- This bounds data is oracular, since the later eras in @xs@ might have not
-- yet started. However, it can be known in test code.
--
-- INVARIANT: the result is completely independent of the 'history.Bound's for
-- eras /after/ the given 'Index'.
oracularInjectionIndex ::
     SListI xs
  => Exactly xs History.Bound
  -> Index xs x
  -> InjectionIndex xs x
oracularInjectionIndex (Exactly np) (Index idx) =
  InjectionIndex
    $ Telescope.bihzipWith
      (\x (K ()) -> x)
      (\(K start) Refl -> State.Current { currentStart = start, currentState = Refl })
      np
    $ Telescope.fromTip idx

-- | NOT EXPORTED
--
-- The first bound in a telescope. This is used in an inductive alternative
-- within 'injectHardForkState', since the end of one era is the start of the
-- next.
firstBound :: InjectionIndex xs x -> History.Bound
firstBound (InjectionIndex tele) = case tele of
    TZ State.Current {currentStart = start} -> start
    TS (K start) _tele'                     -> start

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
     forall x xs result fp.
     Index xs x
  -> BlockQuery x fp      result
  -> QueryIfCurrent xs fp result
injectQuery idx q = case idx of
    IZ      -> QZ q
    IS idx' -> QS (injectQuery idx' q)

injectHardForkState ::
     forall f x xs.
     InjectionIndex xs x
  -> f x
  -> HardForkState f xs
injectHardForkState iidx x =
    HardForkState $ go iidx
  where
    go ::
         InjectionIndex xs' x
      -> Telescope (K State.Past) (State.Current f) xs'
    go (InjectionIndex (TZ current@State.Current { currentState = Refl })) =
        TZ
          current { State.currentState = x }
    go (InjectionIndex (TS (K start) tele)) =
        TS
          (K State.Past {
              pastStart = start
            , pastEnd   = firstBound (InjectionIndex tele)
            }
          )
          (go (InjectionIndex tele))

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

instance Inject I where
  inject = injectNS' (Proxy @I) . forgetInjectionIndex

instance Inject (K a) where
  inject _ (K a) = K a

instance Inject Header where
  inject = injectNS' (Proxy @Header) . forgetInjectionIndex

instance Inject SerialisedHeader where
  inject iidx =
        serialisedHeaderFromPair
      . first (mapSomeNestedCtxt (injectNestedCtxt_ idx))
      . serialisedHeaderToPair
    where
      idx = forgetInjectionIndex iidx

instance Inject WrapHeaderHash where
  inject (iidx :: InjectionIndex xs x) =
    case dictIndexAll (Proxy @SingleEraBlock) idx of
      Dict ->
          WrapHeaderHash
        . OneEraHash
        . toShortRawHash (Proxy @x)
        . unwrapHeaderHash
    where
      idx = forgetInjectionIndex iidx

instance Inject GenTx where
  inject = injectNS' (Proxy @GenTx) . forgetInjectionIndex

instance Inject WrapGenTxId where
  inject = injectNS' (Proxy @WrapGenTxId) . forgetInjectionIndex

instance Inject WrapApplyTxErr where
  inject =
      (   (WrapApplyTxErr . HardForkApplyTxErrFromEra)
       .: injectNS' (Proxy @WrapApplyTxErr)
      )
    . forgetInjectionIndex

instance Inject (SomeBlockQuery :.: BlockQuery) where
  inject iidx (Comp (SomeBlockQuery q)) =
      Comp $ SomeBlockQuery (QueryIfCurrent (injectQuery idx q))
    where
      idx = forgetInjectionIndex iidx

instance Inject AnnTip where
  inject =
      (undistribAnnTip .: injectNS' (Proxy @AnnTip)) . forgetInjectionIndex

instance Inject (Flip LedgerState mk) where
  inject iidx =
      Flip . HardForkLedgerState . injectHardForkState iidx

instance Inject WrapChainDepState where
  inject = coerce .: injectHardForkState

instance Inject HeaderState where
  inject iidx HeaderState {..} = HeaderState {
        headerStateTip      = inject iidx <$> headerStateTip
      , headerStateChainDep = unwrapChainDepState
                            $ inject iidx
                            $ WrapChainDepState headerStateChainDep
      }

instance Inject (Flip ExtLedgerState mk) where
  inject iidx (Flip ExtLedgerState {..}) = Flip $ ExtLedgerState {
        ledgerState = unFlip $ inject iidx (Flip ledgerState)
      , headerState = inject iidx headerState
      }

{-------------------------------------------------------------------------------
  Initial ExtLedgerState
-------------------------------------------------------------------------------}

-- | Inject the first era's initial 'ExtLedgerState' and trigger any
-- translations that should take place in the very first slot.
--
-- Performs any hard forks scheduled via 'TriggerHardForkAtEpoch'.
--
-- Note: we can translate across multiple eras when computing the initial ledger
-- state, but we do not support translation across multiple eras in general;
-- extending 'applyChainTick' to translate across more than one era is not
-- problematic, but extending 'ledgerViewForecastAt' is a lot more subtle; see
-- @forecastNotFinal@.
--
-- Note: this function is an /alternative/ to the 'Inject' class above. It does
-- not rely on that class.
injectInitialExtLedgerState ::
     forall x xs. (CanHardFork (x ': xs), HasLedgerTables (LedgerState (HardForkBlock (x : xs))))
  => TopLevelConfig (HardForkBlock (x ': xs))
  -> ExtLedgerState x ValuesMK
  -> ExtLedgerState (HardForkBlock (x ': xs)) ValuesMK
injectInitialExtLedgerState cfg extLedgerState0 =
    ExtLedgerState {
        ledgerState = targetEraLedgerState
      , headerState = targetEraHeaderState
      }
  where
    cfgs :: NP TopLevelConfig (x ': xs)
    cfgs =
        distribTopLevelConfig
          (State.epochInfoLedger
             (configLedger cfg)
             (hardForkLedgerStatePerEra targetEraLedgerState))
          cfg

    targetEraLedgerState :: LedgerState (HardForkBlock (x ': xs)) ValuesMK
    targetEraLedgerState = applyDiffs st st'
      where
        st :: LedgerState (HardForkBlock (x ': xs)) ValuesMK
        st = HardForkLedgerState . initHardForkState . Flip . ledgerState $ extLedgerState0
        st' = HardForkLedgerState
                -- We can immediately extend it to the right slot, executing any
                -- scheduled hard forks in the first slot
                  (State.extendToSlot
                      (configLedger cfg)
                      (SlotNo 0)
                      (initHardForkState $ Flip $ forgetLedgerTables $ ledgerState extLedgerState0))


    firstEraChainDepState :: HardForkChainDepState (x ': xs)
    firstEraChainDepState =
        initHardForkState $
          WrapChainDepState $
            headerStateChainDep $
              headerState extLedgerState0

    targetEraChainDepState :: HardForkChainDepState (x ': xs)
    targetEraChainDepState =
        -- Align the 'ChainDepState' with the ledger state of the target era.
        State.align
          (InPairs.requiringBoth
            (hmap (WrapConsensusConfig . configConsensus) cfgs)
            (translateChainDepState hardForkEraTranslation))
          (hpure (fn_2 (\_ st -> st)))
          (hardForkLedgerStatePerEra targetEraLedgerState)
          firstEraChainDepState

    targetEraHeaderState :: HeaderState (HardForkBlock (x ': xs))
    targetEraHeaderState = genesisHeaderState targetEraChainDepState
