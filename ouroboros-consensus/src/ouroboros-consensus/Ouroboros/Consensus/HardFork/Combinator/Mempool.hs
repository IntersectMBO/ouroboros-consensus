{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.HardFork.Combinator.Mempool
  ( GenTx (..)
  , HardForkApplyTxErr (..)
  , TxId (..)
  , Validated (..)
  , hardForkApplyTxErrFromEither
  , hardForkApplyTxErrToEither
  ) where

import Control.Arrow ((+++))
import Control.Monad.Except
import Data.Functor.Identity
import Data.Functor.Product
import Data.Kind (Type)
import qualified Data.Measure as Measure
import Data.SOP.BasicFunctors
import Data.SOP.Constraint
import Data.SOP.Functors
import Data.SOP.InPairs (InPairs)
import qualified Data.SOP.InPairs as InPairs
import Data.SOP.Index
import qualified Data.SOP.Match as Match
import Data.SOP.Strict
import qualified Data.SOP.Telescope as Tele
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.HardFork.Combinator.Abstract
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras
import Ouroboros.Consensus.HardFork.Combinator.Basics
import Ouroboros.Consensus.HardFork.Combinator.Info
import Ouroboros.Consensus.HardFork.Combinator.InjectTxs
import Ouroboros.Consensus.HardFork.Combinator.Ledger
import Ouroboros.Consensus.HardFork.Combinator.PartialConfig
import qualified Ouroboros.Consensus.HardFork.Combinator.State as State
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.SupportsMempool
import Ouroboros.Consensus.TypeFamilyWrappers
import Ouroboros.Consensus.Util

data HardForkApplyTxErr xs
  = -- | Validation error from one of the eras
    HardForkApplyTxErrFromEra !(OneEraApplyTxErr xs)
  | -- | We tried to apply a block from the wrong era
    HardForkApplyTxErrWrongEra !(MismatchEraInfo xs)
  deriving Generic

instance Typeable xs => ShowProxy (HardForkApplyTxErr xs)

hardForkApplyTxErrToEither ::
  HardForkApplyTxErr xs ->
  Either (MismatchEraInfo xs) (OneEraApplyTxErr xs)
hardForkApplyTxErrToEither (HardForkApplyTxErrFromEra err) = Right err
hardForkApplyTxErrToEither (HardForkApplyTxErrWrongEra err) = Left err

hardForkApplyTxErrFromEither ::
  Either (MismatchEraInfo xs) (OneEraApplyTxErr xs) ->
  HardForkApplyTxErr xs
hardForkApplyTxErrFromEither (Right err) = HardForkApplyTxErrFromEra err
hardForkApplyTxErrFromEither (Left err) = HardForkApplyTxErrWrongEra err

deriving stock instance CanHardFork xs => Show (HardForkApplyTxErr xs)

deriving stock instance CanHardFork xs => Eq (HardForkApplyTxErr xs)

newtype instance GenTx (HardForkBlock xs) = HardForkGenTx
  { getHardForkGenTx :: OneEraGenTx xs
  }
  deriving (Eq, Generic, Show)
  deriving anyclass NoThunks

newtype instance Validated (GenTx (HardForkBlock xs)) = HardForkValidatedGenTx
  { getHardForkValidatedGenTx :: OneEraValidatedGenTx xs
  }
  deriving (Eq, Generic, Show)
  deriving anyclass NoThunks

instance Typeable xs => ShowProxy (GenTx (HardForkBlock xs))

type instance ApplyTxErr (HardForkBlock xs) = HardForkApplyTxErr xs

-- | Just to discharge cognitive load, this is equivalent to:
--
-- > ([invalidTxs, ...], [validTxs, ...], st)
--
-- Where @invalidTxs@ and @validTxs@ are hard-fork transactions, and only @st@
-- depends on a particular @blk@.
--
-- We do not define this as a new data type to reuse the @Applicative@ and
-- friends instances of these type constructors, which are useful to
-- @hsequence'@ a @HardForkState@ of this.
--
-- This is also isomorphic to
-- @'Ouroboros.Consensus.Ledger.SupportsMempool.ReapplyTxsResult' (HardForkBlock xs)@
type DecomposedReapplyTxsResult l extra xs wtd =
  (,,)
    [Invalidated (HardForkBlock xs)]
    [ ( Validated (GenTx (HardForkBlock xs))
      , InputTxDiffs (HardForkBlock xs) wtd
      , extra
      )
    ]
    :.: (Flip l EmptyMK)

instance
  ( CanHardFork xs
  , HasCanonicalTxIn xs
  , HasHardForkTxOut xs
  ) =>
  LedgerSupportsMempool (HardForkBlock xs)
  where
  applyTx = applyHelper ModeApply

  reapplyTxBoth mode cfg slot vtx tls =
    fst
      <$> applyHelper
        (ModeReapply mode slot)
        cfg
        DoNotIntervene
        (WrapValidatedGenTx vtx)
        tls

  reapplyTxsBoth ::
    forall l wtd extra.
    ReapplyMode l ->
    LedgerConfig (HardForkBlock xs) ->
    SlotNo ->
    [ ( Validated (GenTx (HardForkBlock xs))
      , InputTxDiffs (HardForkBlock xs) wtd
      , extra
      )
    ] ->
    l (HardForkBlock xs) ValuesMK ->
    ReapplyTxsResult l extra (HardForkBlock xs) wtd
  reapplyTxsBoth
    mode
    hcfg@HardForkLedgerConfig{..}
    slot
    vtxs
    hardForkState =
      ( \(err, val, st) ->
          ReapplyTxsResult (mismatched' ++ err) val $ case (mode, hardForkState) of
            (ReapplyLedgerState, _) -> HardForkLedgerState st
            (ReapplyTickedLedgerState, CompAp (TickedHardForkLedgerState transition _)) ->
              CompAp $
                TickedHardForkLedgerState transition $
                  hmap (FlipTickedLedgerState . unCompAp . unFlip) st
      )
        $ hsequence'
        $ hcizipWith
          proxySingle
          modeApplyCurrent
          cfgs
          (State.HardForkState $ hmap flipCurrentAndProduct matched)
     where
      pcfgs = getPerEraLedgerConfig hardForkLedgerConfigPerEra
      cfgs = hcmap proxySingle (completeLedgerConfig'' ei) pcfgs
      ei = case (mode, hardForkState) of
        (ReapplyLedgerState, HardForkLedgerState s) -> State.epochInfoLedger hcfg s
        (ReapplyTickedLedgerState, CompAp (TickedHardForkLedgerState transition s)) ->
          State.epochInfoPrecomputedTransitionInfo
            hardForkLedgerConfigShape
            transition
            s

      -- Transactions are unwrapped into the particular era transactions.
      (mismatched, matched) =
        matchPolyTxsTele
          -- How to translate txs to later eras
          ( InPairs.hmap
              (\(Pair2 _ (InjectPolyTx w)) -> InjectPolyTx (\(Comp (ex, df, tx)) -> Comp . (ex,df,) <$> w tx))
              (InPairs.requiringBoth cfgs hardForkInjectTxs)
          )
          ( case mode of
              ReapplyLedgerState -> State.getHardForkState $ hardForkLedgerStatePerEra hardForkState
              ReapplyTickedLedgerState ->
                hmap (\(State.Current a b) -> State.Current a . Flip . CompAp . getFlipTickedLedgerState $ b) $
                  State.getHardForkState $
                    tickedHardForkLedgerStatePerEra $
                      unCompAp hardForkState
          )
          [ hmap (Comp . (extra,df,)) . getOneEraValidatedGenTx . getHardForkValidatedGenTx $ tx
          | (tx, df, extra) <- vtxs
          ]

      mismatched' :: [Invalidated (HardForkBlock xs)]
      mismatched' =
        [ flip
            Invalidated
            ( HardForkApplyTxErrWrongEra $
                MismatchEraInfo $
                  Match.bihcmap proxySingle singleEraInfo (ledgerInfo @(Flip l ValuesMK)) y
            )
            . HardForkValidatedGenTx
            . OneEraValidatedGenTx
            . hmap (thd . unComp)
            $ x
        | (x, y) <- mismatched
        ]

      thd (_, _, x) = x

      flipCurrentAndProduct (Pair (State.Current c s) b) = State.Current c (Pair s b)

      modeApplyCurrent ::
        forall blk.
        SingleEraBlock blk =>
        Index xs blk ->
        WrapLedgerConfig blk ->
        Product
          (Flip l ValuesMK)
          ( []
              :.: (,,) extra (InputTxDiffs (HardForkBlock xs) wtd)
              :.: WrapValidatedGenTx
          )
          blk ->
        DecomposedReapplyTxsResult l extra xs wtd blk
      modeApplyCurrent index cfg (Pair (Flip st) txs) =
        let ReapplyTxsResult err val st' =
              reapplyTxsBoth @blk @l @Discard
                mode
                (unwrapLedgerConfig cfg)
                slot
                [(unwrapValidatedGenTx tx, (), (df, tk)) | (Comp (tk, df, tx)) <- unComp txs]
                st
         in Comp
              ( [ injectValidatedGenTx index (getInvalidated x) `Invalidated` injectApplyTxErr index (getReason x)
                | x <- err
                ]
              , [ (HardForkValidatedGenTx . OneEraValidatedGenTx . injectNS index . WrapValidatedGenTx $ x, z1, z2)
                | (x, (), (z1, z2)) <- val
                ]
              , Flip st'
              )

  txForgetValidated =
    HardForkGenTx
      . OneEraGenTx
      . hcmap proxySingle (txForgetValidated . unwrapValidatedGenTx)
      . getOneEraValidatedGenTx
      . getHardForkValidatedGenTx

  getTransactionKeySets (HardForkGenTx (OneEraGenTx ns)) =
    hcollapse $
      hcimap proxySingle f ns
   where
    f ::
      SingleEraBlock x =>
      Index xs x ->
      GenTx x ->
      K (LedgerTables (LedgerState (HardForkBlock xs)) KeysMK) x
    f idx tx = K $ injectLedgerTables idx $ getTransactionKeySets tx

  -- This optimization is worthwile because we can save the projection and
  -- injection of ledger tables.
  --
  -- These operations are used when adding new transactions to the mempool,
  -- which is _not_ in the critical path for the forging loop but still will
  -- make adoption of new transactions faster. As adding a transaction takes a
  -- TMVar, it is interesting to hold it for as short of a time as possible.
  prependMempoolDiffs
    (HardForkLedgerState (State.HardForkState st1))
    (HardForkLedgerState (State.HardForkState st2)) =
      HardForkLedgerState $
        State.HardForkState $
          runIdentity
            ( Tele.alignExtend
                ( InPairs.hpure
                    (error "When prepending mempool diffs we used to un-aligned states, this should be impossible!")
                )
                ( hcpure proxySingle $ fn_2 $ \(State.Current _ a) (State.Current start b) ->
                    State.Current start $
                      Flip $
                        prependMempoolDiffs
                          (unFlip a)
                          (unFlip b)
                )
                st1
                st2
            )

  -- This optimization is worthwile because we can save the projection and
  -- injection of ledger tables.
  --
  -- These operations are used when adding new transactions to the mempool,
  -- which is _not_ in the critical path for the forging loop but still will
  -- make adoption of new transactions faster. As adding a transaction takes a
  -- TMVar, it is interesting to hold it for as short of a time as possible.
  applyMempoolDiffsMode mode vals keys st = case (mode, st) of
    (ReapplyLedgerState, HardForkLedgerState (State.HardForkState st')) ->
      HardForkLedgerState $
        State.HardForkState $
          hcimap
            proxySingle
            ( \idx (State.Current start (Flip a)) ->
                State.Current start $
                  Flip $
                    applyMempoolDiffsMode
                      mode
                      (ejectLedgerTables idx vals)
                      (ejectLedgerTables idx keys)
                      a
            )
            st'
    (ReapplyTickedLedgerState, CompAp (TickedHardForkLedgerState transition (State.HardForkState st'))) ->
      CompAp $
        TickedHardForkLedgerState transition $
          State.HardForkState $
            hcimap
              proxySingle
              ( \idx (State.Current start (FlipTickedLedgerState a)) ->
                  State.Current start $
                    FlipTickedLedgerState $
                      unCompAp $
                        applyMempoolDiffsMode
                          mode
                          (ejectLedgerTables idx vals)
                          (ejectLedgerTables idx keys)
                          (CompAp a)
              )
              st'

  mkMempoolApplyTxError (HardForkLedgerState hardForkState) txt =
    hcollapse $ hcimap proxySingle f hardForkState
   where
    f ::
      SingleEraBlock x =>
      Index xs x ->
      Flip LedgerState mk x ->
      K (Maybe (ApplyTxErr (HardForkBlock xs))) x
    f idx (Flip tlst) =
      K $ injectApplyTxErr idx <$> mkMempoolApplyTxError tlst txt

instance CanHardFork xs => TxLimits (HardForkBlock xs) where
  type TxMeasure (HardForkBlock xs) = HardForkTxMeasure xs

  txWireSize =
    \tx ->
      let tx' :: NS GenTx xs
          tx' = getOneEraGenTx (getHardForkGenTx tx)
          -- HFC overhead
          -- note that HFC might be disabled, then this gives an upperbound.
          overhead
            | nsToIndex tx' <= 23 = 2
            | otherwise = 3
       in (+ overhead)
            . hcollapse
            . hcmap proxySingle (K . txWireSize)
            $ tx'

  blockCapacityTxMeasure ::
    forall l mk.
    ReapplyMode l ->
    LedgerConfig (HardForkBlock xs) ->
    l (HardForkBlock xs) mk ->
    TxMeasure (HardForkBlock xs)
  blockCapacityTxMeasure mode cfg@HardForkLedgerConfig{..} hardForkState =
    hcollapse $
      hcizipWith
        proxySingle
        aux
        pcfgs
        ( case mode of
            ReapplyLedgerState -> hardForkLedgerStatePerEra hardForkState
            ReapplyTickedLedgerState ->
              hmap (Flip . CompAp . getFlipTickedLedgerState) $
                tickedHardForkLedgerStatePerEra $
                  unCompAp hardForkState
        )
   where
    pcfgs = getPerEraLedgerConfig hardForkLedgerConfigPerEra
    ei = case (mode, hardForkState) of
      (ReapplyLedgerState, HardForkLedgerState s) -> State.epochInfoLedger cfg s
      (ReapplyTickedLedgerState, CompAp (TickedHardForkLedgerState transition s)) ->
        State.epochInfoPrecomputedTransitionInfo
          hardForkLedgerConfigShape
          transition
          s

    aux ::
      SingleEraBlock blk =>
      Index xs blk ->
      WrapPartialLedgerConfig blk ->
      Flip l mk blk ->
      K (HardForkTxMeasure xs) blk
    aux idx pcfg st' =
      K $
        hardForkInjTxMeasure . injectNS idx . WrapTxMeasure $
          blockCapacityTxMeasure
            mode
            (completeLedgerConfig' ei pcfg)
            (unFlip st')

  txMeasure
    hcfg@HardForkLedgerConfig{..}
    (HardForkLedgerState hardForkState)
    tx =
      case matchTx injs (unwrapTx tx) hardForkState of
        Left{} -> pure Measure.zero -- safe b/c the tx will be found invalid
        Right pair -> hcollapse $ hcizipWith proxySingle aux cfgs pair
     where
      pcfgs = getPerEraLedgerConfig hardForkLedgerConfigPerEra
      ei = State.epochInfoLedger hcfg hardForkState
      cfgs = hcmap proxySingle (completeLedgerConfig'' ei) pcfgs

      unwrapTx = getOneEraGenTx . getHardForkGenTx

      injs :: InPairs (InjectPolyTx GenTx) xs
      injs =
        InPairs.hmap (\(Pair2 injTx _injValidatedTx) -> injTx) $
          InPairs.requiringBoth cfgs hardForkInjectTxs

      aux ::
        forall blk.
        SingleEraBlock blk =>
        Index xs blk ->
        WrapLedgerConfig blk ->
        (Product GenTx (Flip LedgerState ValuesMK)) blk ->
        K (Except (HardForkApplyTxErr xs) (HardForkTxMeasure xs)) blk
      aux idx cfg (Pair tx' st') =
        K
          $ mapExcept
            ( ( HardForkApplyTxErrFromEra
                  . OneEraApplyTxErr
                  . injectNS idx
                  . WrapApplyTxErr
              )
                +++ (hardForkInjTxMeasure . injectNS idx . WrapTxMeasure)
            )
          $ txMeasure
            (unwrapLedgerConfig cfg)
            (unFlip st')
            tx'

-- | A private type used only to clarify the parameterization of 'applyHelper'
data ApplyHelperMode :: (Type -> (Type -> Type -> Type) -> Type) -> (Type -> Type) -> Type where
  ModeApply :: ApplyHelperMode LedgerState GenTx
  ModeReapply :: ReapplyMode l -> SlotNo -> ApplyHelperMode l WrapValidatedGenTx

-- | 'applyHelper' has to return one of these, depending on the apply mode used.
type family ApplyMK k where
  ApplyMK (ApplyHelperMode l GenTx) = DiffMK
  ApplyMK (ApplyHelperMode l WrapValidatedGenTx) = ValuesMK

-- | A private type used only to clarify the definition of 'applyHelper'
data ApplyResult xs l txIn blk = ApplyResult
  { arState :: l blk (ApplyMK (ApplyHelperMode l txIn))
  , arValidatedTx :: Validated (GenTx (HardForkBlock xs))
  }

-- | The shared logic between 'applyTx' and 'reapplyTx' for 'HardForkBlock'
--
-- The @txIn@ variable is 'GenTx' or 'WrapValidatedGenTx', respectively. See
-- 'ApplyHelperMode'.
applyHelper ::
  forall xs l txIn.
  CanHardFork xs =>
  ApplyHelperMode l txIn ->
  LedgerConfig (HardForkBlock xs) ->
  WhetherToIntervene ->
  txIn (HardForkBlock xs) ->
  l (HardForkBlock xs) ValuesMK ->
  Except
    (HardForkApplyTxErr xs)
    ( l (HardForkBlock xs) (ApplyMK (ApplyHelperMode l txIn))
    , Validated (GenTx (HardForkBlock xs))
    )
applyHelper
  mode
  hcfg@HardForkLedgerConfig{..}
  wti
  tx
  hardForkState =
    case matchPolyTx
      injs
      (modeGetTx tx)
      ( case (mode, hardForkState) of
          (ModeApply, HardForkLedgerState st) -> st
          (ModeReapply ReapplyLedgerState _, HardForkLedgerState st) -> st
          (ModeReapply ReapplyTickedLedgerState _, CompAp (TickedHardForkLedgerState _ st)) -> hmap (Flip . CompAp . getFlipTickedLedgerState) st
      ) of
      Left mismatch ->
        throwError $
          HardForkApplyTxErrWrongEra . MismatchEraInfo $
            Match.bihcmap proxySingle singleEraInfo ledgerInfo mismatch
      Right matched ->
        -- We are updating the ticked ledger state by applying a transaction,
        -- but for the HFC that ledger state contains a bundled
        -- 'TransitionInfo'. We don't change that 'TransitionInfo' here, which
        -- requires justification. Three cases:
        --
        -- o 'TransitionUnknown'. Transitions become known only when the
        --    transaction that confirms them becomes stable, so this cannot
        --    happen simply by applying a transaction. In this case we record
        --    the tip of the ledger, which is also not changed halfway a block.
        -- o 'TransitionKnown'. In this case, we record the 'EpochNo' of the
        --    epoch that starts the new era; this information similarly won't
        --    halfway a block (it can only change, in fact, when we do transition
        --    to that new era).
        -- o 'TransitionImpossible'. Two subcases: we are in the final era (in
        --    which we will remain to be) or we are forecasting, which is not
        --    applicable here.
        do
          result <-
            hsequence' $
              hcizipWith proxySingle modeApplyCurrent cfgs matched
          let _ = result :: State.HardForkState (ApplyResult xs l txIn) xs

              vtx :: Validated (GenTx (HardForkBlock xs))
              vtx = hcollapse $ (K . arValidatedTx) `hmap` result

          let
            ret :: l (HardForkBlock xs) (ApplyMK (ApplyHelperMode l txIn))
            ret = case (mode, hardForkState) of
              (ModeApply, _) -> HardForkLedgerState $ (Flip . arState) `hmap` result
              (ModeReapply ReapplyLedgerState _, _) -> HardForkLedgerState $ (Flip . arState) `hmap` result
              (ModeReapply ReapplyTickedLedgerState _, CompAp (TickedHardForkLedgerState transition _)) ->
                CompAp $
                  TickedHardForkLedgerState transition $
                    (FlipTickedLedgerState . unCompAp . arState) `hmap` result
          return
            (ret, vtx)
   where
    pcfgs = getPerEraLedgerConfig hardForkLedgerConfigPerEra
    cfgs = hcmap proxySingle (completeLedgerConfig'' ei) pcfgs
    ei = case (mode, hardForkState) of
      (ModeApply, HardForkLedgerState s) -> State.epochInfoLedger hcfg s
      (ModeReapply ReapplyLedgerState _, HardForkLedgerState s) -> State.epochInfoLedger hcfg s
      (ModeReapply ReapplyTickedLedgerState _, CompAp (TickedHardForkLedgerState transition s)) ->
        State.epochInfoPrecomputedTransitionInfo
          hardForkLedgerConfigShape
          transition
          s

    injs :: InPairs (InjectPolyTx txIn) xs
    injs =
      InPairs.hmap
        modeGetInjection
        (InPairs.requiringBoth cfgs hardForkInjectTxs)

    modeGetTx :: txIn (HardForkBlock xs) -> NS txIn xs
    modeGetTx = case mode of
      ModeApply ->
        getOneEraGenTx
          . getHardForkGenTx
      ModeReapply{} ->
        getOneEraValidatedGenTx
          . getHardForkValidatedGenTx
          . unwrapValidatedGenTx

    modeGetInjection ::
      forall blk1 blk2.
      Product2 InjectTx InjectValidatedTx blk1 blk2 ->
      InjectPolyTx txIn blk1 blk2
    modeGetInjection (Pair2 injTx injValidatedTx) = case mode of
      ModeApply -> injTx
      ModeReapply{} -> injValidatedTx

    modeApplyCurrent ::
      forall blk.
      SingleEraBlock blk =>
      Index xs blk ->
      WrapLedgerConfig blk ->
      Product txIn (Flip l ValuesMK) blk ->
      ( Except (HardForkApplyTxErr xs)
          :.: ApplyResult xs l txIn
      )
        blk
    modeApplyCurrent index cfg (Pair tx' (Flip st)) =
      Comp $
        withExcept (injectApplyTxErr index) $
          do
            let lcfg = unwrapLedgerConfig cfg
            case mode of
              ModeApply -> do
                (st', vtx) <- applyTx lcfg wti tx' st
                pure
                  ApplyResult
                    { arValidatedTx = injectValidatedGenTx index vtx
                    , arState = st'
                    }
              ModeReapply mode' slot -> do
                let vtx' = unwrapValidatedGenTx tx'
                st' <- reapplyTxBoth mode' lcfg slot vtx' st
                -- provide the given transaction, which was already validated
                pure
                  ApplyResult
                    { arValidatedTx = injectValidatedGenTx index vtx'
                    , arState = st'
                    }

newtype instance TxId (GenTx (HardForkBlock xs)) = HardForkGenTxId
  { getHardForkGenTxId :: OneEraGenTxId xs
  }
  deriving (Eq, Generic, Ord, Show)
  deriving anyclass NoThunks

instance Typeable xs => ShowProxy (TxId (GenTx (HardForkBlock xs)))

instance CanHardFork xs => HasTxId (GenTx (HardForkBlock xs)) where
  txId =
    HardForkGenTxId
      . OneEraGenTxId
      . hcmap proxySingle (WrapGenTxId . txId)
      . getOneEraGenTx
      . getHardForkGenTx

{-------------------------------------------------------------------------------
  HasTxs

  This is not required by consensus itself, but is required by RunNode.
-------------------------------------------------------------------------------}

instance All HasTxs xs => HasTxs (HardForkBlock xs) where
  extractTxs =
    hcollapse
      . hcimap (Proxy @HasTxs) aux
      . getOneEraBlock
      . getHardForkBlock
   where
    aux ::
      HasTxs blk =>
      Index xs blk ->
      I blk ->
      K [GenTx (HardForkBlock xs)] blk
    aux index = K . map (injectNS' (Proxy @GenTx) index) . extractTxs . unI

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- TODO @js remove
ledgerInfo ::
  forall f blk.
  SingleEraBlock blk =>
  State.Current f blk -> LedgerEraInfo blk
ledgerInfo _ = LedgerEraInfo $ singleEraInfo (Proxy @blk)

injectApplyTxErr :: SListI xs => Index xs blk -> ApplyTxErr blk -> HardForkApplyTxErr xs
injectApplyTxErr index =
  HardForkApplyTxErrFromEra
    . OneEraApplyTxErr
    . injectNS index
    . WrapApplyTxErr

injectValidatedGenTx ::
  SListI xs => Index xs blk -> Validated (GenTx blk) -> Validated (GenTx (HardForkBlock xs))
injectValidatedGenTx index =
  HardForkValidatedGenTx
    . OneEraValidatedGenTx
    . injectNS index
    . WrapValidatedGenTx
