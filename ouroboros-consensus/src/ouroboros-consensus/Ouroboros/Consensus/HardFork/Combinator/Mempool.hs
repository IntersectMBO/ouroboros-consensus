{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
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
import Data.Bifunctor
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
import qualified Data.SOP.Telescope as Telescope
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

instance
  CanHardFork xs =>
  LedgerSupportsMempool (HardForkBlock xs)
  where
  data MempoolCache (HardForkBlock xs) = HardForkMempoolCache (NS MempoolCache xs)

  emptyMempoolCache (HardForkTickedStateHandle _ (State.HardForkState st)) =
    HardForkMempoolCache
      (hcmap proxySingle (emptyMempoolCache . State.currentState) $ Telescope.tip st)

  applyTx = withExceptT fst .....: applyHelper ModeApply

  reapplyTx cfg slot vtx tls mch =
    (\(a, b, _) -> (a, b))
      <$> applyHelper
        ModeReapply
        cfg
        DoNotIntervene
        slot
        (WrapValidatedGenTx vtx)
        tls
        mch

  txForgetValidated =
    HardForkGenTx
      . OneEraGenTx
      . hcmap proxySingle (txForgetValidated . unwrapValidatedGenTx)
      . getOneEraValidatedGenTx
      . getHardForkValidatedGenTx

  mkMempoolApplyTxError (TickedHardForkLedgerState _transition hardForkState) txt =
    hcollapse $ hcimap proxySingle f hardForkState
   where
    f ::
      SingleEraBlock x =>
      Index xs x ->
      Ticked LedgerState x ->
      K (Maybe (ApplyTxErr (HardForkBlock xs))) x
    f idx tlst =
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

  blockCapacityTxMeasure
    HardForkLedgerConfig{..}
    (TickedHardForkLedgerState transition hardForkState) =
      hcollapse $
        hcizipWith proxySingle aux pcfgs hardForkState
     where
      pcfgs = getPerEraLedgerConfig hardForkLedgerConfigPerEra
      ei =
        State.epochInfoPrecomputedTransitionInfo
          hardForkLedgerConfigShape
          transition
          hardForkState

      aux ::
        SingleEraBlock blk =>
        Index xs blk ->
        WrapPartialLedgerConfig blk ->
        Ticked LedgerState blk ->
        K (HardForkTxMeasure xs) blk
      aux idx pcfg st' =
        K $
          hardForkInjTxMeasure . injectNS idx . WrapTxMeasure $
            blockCapacityTxMeasure
              (completeLedgerConfig' ei pcfg)
              st'

  txMeasure
    HardForkLedgerConfig{..}
    (TickedHardForkLedgerState transition hardForkState)
    tx =
      case matchTx injs (unwrapTx tx) hardForkState of
        Left{} -> pure Measure.zero -- safe b/c the tx will be found invalid
        Right pair -> hcollapse $ hcizipWith proxySingle aux cfgs pair
     where
      pcfgs = getPerEraLedgerConfig hardForkLedgerConfigPerEra
      ei =
        State.epochInfoPrecomputedTransitionInfo
          hardForkLedgerConfigShape
          transition
          hardForkState
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
        (Product GenTx (Ticked LedgerState)) blk ->
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
            st'
            tx'

-- | A private type used only to clarify the parameterization of 'applyHelper'
data ApplyHelperMode :: (Type -> Type) -> Type where
  ModeApply :: ApplyHelperMode GenTx
  ModeReapply :: ApplyHelperMode WrapValidatedGenTx

-- | A private type used only to clarify the definition of 'applyHelper'
data ApplyResult m xs blk = ApplyResult
  { arState :: TickedStateHandle m blk
  , arCache :: MempoolCache blk
  , arValidatedTx :: Validated (GenTx (HardForkBlock xs))
  }

-- | The shared logic between 'applyTx' and 'reapplyTx' for 'HardForkBlock'
--
-- The @txIn@ variable is 'GenTx' or 'WrapValidatedGenTx', respectively. See
-- 'ApplyHelperMode'.
applyHelper ::
  forall xs m txIn.
  (CanHardFork xs, Monad m) =>
  ApplyHelperMode txIn ->
  LedgerConfig (HardForkBlock xs) ->
  WhetherToIntervene ->
  SlotNo ->
  txIn (HardForkBlock xs) ->
  MempoolCache (HardForkBlock xs) ->
  TickedStateHandle m (HardForkBlock xs) ->
  ExceptT
    (HardForkApplyTxErr xs, MempoolCache (HardForkBlock xs))
    m
    ( TickedStateHandle m (HardForkBlock xs)
    , MempoolCache (HardForkBlock xs)
    , Validated (GenTx (HardForkBlock xs))
    )
applyHelper
  mode
  HardForkLedgerConfig{..}
  wti
  slot
  tx
  cache@(HardForkMempoolCache hardForkCache)
  (HardForkTickedStateHandle transition hardForkState) =
    case matchPolyTx injs (modeGetTx tx) hardForkState of
      Left mismatch ->
        throwError $
          ( HardForkApplyTxErrWrongEra . MismatchEraInfo $
              Match.bihcmap proxySingle singleEraInfo ledgerInfo mismatch
          , cache
          )
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
          let matched' = case Match.matchTelescope hardForkCache (State.getHardForkState matched) of
                Left _ ->
                  hcmap
                    proxySingle
                    (\(Pair tx0 st) -> Pair (emptyMempoolCache st) (Pair tx0 st))
                    matched
                Right m ->
                  State.HardForkState $
                    hcmap proxySingle (\(Pair a (State.Current s t)) -> State.Current s (Pair a t)) m
          result <-
            hsequence' $
              hcizipWith proxySingle modeApplyCurrent cfgs matched'
          let
            st' :: NS MempoolCache xs
            st' = hmap State.currentState $ Telescope.tip $ State.getHardForkState $ arCache `hmap` result

            vtx :: Validated (GenTx (HardForkBlock xs))
            vtx = hcollapse $ (K . arValidatedTx) `hmap` result

          return
            ( HardForkTickedStateHandle transition (arState `hmap` result)
            , HardForkMempoolCache st'
            , vtx
            )
   where
    pcfgs = getPerEraLedgerConfig hardForkLedgerConfigPerEra
    cfgs = hcmap proxySingle (completeLedgerConfig'' ei) pcfgs
    ei =
      State.epochInfoPrecomputedTransitionInfo
        hardForkLedgerConfigShape
        transition
        hardForkState

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
      ModeReapply ->
        getOneEraValidatedGenTx
          . getHardForkValidatedGenTx
          . unwrapValidatedGenTx

    modeGetInjection ::
      forall blk1 blk2.
      Product2 InjectTx InjectValidatedTx blk1 blk2 ->
      InjectPolyTx txIn blk1 blk2
    modeGetInjection (Pair2 injTx injValidatedTx) = case mode of
      ModeApply -> injTx
      ModeReapply -> injValidatedTx

    modeApplyCurrent ::
      forall blk.
      SingleEraBlock blk =>
      Index xs blk ->
      WrapLedgerConfig blk ->
      Product MempoolCache (Product txIn (TickedStateHandle m)) blk ->
      ( ExceptT (HardForkApplyTxErr xs, MempoolCache (HardForkBlock xs)) m
          :.: ApplyResult m xs
      )
        blk
    modeApplyCurrent index cfg (Pair mcache (Pair tx' st)) =
      Comp $ ExceptT $ do
        let lcfg = unwrapLedgerConfig cfg
        res <- case mode of
          ModeApply ->
            fmap
              ( bimap (,mcache) $
                  \(st', mcache', vtx) ->
                    ApplyResult
                      { arValidatedTx = injectValidatedGenTx index vtx
                      , arState = st'
                      , arCache = mcache'
                      }
              )
              (runExceptT (applyTx lcfg wti slot tx' mcache st))
          ModeReapply ->
            let vtx' = unwrapValidatedGenTx tx'
             in fmap
                  ( fmap $ \(st', mcache') ->
                      ApplyResult
                        { arValidatedTx = injectValidatedGenTx index vtx'
                        , arState = st'
                        , arCache = mcache'
                        }
                  )
                  (runExceptT (reapplyTx lcfg slot vtx' mcache st))
        pure $ case res of
          Left (e, c) -> Left (injectApplyTxErr index e, injectCache index c)
          Right r -> Right r

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

ledgerInfo ::
  forall blk f.
  SingleEraBlock blk =>
  f blk -> LedgerEraInfo blk
ledgerInfo _ = LedgerEraInfo $ singleEraInfo (Proxy @blk)

injectApplyTxErr :: SListI xs => Index xs blk -> ApplyTxErr blk -> HardForkApplyTxErr xs
injectApplyTxErr index =
  HardForkApplyTxErrFromEra
    . OneEraApplyTxErr
    . injectNS index
    . WrapApplyTxErr

injectCache :: SListI xs => Index xs blk -> MempoolCache blk -> MempoolCache (HardForkBlock xs)
injectCache index =
  HardForkMempoolCache
    . injectNS index

injectValidatedGenTx ::
  SListI xs => Index xs blk -> Validated (GenTx blk) -> Validated (GenTx (HardForkBlock xs))
injectValidatedGenTx index =
  HardForkValidatedGenTx
    . OneEraValidatedGenTx
    . injectNS index
    . WrapValidatedGenTx
