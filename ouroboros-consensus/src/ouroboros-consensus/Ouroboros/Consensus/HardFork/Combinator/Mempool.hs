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
import Data.ByteString.Short (ShortByteString)
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
import Ouroboros.Network.Tx (HasRawTxId (..))

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
instance
  CanHardFork xs =>
  LedgerSupportsMempool (HardForkBlock xs)
  where
  applyTx = applyHelper ModeApply

  reapplyTx cfg slot vtx vals tls =
    (\(st', diff, _vtx) -> (st', diff))
      <$> applyHelper
        ModeReapply
        cfg
        DoNotIntervene
        slot
        (WrapValidatedGenTx vtx)
        vals
        tls

  -- We use the default 'reapplyTxs' (a fold of 'reapplyTx', forwarding the
  -- values through each tx's diff). Because @Values (HardForkBlock xs)@ is an
  -- era-tagged @NS@, the per-era dispatch already happens inside 'reapplyTx' and
  -- 'forward', so there is no benefit to overriding 'reapplyTxs' to project the
  -- whole batch from the hard-fork layer into the current era once.

  txForgetValidated =
    HardForkGenTx
      . OneEraGenTx
      . hcmap proxySingle (txForgetValidated . unwrapValidatedGenTx)
      . getOneEraValidatedGenTx
      . getHardForkValidatedGenTx

  getTransactionKeySets (HardForkGenTx (OneEraGenTx ns)) =
    hcmap proxySingle (\tx -> WrapKeys (getTransactionKeySets tx)) ns

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
  type TxMeasurePhase1 (HardForkBlock xs) = HardForkTxMeasurePhase1 xs
  type TxMeasurePhase2 (HardForkBlock xs) = HardForkTxMeasurePhase2 xs

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
        K (TxMeasure (HardForkBlock xs)) blk
      aux idx pcfg st' =
        K $
          let TxMeasure p1 p2 =
                blockCapacityTxMeasure
                  (completeLedgerConfig' ei pcfg)
                  st'
           in TxMeasure
                (hardForkInjTxMeasurePhase1 . injectNS idx $ WrapTxMeasurePhase1 p1)
                (hardForkInjTxMeasurePhase2 . injectNS idx $ WrapTxMeasurePhase2 p2)

  txMeasurePhase1
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
        K (Except (HardForkApplyTxErr xs) (HardForkTxMeasurePhase1 xs)) blk
      aux idx cfg (Pair tx' st') =
        K
          $ mapExcept
            ( ( HardForkApplyTxErrFromEra
                  . OneEraApplyTxErr
                  . injectNS idx
                  . WrapApplyTxErr
              )
                +++ (hardForkInjTxMeasurePhase1 . injectNS idx . WrapTxMeasurePhase1)
            )
          $ txMeasurePhase1
            (unwrapLedgerConfig cfg)
            st'
            tx'

  txMeasurePhase2
    HardForkLedgerConfig{..}
    values
    (TickedHardForkLedgerState transition hardForkState)
    tx =
      case matchTx injs (unwrapTx tx) hardForkState of
        Left{} -> pure Measure.zero -- safe b/c the tx will be found invalid
        Right pair -> case State.match values pair of
          -- Should this mismatch, it will fail the same way when applying the
          -- transaction, so it will be rejected.
          Left{} -> pure Measure.zero
          Right pair' -> hcollapse $ hcizipWith proxySingle aux cfgs pair'
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
        Product WrapValues (Product GenTx (Ticked LedgerState)) blk ->
        K (Except (HardForkApplyTxErr xs) (HardForkTxMeasurePhase2 xs)) blk
      aux idx cfg (Pair (WrapValues vals) (Pair tx' st')) =
        K
          $ mapExcept
            ( ( HardForkApplyTxErrFromEra
                  . OneEraApplyTxErr
                  . injectNS idx
                  . WrapApplyTxErr
              )
                +++ (hardForkInjTxMeasurePhase2 . injectNS idx . WrapTxMeasurePhase2)
            )
          $ txMeasurePhase2
            (unwrapLedgerConfig cfg)
            vals
            st'
            tx'

-- | A private type used only to clarify the parameterization of 'applyHelper'
data ApplyHelperMode :: (Type -> Type) -> Type where
  ModeApply :: ApplyHelperMode GenTx
  ModeReapply :: ApplyHelperMode WrapValidatedGenTx

-- | A private type used only to clarify the definition of 'applyHelper'
data ApplyResult xs blk = ApplyResult
  { arState :: Ticked LedgerState blk
  , arDiff :: TxsDiff blk
  , arValidatedTx :: Validated (GenTx (HardForkBlock xs))
  }

-- | The shared logic between 'applyTx' and 'reapplyTx' for 'HardForkBlock'
--
-- The @txIn@ variable is 'GenTx' or 'WrapValidatedGenTx', respectively. See
-- 'ApplyHelperMode'.
applyHelper ::
  forall xs txIn.
  CanHardFork xs =>
  ApplyHelperMode txIn ->
  LedgerConfig (HardForkBlock xs) ->
  WhetherToIntervene ->
  SlotNo ->
  txIn (HardForkBlock xs) ->
  Values (HardForkBlock xs) ->
  TickedLedgerState (HardForkBlock xs) ->
  Except
    (HardForkApplyTxErr xs)
    ( TickedLedgerState (HardForkBlock xs)
    , TxsDiff (HardForkBlock xs)
    , Validated (GenTx (HardForkBlock xs))
    )
applyHelper
  mode
  HardForkLedgerConfig{..}
  wti
  slot
  tx
  values
  (TickedHardForkLedgerState transition hardForkState) =
    case matchPolyTx injs (modeGetTx tx) hardForkState of
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
        --
        -- The read values are at the current era (the same as the matched
        -- ticked state), so attaching them never mismatches.
        case State.match values matched of
          Left _ ->
            error "applyHelper: values were not read for the current era"
          Right matched' -> do
            result <-
              hsequence' $
                hcizipWith proxySingle modeApplyCurrent cfgs matched'
            let st' :: State.HardForkState (Ticked LedgerState) xs
                st' = arState `hmap` result

                diffs :: TxsDiff (HardForkBlock xs)
                diffs = TxsDiff $ State.tip $ (WrapDiff . unTxsDiff . arDiff) `hmap` result

                vtx :: Validated (GenTx (HardForkBlock xs))
                vtx = hcollapse $ (K . arValidatedTx) `hmap` result

            return (TickedHardForkLedgerState transition st', diffs, vtx)
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
      Product WrapValues (Product txIn (Ticked LedgerState)) blk ->
      ( Except (HardForkApplyTxErr xs)
          :.: ApplyResult xs
      )
        blk
    modeApplyCurrent index cfg (Pair (WrapValues vals) (Pair tx' st)) =
      Comp $
        withExcept (injectApplyTxErr index) $
          do
            let lcfg = unwrapLedgerConfig cfg
            case mode of
              ModeApply -> do
                (st', diff, vtx) <- applyTx lcfg wti slot tx' vals st
                pure
                  ApplyResult
                    { arValidatedTx = injectValidatedGenTx index vtx
                    , arDiff = diff
                    , arState = st'
                    }
              ModeReapply -> do
                let vtx' = unwrapValidatedGenTx tx'
                (st', diff) <- reapplyTx lcfg slot vtx' vals st
                -- provide the given transaction, which was already validated
                pure
                  ApplyResult
                    { arValidatedTx = injectValidatedGenTx index vtx'
                    , arDiff = diff
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

instance CanHardFork xs => HasRawTxId (TxId (GenTx (HardForkBlock xs))) where
  type RawTxId (TxId (GenTx (HardForkBlock xs))) = ShortByteString
  getRawTxId = oneEraGenTxIdRawHash . getHardForkGenTxId

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
  forall blk.
  SingleEraBlock blk =>
  State.Current (Ticked LedgerState) blk -> LedgerEraInfo blk
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
