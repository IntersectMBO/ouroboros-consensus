{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
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

-- | Per-tx local data for an HFC tx is era-tagged: each tx lives in
-- exactly one era and the disk reads happen in that era's @TxOut@ type.
newtype instance TxLocalData (HardForkBlock xs) = HardForkTxLocalData
  { getHardForkTxLocalData :: NS TxLocalData xs
  }
  deriving stock Generic

deriving anyclass instance
  (CanHardFork xs, All (Compose NoThunks TxLocalData) xs) =>
  NoThunks (TxLocalData (HardForkBlock xs))

-- | Mempool accumulator for the HFC. The 'TransitionInfo' is the same
-- one carried by the chain-ticked ledger state; the per-era
-- accumulator is a telescope (past era markers + current era's acc).
data instance MempoolAcc (HardForkBlock xs) = HardForkMempoolAcc
  { hardForkMempoolAccTransition :: !State.TransitionInfo
  , hardForkMempoolAccPerEra :: !(State.HardForkState MempoolAcc xs)
  }
  deriving stock Generic

deriving anyclass instance
  (CanHardFork xs, All (Compose NoThunks MempoolAcc) xs) =>
  NoThunks (MempoolAcc (HardForkBlock xs))

instance
  ( CanHardFork xs
  , All (Compose NoThunks TxLocalData) xs
  , All (Compose NoThunks MempoolAcc) xs
  ) =>
  LedgerSupportsMempool (HardForkBlock xs)
  where
  emptyAcc (TickedHardForkLedgerState transition perEra) =
    HardForkMempoolAcc transition (hcmap proxySingle emptyAcc perEra)

  accTickedState (HardForkMempoolAcc transition perEra) =
    TickedHardForkLedgerState transition (hcmap proxySingle accTickedState perEra)

  prepareTx ::
    forall m.
    Monad m =>
    LedgerConfig (HardForkBlock xs) ->
    SlotNo ->
    TickedStateHandle m (HardForkBlock xs) ->
    MempoolAcc (HardForkBlock xs) ->
    GenTx (HardForkBlock xs) ->
    m (TxLocalData (HardForkBlock xs))
  prepareTx cfg slot ts (HardForkMempoolAcc transition perEraAcc) tx =
    let HardForkLedgerConfig{hardForkLedgerConfigShape, hardForkLedgerConfigPerEra} = cfg
        HardForkTickedStateHandle _ tsPerEra _ = ts
        HardForkGenTx (OneEraGenTx nsTx) = tx

        pcfgs = getPerEraLedgerConfig hardForkLedgerConfigPerEra
        ei =
          State.epochInfoPrecomputedTransitionInfo
            hardForkLedgerConfigShape
            transition
            perEraAcc
        cfgs = hcmap proxySingle (completeLedgerConfig'' ei) pcfgs

        injs :: InPairs (InjectPolyTx GenTx) xs
        injs =
          InPairs.hmap (\(Pair2 injTx _) -> injTx) $
            InPairs.requiringBoth cfgs hardForkInjectTxs
     in case matchPolyTx injs nsTx perEraAcc of
          Left _ ->
            -- Cross-era tx that can't be injected forward. Return a TLD
            -- still positioned at the tx's original era; a subsequent
            -- applyTx/reapplyTx will fail with 'WrongEra' and the tx will
            -- be dropped.
            pure $
              HardForkTxLocalData $
                hcmap proxySingle (const $ error "HFC prepareTx: unreachable position") nsTx
          Right matchedAccTx ->
            -- The matched HardForkState has GenTx product-paired with
            -- MempoolAcc at the acc's current era. Zip in the per-era
            -- TickedStateHandle so we have everything aligned, then
            -- delegate to the per-era prepareTx at the matched position.
            case Match.matchTelescope
              (Telescope.tip $ State.getHardForkState tsPerEra)
              (State.getHardForkState matchedAccTx) of
              Left _ ->
                pure $
                  HardForkTxLocalData $
                    hcmap proxySingle (const $ error "HFC prepareTx: unreachable") nsTx
              Right tele -> do
                let distrib ::
                      forall x.
                      Product (State.Current (TickedStateHandle m)) (State.Current (Product GenTx MempoolAcc)) x ->
                      State.Current (Product (TickedStateHandle m) (Product GenTx MempoolAcc)) x
                    distrib (Pair (State.Current _ tsBlk) (State.Current start p)) =
                      State.Current start (Pair tsBlk p)
                    zipped = State.HardForkState (hmap distrib tele)
                    perEraPrepare ::
                      forall blk.
                      SingleEraBlock blk =>
                      Index xs blk ->
                      WrapLedgerConfig blk ->
                      Product (TickedStateHandle m) (Product GenTx MempoolAcc) blk ->
                      (m :.: TxLocalData) blk
                    perEraPrepare _idx cfg' (Pair tsBlk (Pair gtx accBlk)) =
                      Comp $ prepareTx (unwrapLedgerConfig cfg') slot tsBlk accBlk gtx
                tlds <-
                  hsequence' $
                    hcizipWith proxySingle perEraPrepare cfgs zipped
                pure $ HardForkTxLocalData $ State.tip tlds

  applyTx = applyHelper ModeApply

  reapplyTx cfg slot acc vtx tld =
    fmap snd $
      applyHelper
        ModeReapply
        cfg
        DoNotIntervene
        slot
        acc
        (WrapValidatedGenTx vtx)
        tld

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

  txMeasure
    HardForkLedgerConfig{..}
    (TickedHardForkLedgerState transition hardForkState)
    (HardForkTxLocalData nsTld)
    tx =
      case matchTx injs (unwrapTx tx) hardForkState of
        Left{} -> pure Measure.zero -- safe b/c the tx will be found invalid
        Right matched ->
          case Match.matchTelescope nsTld (State.getHardForkState matched) of
            Left{} -> pure Measure.zero -- as above
            Right tele ->
              let matchedFull ::
                    State.HardForkState
                      (Product TxLocalData (Product GenTx (Ticked LedgerState)))
                      xs
                  matchedFull = State.HardForkState (hmap distrib tele)
                   where
                    distrib ::
                      Product TxLocalData (State.Current (Product GenTx (Ticked LedgerState))) x ->
                      State.Current (Product TxLocalData (Product GenTx (Ticked LedgerState))) x
                    distrib (Pair tld (State.Current start p)) =
                      State.Current start (Pair tld p)
               in hcollapse $
                    hcizipWith proxySingle aux cfgs matchedFull
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
        Product TxLocalData (Product GenTx (Ticked LedgerState)) blk ->
        K (Except (HardForkApplyTxErr xs) (HardForkTxMeasure xs)) blk
      aux idx cfg (Pair tld (Pair tx' st)) =
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
            st
            tld
            tx'

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

-- | A private type used only to clarify the parameterization of 'applyHelper'
data ApplyHelperMode :: (Type -> Type) -> Type where
  ModeApply :: ApplyHelperMode GenTx
  ModeReapply :: ApplyHelperMode WrapValidatedGenTx

-- | A private type used only to clarify the definition of 'applyHelper'
data ApplyResult xs blk = ApplyResult
  { arAcc :: MempoolAcc blk
  , arValidatedTx :: Validated (GenTx (HardForkBlock xs))
  }

-- | The shared logic between 'applyTx' and 'reapplyTx' for 'HardForkBlock'.
--
-- The @txIn@ variable is 'GenTx' or 'WrapValidatedGenTx', respectively. See
-- 'ApplyHelperMode'.
--
-- We use 'matchPolyTx' to forward-inject the input tx across past-era
-- boundaries onto the acc's current era; then 'matchTelescope' to splice
-- the TLD into the matched telescope at the same position. If either
-- alignment fails we throw 'HardForkApplyTxErrWrongEra' — which evicts
-- the tx — rather than panicking.
applyHelper ::
  forall xs txIn.
  CanHardFork xs =>
  ApplyHelperMode txIn ->
  LedgerConfig (HardForkBlock xs) ->
  WhetherToIntervene ->
  SlotNo ->
  MempoolAcc (HardForkBlock xs) ->
  txIn (HardForkBlock xs) ->
  TxLocalData (HardForkBlock xs) ->
  Except
    (HardForkApplyTxErr xs)
    ( Validated (GenTx (HardForkBlock xs))
    , MempoolAcc (HardForkBlock xs)
    )
applyHelper
  mode
  HardForkLedgerConfig{..}
  wti
  slot
  (HardForkMempoolAcc transition perEraAcc)
  tx
  (HardForkTxLocalData nsTld) =
    case matchPolyTx injs (modeGetTx tx) perEraAcc of
      Left mismatch ->
        throwError $
          HardForkApplyTxErrWrongEra . MismatchEraInfo $
            Match.bihcmap proxySingle singleEraInfo ledgerInfo mismatch
      Right matched ->
        case Match.matchTelescope nsTld (State.getHardForkState matched) of
          Left tldMismatch ->
            -- TLD was prepared at a different era than the acc-injected
            -- tx. This is a caller bug (prepareTx must align TLD with
            -- acc's current era), but we surface it as 'WrongEra' rather
            -- than panic so the failing tx is dropped cleanly.
            throwError $
              HardForkApplyTxErrWrongEra . MismatchEraInfo $
                Match.bihcmap proxySingle singleEraInfo ledgerInfo tldMismatch
          Right tele -> do
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
            let matchedFull ::
                  State.HardForkState
                    (Product TxLocalData (Product txIn MempoolAcc))
                    xs
                matchedFull = State.HardForkState (hmap distrib tele)
                 where
                  distrib ::
                    Product TxLocalData (State.Current (Product txIn MempoolAcc)) x ->
                    State.Current (Product TxLocalData (Product txIn MempoolAcc)) x
                  distrib (Pair tld (State.Current start p)) =
                    State.Current start (Pair tld p)
            result <-
              hsequence' $
                hcizipWith proxySingle modeApplyCurrent cfgs matchedFull
            let
              acc' :: State.HardForkState MempoolAcc xs
              acc' = arAcc `hmap` result

              vtx :: Validated (GenTx (HardForkBlock xs))
              vtx = hcollapse $ (K . arValidatedTx) `hmap` result

            return
              ( vtx
              , HardForkMempoolAcc transition acc'
              )
   where
    pcfgs = getPerEraLedgerConfig hardForkLedgerConfigPerEra
    cfgs = hcmap proxySingle (completeLedgerConfig'' ei) pcfgs
    ei =
      State.epochInfoPrecomputedTransitionInfo
        hardForkLedgerConfigShape
        transition
        perEraAcc

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
      Product TxLocalData (Product txIn MempoolAcc) blk ->
      ( Except (HardForkApplyTxErr xs)
          :.: ApplyResult xs
      )
        blk
    modeApplyCurrent index cfg (Pair tld (Pair tx' acc')) =
      Comp $
        withExcept (injectApplyTxErr index) $
          let lcfg = unwrapLedgerConfig cfg
           in case mode of
                ModeApply -> do
                  (vtx, acc'') <- applyTx lcfg wti slot acc' tx' tld
                  pure
                    ApplyResult
                      { arValidatedTx = injectValidatedGenTx index vtx
                      , arAcc = acc''
                      }
                ModeReapply -> do
                  let vtx' = unwrapValidatedGenTx tx'
                  acc'' <- reapplyTx lcfg slot acc' vtx' tld
                  -- provide the given transaction, which was already validated
                  pure
                    ApplyResult
                      { arValidatedTx = injectValidatedGenTx index vtx'
                      , arAcc = acc''
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

injectValidatedGenTx ::
  SListI xs => Index xs blk -> Validated (GenTx blk) -> Validated (GenTx (HardForkBlock xs))
injectValidatedGenTx index =
  HardForkValidatedGenTx
    . OneEraValidatedGenTx
    . injectNS index
    . WrapValidatedGenTx
