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
import qualified Data.Foldable as Foldable
import Data.Functor.Identity
import Data.Functor.Product
import qualified Data.Measure as Measure
import Data.SOP.BasicFunctors
import Data.SOP.Constraint
import qualified Data.SOP.InPairs as InPairs
import Data.SOP.Index
import Data.SOP.Strict
import qualified Data.SOP.Telescope as Tele
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.HardFork.Combinator.Abstract
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras
import Ouroboros.Consensus.HardFork.Combinator.Basics
import Ouroboros.Consensus.HardFork.Combinator.InjectTxs
import Ouroboros.Consensus.HardFork.Combinator.Ledger
import Ouroboros.Consensus.HardFork.Combinator.PartialConfig
import qualified Ouroboros.Consensus.HardFork.Combinator.State as State
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.SupportsMempool
import Ouroboros.Consensus.Ledger.Tables.Utils (applyDiffs, forgetLedgerTables)
import Ouroboros.Consensus.TypeFamilyWrappers
import Ouroboros.Consensus.Util
import Ouroboros.Network.Tx (HasRawTxId (..))

data HardForkApplyTxErr xs
  = -- | Validation error from one of the eras
    HardForkApplyTxErrFromEra !(OneEraApplyTxErr xs)
  | -- | We tried to apply a transaction from the wrong era
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
type DecomposedReapplyTxsResult extra xs wtd =
  (,,)
    [Invalidated (HardForkBlock xs)]
    [ ( Validated (GenTx (HardForkBlock xs))
      , InputTxDiffs (HardForkBlock xs) wtd
      , extra
      )
    ]
    :.: FlipTickedLedgerState EmptyMK

instance
  ( CanHardFork xs
  , HasCanonicalTxIn xs
  , HasHardForkTxOut xs
  ) =>
  LedgerSupportsMempool (HardForkBlock xs)
  where
  applyTx = applyHelper

  reapplyTx =
    error "This method is unreachable"

  reapplyTxs ::
    forall wtd extra.
    LedgerConfig (HardForkBlock xs) ->
    SlotNo ->
    -- \^ Slot number of the block containing the tx
    [ ( Validated (GenTx (HardForkBlock xs))
      , InputTxDiffs (HardForkBlock xs) wtd
      , extra
      )
    ] ->
    TickedLedgerState (HardForkBlock xs) ValuesMK ->
    ReapplyTxsResult extra (HardForkBlock xs) wtd
  reapplyTxs
    HardForkLedgerConfig{..}
    slot
    vtxs
    (TickedHardForkLedgerState transition hardForkState) =
      ( \(err, val, st) ->
          ReapplyTxsResult
            (map (\(x, y) -> Invalidated (txForgetValidated x) $ HardForkApplyTxErrWrongEra y) mismatched ++ err)
            val
            (TickedHardForkLedgerState transition st)
      )
        $ hsequence'
        $ hcizipWith
          proxySingle
          modeApplyCurrent
          cfgs
          matched
     where
      pcfgs = getPerEraLedgerConfig hardForkLedgerConfigPerEra
      cfgs = hcmap proxySingle (completeLedgerConfig'' ei) pcfgs
      ei =
        State.epochInfoPrecomputedTransitionInfo
          hardForkLedgerConfigShape
          transition
          hardForkState

      (mismatched, matched) = rematchValidatedTxs getHardForkValidatedGenTx hardForkState vtxs

      modeApplyCurrent ::
        forall blk.
        SingleEraBlock blk =>
        Index xs blk ->
        WrapLedgerConfig blk ->
        Product
          (FlipTickedLedgerState ValuesMK)
          (TxsToApply (InputTxDiffs (HardForkBlock xs) wtd) extra)
          blk ->
        DecomposedReapplyTxsResult extra xs wtd blk
      modeApplyCurrent index cfg (Pair (FlipTickedLedgerState st) (ReapplyTxs txs)) =
        let ReapplyTxsResult err val st' =
              reapplyTxs @blk @Discard
                (unwrapLedgerConfig cfg)
                slot
                [(unwrapValidatedGenTx tx, (), (df, tk)) | (tx, tk, df) <- txs]
                st
         in Comp
              ( [ injectGenTx index (getInvalidated x) `Invalidated` injectApplyTxErr index (getReason x)
                | x <- err
                ]
              , [ (HardForkValidatedGenTx . OneEraValidatedGenTx . injectNS index . WrapValidatedGenTx $ x, z1, z2)
                | (x, (), (z2, z1)) <- val
                ]
              , FlipTickedLedgerState st'
              )
      modeApplyCurrent index cfg (Pair (FlipTickedLedgerState st) (ApplyTxs txs)) =
        let (err, val, st') = foldApplyTxs txs
         in Comp
              ( [ injectGenTx index (getInvalidated x) `Invalidated` injectApplyTxErr index (getReason x)
                | x <- err
                ]
              , [ (HardForkValidatedGenTx . OneEraValidatedGenTx . injectNS index . WrapValidatedGenTx $ x, z1, z2)
                | (x, z1, z2) <- val
                ]
              , FlipTickedLedgerState $ forgetLedgerTables st'
              )
       where
        foldApplyTxs ::
          [(GenTx blk, InputTxDiffs (HardForkBlock xs) wtd, extra)] ->
          ( [Invalidated blk]
          , [(Validated (GenTx blk), InputTxDiffs (HardForkBlock xs) wtd, extra)]
          , TickedLedgerState blk ValuesMK
          )
        foldApplyTxs =
          Foldable.foldl'
            ( \(accE, accV, st') (a, d, e) ->
                case runExcept (applyTx (unwrapLedgerConfig cfg) DoNotIntervene slot a st') of
                  Left err -> (Invalidated a err : accE, accV, st')
                  Right (st'', validated) -> (accE, (validated, d, e) : accV, applyDiffs st' st'')
            )
            ([], [], st)

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
      K (LedgerTables (HardForkBlock xs) KeysMK) x
    f idx tx = K $ injectLedgerTables idx $ getTransactionKeySets tx

  -- This optimization is worthwile because we can save the projection and
  -- injection of ledger tables.
  --
  -- These operations are used when adding new transactions to the mempool,
  -- which is _not_ in the critical path for the forging loop but still will
  -- make adoption of new transactions faster. As adding a transaction takes a
  -- TMVar, it is interesting to hold it for as short of a time as possible.
  prependMempoolDiffs
    (TickedHardForkLedgerState _ (State.HardForkState st1))
    (TickedHardForkLedgerState tr (State.HardForkState st2)) =
      TickedHardForkLedgerState
        tr
        $ State.HardForkState
        $ runIdentity
          ( Tele.alignExtend
              ( InPairs.hpure
                  (error "When prepending mempool diffs we used to un-aligned states, this should be impossible!")
              )
              ( hcpure proxySingle $ fn_2 $ \(State.Current _ a) (State.Current start b) ->
                  State.Current start $
                    FlipTickedLedgerState $
                      prependMempoolDiffs
                        (getFlipTickedLedgerState a)
                        (getFlipTickedLedgerState b)
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
  applyMempoolDiffs
    vals
    keys
    (TickedHardForkLedgerState tr (State.HardForkState st)) =
      TickedHardForkLedgerState tr $
        State.HardForkState $
          hcimap
            proxySingle
            ( \idx (State.Current start (FlipTickedLedgerState a)) ->
                State.Current start $
                  FlipTickedLedgerState $
                    applyMempoolDiffs
                      (ejectLedgerTables idx vals)
                      (ejectLedgerTables idx keys)
                      a
            )
            st

  mkMempoolApplyTxError (TickedHardForkLedgerState _transition hardForkState) txt =
    hcollapse $ hcimap proxySingle f hardForkState
   where
    f ::
      SingleEraBlock x =>
      Index xs x ->
      FlipTickedLedgerState mk x ->
      K (Maybe (ApplyTxErr (HardForkBlock xs))) x
    f idx (FlipTickedLedgerState tlst) =
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
        FlipTickedLedgerState mk blk ->
        K (TxMeasure (HardForkBlock xs)) blk
      aux idx pcfg st' =
        K $
          let TxMeasure p1 p2 =
                blockCapacityTxMeasure
                  (completeLedgerConfig' ei pcfg)
                  (getFlipTickedLedgerState st')
           in TxMeasure
                (hardForkInjTxMeasurePhase1 . injectNS idx $ WrapTxMeasurePhase1 p1)
                (hardForkInjTxMeasurePhase2 . injectNS idx $ WrapTxMeasurePhase2 p2)

  txMeasurePhase1
    HardForkLedgerConfig{..}
    (TickedHardForkLedgerState transition hardForkState)
    tx =
      case matchTx (unwrapTx tx) hardForkState of
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

      aux ::
        forall blk.
        SingleEraBlock blk =>
        Index xs blk ->
        WrapLedgerConfig blk ->
        (Product (FlipTickedLedgerState EmptyMK) GenTx) blk ->
        K (Except (HardForkApplyTxErr xs) (HardForkTxMeasurePhase1 xs)) blk
      aux idx cfg (Pair st' tx') =
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
            (getFlipTickedLedgerState st')
            tx'

  txMeasurePhase2
    HardForkLedgerConfig{..}
    (TickedHardForkLedgerState transition hardForkState)
    tx =
      case matchTx (unwrapTx tx) hardForkState of
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

      aux ::
        forall blk.
        SingleEraBlock blk =>
        Index xs blk ->
        WrapLedgerConfig blk ->
        (Product (FlipTickedLedgerState ValuesMK) GenTx) blk ->
        K (Except (HardForkApplyTxErr xs) (HardForkTxMeasurePhase2 xs)) blk
      aux idx cfg (Pair st' tx') =
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
            (getFlipTickedLedgerState st')
            tx'

-- | A private type used only to clarify the definition of 'applyHelper'
data ApplyResult xs blk = ApplyResult
  { arState :: Ticked LedgerState blk DiffMK
  , arValidatedTx :: Validated (GenTx (HardForkBlock xs))
  }

-- | The shared logic between 'applyTx' and 'reapplyTx' for 'HardForkBlock'
--
-- The @txIn@ variable is 'GenTx' or 'WrapValidatedGenTx', respectively. See
-- 'ApplyHelperMode'.
applyHelper ::
  forall xs.
  CanHardFork xs =>
  LedgerConfig (HardForkBlock xs) ->
  WhetherToIntervene ->
  SlotNo ->
  GenTx (HardForkBlock xs) ->
  TickedLedgerState (HardForkBlock xs) ValuesMK ->
  Except
    (HardForkApplyTxErr xs)
    ( TickedLedgerState (HardForkBlock xs) DiffMK
    , Validated (GenTx (HardForkBlock xs))
    )
applyHelper
  HardForkLedgerConfig{..}
  wti
  slot
  tx
  (TickedHardForkLedgerState transition hardForkState) =
    case matchTx (unwrapTx tx) hardForkState of
      Left mismatch -> throwError $ HardForkApplyTxErrWrongEra mismatch
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
          let _ = result :: State.HardForkState (ApplyResult xs) xs

              st' :: State.HardForkState (FlipTickedLedgerState DiffMK) xs
              st' = (FlipTickedLedgerState . arState) `hmap` result

              vtx :: Validated (GenTx (HardForkBlock xs))
              vtx = hcollapse $ (K . arValidatedTx) `hmap` result

          return (TickedHardForkLedgerState transition st', vtx)
   where
    pcfgs = getPerEraLedgerConfig hardForkLedgerConfigPerEra
    cfgs = hcmap proxySingle (completeLedgerConfig'' ei) pcfgs
    ei =
      State.epochInfoPrecomputedTransitionInfo
        hardForkLedgerConfigShape
        transition
        hardForkState

    unwrapTx = getOneEraGenTx . getHardForkGenTx

    modeApplyCurrent ::
      forall blk.
      SingleEraBlock blk =>
      Index xs blk ->
      WrapLedgerConfig blk ->
      Product (FlipTickedLedgerState ValuesMK) GenTx blk ->
      ( Except (HardForkApplyTxErr xs)
          :.: ApplyResult xs
      )
        blk
    modeApplyCurrent index cfg (Pair (FlipTickedLedgerState st) tx') =
      Comp $ withExcept (injectApplyTxErr index) $ do
        let lcfg = unwrapLedgerConfig cfg
        (st', vtx) <- applyTx lcfg wti slot tx' st
        pure
          ApplyResult
            { arValidatedTx = injectValidatedGenTx index vtx
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

injectGenTx ::
  SListI xs => Index xs blk -> GenTx blk -> GenTx (HardForkBlock xs)
injectGenTx index =
  HardForkGenTx
    . OneEraGenTx
    . injectNS index
