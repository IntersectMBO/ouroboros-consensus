{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.HardFork.Combinator.Mempool (
    GenTx (..)
  , HardForkApplyTxErr (..)
  , TxId (..)
  , Validated (..)
  , hardForkApplyTxErrFromEither
  , hardForkApplyTxErrToEither
  ) where

import           Control.Monad.Except
import           Data.Functor.Product
import           Data.Kind (Type)
import           Data.SOP.BasicFunctors
import           Data.SOP.Constraint
import           Data.SOP.Functors
import           Data.SOP.Index
import           Data.SOP.InPairs (InPairs)
import qualified Data.SOP.InPairs as InPairs
import qualified Data.SOP.Match as Match
import           Data.SOP.Strict
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras
import           Ouroboros.Consensus.HardFork.Combinator.Basics
import           Ouroboros.Consensus.HardFork.Combinator.Info
import           Ouroboros.Consensus.HardFork.Combinator.InjectTxs
import           Ouroboros.Consensus.HardFork.Combinator.Ledger
import qualified Ouroboros.Consensus.HardFork.Combinator.State as State
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Consensus.Util

data HardForkApplyTxErr xs =
    -- | Validation error from one of the eras
    HardForkApplyTxErrFromEra !(OneEraApplyTxErr xs)

    -- | We tried to apply a block from the wrong era
  | HardForkApplyTxErrWrongEra !(MismatchEraInfo xs)
  deriving (Generic)

instance Typeable xs => ShowProxy (HardForkApplyTxErr xs) where

hardForkApplyTxErrToEither :: HardForkApplyTxErr xs
                           -> Either (MismatchEraInfo xs) (OneEraApplyTxErr xs)
hardForkApplyTxErrToEither (HardForkApplyTxErrFromEra  err) = Right err
hardForkApplyTxErrToEither (HardForkApplyTxErrWrongEra err) = Left  err

hardForkApplyTxErrFromEither :: Either (MismatchEraInfo xs) (OneEraApplyTxErr xs)
                             -> HardForkApplyTxErr xs
hardForkApplyTxErrFromEither (Right err) = HardForkApplyTxErrFromEra  err
hardForkApplyTxErrFromEither (Left  err) = HardForkApplyTxErrWrongEra err

deriving stock instance CanHardFork xs => Show (HardForkApplyTxErr xs)

deriving stock instance CanHardFork xs => Eq (HardForkApplyTxErr xs)

newtype instance GenTx (HardForkBlock xs) = HardForkGenTx {
      getHardForkGenTx :: OneEraGenTx xs
    }
  deriving (Eq, Generic, Show)
  deriving anyclass (NoThunks)

newtype instance Validated (GenTx (HardForkBlock xs)) = HardForkValidatedGenTx {
      getHardForkValidatedGenTx :: OneEraValidatedGenTx xs
    }
  deriving (Eq, Generic, Show)
  deriving anyclass (NoThunks)

instance Typeable xs => ShowProxy (GenTx (HardForkBlock xs)) where

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
type ComposedReapplyTxsResult xs =
  (,,)
    [Invalidated (HardForkBlock xs)]
    [Validated (GenTx (HardForkBlock xs))]
  :.:
    FlipTickedLedgerState DiffMK

instance ( CanHardFork xs
         , HardForkHasLedgerTables xs
         , HasCanonicalTxIn xs
         , HasHardForkTxOut xs
         ) => LedgerSupportsMempool (HardForkBlock xs) where
  applyTx = applyHelper ModeApply

  reapplyTx cfg slot vtx tls =
          fst
      <$> applyHelper
          ModeReapply
          cfg
          DoNotIntervene
          slot
          (WrapValidatedGenTx vtx)
          tls

  reapplyTxs
    HardForkLedgerConfig{..}
    slot
    vtxs
    (TickedHardForkLedgerState transition hardForkState) =
      (\(err, val, st') ->
           ReapplyTxsResult (mismatched' ++ err) val (TickedHardForkLedgerState transition st'))
      . hsequence'
      $ hcizipWith proxySingle modeApplyCurrent cfgs matched

    where
      pcfgs = getPerEraLedgerConfig hardForkLedgerConfigPerEra
      cfgs  = hcmap proxySingle (completeLedgerConfig'' ei) pcfgs
      ei    = State.epochInfoPrecomputedTransitionInfo
                hardForkLedgerConfigShape
                transition
                hardForkState

      -- Transactions are unwrapped into the particular era transactions.
      (mismatched, matched) =
        matchPolyTxs
          -- How to translate txs to later eras
          (InPairs.hmap snd2 (InPairs.requiringBoth cfgs hardForkInjectTxs))
          (map (getOneEraValidatedGenTx . getHardForkValidatedGenTx) vtxs)
          hardForkState

      mismatched' :: [Invalidated (HardForkBlock xs)]
      mismatched' =
        map (\x -> flip Invalidated ( HardForkApplyTxErrWrongEra
                                    $ MismatchEraInfo
                                    $ Match.bihcmap proxySingle singleEraInfo ledgerInfo
                                    $ snd x)
                 . HardForkValidatedGenTx
                 . OneEraValidatedGenTx
                 . fst
                 $ x)
            mismatched

      modeApplyCurrent :: forall blk.
           SingleEraBlock                      blk
        => Index xs                            blk
        -> WrapLedgerConfig                    blk
        -> Product
             (ListOfTxs WrapValidatedGenTx xs)
             (FlipTickedLedgerState ValuesMK)  blk
        -> ComposedReapplyTxsResult xs         blk
      modeApplyCurrent index cfg (Pair txs (FlipTickedLedgerState st)) =
        let ReapplyTxsResult err val st' =
              reapplyTxs (unwrapLedgerConfig cfg) slot [ unwrapValidatedGenTx t | TxsWithOriginal _ t <- txsList txs ] st
        in Comp
           ( map (\x -> flip Invalidated (injectApplyTxErr index $ getReason x) . injectValidatedGenTx index . getInvalidated $ x) err
           , map (HardForkValidatedGenTx . OneEraValidatedGenTx . injectNS index . WrapValidatedGenTx) val
           , FlipTickedLedgerState st'
           )

  txsMaxBytes =
        hcollapse
      . hcmap proxySingle (K . txsMaxBytes . getFlipTickedLedgerState)
      . State.tip
      . tickedHardForkLedgerStatePerEra

  txInBlockSize =
        hcollapse
      . hcmap proxySingle (K . txInBlockSize)
      . getOneEraGenTx
      . getHardForkGenTx

  txForgetValidated =
        HardForkGenTx
      . OneEraGenTx
      . hcmap proxySingle (txForgetValidated . unwrapValidatedGenTx)
      . getOneEraValidatedGenTx
      . getHardForkValidatedGenTx

  txRefScriptSize cfg st tx = case matchPolyTx injs tx' hardForkState of
      Left {}       ->
        -- This is ugly/adhoc, but fine, as in the mempool, we only call
        -- txRefScriptSize after applyTx (which internall also calls
        -- matchPolyTx), so this case is unreachable.
        0
      Right matched ->
          hcollapse
        $ hczipWith proxySingle
            (\(WrapLedgerConfig eraCfg) (Pair eraTx (FlipTickedLedgerState eraSt)) ->
               K $ txRefScriptSize eraCfg eraSt eraTx)
            cfgs
            (State.tip matched)
    where
      HardForkLedgerConfig {
          hardForkLedgerConfigPerEra
        , hardForkLedgerConfigShape
        } = cfg
      TickedHardForkLedgerState transition hardForkState = st
      tx' = getOneEraGenTx . getHardForkGenTx $ tx

      pcfgs = getPerEraLedgerConfig hardForkLedgerConfigPerEra
      cfgs  = hcmap proxySingle (completeLedgerConfig'' ei) pcfgs
      ei    = State.epochInfoPrecomputedTransitionInfo
                hardForkLedgerConfigShape
                transition
                hardForkState

      injs :: InPairs (InjectPolyTx GenTx) xs
      injs =
          InPairs.hmap
            (\(Pair2 injTx _injValidatedTx) -> injTx)
            (InPairs.requiringBoth cfgs hardForkInjectTxs)

  getTransactionKeySets (HardForkGenTx (OneEraGenTx ns)) =
        hcollapse
      $ hcimap proxySingle f ns
    where
      f ::
           SingleEraBlock                                           x
        => Index                                       xs           x
        -> GenTx                                                    x
        -> K (LedgerTables (LedgerState (HardForkBlock xs)) KeysMK) x
      f idx tx = K $ injectLedgerTables idx $ getTransactionKeySets tx

-- | A private type used only to clarify the parameterization of 'applyHelper'
data ApplyHelperMode :: (Type -> Type) -> Type where
  ModeApply   :: ApplyHelperMode GenTx
  ModeReapply :: ApplyHelperMode WrapValidatedGenTx

-- | 'applyHelper' has to return one of these, depending on the apply mode used.
type family ApplyMK k where
  ApplyMK (ApplyHelperMode GenTx) = DiffMK
  ApplyMK (ApplyHelperMode WrapValidatedGenTx) = ValuesMK

-- | A private type used only to clarify the definition of 'applyHelper'
data ApplyResult xs txIn blk = ApplyResult {
    arState       :: Ticked1 (LedgerState blk) (ApplyMK (ApplyHelperMode txIn))
  , arValidatedTx :: Validated (GenTx (HardForkBlock xs))
  }

-- | The shared logic between 'applyTx' and 'reapplyTx' for 'HardForkBlock'
--
-- The @txIn@ variable is 'GenTx' or 'WrapValidatedGenTx', respectively. See
-- 'ApplyHelperMode'.
applyHelper :: forall xs txIn. CanHardFork xs
  => ApplyHelperMode txIn
  -> LedgerConfig (HardForkBlock xs)
  -> WhetherToIntervene
  -> SlotNo
  -> txIn (HardForkBlock xs)
  -> TickedLedgerState (HardForkBlock xs) ValuesMK
  -> Except
      (HardForkApplyTxErr xs)
      ( TickedLedgerState (HardForkBlock xs) (ApplyMK (ApplyHelperMode txIn))
      , Validated (GenTx (HardForkBlock xs))
      )
applyHelper mode
            HardForkLedgerConfig{..}
            wti
            slot
            tx
            (TickedHardForkLedgerState transition hardForkState) =
    case matchPolyTx injs (modeGetTx tx) hardForkState of
      Left mismatch ->
        throwError $ HardForkApplyTxErrWrongEra . MismatchEraInfo $
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
              hsequence'
            $ hcizipWith proxySingle modeApplyCurrent cfgs matched
          let _ = result :: State.HardForkState (ApplyResult xs txIn) xs

              st' :: State.HardForkState (FlipTickedLedgerState (ApplyMK (ApplyHelperMode txIn))) xs
              st' = (FlipTickedLedgerState . arState) `hmap` result

              vtx :: Validated (GenTx (HardForkBlock xs))
              vtx = hcollapse $ (K . arValidatedTx) `hmap` result

          return (TickedHardForkLedgerState transition st', vtx)
  where
    pcfgs = getPerEraLedgerConfig hardForkLedgerConfigPerEra
    cfgs  = hcmap proxySingle (completeLedgerConfig'' ei) pcfgs
    ei    = State.epochInfoPrecomputedTransitionInfo
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
        ModeApply   ->
              getOneEraGenTx
            . getHardForkGenTx
        ModeReapply ->
              getOneEraValidatedGenTx
            . getHardForkValidatedGenTx
            . unwrapValidatedGenTx

    modeGetInjection :: forall blk1 blk2.
         Product2 InjectTx InjectValidatedTx blk1 blk2
      -> InjectPolyTx txIn                   blk1 blk2
    modeGetInjection (Pair2 injTx injValidatedTx) = case mode of
        ModeApply   -> injTx
        ModeReapply -> injValidatedTx

    modeApplyCurrent :: forall blk.
         SingleEraBlock                                blk
      => Index xs                                      blk
      -> WrapLedgerConfig                              blk
      -> Product txIn (FlipTickedLedgerState ValuesMK) blk
      -> (     Except (HardForkApplyTxErr xs)
           :.: ApplyResult xs txIn
         )                                             blk
    modeApplyCurrent index cfg (Pair tx' (FlipTickedLedgerState st)) =
          Comp
        $ withExcept (injectApplyTxErr index)
        $ do
            let lcfg = unwrapLedgerConfig cfg
            case mode of
              ModeApply   -> do
                (st', vtx) <- applyTx lcfg wti slot tx' st
                pure ApplyResult {
                    arValidatedTx = injectValidatedGenTx index vtx
                  , arState       = st'
                  }
              ModeReapply -> do
                  let vtx' = unwrapValidatedGenTx tx'
                  st' <- reapplyTx lcfg slot vtx' st
                  -- provide the given transaction, which was already validated
                  pure ApplyResult {
                      arValidatedTx = injectValidatedGenTx index vtx'
                    , arState       = st'
                    }

newtype instance TxId (GenTx (HardForkBlock xs)) = HardForkGenTxId {
      getHardForkGenTxId :: OneEraGenTxId xs
    }
  deriving (Eq, Generic, Ord, Show)
  deriving anyclass (NoThunks)

instance Typeable xs => ShowProxy (TxId (GenTx (HardForkBlock xs))) where

instance CanHardFork xs => HasTxId (GenTx (HardForkBlock xs)) where
  txId = HardForkGenTxId . OneEraGenTxId
       . hcmap proxySingle (WrapGenTxId . txId)
       . getOneEraGenTx . getHardForkGenTx

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
           HasTxs blk
        => Index xs blk
        -> I blk
        -> K [GenTx (HardForkBlock xs)] blk
      aux index = K . map (injectNS' (Proxy @GenTx) index) . extractTxs . unI

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

ledgerInfo :: forall blk mk. SingleEraBlock blk
           => State.Current (FlipTickedLedgerState mk) blk -> LedgerEraInfo blk
ledgerInfo _ = LedgerEraInfo $ singleEraInfo (Proxy @blk)

injectApplyTxErr :: Index xs blk -> ApplyTxErr blk -> HardForkApplyTxErr xs
injectApplyTxErr index =
      HardForkApplyTxErrFromEra
    . OneEraApplyTxErr
    . injectNS index
    . WrapApplyTxErr

injectValidatedGenTx :: Index xs blk -> Validated (GenTx blk) -> Validated (GenTx (HardForkBlock xs))
injectValidatedGenTx index =
      HardForkValidatedGenTx
    . OneEraValidatedGenTx
    . injectNS index
    . WrapValidatedGenTx
