{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.HardFork.Combinator.Forging
  ( HardForkCannotForge
  , HardForkForgeStateInfo (..)
  , HardForkForgeStateUpdateError
  , hardForkBlockForging
  , projectMempoolSnapshot
  , injectForgedBlock
  ) where

import Control.Monad (void)
import Data.Functor.Product
import Data.Maybe (fromMaybe)
import Data.SOP (Top)
import Data.SOP.BasicFunctors
import Data.SOP.Constraint (All)
import Data.SOP.Index
import qualified Data.SOP.Match as Match
import Data.SOP.OptNP (NonEmptyOptNP, OptNP, ViewOptNP (..))
import qualified Data.SOP.OptNP as OptNP
import Data.SOP.Strict
import Data.Text (Text)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.HardFork.Combinator.Abstract
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras
import Ouroboros.Consensus.HardFork.Combinator.Basics
import Ouroboros.Consensus.HardFork.Combinator.Ledger
import Ouroboros.Consensus.HardFork.Combinator.Mempool
import Ouroboros.Consensus.HardFork.Combinator.Protocol
import qualified Ouroboros.Consensus.HardFork.Combinator.State as State
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.SupportsMempool
import Ouroboros.Consensus.Mempool.API
  ( MempoolMeasure (..)
  , MempoolSnapshot (..)
  , TicketNo
  )
import Ouroboros.Consensus.TypeFamilyWrappers

-- | If we cannot forge, it's because the current era could not forge
type HardForkCannotForge xs = OneEraCannotForge xs

type instance CannotForge (HardForkBlock xs) = HardForkCannotForge xs

-- | For each era in which we want to forge blocks, we have a 'BlockForging',
-- and thus 'ForgeStateInfo'.
--
-- When we update the hard fork forge state, we only update the forge state of
-- the current era. However, the current era /might not/ have a forge state as
-- it lacks a 'BlockForging'.
--
-- TODO #2766: expire past 'ForgeState'
data HardForkForgeStateInfo xs where
  -- | There is no 'BlockForging' record for the current era.
  CurrentEraLacksBlockForging ::
    EraIndex (x ': y ': xs) ->
    HardForkForgeStateInfo (x ': y ': xs)
  -- | The 'ForgeState' of the current era was updated.
  CurrentEraForgeStateUpdated ::
    OneEraForgeStateInfo xs ->
    HardForkForgeStateInfo xs

deriving instance CanHardFork xs => Show (HardForkForgeStateInfo xs)

type instance ForgeStateInfo (HardForkBlock xs) = HardForkForgeStateInfo xs

-- | For each era in which we want to forge blocks, we have a 'BlockForging',
-- and thus 'ForgeStateUpdateError'.
type HardForkForgeStateUpdateError xs = OneEraForgeStateUpdateError xs

type instance
  ForgeStateUpdateError (HardForkBlock xs) =
    HardForkForgeStateUpdateError xs

hardForkBlockForging ::
  forall m xs.
  (CanHardFork xs, Monad m) =>
  -- | Used as the 'forgeLabel', the labels of the given 'BlockForging's will
  -- be ignored.
  (NonEmptyOptNP (BlockForging m) xs -> Text) ->
  NonEmptyOptNP (MkBlockForging m) xs ->
  MkBlockForging m (HardForkBlock xs)
hardForkBlockForging labelF mkBlockForgings = MkBlockForging $ do
  blockForgings <- htraverse' mkBlockForging mkBlockForgings
  pure
    BlockForging
      { forgeLabel = labelF blockForgings
      , canBeLeader = hardForkCanBeLeader blockForgings
      , updateForgeState = hardForkUpdateForgeState blockForgings
      , checkCanForge = hardForkCheckCanForge blockForgings
      , forgeBlock = hardForkForgeBlock blockForgings
      , finalize = hardForkFinalize blockForgings
      }

hardForkCanBeLeader ::
  CanHardFork xs =>
  NonEmptyOptNP (BlockForging m) xs -> HardForkCanBeLeader xs
hardForkCanBeLeader =
  SomeErasCanBeLeader
    . hmap (WrapCanBeLeader . canBeLeader)

hardForkFinalize ::
  (Monad m, All Top xs) =>
  NonEmptyOptNP (BlockForging m) xs -> m ()
hardForkFinalize blockForging =
  void $ htraverse_ finalize blockForging

-- | POSTCONDITION: the returned 'ForgeStateUpdateInfo' is from the same era as
-- the ticked 'ChainDepState'.
hardForkUpdateForgeState ::
  forall m xs.
  (CanHardFork xs, Monad m) =>
  NonEmptyOptNP (BlockForging m) xs ->
  TopLevelConfig (HardForkBlock xs) ->
  SlotNo ->
  Ticked (HardForkChainDepState xs) ->
  m (ForgeStateUpdateInfo (HardForkBlock xs))
hardForkUpdateForgeState
  blockForging
  cfg
  curSlot
  (TickedHardForkChainDepState chainDepState ei) =
    case OptNP.view blockForging of
      OptNP_ExactlyOne blockForging' ->
        injectSingle
          <$> updateForgeState
            blockForging'
            (hd (distribTopLevelConfig ei cfg))
            curSlot
            (unwrapTickedChainDepState . unComp . State.fromTZ $ chainDepState)
      OptNP_AtLeastTwo ->
        fmap undistrib
          $ hsequence'
          $ hzipWith3
            aux
            (OptNP.toNP blockForging)
            (distribTopLevelConfig ei cfg)
          $ State.tip chainDepState
   where
    injectSingle ::
      xs ~ '[blk] =>
      ForgeStateUpdateInfo blk ->
      ForgeStateUpdateInfo (HardForkBlock '[blk])
    injectSingle forgeStateUpdateInfo =
      case forgeStateUpdateInfo of
        ForgeStateUpdated info -> ForgeStateUpdated $ injInfo index info
        ForgeStateUpdateFailed err -> ForgeStateUpdateFailed $ injUpdateError index err
        ForgeStateUpdateSuppressed -> ForgeStateUpdateSuppressed
     where
      index :: Index '[blk] blk
      index = IZ

    aux ::
      (Maybe :.: BlockForging m) blk ->
      TopLevelConfig blk ->
      (Ticked :.: WrapChainDepState) blk ->
      (m :.: (Maybe :.: ForgeStateUpdateInfo)) blk
    aux (Comp mBlockForging) cfg' (Comp chainDepState') =
      Comp $ fmap Comp $ case mBlockForging of
        Nothing -> return Nothing
        Just blockForging' ->
          Just
            <$> updateForgeState
              blockForging'
              cfg'
              curSlot
              (unwrapTickedChainDepState chainDepState')

    injInfo ::
      Index xs blk ->
      ForgeStateInfo blk ->
      ForgeStateInfo (HardForkBlock xs)
    injInfo index =
      CurrentEraForgeStateUpdated
        . OneEraForgeStateInfo
        . injectNS index
        . WrapForgeStateInfo

    injUpdateError ::
      Index xs blk ->
      ForgeStateUpdateError blk ->
      ForgeStateUpdateError (HardForkBlock xs)
    injUpdateError index =
      OneEraForgeStateUpdateError
        . injectNS index
        . WrapForgeStateUpdateError

    undistrib ::
      xs ~ (x ': y ': zs) =>
      NS (Maybe :.: ForgeStateUpdateInfo) xs ->
      ForgeStateUpdateInfo (HardForkBlock xs)
    undistrib = hcollapse . himap inj
     where
      inj ::
        forall blk.
        Index xs blk ->
        (Maybe :.: ForgeStateUpdateInfo) blk ->
        K (ForgeStateUpdateInfo (HardForkBlock xs)) blk
      inj index (Comp mForgeStateUpdateInfo) =
        K $ case mForgeStateUpdateInfo of
          Nothing -> ForgeStateUpdated $ CurrentEraLacksBlockForging $ eraIndexFromIndex index
          Just forgeStateUpdateInfo ->
            case forgeStateUpdateInfo of
              ForgeStateUpdated info -> ForgeStateUpdated $ injInfo index info
              ForgeStateUpdateFailed err -> ForgeStateUpdateFailed $ injUpdateError index err
              ForgeStateUpdateSuppressed -> ForgeStateUpdateSuppressed

-- | PRECONDITION: the ticked 'ChainDepState', the 'HardForkIsLeader', and the
-- 'HardForkStateInfo' are all from the same era, and we must have a
-- 'BlockForging' for that era.
--
-- This follows from the postconditions of 'check' and
-- 'hardForkUpdateForgeState'.
hardForkCheckCanForge ::
  forall m xs empty.
  CanHardFork xs =>
  OptNP empty (BlockForging m) xs ->
  TopLevelConfig (HardForkBlock xs) ->
  SlotNo ->
  Ticked (HardForkChainDepState xs) ->
  HardForkIsLeader xs ->
  HardForkForgeStateInfo xs ->
  Either (HardForkCannotForge xs) ()
hardForkCheckCanForge
  blockForging
  cfg
  curSlot
  (TickedHardForkChainDepState chainDepState ei)
  isLeader
  forgeStateInfo =
    distrib $
      hizipWith3
        checkOne
        (distribTopLevelConfig ei cfg)
        (OptNP.toNP blockForging)
        -- We know all three NSs must be from the same era, because they were
        -- all produced from the same 'BlockForging'. Unfortunately, we can't
        -- enforce it statically.
        ( Match.mustMatchNS "ForgeStateInfo" forgeStateInfo' $
            Match.mustMatchNS "IsLeader" (getOneEraIsLeader isLeader) $
              State.tip chainDepState
        )
   where
    distrib ::
      NS (Maybe :.: WrapCannotForge) xs ->
      Either (HardForkCannotForge xs) ()
    distrib = maybe (Right ()) (Left . OneEraCannotForge) . hsequence'

    missingBlockForgingImpossible :: EraIndex xs -> String
    missingBlockForgingImpossible eraIndex =
      "impossible: current era lacks block forging but we have an IsLeader proof "
        <> show eraIndex

    forgeStateInfo' :: NS WrapForgeStateInfo xs
    forgeStateInfo' = case forgeStateInfo of
      CurrentEraForgeStateUpdated info -> getOneEraForgeStateInfo info
      CurrentEraLacksBlockForging eraIndex ->
        error $ missingBlockForgingImpossible eraIndex

    checkOne ::
      Index xs blk ->
      TopLevelConfig blk ->
      (Maybe :.: BlockForging m) blk ->
      Product
        WrapForgeStateInfo
        ( Product
            WrapIsLeader
            (Ticked :.: WrapChainDepState)
        )
        blk ->
      (Maybe :.: WrapCannotForge) blk
    -- \^ We use @Maybe x@ instead of @Either x ()@ because the former can
    -- be partially applied.
    checkOne
      index
      cfg'
      (Comp mBlockForging')
      ( Pair
          (WrapForgeStateInfo forgeStateInfo'')
          ( Pair
              (WrapIsLeader isLeader')
              (Comp tickedChainDepState)
            )
        ) =
        Comp $
          either (Just . WrapCannotForge) (const Nothing) $
            checkCanForge
              ( fromMaybe
                  (error (missingBlockForgingImpossible (eraIndexFromIndex index)))
                  mBlockForging'
              )
              cfg'
              curSlot
              (unwrapTickedChainDepState tickedChainDepState)
              isLeader'
              forgeStateInfo''

-- | PRECONDITION: the ticked 'LedgerState' and 'HardForkIsLeader' are from the
-- same era, and we must have a 'BlockForging' for that era.
--
-- This follows from the postcondition of 'check' and the fact that the ticked
-- 'ChainDepState' and ticked 'LedgerState' are from the same era.
hardForkForgeBlock ::
  forall m xs empty.
  (CanHardFork xs, Monad m) =>
  OptNP empty (BlockForging m) xs ->
  ForgeBlockArgs (HardForkBlock xs) ->
  m (ForgedBlock (HardForkBlock xs))
hardForkForgeBlock blockForging ForgeBlockArgs{..} =
  hcollapse
    $ hcizipWith3
      proxySingle
      forgeBlockOne
      cfgs
      (OptNP.toNP blockForging)
    -- We know both NSs must be from the same era, because they were all
    -- produced from the same 'BlockForging'. Unfortunately, we can't enforce
    -- it statically.
    $ Match.mustMatchNS
      "IsLeader"
      (getOneEraIsLeader fbIsLeader)
    $ State.tip ledgerState
 where
  TickedHardForkLedgerState transition ledgerState = fbCurrentTickedLedgerState
  cfgs = distribTopLevelConfig ei fbConfig
  ei =
    State.epochInfoPrecomputedTransitionInfo
      (hardForkLedgerConfigShape (configLedger fbConfig))
      transition
      ledgerState

  missingBlockForgingImpossible :: EraIndex xs -> String
  missingBlockForgingImpossible eraIndex =
    "impossible: current era lacks block forging but we have an IsLeader proof "
      <> show eraIndex

  -- If we crossed an era boundary in this forge, and we are supposed to
  -- include a Peras certificate in this block, we must ensure that the
  -- certificate being passed to us (i.e., the latest certificate seen) is
  -- from the same era as the block being forged. Otherwise, we drop it
  -- (treating it as absent), since inter-era certificate inclusion is not
  -- supported for now. In the unlikely event of recovering from a cooldown
  -- period that crosses an era boundary, one should jumpstart the voting
  -- process again via the same type of governance action that started this
  -- process in the first place, but in the new era.
  injectPerasCertIfSameEra ::
    Index xs blk ->
    PerasCert (HardForkBlock xs) ->
    Maybe (PerasCert blk)
  injectPerasCertIfSameEra index hardForkPerasCert =
    case ( Match.matchNS
             (getIndex index)
             (getOneEraPerasCert hardForkPerasCert)
         ) of
      -- The Peras certificate is from a different era than the block being
      -- forged, so we drop it (treating it as absent).
      Left _mismatch ->
        Nothing
      -- The Peras certificate is from the same era as the block being forged,
      -- so we keep it and pass it down to the current era's 'forgeBlock'.
      Right nsPair ->
        hcollapse $
          hmap (\(Pair Refl (WrapPerasCert cert)) -> K (Just cert)) $
            nsPair

  -- \| Unwraps all the layers needed for SOP and call 'forgeBlock'.
  forgeBlockOne ::
    SingleEraBlock blk =>
    Index xs blk ->
    TopLevelConfig blk ->
    (Maybe :.: BlockForging m) blk ->
    Product WrapIsLeader (FlipTickedLedgerState EmptyMK) blk ->
    K (m (ForgedBlock (HardForkBlock xs))) blk
  forgeBlockOne
    index
    cfg'
    (Comp mBlockForging')
    (Pair (WrapIsLeader isLeader') (FlipTickedLedgerState ledgerState')) =
      K $
        injectForgedBlock index
          <$> forgeBlock
            ( fromMaybe
                (error (missingBlockForgingImpossible (eraIndexFromIndex index)))
                mBlockForging'
            )
            ForgeBlockArgs
              { fbConfig = cfg'
              , fbCurrentBlockNo = fbCurrentBlockNo
              , fbCurrentSlotNo = fbCurrentSlotNo
              , fbPerasCert = fbPerasCert >>= injectPerasCertIfSameEra index
              , fbCurrentTickedLedgerState = ledgerState'
              , fbMempoolSnapshot = projectMempoolSnapshot index fbMempoolSnapshot
              , fbIsLeader = isLeader'
              }

-- | Inject the 'ForgedBlock' of the era at @index@ into the hard fork block.
injectForgedBlock ::
  CanHardFork xs =>
  Index xs blk ->
  ForgedBlock blk ->
  ForgedBlock (HardForkBlock xs)
injectForgedBlock index (ForgedBlock blk txs txsMeasure) =
  ForgedBlock
    { forgedBlock = HardForkBlock $ OneEraBlock $ injectNS index (I blk)
    , forgedTxs = map (injectValidatedGenTx index) txs
    , forgedTxsMeasure = injectMempoolMeasure index txsMeasure
    }

{-------------------------------------------------------------------------------
  Mempool snapshot of one era
-------------------------------------------------------------------------------}

-- | The mempool snapshot of the era at @index@.
--
-- PRECONDITION: every transaction in the snapshot is from that era.
-- 'hardForkBlockForging' passes the era of its ticked ledger state, and the
-- transactions of 'fbMempoolSnapshot' apply to that state.
-- The measures in the snapshot can come from an earlier era, because
-- 'Ouroboros.Consensus.Mempool.API.getSnapshotFor' keeps the measure that
-- each transaction got when the mempool added it.
--
-- The endorser-block part of 'snapshotPartition' uses the combined
-- endorser-block measure. If that measure has no field that the era's measure
-- lacks, the part is the part that the era's own measures give. Otherwise the
-- part can end early. It stops before the first transaction that makes the sum
-- in such a field exceed that field of the injected endorser-block capacity.
projectMempoolSnapshot ::
  forall xs blk.
  (CanHardFork xs, SingleEraBlock blk) =>
  Index xs blk ->
  MempoolSnapshot (HardForkBlock xs) ->
  MempoolSnapshot blk
projectMempoolSnapshot index snapshot =
  MempoolSnapshot
    { snapshotTxs = map projectTicket (snapshotTxs snapshot)
    , snapshotTxsAfter = map projectTicket . snapshotTxsAfter snapshot
    , snapshotPartition = \blockCapacity ebCapacity ->
        let (rbTxs, rbTxsMeasure, ebTxs, ebTxsMeasure) =
              snapshotPartition
                snapshot
                (injectTxMeasure index blockCapacity)
                (injectTxEbMeasure index ebCapacity)
         in ( map projectTx rbTxs
            , projectMempoolMeasure rbTxsMeasure
            , map projectTx ebTxs
            , projectMempoolMeasure ebTxsMeasure
            )
    , snapshotLookupTx = fmap projectTx . snapshotLookupTx snapshot
    , snapshotHasTx =
        snapshotHasTx snapshot
          . HardForkGenTxId
          . OneEraGenTxId
          . injectNS index
          . WrapGenTxId
    , snapshotMempoolSize = snapshotMempoolSize snapshot
    , snapshotSlotNo = snapshotSlotNo snapshot
    , snapshotStateHash = case snapshotStateHash snapshot of
        GenesisHash -> GenesisHash
        BlockHash h -> BlockHash (projectHash h)
    , snapshotPoint = case snapshotPoint snapshot of
        GenesisPoint -> GenesisPoint
        BlockPoint s h -> BlockPoint s (projectHash h)
    }
 where
  projectTicket ::
    (Validated (GenTx (HardForkBlock xs)), TicketNo, TxMeasure (HardForkBlock xs)) ->
    (Validated (GenTx blk), TicketNo, TxMeasure blk)
  projectTicket (tx, ticketNo, txMeasure) =
    (projectTx tx, ticketNo, projectTxMeasure txMeasure)

  projectTx :: Validated (GenTx (HardForkBlock xs)) -> Validated (GenTx blk)
  projectTx tx =
    case Match.matchNS
      (getIndex index)
      (getOneEraValidatedGenTx (getHardForkValidatedGenTx tx)) of
      Left _mismatch ->
        error "Impossible! the mempool snapshot has a transaction from another era"
      Right nsPair ->
        hcollapse $
          hmap (\(Pair Refl (WrapValidatedGenTx tx')) -> K tx') nsPair

  projectTxMeasure :: TxMeasure (HardForkBlock xs) -> TxMeasure blk
  projectTxMeasure (TxMeasure p1 p2) =
    TxMeasure
      (unwrapTxMeasurePhase1 $ projectNP index $ hardForkProjTxMeasurePhase1 p1)
      (unwrapTxMeasurePhase2 $ projectNP index $ hardForkProjTxMeasurePhase2 p2)

  projectMempoolMeasure :: MempoolMeasure (HardForkBlock xs) -> MempoolMeasure blk
  projectMempoolMeasure (MempoolMeasure txMeasure ebMeasure diffTime) =
    MempoolMeasure
      (projectTxMeasure txMeasure)
      (unwrapTxEbMeasure $ projectNP index $ hardForkProjTxEbMeasure ebMeasure)
      diffTime

  -- 'CanHardFork' requires that every era has the hash size of the first era
  -- ('EqualHashSizeOfHead'), so the raw bytes have the size that @blk@ needs.
  projectHash :: HeaderHash (HardForkBlock xs) -> HeaderHash blk
  projectHash = unsafeFromShortRawHash (Proxy @blk) . getOneEraHash

injectTxMeasure ::
  CanHardFork xs =>
  Index xs blk ->
  TxMeasure blk ->
  TxMeasure (HardForkBlock xs)
injectTxMeasure index (TxMeasure p1 p2) =
  TxMeasure
    (hardForkInjTxMeasurePhase1 $ injectNS index $ WrapTxMeasurePhase1 p1)
    (hardForkInjTxMeasurePhase2 $ injectNS index $ WrapTxMeasurePhase2 p2)

injectTxEbMeasure ::
  CanHardFork xs =>
  Index xs blk ->
  TxEbMeasure blk ->
  TxEbMeasure (HardForkBlock xs)
injectTxEbMeasure index = hardForkInjTxEbMeasure . injectNS index . WrapTxEbMeasure

injectMempoolMeasure ::
  CanHardFork xs =>
  Index xs blk ->
  MempoolMeasure blk ->
  MempoolMeasure (HardForkBlock xs)
injectMempoolMeasure index (MempoolMeasure txMeasure ebMeasure diffTime) =
  MempoolMeasure
    (injectTxMeasure index txMeasure)
    (injectTxEbMeasure index ebMeasure)
    diffTime
