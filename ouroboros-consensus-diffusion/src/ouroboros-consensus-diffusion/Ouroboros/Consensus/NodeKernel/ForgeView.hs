{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Prepare a forge view ahead of each slot the node leads.
--
-- A lead time before the slot, re-apply the mempool's transactions on the tip
-- ticked to it, and keep taking in those that arrive until the slot, best
-- effort. The forge then reads the view instead of re-applying the mempool.
module Ouroboros.Consensus.NodeKernel.ForgeView
  ( ForgeViews (..)
  , noForgeViews
  , prepareForgeViews
  ) where

import Control.Monad (when)
import Control.Monad.Except (runExcept)
import Data.Maybe (isJust, listToMaybe)
import qualified Data.Measure as Measure
import Data.Time (NominalDiffTime)
import Data.Void (absurd)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.BlockchainTime
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.Forecast (forecastFor)
import Ouroboros.Consensus.HardFork.Abstract (HasHardForkHistory (..))
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import Ouroboros.Consensus.HeaderValidation (headerStateChainDep)
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.Extended (ExtLedgerState (..))
import Ouroboros.Consensus.Ledger.SupportsMempool (TxLimits (..))
import Ouroboros.Consensus.Ledger.SupportsProtocol (LedgerSupportsProtocol (..))
import Ouroboros.Consensus.Ledger.Tables.Utils (forgetLedgerTables, prependDiffs)
import Ouroboros.Consensus.Leios.Types (SerialisedEndorserBlock (..))
import Ouroboros.Consensus.Mempool (Mempool (..))
import Ouroboros.Consensus.Mempool.ForgeView
  ( ForgeView
  , extendView
  , forgeView
  , forgeViewIsFull
  , forgeViewLastTicket
  )
import Ouroboros.Consensus.Node.Run (RunNode)
import Ouroboros.Consensus.Protocol.Abstract
import Ouroboros.Consensus.Storage.ChainDB.API (ChainDB)
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import Ouroboros.Consensus.Storage.LedgerDB
  ( ledgerStateReadOnlyForker
  , roforkerGetLedgerState
  , roforkerReadTables
  )
import Ouroboros.Consensus.Storage.LeiosDB.API
  ( LeiosDbHandle
  , LeiosDbReader (..)
  , withReader
  )
import Ouroboros.Consensus.Util.EarlyExit (lift, withEarlyExit)
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Consensus.Util.STM (Watcher)
import Ouroboros.Network.Protocol.LocalStateQuery.Type (Target (..))

-- | On each slot, look for a slot this node leads within the lead time, and
-- prepare a view for it until that slot begins.
prepareForgeViews ::
  forall m blk.
  (IOLike m, RunNode blk) =>
  TopLevelConfig blk ->
  BlockchainTime m ->
  ChainDB m blk ->
  LeiosDbHandle m ->
  Mempool m blk ->
  CanBeLeader (BlockProtocol blk) ->
  -- | The lead time.
  NominalDiffTime ->
  -- | Where the forge reads the views from.
  StrictTVar m (ForgeViews blk) ->
  Watcher m (SlotNo, RelativeTime) SlotNo
prepareForgeViews cfg btime chainDB leiosDb mempool creds leadTime varViews =
  knownSlotWatcher btime $ \currentSlot _onset -> do
    st <- atomically $ ChainDB.getCurrentLedger chainDB
    mapM_ prepare (nextLeaderSlot st currentSlot)
 where
  lcfg = configLedger cfg

  -- The first slot after the current one, starting within the lead time,
  -- that this node leads.
  nextLeaderSlot :: ExtLedgerState blk EmptyMK -> SlotNo -> Maybe SlotNo
  nextLeaderSlot st currentSlot = do
    now <- onset currentSlot
    listToMaybe
      [ s
      | s <- takeWhile (withinLeadTime now) [succ currentSlot ..]
      , leads s
      ]
   where
    summary = hardForkSummary lcfg (ledgerState st)

    onset s =
      either (const Nothing) (Just . fst) $
        HardFork.runQuery (HardFork.slotToWallclock s) summary

    withinLeadTime now s =
      maybe False (\t -> diffRelTime t now <= leadTime) (onset s)

    leads s = either (const False) id $ runExcept $ do
      ledgerView <- forecastFor (ledgerViewForecastAt lcfg (ledgerState st)) s
      let ticked =
            tickChainDepState
              (configConsensus cfg)
              ledgerView
              s
              (headerStateChainDep (headerState st))
      pure $ isJust $ checkIsLeader (configConsensus cfg) creds s ticked

  -- Prepare the views for the slot on the tip, starting over whenever the tip
  -- changes, until the slot begins. Each view has a thread of its own.
  prepare :: SlotNo -> m ()
  prepare s = do
    tip <- atomically $ do
      writeTVar varViews noForgeViews
      ChainDB.getTipPoint chainDB
    (restartTip, restartCert) <-
      concurrently
        (withForkerAt tip $ prepareTip s tip)
        (withForkerAt tip $ prepareCertifying s tip)
    when (restartTip || restartCert) $ prepare s

  -- Run with a forker on the tip, which tells whether to start over.
  withForkerAt tip k =
    fmap (== Just True) $
      withEarlyExit $
        ChainDB.withReadOnlyForkerAtPoint chainDB (SpecificPoint tip) $ \case
          Left _ -> pure False
          Right forker -> lift (k forker)

  prepareTip s tip forker = do
    unticked <- ledgerState <$> atomically (roforkerGetLedgerState forker)
    fill s tip forker (\v vs -> vs{tipView = Just v}) $
      let ticked = applyChainTick OmitLedgerEvents lcfg s unticked
       in forgeView lcfg s (blockCapacityTxMeasure lcfg ticked) ticked

  -- Once the node holds the closure of the endorser block the tip announced,
  -- a view with that closure applied.
  prepareCertifying s tip forker = do
    unticked <- ledgerState <$> atomically (roforkerGetLedgerState forker)
    next <- atomically $ do
      acquired <- ChainDB.getIsLeiosClosureAcquired chainDB
      untilSlotOrTipChange s tip $
        case ledgerAnnouncedLeiosPoint unticked of
          Just point | acquired point -> pure (Continue point)
          _ -> retry
    case next of
      Stop restart -> pure restart
      Continue point ->
        certifying forker unticked s point >>= \case
          -- A closure that cannot be applied leaves the producer with the tip
          -- view only.
          Nothing ->
            atomically (untilSlotOrTipChange s tip retry) >>= \case
              Stop restart -> pure restart
              Continue v -> absurd v
          Just view -> fill s tip forker (\v vs -> vs{certifyingView = Just v}) view

  -- Take in the mempool's transactions until the slot begins, publishing the
  -- view after each chunk.
  fill s tip forker publish = go
   where
    go view = do
      atomically $ modifyTVar varViews (publish view)
      next <-
        atomically $
          untilSlotOrTipChange s tip $
            if forgeViewIsFull view
              then retry
              else do
                txs <- getTxsAfter mempool (forgeViewLastTicket view)
                if null txs then retry else pure (Continue (take chunkSize txs))
      case next of
        Stop restart -> pure restart
        Continue txs -> extendView (ledgerStateReadOnlyForker forker) view txs >>= go

  -- Stop at the slot (False) or when the tip changes (True), otherwise go on.
  untilSlotOrTipChange s tip continue = do
    slotNow <- getCurrentSlot btime
    tip' <- ChainDB.getTipPoint chainDB
    case slotNow of
      CurrentSlot now _ | now >= s -> pure (Stop False)
      _
        | tip' /= tip -> pure (Stop True)
        | otherwise -> continue

  -- A view on the tip with the closure of the endorser block it announced
  -- applied, ticked to the slot.
  certifying forker unticked s point = do
    mClosure <- withReader leiosDb $ \reader ->
      liftA2 MkSerialisedEndorserBlock
        <$> lookupEbBody reader point
        <*> lookupEbClosure reader point
    case mClosure of
      Nothing -> pure Nothing
      Just closure ->
        applyEndorserBlock lcfg (roforkerReadTables forker) closure unticked >>= \case
          Left _ -> pure Nothing
          Right withClosure ->
            let ticked = applyChainTick OmitLedgerEvents lcfg s (forgetLedgerTables withClosure)
             in -- A certifying block carries no transactions of its own.
                pure $ Just $ forgeView lcfg s Measure.zero (prependDiffs withClosure ticked)

  -- How many transactions the view takes in at a time, so that it is ready
  -- in part when the slot begins.
  chunkSize = 256

-- | The views prepared for the next slot the node leads: on the tip, and on
-- the tip with the closure of the endorser block it announced applied, for a
-- block certifying that endorser block.
data ForgeViews blk = ForgeViews
  { tipView :: !(Maybe (ForgeView blk))
  , certifyingView :: !(Maybe (ForgeView blk))
  }

noForgeViews :: ForgeViews blk
noForgeViews = ForgeViews Nothing Nothing

data Next a = Stop Bool | Continue a
