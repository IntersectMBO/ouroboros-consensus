{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tools.DBSynthesizer.Forging
  ( GenTxs
  , runForge
  ) where

import Cardano.Tools.DBSynthesizer.Types
  ( ForgeLimit (..)
  , ForgeResult (..)
  )
import Control.Monad (void)
import Control.Tracer (nullTracer)
import Data.Maybe (isJust)
import Data.Word (Word64)
import Ouroboros.Consensus.Block
  ( BlockForging
  , EpochSize (..)
  , SlotNo (..)
  )
import Ouroboros.Consensus.Config
  ( TopLevelConfig
  , configLedger
  )
import Ouroboros.Consensus.Ledger.Abstract
  ( ComputeLedgerEvents (..)
  , DiffMK
  , TickedLedgerState
  , applyChainTick
  )
import Ouroboros.Consensus.Ledger.Extended (ledgerState)
import Ouroboros.Consensus.Ledger.SupportsMempool
  ( GenTx
  , Validated
  , txForgetValidated
  )
import Ouroboros.Consensus.Mempool
  ( Mempool (..)
  , MempoolCapacityBytesOverride (..)
  , addLocalTxs
  , chainDBLedgerInterface
  , openMempoolWithoutSyncThread
  )
import Ouroboros.Consensus.Node.Run (RunNode)
import Ouroboros.Consensus.NodeKernel.Forge (forge)
import Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
  ( ChainDB
  , getTipPoint
  , withReadOnlyForkerAtPoint
  )
import Ouroboros.Consensus.Storage.LedgerDB
  ( ReadOnlyForker'
  , roforkerGetLedgerState
  )
import Ouroboros.Consensus.Util.EarlyExit
  ( exitEarly
  , lift
  , withEarlyExit
  )
import Ouroboros.Consensus.Util.IOLike (atomically)
import Ouroboros.Network.Protocol.LocalStateQuery.Type (Target (..))

data ForgeState
  = ForgeState
  { currentSlot :: !SlotNo
  , forged :: !Word64
  , currentEpoch :: !Word64
  , processed :: !SlotNo
  }

initialForgeState :: ForgeState
initialForgeState = ForgeState 0 0 0 0

-- | An action to generate transactions for a given block
type GenTxs blk =
  SlotNo ->
  ReadOnlyForker' IO blk ->
  TickedLedgerState blk DiffMK ->
  IO [Validated (GenTx blk)]

runForge ::
  forall blk.
  RunNode blk =>
  EpochSize ->
  SlotNo ->
  ForgeLimit ->
  ChainDB IO blk ->
  [BlockForging IO blk] ->
  TopLevelConfig blk ->
  GenTxs blk ->
  IO ForgeResult
runForge epochSize_ nextSlot opts chainDB blockForging cfg genTxs = do
  putStrLn $ "--> epoch size: " ++ show epochSize_
  putStrLn $ "--> will process until: " ++ show opts
  mempool <-
    openMempoolWithoutSyncThread
      (chainDBLedgerInterface chainDB)
      (configLedger cfg)
      NoMempoolCapacityBytesOverride
      Nothing
      nullTracer
  endState <- go mempool initialForgeState{currentSlot = nextSlot}
  putStrLn $
    "--> forged and adopted "
      ++ show (forged endState)
      ++ " blocks; reached "
      ++ show (currentSlot endState)
  pure $ ForgeResult $ fromIntegral $ forged endState
 where
  epochSize = unEpochSize epochSize_

  forgingDone :: ForgeState -> Bool
  forgingDone = case opts of
    ForgeLimitSlot s -> (s ==) . processed
    ForgeLimitBlock b -> (b ==) . forged
    ForgeLimitEpoch e -> (e ==) . currentEpoch

  go :: Mempool IO blk -> ForgeState -> IO ForgeState
  go mempool forgeState
    | forgingDone forgeState = pure forgeState
    | otherwise = do
        didForge <- goSlot mempool (currentSlot forgeState)
        go mempool (nextForgeState forgeState didForge)

  nextForgeState :: ForgeState -> Bool -> ForgeState
  nextForgeState ForgeState{currentSlot, forged, currentEpoch, processed} didForge =
    ForgeState
      { currentSlot = currentSlot + 1
      , forged = forged + if didForge then 1 else 0
      , currentEpoch = epoch'
      , processed = processed'
      }
   where
    processed' = processed + 1
    epoch' = currentEpoch + if unSlotNo processed' `rem` epochSize == 0 then 1 else 0

  goSlot :: Mempool IO blk -> SlotNo -> IO Bool
  goSlot mempool currentSlot = do
    -- Sync mempool with the latest block adoption at 'tip'
    _ <- testSyncWithLedger mempool
    -- Populate mempool (synced against 'tip') with transactions for this slot
    tip <- atomically $ ChainDB.getTipPoint chainDB
    void $
      withEarlyExit $
        ChainDB.withReadOnlyForkerAtPoint chainDB (SpecificPoint tip) $ \case
          Left{} -> exitEarly
          Right frk -> do
            extLedgerState <- lift $ atomically $ roforkerGetLedgerState frk
            let tickedLedgerState =
                  applyChainTick
                    OmitLedgerEvents
                    (configLedger cfg)
                    currentSlot
                    (ledgerState extLedgerState)
            txs <- lift $ genTxs currentSlot frk tickedLedgerState
            lift $ void $ addLocalTxs mempool (map txForgetValidated txs)
    -- Forge a block for each credential; a block is adopted when forge returns Just ()
    results <-
      mapM
        (\bf -> withEarlyExit $ forge nullTracer nullTracer cfg chainDB mempool bf currentSlot)
        blockForging
    pure $ any isJust results
