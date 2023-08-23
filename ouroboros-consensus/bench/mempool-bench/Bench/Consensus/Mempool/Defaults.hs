{-# LANGUAGE TupleSections #-}

{- HLINT ignore "Use camelCase" -}

-- | Functions related to initial parameters for the mempool.
module Bench.Consensus.Mempool.Defaults (
    defaultInMemoryBSS
  , defaultLMDB_BSS
  , defaultLedgerDbCfg
  , defaultParams
    -- * Construction of configurations
  , sampleLedgerConfig
  , sampleLedgerDbCfg
    -- * Construction of initial state
  , ledgerStateFromTokens
  , testBlocksFromTxs
  ) where

import           Bench.Consensus.Mempool.TestBlock
import qualified Cardano.Slotting.Time as Time
import           Control.Monad.IO.Class
import qualified Data.Map.Strict as Map
import qualified Ouroboros.Consensus.Block as Block
import           Ouroboros.Consensus.Config.SecurityParam
                     (SecurityParam (SecurityParam))
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import           Ouroboros.Consensus.Ledger.Basics (LedgerConfig, ValuesMK (..))
import           Ouroboros.Consensus.Storage.LedgerDB.BackingStore.Init
                     (BackingStoreSelector (..))
import           Ouroboros.Consensus.Storage.LedgerDB.BackingStore.LMDB
                     (LMDBLimits (..))
import           Ouroboros.Consensus.Storage.LedgerDB.Config (LedgerDbCfg (..))
import           Test.Consensus.Mempool.Mocked

{-------------------------------------------------------------------------------
  Defaults
-------------------------------------------------------------------------------}

defaultLedgerDbCfg :: LedgerDbCfg (LedgerState TestBlock)
defaultLedgerDbCfg = sampleLedgerDbCfg (SecurityParam 10)

defaultInMemoryBSS :: BackingStoreSelector m
defaultInMemoryBSS = InMemoryBackingStore

defaultLMDB_BSS :: MonadIO m => BackingStoreSelector m
defaultLMDB_BSS = LMDBBackingStore LMDBLimits {
      lmdbMapSize = 100 * 1024 * 1024
    , lmdbMaxDatabases = 3
    , lmdbMaxReaders = 16
    }

-- | Default parameters: empty in-memory backing store, empty changelog.
defaultParams :: InitialMempoolAndModelParams m TestBlock
defaultParams =
    InitialMempoolAndModelParams
      (ledgerStateFromTokens [])
      []
      defaultLedgerDbCfg
      defaultInMemoryBSS

{-------------------------------------------------------------------------------
  Construction of configurations
-------------------------------------------------------------------------------}

sampleLedgerConfig :: SecurityParam -> LedgerConfig TestBlock
sampleLedgerConfig secparam =
  HardFork.defaultEraParams secparam (Time.slotLengthFromSec 2)

sampleLedgerDbCfg :: SecurityParam -> LedgerDbCfg (LedgerState TestBlock)
sampleLedgerDbCfg secparam = LedgerDbCfg {
    ledgerDbCfgSecParam = secparam
  , ledgerDbCfg        = sampleLedgerConfig secparam
  }

{-------------------------------------------------------------------------------
  Construction of initial state
-------------------------------------------------------------------------------}

ledgerStateFromTokens :: [Token] -> LedgerState TestBlock ValuesMK
ledgerStateFromTokens tks = TestLedger {
      lastAppliedPoint      = Block.GenesisPoint
    , payloadDependentState = TestPLDS $ ValuesMK $
                                Map.fromList $ map (,()) tks
    }

-- | @'testBlocksFromTxs' txs@ creates a list of successive 'TestBlock's, where
-- each block corresponds to one of the 'Tx's in @txs@.
--
-- POSTCONDITION: The @i@-th result block contains only transaction @txs !! i@.
-- The length of the resulting list is equal to the length of @txs@.
testBlocksFromTxs :: [Tx] -> [TestBlock]
testBlocksFromTxs []         = []
testBlocksFromTxs (tx0:txs0) = go [firstBlk] txs0
  where
    firstBlk :: TestBlock
    firstBlk = firstBlockWithPayload
                0
                tx0

    -- Create a new block using the latest block in the accumulator as a
    -- predecessor.
    go :: [TestBlock] -> [Tx] -> [TestBlock]
    go [] _                     = error "impossible: there should be a \
                                        \previous block!"
    go acc []                   = reverse acc
    go acc@(prevBlk:_) (tx:txs) = go (nextBlk:acc) txs
      where
        nextBlk = mkSuccessorBlock prevBlk tx

    mkSuccessorBlock :: TestBlock -> Tx -> TestBlock
    mkSuccessorBlock prevBlk =
        successorBlockWithPayload
          (Block.blockHash prevBlk)
          (Block.blockSlot prevBlk)
