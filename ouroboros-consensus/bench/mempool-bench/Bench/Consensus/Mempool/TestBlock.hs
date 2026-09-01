{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Bench.Consensus.Mempool.TestBlock
  ( -- * Test block
    TestBlock

    -- * Initial parameters
  , initialLedgerState
  , mkInitialLedgerState
  , advanceTip
  , sampleLedgerConfig

    -- * Transactions
  , Token (Token)
  , Tx (Tx)
  , mkTx
  , txSize
  ) where

import Cardano.Ledger.BaseTypes (knownNonZeroBounded)
import qualified Cardano.Slotting.Time as Time
import Codec.Serialise (Serialise (..))
import Control.DeepSeq (NFData)
import Control.Monad.Trans.Except (except)
import qualified Data.Map.Strict as Map
import Data.MemPack
import Data.Set (Set)
import qualified Data.Set as Set
import Data.TreeDiff (ToExpr)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import qualified Ouroboros.Consensus.Block as Block
import Ouroboros.Consensus.Block.SupportsPeras (pattern PerasEnabled)
import Ouroboros.Consensus.Config.SecurityParam as Consensus
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import qualified Ouroboros.Consensus.Ledger.Abstract as Ledger
import qualified Ouroboros.Consensus.Ledger.SupportsMempool as Ledger
import Ouroboros.Consensus.Ledger.Tables
import qualified Ouroboros.Consensus.Ledger.Tables.Diff as Diff
import qualified Ouroboros.Consensus.Ledger.Tables.Utils as Ledger
import Ouroboros.Consensus.Util.IndexedMemPack (IndexedMemPack (..))
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)
import Test.Util.TestBlock hiding (TestBlock)
import Text.Read (readMaybe)

{-------------------------------------------------------------------------------
  MempoolTestBlock
-------------------------------------------------------------------------------}

type TestBlock = TestBlockWith Tx

data Tx = Tx
  { consumed :: !(Set Token)
  , produced :: !(Set Token)
  }
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (NoThunks, NFData)

newtype Token = Token {unToken :: Int}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (MemPack, Num, Enum)
  deriving anyclass (NoThunks, ToExpr, Serialise, NFData)

mkTx ::
  -- | Consumed
  [Token] ->
  -- | Produced
  [Token] ->
  Ledger.GenTx TestBlock
mkTx cons prod =
  TestBlockGenTx $
    Tx
      { consumed = Set.fromList cons
      , produced = Set.fromList prod
      }

{-------------------------------------------------------------------------------
  Initial parameters
-------------------------------------------------------------------------------}

initialLedgerState :: LedgerState (TestBlockWith Tx) ValuesMK
initialLedgerState = mkInitialLedgerState []

-- | Like 'initialLedgerState' but seeded with a set of available tokens (the
-- UTxO). Chains of transactions can then be built by consuming a seed token and
-- producing the next one.
mkInitialLedgerState :: [Token] -> LedgerState (TestBlockWith Tx) ValuesMK
mkInitialLedgerState toks =
  TestLedger
    { lastAppliedPoint = Block.GenesisPoint
    , payloadDependentState =
        TestPLDS
          { getTestPLDS = ValuesMK (Map.fromList [(t, ()) | t <- toks])
          }
    }

-- | Move the tip to a fresh point (distinct per @n@) while keeping the ledger
-- tables unchanged. Used to force the mempool to resync/revalidate against a
-- "new" tip without invalidating any of its transactions.
advanceTip ::
  Word64 -> LedgerState (TestBlockWith Tx) ValuesMK -> LedgerState (TestBlockWith Tx) ValuesMK
advanceTip n st =
  st{lastAppliedPoint = Block.blockPoint (firstBlockWithPayload n (Tx Set.empty Set.empty))}

sampleLedgerConfig :: Ledger.LedgerConfig TestBlock
sampleLedgerConfig =
  testBlockLedgerConfigFrom $
    HardFork.defaultEraParams
      (Consensus.SecurityParam $ knownNonZeroBounded @10)
      (Time.slotLengthFromSec 2)
      (PerasEnabled ())

{-------------------------------------------------------------------------------
  Payload semantics
-------------------------------------------------------------------------------}

data TestLedgerState = TestLedgerState
  { availableTokens :: !(Set Token)
  }
  deriving stock (Generic, Eq, Show)
  deriving anyclass (NoThunks, ToExpr, Serialise)

data TxApplicationError
  = -- | The transaction could not be applied due to the given unavailable tokens.
    TxApplicationError {unavailable :: Set Token}
  deriving stock (Generic, Eq, Show)
  deriving anyclass (NoThunks, ToExpr, Serialise)

instance PayloadSemantics Tx where
  newtype PayloadDependentState Tx mk = TestPLDS
    { getTestPLDS :: mk Token ()
    }
    deriving stock Generic

  type PayloadDependentError Tx = TxApplicationError

  applyPayload plds tx =
    let
      notFound = Set.filter (not . (`Map.member` tokMap)) consumed
     in
      if Set.null notFound
        then Right $ TestPLDS (Ledger.rawAttachAndApplyDiffs toks fullDiff)
        else Left $ TxApplicationError notFound
   where
    TestPLDS toks@(ValuesMK tokMap) = plds
    Tx{consumed, produced} = tx

    consumedDiff, producedDiff :: Diff.Diff Token ()
    consumedDiff = Diff.fromListDeletes [(t, ()) | t <- Set.toList consumed]
    producedDiff = Diff.fromListInserts [(t, ()) | t <- Set.toList produced]

    fullDiff :: DiffMK Token ()
    fullDiff = DiffMK $ consumedDiff <> producedDiff

  getPayloadKeySets tx = LedgerTables $ KeysMK consumed
   where
    Tx{consumed} = tx

deriving stock instance
  EqMK mk =>
  Eq (PayloadDependentState Tx mk)
deriving stock instance
  ShowMK mk =>
  Show (PayloadDependentState Tx mk)
deriving anyclass instance
  NoThunksMK mk =>
  NoThunks (PayloadDependentState Tx mk)

instance Serialise (PayloadDependentState Tx EmptyMK) where
  encode = error "Mempool bench TestBlock unused: encode"
  decode = error "Mempool bench TestBlock unused: decode"

-- | TODO: for the time being 'TestBlock' does not have any codec config
data instance Block.CodecConfig TestBlock = TestBlockCodecConfig
  deriving (Show, Generic, NoThunks)

-- | TODO: for the time being 'TestBlock' does not have any storage config
data instance Block.StorageConfig TestBlock = TestBlockStorageConfig
  deriving (Show, Generic, NoThunks)

{-------------------------------------------------------------------------------
  Ledger tables
-------------------------------------------------------------------------------}

type instance TxIn TestBlock = Token
type instance TxOut TestBlock = ()

instance HasLedgerTables LedgerState TestBlock where
  projectLedgerTables st =
    LedgerTables $ getTestPLDS $ payloadDependentState st
  withLedgerTables st table =
    st
      { payloadDependentState =
          plds
            { getTestPLDS = Ledger.getLedgerTables table
            }
      }
   where
    TestLedger{payloadDependentState = plds} = st

instance HasLedgerTables (Ticked LedgerState) TestBlock where
  projectLedgerTables (TickedTestLedger st) =
    Ledger.projectLedgerTables st
  withLedgerTables (TickedTestLedger st) tables =
    TickedTestLedger $ Ledger.withLedgerTables st tables

instance CanStowLedgerTables (LedgerState TestBlock) where
  stowLedgerTables = error "Mempool bench TestBlock unused: stowLedgerTables"
  unstowLedgerTables = error "Mempool bench TestBlock unused: unstowLedgerTables"

instance IndexedMemPack LedgerState TestBlock () where
  indexedTypeName _ _ = typeName @()
  indexedPackedByteCount _ = packedByteCount
  indexedPackM _ = packM
  indexedUnpackM _ = unpackM

{-------------------------------------------------------------------------------
  Mempool support
-------------------------------------------------------------------------------}

newtype instance Ledger.GenTx TestBlock = TestBlockGenTx {unGenTx :: Tx}
  deriving stock Generic
  deriving newtype (Show, NoThunks, Eq, Ord, NFData)

-- | For the mempool tests and benchmarks it is not imporant that we calculate
-- the actual size of the transaction in bytes.
txSize :: Ledger.GenTx TestBlock -> Ledger.ByteSize32
txSize (TestBlockGenTx tx) =
  Ledger.ByteSize32 $
    fromIntegral $
      1 + length (consumed tx) + length (produced tx)

-- | Simulated CPU cost, in microseconds, of /fully/ validating a transaction
-- (the script and signature checks a real ledger performs in 'applyTx'). Set via
-- @MEMPOOL_APPLY_CPU_US@; defaults to @0@ (no injected cost) so this shared
-- 'TestBlock' does not slow down the criterion @mempool-bench@ that CI
-- regression-gates. The @mempool-state-bench@ opts in — it sets the env var from
-- its @--apply-us@ flag (default 128us, the measured median applyBlock cost on
-- an nvme SSD, see input-output-hk/ouroboros-leios#553) before this CAF is
-- forced.
--
-- Together with 'reapplyCpuMicros' this lets a benchmark reproduce the
-- real-node relationship @reapply ≪ apply@: reapplication skips the expensive
-- checks, so a mempool sync (which only reapplies) is strictly cheaper per tx
-- than ingestion (which fully validates). The mempool-state-bench relies on
-- this for its sync-vs-ingest convergence to be faithful.
{-# NOINLINE applyCpuMicros #-}
applyCpuMicros :: Int
applyCpuMicros = envInt "MEMPOOL_APPLY_CPU_US" 0

-- | Simulated CPU cost, in microseconds, of /reapplying/ an already-validated
-- transaction. Set via @MEMPOOL_REAPPLY_CPU_US@; defaults to @0@ (see
-- 'applyCpuMicros'). The @mempool-state-bench@ sets it from its @--reapply-us@
-- flag (default 20us, an estimate kept well below @--apply-us@ to model
-- @reapply ≪ apply@, as reapply skips the expensive script/signature checks).
{-# NOINLINE reapplyCpuMicros #-}
reapplyCpuMicros :: Int
reapplyCpuMicros = envInt "MEMPOOL_REAPPLY_CPU_US" 0

{-# NOINLINE envInt #-}
envInt :: String -> Int -> Int
envInt k d = unsafePerformIO $ maybe d id . (>>= readMaybe) <$> lookupEnv k

-- | Busy-wait (burning CPU, /not/ sleeping — validation contends for cores)
-- for @us@ microseconds, then return @tx@. The result is @tx@ itself and the
-- caller feeds it into the ledger transition, so the spin depends on the tx
-- and cannot be shared across calls or optimised away.
{-# NOINLINE burnCpuMicros #-}
burnCpuMicros :: Int -> Tx -> Tx
burnCpuMicros us tx
  | us <= 0 = tx
  | otherwise = unsafePerformIO $ do
      let targetNs = fromIntegral us * 1000 :: Word64
      start <- getMonotonicTimeNSec
      let go = do
            now <- getMonotonicTimeNSec
            if now - start >= targetNs then pure tx else go
      go

instance Ledger.LedgerSupportsMempool TestBlock where
  applyTx _cfg _shouldIntervene _slot (TestBlockGenTx tx) tickedSt =
    except $
      fmap ((,ValidatedGenTx (TestBlockGenTx tx)) . Ledger.trackingToDiffs) $
        -- Pay the (simulated) full-validation cost. 'burnCpuMicros' returns the
        -- tx we then apply, so it is forced as part of producing the result.
        applyDirectlyToPayloadDependentState tickedSt (burnCpuMicros applyCpuMicros tx)

  -- Reapplication does /not/ route through 'applyTx' (which would pay the full
  -- validation cost); it runs the ledger transition directly, paying only the
  -- much cheaper 'reapplyCpuMicros'.
  reapplyTx _cfg _slot (ValidatedGenTx (TestBlockGenTx tx)) tickedSt =
    except $
      Ledger.applyDiffs tickedSt . Ledger.trackingToDiffs
        <$> applyDirectlyToPayloadDependentState tickedSt (burnCpuMicros reapplyCpuMicros tx)

  txForgetValidated (ValidatedGenTx tx) = tx

  getTransactionKeySets (TestBlockGenTx tx) = getPayloadKeySets tx

  mkMempoolApplyTxError = Ledger.nothingMkMempoolApplyTxError

instance Ledger.TxLimits TestBlock where
  type TxMeasurePhase1 TestBlock = Ledger.IgnoringOverflow Ledger.ByteSize32
  type TxMeasurePhase2 TestBlock = Ledger.TrivialTxMeasurePhase2

  txWireSize = fromIntegral . Ledger.unByteSize32 . txSize

  -- We tweaked this in such a way that we test the case in which we exceed the
  -- maximum mempool capacity. The value used here depends on 'txInBlockSize'.
  blockCapacityTxMeasure _cfg _st =
    Ledger.TxMeasure (Ledger.IgnoringOverflow $ Ledger.ByteSize32 20) Ledger.TrivialTxMeasurePhase2

  txMeasurePhase1 _cfg _st = pure . Ledger.IgnoringOverflow . txSize
  txMeasurePhase2 _cfg _st _tx = pure Ledger.TrivialTxMeasurePhase2

newtype instance Ledger.TxId (Ledger.GenTx TestBlock) = TestBlockTxId Tx
  deriving stock Generic
  deriving newtype (Show, Ord, Eq)
  deriving anyclass NoThunks

instance Ledger.HasTxId (Ledger.GenTx TestBlock) where
  txId (TestBlockGenTx tx) = TestBlockTxId tx

newtype instance Ledger.Validated (Ledger.GenTx TestBlock)
  = ValidatedGenTx (Ledger.GenTx TestBlock)
  deriving stock Generic
  deriving newtype (Show, NoThunks)

type instance Ledger.ApplyTxErr TestBlock = TxApplicationError
