{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | The mempool capacity ignores the endorser-block closure capacity.
--
-- No block type in the repository overrides 'Ledger.ebClosureCapacityTxMeasure',
-- so a test block that does is the only way to observe whether
-- 'computeMempoolCapacity' reads it.
module Test.Consensus.Mempool.Capacity (tests) where

import Cardano.Ledger.BaseTypes (knownNonZeroBounded)
import qualified Cardano.Slotting.Time as Time
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import Codec.Serialise
import Control.DeepSeq (NFData)
import qualified Data.Map.Strict as Map
import qualified Data.Measure as Measure
import Data.Void (Void)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import qualified Ouroboros.Consensus.Block as Block
import Ouroboros.Consensus.Block.SupportsPeras (pattern PerasEnabled)
import Ouroboros.Consensus.Config.SecurityParam (SecurityParam (..))
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import Ouroboros.Consensus.Ledger.Abstract
  ( LedgerTables (..)
  , ValuesMK (..)
  , convertMapKind
  )
import qualified Ouroboros.Consensus.Ledger.Abstract as Ledger
import qualified Ouroboros.Consensus.Ledger.SupportsMempool as Ledger
import Ouroboros.Consensus.Ledger.Tables.Utils
import Ouroboros.Consensus.Mempool
  ( MempoolCapacityBytesOverride (..)
  , computeMempoolCapacity
  )
import Ouroboros.Consensus.Storage.LedgerDB
import Ouroboros.Consensus.Ticked (Ticked)
import Ouroboros.Consensus.Util.IndexedMemPack
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Util.TestBlock
  ( TestBlockLedgerConfig
  , TestBlockWith
  , testBlockLedgerConfigFrom
  , testInitLedgerWithState
  )
import qualified Test.Util.TestBlock as TestBlock

tests :: TestTree
tests =
  testGroup
    "Mempool capacity"
    [ testProperty
        "the endorser-block closure capacity does not widen the mempool"
        prop_mempoolCapacityIgnoresEbClosure
    ]

-- | Without an override the mempool holds two blocks, and nothing else.
--
-- 'ebClosureCapacity' is deliberately non-zero, so the property fails if
-- 'computeMempoolCapacity' ever adds it to the block capacity.
prop_mempoolCapacityIgnoresEbClosure :: Property
prop_mempoolCapacityIgnoresEbClosure =
  once $
    computeMempoolCapacity cfg st NoMempoolCapacityBytesOverride
      === Measure.plus oneBlock oneBlock
 where
  oneBlock = Ledger.blockCapacityTxMeasure cfg st

  cfg :: TestBlockLedgerConfig
  cfg =
    testBlockLedgerConfigFrom $
      HardFork.defaultEraParams
        (SecurityParam $ knownNonZeroBounded @10)
        (Time.slotLengthFromSec 2)
        (PerasEnabled ())

  st :: Ledger.TickedLedgerState TestBlock Ledger.EmptyMK
  st = TestBlock.TickedTestLedger $ testInitLedgerWithState NoPayLoadDependentState

type TestBlock = TestBlockWith Tx

-- | The capacity test needs no transaction content, only a block type whose
-- 'Ledger.TxLimits' instance it controls.
data Tx = Tx
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (NoThunks, NFData)

blockCapacity :: Ledger.ByteSize32
blockCapacity = Ledger.ByteSize32 4096

-- | Non-zero, so that a mempool capacity that reads it differs from one that
-- does not.
ebClosureCapacity :: Ledger.ByteSize32
ebClosureCapacity = Ledger.ByteSize32 1024

instance Ledger.TxLimits TestBlock where
  type TxMeasurePhase1 TestBlock = Ledger.IgnoringOverflow Ledger.ByteSize32
  type TxMeasurePhase2 TestBlock = Ledger.TrivialTxMeasurePhase2

  txWireSize _ = 0

  blockCapacityTxMeasure _cfg _st =
    Ledger.TxMeasure
      (Ledger.IgnoringOverflow blockCapacity)
      Ledger.TrivialTxMeasurePhase2

  txMeasurePhase1 _cfg _st _tx = pure $ Ledger.IgnoringOverflow mempty
  txMeasurePhase2 _cfg _st _tx = pure Ledger.TrivialTxMeasurePhase2

  ebClosureCapacityTxMeasure _cfg _st =
    Ledger.TxMeasure
      (Ledger.IgnoringOverflow ebClosureCapacity)
      Ledger.TrivialTxMeasurePhase2

{-------------------------------------------------------------------------------
  Block scaffolding

  Copied from 'Test.Consensus.Mempool.Fairness.TestBlock'. That block fixes its
  block capacity at one byte, which this property cannot use.
-------------------------------------------------------------------------------}

instance TestBlock.PayloadSemantics Tx where
  data PayloadDependentState Tx mk = NoPayLoadDependentState
    deriving (Show, Eq, Ord, Generic, NoThunks)
    deriving anyclass Serialise

  type PayloadDependentError Tx = ()

  applyPayload NoPayLoadDependentState _tx = Right NoPayLoadDependentState

  getPayloadKeySets = const emptyLedgerTables

data instance Block.CodecConfig TestBlock = TestBlockCodecConfig
  deriving (Show, Generic, NoThunks)

data instance Block.StorageConfig TestBlock = TestBlockStorageConfig
  deriving (Show, Generic, NoThunks)

newtype instance Ledger.GenTx TestBlock = TestBlockGenTx Tx
  deriving stock Generic
  deriving newtype (Show, NoThunks, Eq, Ord, NFData)

newtype instance Ledger.Validated (Ledger.GenTx TestBlock)
  = ValidatedGenTx (Ledger.GenTx TestBlock)
  deriving stock Generic
  deriving newtype (Show, NoThunks)

newtype instance Ledger.TxId (Ledger.GenTx TestBlock) = TestBlockTxId Tx
  deriving stock Generic
  deriving newtype (Show, Ord, Eq)
  deriving anyclass NoThunks

instance Ledger.HasTxId (Ledger.GenTx TestBlock) where
  txId (TestBlockGenTx tx) = TestBlockTxId tx

instance Ledger.LedgerSupportsMempool TestBlock where
  applyTx _cfg _shouldIntervene _slot gtx st =
    pure
      ( TestBlock.TickedTestLedger $
          convertMapKind $
            TestBlock.getTickedTestLedger st
      , ValidatedGenTx gtx
      )

  reapplyTx _cfg _slot _gtx gst = pure gst

  txForgetValidated (ValidatedGenTx tx) = tx

  getTransactionKeySets _ = emptyLedgerTables

  mkMempoolApplyTxError = Ledger.nothingMkMempoolApplyTxError

type instance Ledger.ApplyTxErr TestBlock = ()

type instance Ledger.TxIn TestBlock = Void
type instance Ledger.TxOut TestBlock = Void

instance Ledger.LedgerTablesAreTrivial Ledger.LedgerState TestBlock where
  convertMapKind (TestBlock.TestLedger x NoPayLoadDependentState) =
    TestBlock.TestLedger x NoPayLoadDependentState

instance Ledger.LedgerTablesAreTrivial (Ticked Ledger.LedgerState) TestBlock where
  convertMapKind (TestBlock.TickedTestLedger x) =
    TestBlock.TickedTestLedger (Ledger.convertMapKind x)

deriving via Void instance IndexedMemPack Ledger.LedgerState TestBlock Void

instance Ledger.HasLedgerTables Ledger.LedgerState TestBlock where
  projectLedgerTables _ = emptyLedgerTables
  withLedgerTables st _ = convertMapKind st

instance Ledger.HasLedgerTables (Ticked Ledger.LedgerState) TestBlock where
  projectLedgerTables _ = emptyLedgerTables
  withLedgerTables st _ = convertMapKind st

instance Ledger.CanStowLedgerTables (Ledger.LedgerState TestBlock) where
  stowLedgerTables = convertMapKind
  unstowLedgerTables = convertMapKind

instance CanUpgradeLedgerTables Ledger.LedgerState TestBlock where
  upgradeTables _ _ = id

instance Ledger.SerializeTablesWithHint Ledger.LedgerState TestBlock where
  decodeTablesWithHint _ = do
    _ <- CBOR.decodeMapLen
    pure (LedgerTables $ ValuesMK Map.empty)
  encodeTablesWithHint _ _ = CBOR.encodeMapLen 0
