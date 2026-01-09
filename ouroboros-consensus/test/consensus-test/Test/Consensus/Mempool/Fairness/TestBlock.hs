{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Consensus.Mempool.Fairness.TestBlock
  ( TestBlock
  , TestBlock.PayloadDependentState (..)
  , Tx
  , mkGenTx
  , txSize
  , unGenTx
  ) where

import Codec.Serialise
import Control.DeepSeq (NFData)
import Data.Void (Void)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import qualified Ouroboros.Consensus.Block as Block
import Ouroboros.Consensus.Ledger.Abstract
  ( EmptyMK
  , LedgerState
  , convertMapKind
  , trivialLedgerTables
  )
import qualified Ouroboros.Consensus.Ledger.Abstract as Ledger
import qualified Ouroboros.Consensus.Ledger.SupportsMempool as Ledger
import Ouroboros.Consensus.Storage.LedgerDB
import Ouroboros.Consensus.Ticked (Ticked)
import Ouroboros.Consensus.Util.IndexedMemPack
import Test.Util.TestBlock (TestBlockWith)
import qualified Test.Util.TestBlock as TestBlock

type TestBlock = TestBlockWith Tx

-- We use 'Test.Util.TestBlock' because, even though it contains a lot of
-- information we do not actually need for the mempool fairness tests, it
-- already defines most of the many type classes that are needed to open a
-- mempool.

-- | The fairness test for transaction sizes only cares about said aspect.
--
-- We do need to keep track of the transaction id.
--
-- All transactions will be accepted by the mempool.
data Tx = Tx {txNumber :: Int, txSize :: Ledger.ByteSize32} -- TODO(bladyjoker): r/txNumber/txId
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (NoThunks, NFData)

{-------------------------------------------------------------------------------
  Payload semantics
-------------------------------------------------------------------------------}

instance TestBlock.PayloadSemantics Tx where
  data PayloadDependentState Tx mk = NoPayLoadDependentState
    deriving (Show, Eq, Ord, Generic, NoThunks)
    deriving anyclass Serialise

  type PayloadDependentError Tx = ()

  applyPayload NoPayLoadDependentState _tx = Right NoPayLoadDependentState

  getPayloadKeySets = const trivialLedgerTables

data instance Block.CodecConfig TestBlock = TestBlockCodecConfig
  deriving (Show, Generic, NoThunks)

data instance Block.StorageConfig TestBlock = TestBlockStorageConfig
  deriving (Show, Generic, NoThunks)

{-------------------------------------------------------------------------------
  Mempool support
-------------------------------------------------------------------------------}

newtype instance Ledger.GenTx TestBlock = TestBlockGenTx {unGenTx :: Tx}
  deriving stock Generic
  deriving newtype (Show, NoThunks, Eq, Ord, NFData)

newtype instance Ledger.Validated (Ledger.GenTx TestBlock)
  = ValidatedGenTx (Ledger.GenTx TestBlock)
  deriving stock Generic
  deriving newtype (Show, NoThunks)

newtype instance Ledger.TxId (Ledger.GenTx TestBlock) = TestBlockTxId Tx -- TODO(bladyjoker): huh? Why not txNumber?
  deriving stock Generic
  deriving newtype (Show, Ord, Eq)
  deriving anyclass NoThunks

instance Ledger.HasTxId (Ledger.GenTx TestBlock) where
  txId (TestBlockGenTx tx) = TestBlockTxId tx

newtype instance Ledger.TxHash (Ledger.GenTx TestBlock) = TestBlockTxHash Tx
  deriving stock Generic
  deriving newtype (Show, Ord, Eq)
  deriving anyclass NoThunks

instance Ledger.HasTxHash (Ledger.GenTx TestBlock) where
  txHash (TestBlockGenTx tx) = TestBlockTxHash tx

mkGenTx :: Int -> Ledger.ByteSize32 -> Ledger.GenTx TestBlock
mkGenTx anId aSize = TestBlockGenTx $ Tx{txNumber = anId, txSize = aSize}

instance Ledger.LedgerSupportsMempool TestBlock where
  applyTx _cfg _shouldIntervene _slot gtx st =
    pure
      ( TestBlock.TickedTestLedger $
          convertMapKind $
            TestBlock.getTickedTestLedger
              st
      , ValidatedGenTx gtx
      )

  reapplyTx _ _cfg _slot _gtx gst =
    pure $
      TestBlock.TickedTestLedger $
        convertMapKind $
          TestBlock.getTickedTestLedger
            gst

  txForgetValidated (ValidatedGenTx tx) = tx

  getTransactionKeySets _ = trivialLedgerTables

instance Ledger.TxLimits TestBlock where
  type TxMeasure TestBlock = Ledger.IgnoringOverflow Ledger.ByteSize32

  blockCapacityTxMeasure _cfg _st =
    -- The tests will override this value. By using 1, @computeMempoolCapacity@
    -- can be exactly what each test requests.
    Ledger.IgnoringOverflow $ Ledger.ByteSize32 1

  txMeasure _cfg _st = pure . Ledger.IgnoringOverflow . txSize . unGenTx

{-------------------------------------------------------------------------------
  Ledger support (empty tables)
-------------------------------------------------------------------------------}

type instance Ledger.ApplyTxErr TestBlock = ()

type instance Ledger.TxIn (Ledger.LedgerState TestBlock) = Void
type instance Ledger.TxOut (Ledger.LedgerState TestBlock) = Void

deriving via
  Ledger.TrivialLedgerTables (Ledger.LedgerState TestBlock)
  instance
    Ledger.HasLedgerTables (Ledger.LedgerState TestBlock)

deriving via
  Ledger.TrivialLedgerTables (Ledger.LedgerState TestBlock)
  instance
    Ledger.HasLedgerTables (Ticked (Ledger.LedgerState TestBlock))

deriving via
  Void
  instance
    IndexedMemPack (LedgerState TestBlock EmptyMK) Void

instance Ledger.LedgerTablesAreTrivial (Ledger.LedgerState TestBlock) where
  convertMapKind (TestBlock.TestLedger x NoPayLoadDependentState) =
    TestBlock.TestLedger x NoPayLoadDependentState
instance Ledger.LedgerTablesAreTrivial (Ticked (Ledger.LedgerState TestBlock)) where
  convertMapKind (TestBlock.TickedTestLedger x) =
    TestBlock.TickedTestLedger (Ledger.convertMapKind x)
deriving via
  Ledger.TrivialLedgerTables (Ledger.LedgerState TestBlock)
  instance
    Ledger.CanStowLedgerTables (Ledger.LedgerState TestBlock)
deriving via
  Ledger.TrivialLedgerTables (Ledger.LedgerState TestBlock)
  instance
    CanUpgradeLedgerTables (Ledger.LedgerState TestBlock)
