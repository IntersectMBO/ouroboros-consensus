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
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import qualified Ouroboros.Consensus.Block as Block
import qualified Ouroboros.Consensus.Ledger.Basics as Ledger (TickedLedgerState)
import qualified Ouroboros.Consensus.Ledger.SupportsMempool as Ledger
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
data Tx = Tx {txNumber :: Int, txSize :: Ledger.ByteSize32}
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (NoThunks, NFData)

{-------------------------------------------------------------------------------
  Payload semantics
-------------------------------------------------------------------------------}

instance TestBlock.PayloadSemantics Tx where
  data PayloadDependentState Tx = NoPayLoadDependentState
    deriving (Show, Eq, Ord, Generic, NoThunks)
    deriving anyclass Serialise

  type PayloadDependentError Tx = ()

  applyPayload NoPayLoadDependentState _tx = Right NoPayLoadDependentState

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

newtype instance Ledger.TxId (Ledger.GenTx TestBlock) = TestBlockTxId Tx
  deriving stock Generic
  deriving newtype (Show, Ord, Eq)
  deriving anyclass NoThunks

instance Ledger.HasTxId (Ledger.GenTx TestBlock) where
  txId (TestBlockGenTx tx) = TestBlockTxId tx

mkGenTx :: Int -> Ledger.ByteSize32 -> Ledger.GenTx TestBlock
mkGenTx anId aSize = TestBlockGenTx $ Tx{txNumber = anId, txSize = aSize}

-- | TestBlock has no on-disk tables, so the mempool acc is just the ticked
-- ledger state and the per-tx local data is unit.
data instance Ledger.TxLocalData TestBlock = FairnessTxLocalData
  deriving stock Generic
  deriving anyclass NoThunks

newtype instance Ledger.MempoolAcc TestBlock = FairnessMempoolAcc
  {unFairnessMempoolAcc :: Ledger.TickedLedgerState TestBlock}
  deriving stock Generic
  deriving anyclass NoThunks

instance Ledger.LedgerSupportsMempool TestBlock where
  emptyAcc = FairnessMempoolAcc
  accTickedState = unFairnessMempoolAcc

  prepareTx _cfg _slot _tsh _acc _tx = pure FairnessTxLocalData

  applyTx _cfg _wti _slot acc gtx _tld =
    pure (ValidatedGenTx gtx, acc)

  reapplyTx _cfg _slot acc _vtx _tld = pure acc

  txForgetValidated (ValidatedGenTx tx) = tx

  mkMempoolApplyTxError = Ledger.nothingMkMempoolApplyTxError

instance Ledger.TxLimits TestBlock where
  type TxMeasure TestBlock = Ledger.IgnoringOverflow Ledger.ByteSize32

  txWireSize = fromIntegral . Ledger.unByteSize32 . txSize . unGenTx
  blockCapacityTxMeasure _cfg _st =
    -- The tests will override this value. By using 1, @computeMempoolCapacity@
    -- can be exactly what each test requests.
    Ledger.IgnoringOverflow $ Ledger.ByteSize32 1

  txMeasure _cfg _st _tld = pure . Ledger.IgnoringOverflow . txSize . unGenTx

type instance Ledger.ApplyTxErr TestBlock = ()
