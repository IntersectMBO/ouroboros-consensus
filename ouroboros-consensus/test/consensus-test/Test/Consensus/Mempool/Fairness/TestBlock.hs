{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
import qualified Data.Set as Set
import Data.Void (Void)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import qualified Ouroboros.Consensus.Block as Block
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

  type PayloadTxIn Tx = Void
  type PayloadTxOut Tx = Void

  applyPayload _vals NoPayLoadDependentState _tx = Right (NoPayLoadDependentState, mempty)

  getPayloadKeySets = const Set.empty

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

instance Ledger.LedgerSupportsMempool TestBlock where
  applyTx _cfg _shouldIntervene _slot gtx _values st =
    pure (st, mempty, ValidatedGenTx gtx)

  reapplyTx _cfg _slot _gtx _values gst =
    pure (gst, mempty)

  txForgetValidated (ValidatedGenTx tx) = tx

  getTransactionKeySets _ = Set.empty

  mkMempoolApplyTxError = Ledger.nothingMkMempoolApplyTxError

instance Ledger.TxLimits TestBlock where
  type TxMeasurePhase1 TestBlock = Ledger.IgnoringOverflow Ledger.ByteSize32
  type TxMeasurePhase2 TestBlock = Ledger.TrivialTxMeasurePhase2

  txWireSize = fromIntegral . Ledger.unByteSize32 . txSize . unGenTx
  blockCapacityTxMeasure _cfg _st =
    -- The tests will override this value. By using 1, @computeMempoolCapacity@
    -- can be exactly what each test requests.
    Ledger.TxMeasure (Ledger.IgnoringOverflow $ Ledger.ByteSize32 1) Ledger.TrivialTxMeasurePhase2

  txMeasurePhase1 _cfg _st = pure . Ledger.IgnoringOverflow . txSize . unGenTx
  txMeasurePhase2 _cfg _values _st _tx = pure Ledger.TrivialTxMeasurePhase2

{-------------------------------------------------------------------------------
  Ledger support (trivial tables)
-------------------------------------------------------------------------------}

-- The block's @'Test.Util.TestBlock.BlockSupportsLedgerHD'@ (and the rest of the
-- UTxO-HD axis) is provided generically by 'TestBlockWith' through the trivial
-- @'PayloadTxIn' = 'PayloadTxOut' = 'Void'@ instance above.

type instance Ledger.ApplyTxErr TestBlock = ()
