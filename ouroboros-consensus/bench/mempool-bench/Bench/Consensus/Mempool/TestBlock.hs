{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Bench.Consensus.Mempool.TestBlock
  ( -- * Test block
    TestBlock

    -- * Initial parameters
  , initialLedgerState
  , sampleLedgerConfig

    -- * Transactions
  , Token (Token)
  , Tx (Tx)
  , initialLedgerTables
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
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import qualified Ouroboros.Consensus.Block as Block
import Ouroboros.Consensus.Config.SecurityParam as Consensus
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import qualified Ouroboros.Consensus.Ledger.Abstract as Ledger
import Ouroboros.Consensus.Ledger.HD (BlockSupportsLedgerHD (Values), TxsDiff (..))
import qualified Ouroboros.Consensus.Ledger.SupportsMempool as Ledger
import qualified Ouroboros.Consensus.Ledger.Tables.Diff as Diff
import Test.Util.TestBlock hiding (TestBlock)

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

initialLedgerState :: LedgerState (TestBlockWith Tx)
initialLedgerState =
  TestLedger
    { lastAppliedPoint = Block.GenesisPoint
    , payloadDependentState = TestPLDS
    }

-- | The genesis ledger tables: no tokens available yet.
initialLedgerTables :: Values TestBlock
initialLedgerTables = Map.empty

sampleLedgerConfig :: Ledger.LedgerConfig TestBlock
sampleLedgerConfig =
  testBlockLedgerConfigFrom $
    HardFork.defaultEraParams
      (Consensus.SecurityParam $ knownNonZeroBounded @10)
      (Time.slotLengthFromSec 2)

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
  -- This block keeps all of its state in the on-disk tables, so the table-free
  -- payload-dependent state is trivial.
  data PayloadDependentState Tx = TestPLDS
    deriving stock (Eq, Show, Generic)
    deriving anyclass (Serialise, NoThunks)

  type PayloadDependentError Tx = TxApplicationError

  type PayloadTxIn Tx = Token
  type PayloadTxOut Tx = ()

  applyPayload tokMap plds tx =
    if Set.null notFound
      then Right (plds, consumedDiff <> producedDiff)
      else Left $ TxApplicationError notFound
   where
    Tx{consumed, produced} = tx
    notFound = Set.filter (not . (`Map.member` tokMap)) consumed

    consumedDiff, producedDiff :: Diff.Diff Token ()
    consumedDiff = Diff.fromListDeletes [(t, ()) | t <- Set.toList consumed]
    producedDiff = Diff.fromListInserts [(t, ()) | t <- Set.toList produced]

  getPayloadKeySets = consumed

-- | TODO: for the time being 'TestBlock' does not have any codec config
data instance Block.CodecConfig TestBlock = TestBlockCodecConfig
  deriving (Show, Generic, NoThunks)

-- | TODO: for the time being 'TestBlock' does not have any storage config
data instance Block.StorageConfig TestBlock = TestBlockStorageConfig
  deriving (Show, Generic, NoThunks)

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

instance Ledger.LedgerSupportsMempool TestBlock where
  applyTx _cfg _shouldIntervene _slot (TestBlockGenTx tx) tickedSt values =
    except $
      fmap (\(st', d) -> (st', TxsDiff d, ValidatedGenTx (TestBlockGenTx tx))) $
        applyDirectlyToPayloadDependentState values tickedSt tx

  reapplyTx cfg slot (ValidatedGenTx genTx) tickedSt values =
    (\(st', d, _) -> (st', d))
      <$> Ledger.applyTx cfg Ledger.DoNotIntervene slot genTx tickedSt values

  -- FIXME: it is ok to use 'DoNotIntervene' here?

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
  txMeasurePhase2 _cfg _values _st _tx = pure Ledger.TrivialTxMeasurePhase2

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
