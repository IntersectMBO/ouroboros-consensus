{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
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
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import qualified Ouroboros.Consensus.Block as Block
import Ouroboros.Consensus.Config.SecurityParam as Consensus
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import qualified Ouroboros.Consensus.Ledger.Basics as Ledger
import qualified Ouroboros.Consensus.Ledger.SupportsMempool as Ledger
import Ouroboros.Consensus.Ledger.Tables
import qualified Ouroboros.Consensus.Ledger.Tables.Diff as Diff
import qualified Ouroboros.Consensus.Ledger.Tables.Utils as Ledger
import Ouroboros.Consensus.Util.IndexedMemPack (IndexedMemPack (..))
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

initialLedgerState :: LedgerState (TestBlockWith Tx) ValuesMK
initialLedgerState =
  TestLedger
    { lastAppliedPoint = Block.GenesisPoint
    , payloadDependentState =
        TestPLDS
          { getTestPLDS = ValuesMK Map.empty
          }
    }

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

type instance TxIn (LedgerState TestBlock) = Token
type instance TxOut (LedgerState TestBlock) = ()

instance HasLedgerTables (LedgerState TestBlock) where
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

instance HasLedgerTables (Ticked (LedgerState TestBlock)) where
  projectLedgerTables (TickedTestLedger st) =
    Ledger.castLedgerTables $
      Ledger.projectLedgerTables st
  withLedgerTables (TickedTestLedger st) tables =
    TickedTestLedger $ Ledger.withLedgerTables st $ Ledger.castLedgerTables tables

instance CanStowLedgerTables (LedgerState TestBlock) where
  stowLedgerTables = error "Mempool bench TestBlock unused: stowLedgerTables"
  unstowLedgerTables = error "Mempool bench TestBlock unused: unstowLedgerTables"

instance IndexedMemPack (LedgerState TestBlock EmptyMK) () where
  indexedTypeName _ = typeName @()
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

instance Ledger.LedgerSupportsMempool TestBlock where
  applyTx _cfg _shouldIntervene _slot (TestBlockGenTx tx) tickedSt =
    except $
      fmap ((,ValidatedGenTx (TestBlockGenTx tx)) . Ledger.trackingToDiffs) $
        applyDirectlyToPayloadDependentState tickedSt tx

  reapplyTx _ cfg slot (ValidatedGenTx genTx) tickedSt =
    Ledger.attachAndApplyDiffs tickedSt . fst
      <$> Ledger.applyTx cfg Ledger.DoNotIntervene slot genTx tickedSt

  -- FIXME: it is ok to use 'DoNotIntervene' here?

  txForgetValidated (ValidatedGenTx tx) = tx

  getTransactionKeySets (TestBlockGenTx tx) = getPayloadKeySets tx

  mkMempoolPredicateFailure = Ledger.nothingMkMempoolPredicateFailure

instance Ledger.TxLimits TestBlock where
  type TxMeasure TestBlock = Ledger.IgnoringOverflow Ledger.ByteSize32

  -- We tweaked this in such a way that we test the case in which we exceed the
  -- maximum mempool capacity. The value used here depends on 'txInBlockSize'.
  blockCapacityTxMeasure _cfg _st =
    Ledger.IgnoringOverflow $ Ledger.ByteSize32 20

  txMeasure _cfg _st = pure . Ledger.IgnoringOverflow . txSize

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
