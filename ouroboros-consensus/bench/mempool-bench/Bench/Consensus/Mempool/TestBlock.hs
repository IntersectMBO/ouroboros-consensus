{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Bench.Consensus.Mempool.TestBlock (
    -- * Test block
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

import           Cardano.Ledger.BaseTypes (knownNonZeroBounded)
import qualified Cardano.Slotting.Time as Time
import           Codec.Serialise (Serialise)
import           Control.DeepSeq (NFData)
import           Control.Monad.Trans.Except (except)
import           Data.Set (Set, (\\))
import qualified Data.Set as Set
import           Data.TreeDiff (ToExpr)
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)
import qualified Ouroboros.Consensus.Block as Block
import           Ouroboros.Consensus.Config.SecurityParam as Consensus
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import qualified Ouroboros.Consensus.Ledger.Basics as Ledger
import qualified Ouroboros.Consensus.Ledger.SupportsMempool as Ledger
import           Test.Util.TestBlock (LedgerState (TestLedger),
                     PayloadSemantics (PayloadDependentError, PayloadDependentState, applyPayload),
                     TestBlockWith, applyDirectlyToPayloadDependentState,
                     lastAppliedPoint, payloadDependentState,
                     testBlockLedgerConfigFrom)

{-------------------------------------------------------------------------------
  MempoolTestBlock
-------------------------------------------------------------------------------}

type TestBlock = TestBlockWith Tx

data Tx = Tx {
    consumed :: !(Set Token)
  , produced :: !(Set Token)
  }
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (NoThunks, NFData)

newtype Token = Token { unToken :: Int  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NoThunks, ToExpr, Serialise, NFData)

{-------------------------------------------------------------------------------
  Initial parameters
-------------------------------------------------------------------------------}

initialLedgerState :: LedgerState (TestBlockWith Tx)
initialLedgerState = TestLedger {
      lastAppliedPoint      = Block.GenesisPoint
    , payloadDependentState = TestLedgerState {
          availableTokens = Set.empty :: Set Token
        }
    }

sampleLedgerConfig :: Ledger.LedgerConfig TestBlock
sampleLedgerConfig = testBlockLedgerConfigFrom $
  HardFork.defaultEraParams (Consensus.SecurityParam $ knownNonZeroBounded @10) (Time.slotLengthFromSec 2)

{-------------------------------------------------------------------------------
  Payload semantics
-------------------------------------------------------------------------------}

data TestLedgerState = TestLedgerState {
    availableTokens :: !(Set Token)
  }
  deriving stock (Generic, Eq, Show)
  deriving anyclass (NoThunks, ToExpr, Serialise)

data TxApplicationError =
    -- | The transaction could not be applied due to the given unavailable tokens.
    TxApplicationError { unavailable :: Set Token }
  deriving stock (Generic, Eq, Show)
  deriving anyclass (NoThunks, ToExpr, Serialise)

instance PayloadSemantics Tx where
  type PayloadDependentState Tx = TestLedgerState

  type PayloadDependentError Tx = TxApplicationError

  applyPayload st@TestLedgerState { availableTokens } Tx { consumed, produced } =
    let
      notFound = Set.filter (not . (`Set.member` availableTokens)) consumed
    in if Set.null notFound
       then Right $ st{ availableTokens = availableTokens \\ consumed <> produced }
       else Left  $ TxApplicationError notFound

-- | TODO: for the time being 'TestBlock' does not have any codec config
data instance Block.CodecConfig TestBlock = TestBlockCodecConfig
  deriving (Show, Generic, NoThunks)

-- | TODO: for the time being 'TestBlock' does not have any storage config
data instance Block.StorageConfig TestBlock = TestBlockStorageConfig
  deriving (Show, Generic, NoThunks)

{-------------------------------------------------------------------------------
  Mempool support
-------------------------------------------------------------------------------}

newtype instance Ledger.GenTx TestBlock = TestBlockGenTx { unGenTx :: Tx }
  deriving stock (Generic)
  deriving newtype (Show, NoThunks, Eq, Ord, NFData)

-- | For the mempool tests and benchmarks it is not imporant that we calculate
-- the actual size of the transaction in bytes.
txSize :: Ledger.GenTx TestBlock -> Ledger.ByteSize32
txSize (TestBlockGenTx tx) =
    Ledger.ByteSize32
  $ fromIntegral
  $ 1 + length (consumed tx) + length (produced tx)

mkTx ::
     [Token]
     -- ^ Consumed
  -> [Token]
     -- ^ Produced
  -> Ledger.GenTx TestBlock
mkTx cons prod = TestBlockGenTx $ Tx { consumed = Set.fromList cons
                                     , produced = Set.fromList prod
                                     }

instance Ledger.LedgerSupportsMempool TestBlock where
  applyTx _cfg _shouldIntervene _slot (TestBlockGenTx tx) tickedSt =
    except $ fmap (, ValidatedGenTx (TestBlockGenTx tx))
           $ applyDirectlyToPayloadDependentState tickedSt tx

  reapplyTx cfg slot (ValidatedGenTx genTx) tickedSt =
    fst <$> Ledger.applyTx cfg Ledger.DoNotIntervene slot genTx tickedSt
    -- FIXME: it is ok to use 'DoNotIntervene' here?

  txForgetValidated (ValidatedGenTx tx) = tx

instance Ledger.TxLimits TestBlock where
  type TxMeasure TestBlock = Ledger.IgnoringOverflow Ledger.ByteSize32

  txWireSize = fromIntegral . Ledger.unByteSize32 . txSize
  -- We tweaked this in such a way that we test the case in which we exceed the
  -- maximum mempool capacity. The value used here depends on 'txInBlockSize'.
  blockCapacityTxMeasure _cfg _st =
    Ledger.IgnoringOverflow $ Ledger.ByteSize32 20

  txMeasure _cfg _st = pure . Ledger.IgnoringOverflow . txSize

newtype instance Ledger.TxId (Ledger.GenTx TestBlock) = TestBlockTxId Tx
  deriving stock (Generic)
  deriving newtype (Show, Ord, Eq)
  deriving anyclass (NoThunks)

instance Ledger.HasTxId (Ledger.GenTx TestBlock) where
  txId (TestBlockGenTx tx) = TestBlockTxId tx

newtype instance Ledger.Validated (Ledger.GenTx TestBlock) =
    ValidatedGenTx (Ledger.GenTx TestBlock)
  deriving stock (Generic)
  deriving newtype (Show, NoThunks)

type instance Ledger.ApplyTxErr TestBlock = TxApplicationError
