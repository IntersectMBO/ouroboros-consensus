{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -Wno-orphans #-}
-- | Ledger that throws impure exceptions. This is meant to be used to test
-- exception handling in the local tx submission server.
module Test.Consensus.MiniProtocol.LocalTxSubmission.FaultyLedger (
    FaultInducingBlock
    -- * Initial parameters
  , initialLedgerState
  , sampleLedgerConfig
    -- * Transactions
  , FaultInducingTx (ErrorWith)
  , mkTxThatErrorsWith
  , txSize
    -- * Errors
  , TxApplicationError (TxApplicationError)
  ) where

import qualified Cardano.Slotting.Time as Time
import           Codec.Serialise (Serialise)
import           Control.DeepSeq (NFData)
import           Control.Monad.Trans.Except (except)
import           Data.TreeDiff (ToExpr)
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)
import qualified Ouroboros.Consensus.Block as Block
import           Ouroboros.Consensus.Config.SecurityParam as Consensus
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import qualified Ouroboros.Consensus.Ledger.Basics as Ledger
import qualified Ouroboros.Consensus.Ledger.SupportsMempool as Ledger
import qualified Ouroboros.Consensus.Mempool as Mempool
import           Test.Util.TestBlock (LedgerState (TestLedger),
                     PayloadSemantics (PayloadDependentError, PayloadDependentState, applyPayload),
                     TestBlockWith, applyDirectlyToPayloadDependentState,
                     lastAppliedPoint, payloadDependentState)

{-------------------------------------------------------------------------------
  Block type
-------------------------------------------------------------------------------}

type FaultInducingBlock = TestBlockWith FaultInducingTx

data FaultInducingTx = ErrorWith String
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NoThunks, ToExpr, Serialise, NFData)

{-------------------------------------------------------------------------------
  Initial parameters
-------------------------------------------------------------------------------}

initialLedgerState :: LedgerState FaultInducingBlock
initialLedgerState = TestLedger {
      lastAppliedPoint      = Block.GenesisPoint
    , payloadDependentState = FaultyLedgerState
    }

sampleLedgerConfig :: Ledger.LedgerConfig FaultInducingBlock
sampleLedgerConfig =
  HardFork.defaultEraParams (Consensus.SecurityParam 10) (Time.slotLengthFromSec 2)

{-------------------------------------------------------------------------------
  Payload semantics
-------------------------------------------------------------------------------}

-- | The ledger state is irrelevant for the application of the error inducing payload.
data FaultyLedgerState = FaultyLedgerState
  deriving stock (Generic, Eq, Show)
  deriving anyclass (NoThunks, ToExpr, Serialise)

data TxApplicationError = TxApplicationError
  deriving stock (Generic, Eq, Show)
  deriving anyclass (NoThunks, ToExpr, Serialise)

instance PayloadSemantics FaultInducingTx where
  type PayloadDependentState FaultInducingTx = FaultyLedgerState

  type PayloadDependentError FaultInducingTx = TxApplicationError

  applyPayload _st (ErrorWith msg) = error msg

data instance Block.CodecConfig FaultInducingBlock = TestBlockCodecConfig
  deriving (Show, Generic, NoThunks)

data instance Block.StorageConfig FaultInducingBlock = TestBlockStorageConfig
  deriving (Show, Generic, NoThunks)

{-------------------------------------------------------------------------------
  Mempool support
-------------------------------------------------------------------------------}

newtype instance Ledger.GenTx FaultInducingBlock = FaultInducingGenTx { unGenTx :: FaultInducingTx }
  deriving stock (Generic)
  deriving newtype (Show, NoThunks, Eq, Ord, NFData)

-- | For tests that use this ledger it is not imporant that we calculate the
-- actual size of the transaction in bytes.
txSize :: Ledger.GenTx FaultInducingBlock -> Mempool.TxSizeInBytes
txSize _genTx = 1

mkTxThatErrorsWith :: String -> Ledger.GenTx FaultInducingBlock
mkTxThatErrorsWith = FaultInducingGenTx . ErrorWith

instance Ledger.LedgerSupportsMempool FaultInducingBlock where
  applyTx _cfg _shouldIntervene _slot (FaultInducingGenTx tx) tickedSt =
    except $ fmap (, ValidatedGenTx (FaultInducingGenTx tx))
           $ applyDirectlyToPayloadDependentState tickedSt tx

  reapplyTx cfg slot (ValidatedGenTx genTx) tickedSt =
    fst <$> Ledger.applyTx cfg Ledger.DoNotIntervene slot genTx tickedSt

  -- TODO: tweak this. If we don't want to test the mempool reaching its maximum
  -- capacity then we should set this to a high value.
  txsMaxBytes _ = 20

  txInBlockSize = txSize

  txForgetValidated (ValidatedGenTx tx) = tx

newtype instance Ledger.TxId (Ledger.GenTx FaultInducingBlock) = Id FaultInducingTx
  deriving stock (Generic)
  deriving newtype (Show, Ord, Eq)
  deriving anyclass (NoThunks)

instance Ledger.HasTxId (Ledger.GenTx FaultInducingBlock) where
  txId (FaultInducingGenTx tx) = Id tx

newtype instance Ledger.Validated (Ledger.GenTx FaultInducingBlock) =
    ValidatedGenTx (Ledger.GenTx FaultInducingBlock)
  deriving stock (Generic)
  deriving newtype (Show, NoThunks)

type instance Ledger.ApplyTxErr FaultInducingBlock = TxApplicationError
