{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}

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

import           Cardano.Binary (FromCBOR (..), ToCBOR (..))
import qualified Cardano.Slotting.Time as Time
import           Codec.Serialise (Serialise (..))
import           Control.DeepSeq (NFData)
import           Control.Monad.Trans.Except (except)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.TreeDiff (ToExpr)
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)
import qualified Ouroboros.Consensus.Block as Block
import           Ouroboros.Consensus.Config.SecurityParam as Consensus
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import qualified Ouroboros.Consensus.Ledger.Basics as Ledger
import qualified Ouroboros.Consensus.Ledger.SupportsMempool as Ledger
import           Ouroboros.Consensus.Ledger.Tables
import qualified Ouroboros.Consensus.Ledger.Tables.UtxoDiff as Diff
import qualified Ouroboros.Consensus.Ledger.Tables.Utils as Ledger
import qualified Ouroboros.Consensus.Mempool as Mempool
import           Test.Util.TestBlock hiding (TestBlock)

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
  deriving newtype (ToCBOR, FromCBOR, Num, Enum)
  deriving anyclass (NoThunks, ToExpr, Serialise, NFData)

mkTx ::
     [Token]
     -- ^ Consumed
  -> [Token]
     -- ^ Produced
  -> Ledger.GenTx TestBlock
mkTx cons prod = TestBlockGenTx $ Tx { consumed = Set.fromList cons
                                     , produced = Set.fromList prod
                                     }

{-------------------------------------------------------------------------------
  Initial parameters
-------------------------------------------------------------------------------}

initialLedgerState :: LedgerState (TestBlockWith Tx) ValuesMK
initialLedgerState = TestLedger {
      lastAppliedPoint      = Block.GenesisPoint
    , payloadDependentState = TestPLDS {
          getTestPLDS = ValuesMK Map.empty
        }
    }

sampleLedgerConfig :: Ledger.LedgerConfig TestBlock
sampleLedgerConfig = testBlockLedgerConfigFrom $
  HardFork.defaultEraParams (Consensus.SecurityParam 10) (Time.slotLengthFromSec 2)

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
  newtype instance PayloadDependentState Tx mk = TestPLDS {
      getTestPLDS :: mk Token ()
    }
    deriving stock Generic

  type PayloadDependentError Tx = TxApplicationError

  applyPayload plds tx =
      let
        notFound = Set.filter (not . (`Map.member` tokMap)) consumed
      in if Set.null notFound
        then Right $ TestPLDS (Ledger.rawAttachAndApplyDiffs fullDiff toks)
        else Left  $ TxApplicationError notFound
    where
      TestPLDS toks@(ValuesMK tokMap) = plds
      Tx {consumed, produced}         = tx

      consumedDiff, producedDiff :: Diff.UtxoDiff Token ()
      consumedDiff = Diff.fromListDeletes [(t, ()) | t <- Set.toList consumed]
      producedDiff = Diff.fromListInserts [(t, ()) | t <- Set.toList produced]

      fullDiff :: DiffMK Token ()
      fullDiff = case Diff.AUtxoDiff consumedDiff <> Diff.AUtxoDiff producedDiff of
        Diff.NotAUtxoDiff -> error "Violation of the UTxO property when applying a transaction!"
        Diff.AUtxoDiff d -> DiffMK d

  getPayloadKeySets tx = LedgerTables $ KeysMK $ consumed <> produced
    where
      Tx {consumed, produced} = tx

deriving stock instance EqMK mk
                        => Eq (PayloadDependentState Tx mk)
deriving stock instance ShowMK mk
                        => Show (PayloadDependentState Tx mk)
deriving anyclass instance NoThunksMK mk
                        => NoThunks (PayloadDependentState Tx mk)

instance Serialise (PayloadDependentState Tx EmptyMK) where
  encode = error "unused: encode"
  decode = error "unused: decode"

-- | TODO: for the time being 'TestBlock' does not have any codec config
data instance Block.CodecConfig TestBlock = TestBlockCodecConfig
  deriving (Show, Generic, NoThunks)

-- | TODO: for the time being 'TestBlock' does not have any storage config
data instance Block.StorageConfig TestBlock = TestBlockStorageConfig
  deriving (Show, Generic, NoThunks)

{-------------------------------------------------------------------------------
  Ledger tables
-------------------------------------------------------------------------------}

type instance Key   (LedgerState TestBlock) = Token
type instance Value (LedgerState TestBlock) = ()

instance HasLedgerTables (LedgerState TestBlock) where
  projectLedgerTables st =
    LedgerTables $ getTestPLDS $ payloadDependentState st
  withLedgerTables st table = st {
        payloadDependentState = plds {
            getTestPLDS = Ledger.getLedgerTables table
          }
      }
    where
      TestLedger { payloadDependentState = plds } = st

instance HasLedgerTables (Ticked1 (LedgerState TestBlock)) where
  projectLedgerTables (TickedTestLedger st) = Ledger.castLedgerTables $
    Ledger.projectLedgerTables st
  withLedgerTables (TickedTestLedger st) tables =
    TickedTestLedger $ Ledger.withLedgerTables st $ Ledger.castLedgerTables tables

instance CanSerializeLedgerTables (LedgerState TestBlock)

instance CanStowLedgerTables (LedgerState TestBlock) where
  stowLedgerTables     = error "unused: stowLedgerTables"
  unstowLedgerTables   = error "unused: unstowLedgerTables"

{-------------------------------------------------------------------------------
  Mempool support
-------------------------------------------------------------------------------}

newtype instance Ledger.GenTx TestBlock = TestBlockGenTx { unGenTx :: Tx }
  deriving stock (Generic)
  deriving newtype (Show, NoThunks, Eq, Ord, NFData)

-- | For the mempool tests and benchmarks it is not imporant that we calculate
-- the actual size of the transaction in bytes.
txSize :: Ledger.GenTx TestBlock -> Mempool.TxSizeInBytes
txSize (TestBlockGenTx tx) = fromIntegral $ 1 + length (consumed tx) + length (produced tx)

instance Ledger.LedgerSupportsMempool TestBlock where
  applyTx _cfg _shouldIntervene _slot (TestBlockGenTx tx) tickedSt =
    except $ fmap ((, ValidatedGenTx (TestBlockGenTx tx)) . Ledger.forgetTrackingValues)
           $ applyDirectlyToPayloadDependentState tickedSt tx

  reapplyTx cfg slot (ValidatedGenTx genTx) tickedSt =
    Ledger.applyDiffs tickedSt . fst <$> Ledger.applyTx cfg Ledger.DoNotIntervene slot genTx tickedSt
    -- FIXME: it is ok to use 'DoNotIntervene' here?

  -- We tweaked this in such a way that we test the case in which we exceed the
  -- maximum mempool capacity. The value used here depends on 'txInBlockSize'.
  txsMaxBytes _ = 20

  txInBlockSize = txSize

  txForgetValidated (ValidatedGenTx tx) = tx

  txRefScriptSize _cfg _tlst _tx = 0

  getTransactionKeySets (TestBlockGenTx tx) = LedgerTables $
    KeysMK $ consumed tx

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
