{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Ouroboros.Storage.LedgerDB.StateMachine.TestBlock
  ( TestBlock
  , extLedgerDbConfig
  , genBlocks
  , genesis
  , genesisValues
  ) where

import Cardano.Binary
  ( FromCBOR (..)
  , ToCBOR (..)
  , serialize'
  , unsafeDeserialize'
  )
import Cardano.Ledger.BaseTypes (NonZero (..))
import qualified Cardano.Slotting.Slot as WithOrigin
import Codec.Serialise (Serialise)
import qualified Codec.Serialise as S
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Map.Diff.Strict.Internal as DS
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict
import Data.MemPack
import Data.Set (Set)
import qualified Data.Set as Set
import Data.TreeDiff
import Data.Word
import GHC.Generics (Generic)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.HardFork.Abstract
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.Extended
import qualified Ouroboros.Consensus.Ledger.Tables.Diff as Diff
import Ouroboros.Consensus.Storage.LedgerDB.API
import Ouroboros.Consensus.Storage.LedgerDB.V2.LSM (BlockSupportsLSM (..))
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Network.Block (Point (Point))
import Ouroboros.Network.Point (Block (Block))
import qualified Test.QuickCheck as QC
import Test.Tasty.QuickCheck
import Test.Util.Orphans.Arbitrary ()
import Test.Util.TestBlock hiding
  ( TestBlock
  , TestBlockCodecConfig
  , TestBlockStorageConfig
  )
import Test.Util.ToExpr ()
import Prelude hiding (elem)

{-------------------------------------------------------------------------------
  TestBlock
-------------------------------------------------------------------------------}

type TestBlock = TestBlockWith Tx

-- | Mock of a UTxO transaction where exactly one (transaction) input is
-- consumed and exactly one output is produced.
data Tx = Tx
  { consumed :: Token
  -- ^ Input that the transaction consumes.
  , produced :: (Token, TValue)
  -- ^ Ouptupt that the transaction produces.
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Serialise, NoThunks, ToExpr)

-- | A token is an identifier for the values produced and consumed by the
-- 'TestBlock' transactions.
--
-- This is analogous to @TxId@: it's how we identify what's in the table. It's
-- also analogous to @TxIn@, since we trivially only have one output per 'Tx'.
newtype Token = Token {unToken :: Point TestBlock}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Serialise, NoThunks, ToExpr, QC.Arbitrary)

instance QC.Arbitrary (Point TestBlock) where
  arbitrary = do
    slot <- SlotNo <$> QC.arbitrary
    hash <- fmap TestHash $ (:|) <$> QC.arbitrary <*> QC.arbitrary
    pure $ Point $ WithOrigin.At $ Block slot hash

-- | Unit of value associated with the output produced by a transaction.
newtype TValue = TValue ()
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Serialise, NoThunks, ToExpr, MemPack)

{-------------------------------------------------------------------------------
  A ledger semantics for TestBlock
-------------------------------------------------------------------------------}

data TxErr
  = TokenWasAlreadyCreated Token
  | TokenDoesNotExist Token
  deriving stock (Generic, Eq, Show)
  deriving anyclass (NoThunks, Serialise, ToExpr)

instance PayloadSemantics Tx where
  -- The on-disk UTxO table flows as the block's 'Values'\/'Diff'. The
  -- table-free payload-dependent state only keeps the history of tokens that
  -- ever existed.
  data PayloadDependentState Tx
    = UTxTok
    { -- \| All the tokens that ever existed. We use this to
      -- make sure a token is not created more than once. See
      -- the definition of 'applyPayload'.
      utxhist :: Set Token
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (Serialise, NoThunks)

  type PayloadDependentError Tx = TxErr

  type PayloadTxIn Tx = Token
  type PayloadTxOut Tx = TValue

  -- We need to exercise the HD backend. This requires that the block
  -- application semantics satisfy:
  --
  -- \* a key is deleted at most once
  -- \* a key is inserted at most once
  --
  applyPayload vals st Tx{consumed, produced} = do
    -- the consumed token must currently exist in the tables
    if Map.member consumed vals
      then Right ()
      else Left $ TokenDoesNotExist consumed
    let (producedTok, producedVal) = produced
    -- the produced token must never have existed
    if producedTok `Set.member` utxhist st
      then Left $ TokenWasAlreadyCreated producedTok
      else Right ()
    let st' = st{utxhist = Set.insert producedTok (utxhist st)}
        diff =
          Diff.fromSetDeletes (Set.singleton consumed)
            <> Diff.fromListInserts [(producedTok, producedVal)]
    pure (st', diff)

  getPayloadKeySets Tx{consumed} = Set.singleton consumed

{-------------------------------------------------------------------------------
  Instances required for on-disk storage of ledger state tables

  The block's 'BlockSupportsLedgerHD' (and the rest of the UTxO-HD axis) is
  provided generically by 'TestBlockWith' through the 'PayloadTxIn'\/
  'PayloadTxOut' instance above. We only need the per-entry codecs.
-------------------------------------------------------------------------------}

instance ToCBOR Token where
  toCBOR (Token pt) = S.encode pt

instance FromCBOR Token where
  fromCBOR = fmap Token S.decode

instance MemPack Token where
  packM = packM . serialize'
  packedByteCount = packedByteCount . serialize'
  unpackM = unsafeDeserialize' <$> unpackM

-- | This single-era block dispatches the LSM backend's era-tagged operations
-- straight to itself.
instance BlockSupportsLSM (TestBlockWith Tx) where
  withKeysEra keys k = k (Proxy @TestBlock) keys id
  withDiffEra d k = k (Proxy @TestBlock) d
  withValuesEra v k = k (Proxy @TestBlock) v

deriving anyclass instance ToExpr v => ToExpr (DS.Delta v)
deriving anyclass instance (ToExpr k, ToExpr v) => ToExpr (DS.Diff k v)
deriving anyclass instance ToExpr v => ToExpr (StrictMaybe v)
deriving instance ToExpr (PayloadDependentState Tx)

instance ToExpr v => ToExpr (DS.DeltaHistory v) where
  toExpr h = App "DeltaHistory" [genericToExpr . toList . DS.getDeltaHistory $ h]

instance ToExpr (LedgerState (TestBlockWith Tx)) where
  toExpr = genericToExpr

instance HasHardForkHistory TestBlock where
  type HardForkIndices TestBlock = '[TestBlock]
  hardForkSummary = neverForksHardForkSummary tblcHardForkParams

{-------------------------------------------------------------------------------
  TestBlock generation

  When we added support for storing parts of the ledger state on disk we needed
  to exercise this new functionality. Therefore, we modified this test so that
  the ledger state associated to the test block contained tables (key-value
  maps) to be stored on disk. This ledger state needs to follow an evolution
  pattern similar to the UTxO one (see the 'PayloadSemantics' instance for more
  details). As a result, block application might fail on a given payload.

  The tests in this module assume that no invalid blocks are generated. Thus we
  have to satisfy this assumption in the block generators. To keep the
  generators simple, eg independent on the ledger state, we follow this strategy
  to block generation:

  - The block payload consist of a single transaction:
      - input: Point
      - output: (Point, SlotNo)
  - The ledger state is a map from Point to ().
  - We start always in an initial state in which 'GenesisPoint' maps to ().
  - When we generate a block for point p, the payload of the block will be:
      - input: point p - 1
      - ouptput: (point p, ())

  A consequence of adopting the strategy above is that the initial state is
  coupled to the generator's semantics.
 -------------------------------------------------------------------------------}

genesis :: ExtLedgerState TestBlock
genesis = testInitExtLedgerWithState initialTestLedgerState

-- | The genesis UTxO values, threaded alongside 'genesis'.
genesisValues :: Values TestBlock
genesisValues = Map.singleton (Token GenesisPoint) (TValue ())

initialTestLedgerState :: PayloadDependentState Tx
initialTestLedgerState =
  UTxTok
    { utxhist = Set.singleton (Token GenesisPoint)
    }

genBlocks ::
  Word64 ->
  Point TestBlock ->
  [TestBlock]
genBlocks n pt0 = take (fromIntegral n) (go pt0)
 where
  go pt = let b = genBlock pt in b : go (blockPoint b)

genBlock ::
  Point TestBlock -> TestBlock
genBlock pt =
  mkBlockFrom
    pt
    Tx
      { consumed = Token pt
      , produced = (Token pt', TValue ())
      }
 where
  mkBlockFrom :: Point (TestBlockWith ptype) -> ptype -> TestBlockWith ptype
  mkBlockFrom GenesisPoint = firstBlockWithPayload 0
  mkBlockFrom (BlockPoint slot hash) = successorBlockWithPayload hash slot

  pt' :: Point (TestBlockWith Tx)
  pt' = castPoint (blockPoint dummyBlk)
   where
    -- This could be the new block itself; we merely wanted to avoid the loop.
    dummyBlk :: TestBlockWith ()
    dummyBlk = mkBlockFrom (castPoint pt) ()

extLedgerDbConfig :: SecurityParam -> LedgerDbCfg ExtLedgerState TestBlock
extLedgerDbConfig secParam =
  LedgerDbCfg
    { ledgerDbCfgSecParam = secParam
    , ledgerDbCfg =
        ExtLedgerCfg $
          singleNodeTestConfigWith
            TestBlockCodecConfig
            TestBlockStorageConfig
            secParam
            (GenesisWindow (2 * unNonZero (maxRollbacks secParam)))
    , ledgerDbCfgComputeLedgerEvents = OmitLedgerEvents
    }

-- | TODO: for the time being 'TestBlock' does not have any codec config
data instance CodecConfig TestBlock = TestBlockCodecConfig
  deriving (Show, Generic, NoThunks)

-- | TODO: for the time being 'TestBlock' does not have any storage config
data instance StorageConfig TestBlock = TestBlockStorageConfig
  deriving (Show, Generic, NoThunks)
