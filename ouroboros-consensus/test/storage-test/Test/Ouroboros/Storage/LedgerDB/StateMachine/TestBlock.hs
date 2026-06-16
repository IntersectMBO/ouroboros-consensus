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
  ) where

import Cardano.Ledger.BaseTypes (NonZero (..))
import qualified Cardano.Slotting.Slot as WithOrigin
import Codec.Serialise (Serialise)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.TreeDiff
import Data.Word
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.HardFork.Abstract
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Storage.LedgerDB.API
import Ouroboros.Network.Block (Point (Point))
import Ouroboros.Network.Point (Block (Block))
import qualified Test.QuickCheck as QC
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
  deriving newtype (Serialise, NoThunks, ToExpr)

{-------------------------------------------------------------------------------
  A ledger semantics for TestBlock
-------------------------------------------------------------------------------}

data TxErr
  = TokenWasAlreadyCreated Token
  | TokenDoesNotExist Token
  deriving stock (Generic, Eq, Show)
  deriving anyclass (NoThunks, Serialise, ToExpr)

instance PayloadSemantics Tx where
  -- \| With the no-MK port the UTxO map lives directly inside
  -- 'PayloadDependentState'; there is no separate 'LedgerTables'
  -- container anymore.
  data PayloadDependentState Tx = UTxTok
    { utxo :: Map Token TValue
    , -- \| All the tokens that ever existed. We use this to
      -- make sure a token is not created more than once. See
      -- the definition of 'applyPayload' in the
      -- 'PayloadSemantics' of 'Tx'.
      utxhist :: Set Token
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (NoThunks, Serialise, ToExpr)

  type PayloadDependentError Tx = TxErr

  applyPayload st Tx{consumed, produced = (tok, val)} = do
    st' <- deleteTok consumed st
    insertTok tok val st'
   where
    insertTok ::
      Token ->
      TValue ->
      PayloadDependentState Tx ->
      Either TxErr (PayloadDependentState Tx)
    insertTok t v s@UTxTok{utxo, utxhist}
      | t `Set.member` utxhist = Left $ TokenWasAlreadyCreated t
      | otherwise =
          Right $
            s
              { utxo = Map.insert t v utxo
              , utxhist = Set.insert t utxhist
              }
    deleteTok ::
      Token ->
      PayloadDependentState Tx ->
      Either TxErr (PayloadDependentState Tx)
    deleteTok t s@UTxTok{utxo}
      | Map.member t utxo =
          Right $ s{utxo = Map.delete t utxo}
      | otherwise = Left $ TokenDoesNotExist t

instance HasHardForkHistory TestBlock where
  type HardForkIndices TestBlock = '[TestBlock]
  hardForkSummary = neverForksHardForkSummary tblcHardForkParams

{-------------------------------------------------------------------------------
  TestBlock generation
-------------------------------------------------------------------------------}

genesis :: ExtLedgerState TestBlock
genesis = testInitExtLedgerWithState initialTestLedgerState

initialTestLedgerState :: PayloadDependentState Tx
initialTestLedgerState =
  UTxTok
    { utxo = Map.singleton initialToken (TValue ())
    , utxhist = Set.singleton initialToken
    }
 where
  initialToken = Token GenesisPoint

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

-- | TODO @js: for the time being 'TestBlock' does not have any codec config
data instance CodecConfig TestBlock = TestBlockCodecConfig
  deriving (Show, Generic, NoThunks)

-- | TODO @js: for the time being 'TestBlock' does not have any storage config
data instance StorageConfig TestBlock = TestBlockStorageConfig
  deriving (Show, Generic, NoThunks)
