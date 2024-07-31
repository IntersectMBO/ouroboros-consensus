{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Ouroboros.Storage.LedgerDB.StateMachine.TestBlock (
    TestBlock
  , extLedgerDbConfig
  , genBlocks
  , genesis
  ) where

import           Cardano.Binary (FromCBOR (..), ToCBOR (..))
import qualified Cardano.Slotting.Slot as WithOrigin
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import           Codec.Serialise (Serialise)
import qualified Codec.Serialise as S
import           Data.List.NonEmpty (nonEmpty)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromJust)
import           Data.Maybe.Strict
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.TreeDiff
import           Data.Word
import           GHC.Generics (Generic)
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract hiding (Key, Value)
import qualified Ouroboros.Consensus.Ledger.Abstract as Ledger
import           Ouroboros.Consensus.Ledger.Extended
import qualified Ouroboros.Consensus.Ledger.Tables.DiffSeq as DS
import           Ouroboros.Consensus.Ledger.Tables.Utils
import qualified Ouroboros.Consensus.Ledger.Tables.UtxoDiff as DS
import           Ouroboros.Consensus.Storage.LedgerDB.API.Config
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Network.Block (Point (Point))
import           Ouroboros.Network.Point (Block (Block))
import           Prelude hiding (elem)
import qualified Test.QuickCheck as QC
import           Test.Tasty.QuickCheck
import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.TestBlock hiding (TestBlock, TestBlockCodecConfig,
                     TestBlockStorageConfig)
import           Test.Util.ToExpr ()

{-------------------------------------------------------------------------------
  TestBlock
-------------------------------------------------------------------------------}

type TestBlock = TestBlockWith Tx

-- | Mock of a UTxO transaction where exactly one (transaction) input is
-- consumed and exactly one output is produced.
--
data Tx = Tx {
    -- | Input that the transaction consumes.
    consumed :: Token
    -- | Ouptupt that the transaction produces.
  , produced :: (Token, TValue)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Serialise, NoThunks, ToExpr)

-- | A token is an identifier for the values produced and consumed by the
-- 'TestBlock' transactions.
--
-- This is analogous to @TxId@: it's how we identify what's in the table. It's
-- also analogous to @TxIn@, since we trivially only have one output per 'Tx'.
newtype Token = Token { unToken :: Point TestBlock }
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Serialise, NoThunks, ToExpr, QC.Arbitrary)

instance QC.Arbitrary (Point TestBlock) where
  arbitrary = do
    slot <- SlotNo <$> QC.arbitrary
    hash <- TestHash . fromJust . nonEmpty . QC.getNonEmpty <$> QC.arbitrary
    pure $ Point $ WithOrigin.At $ Block slot hash

-- | Unit of value associated with the output produced by a transaction.
--
-- This is analogous to @TxOut@: it's what the table maps 'Token's to.
newtype TValue = TValue (WithOrigin SlotNo)
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Serialise, NoThunks, ToExpr)

{-------------------------------------------------------------------------------
  A ledger semantics for TestBlock
-------------------------------------------------------------------------------}

data TxErr
  = TokenWasAlreadyCreated Token
  | TokenDoesNotExist      Token
  deriving stock (Generic, Eq, Show)
  deriving anyclass (NoThunks, Serialise, ToExpr)

instance PayloadSemantics Tx where
  data PayloadDependentState Tx mk =
    UTxTok { utxtoktables :: LedgerTables (LedgerState TestBlock) mk
             -- | All the tokens that ever existed. We use this to
             -- make sure a token is not created more than once. See
             -- the definition of 'applyPayload' in the
             -- 'PayloadSemantics' of 'Tx'.
           , utxhist      :: Set Token
           }
    deriving stock    (Generic)

  type PayloadDependentError Tx = TxErr

  -- We need to exercise the HD backend. This requires that we store key-values
  -- ledger tables and the block application semantics satisfy:
  --
  -- * a key is deleted at most once
  -- * a key is inserted at most once
  --
  applyPayload st Tx{consumed, produced} =
      fmap track $ delete consumed st >>= uncurry insert produced
    where
      insert ::
           Token
        -> TValue
        -> PayloadDependentState Tx ValuesMK
        -> Either TxErr (PayloadDependentState Tx ValuesMK)
      insert tok val st'@UTxTok{utxtoktables, utxhist} =
          if tok `Set.member` utxhist
          then Left  $ TokenWasAlreadyCreated tok
          else Right $ st' { utxtoktables = Map.insert tok val `onValues` utxtoktables
                           , utxhist      = Set.insert tok utxhist
                           }
      delete ::
           Token
        -> PayloadDependentState Tx ValuesMK
        -> Either TxErr (PayloadDependentState Tx ValuesMK)
      delete tok st'@UTxTok{utxtoktables} =
          if Map.member tok `queryKeys` utxtoktables
          then Right $ st' { utxtoktables = Map.delete tok `onValues` utxtoktables
                           }
          else Left  $ TokenDoesNotExist tok

      track :: PayloadDependentState Tx ValuesMK -> PayloadDependentState Tx TrackingMK
      track stAfter =
          stAfter { utxtoktables =
                      LedgerTables $ rawCalculateDifference utxtokBefore utxtokAfter
                  }
        where
          utxtokBefore = getLedgerTables $ utxtoktables st
          utxtokAfter  = getLedgerTables $ utxtoktables stAfter

  getPayloadKeySets Tx{consumed} =
    LedgerTables $ KeysMK $ Set.singleton consumed

deriving instance Eq (LedgerTables (LedgerState TestBlock) mk) => Eq (PayloadDependentState Tx mk)
deriving instance NoThunks (LedgerTables (LedgerState TestBlock) mk) => NoThunks (PayloadDependentState Tx mk)
deriving instance Show (LedgerTables (LedgerState TestBlock) mk) => Show (PayloadDependentState Tx mk)
deriving instance Serialise (LedgerTables (LedgerState TestBlock) mk) => Serialise (PayloadDependentState Tx mk)

onValues ::
     (Map Token TValue -> Map Token TValue)
  -> LedgerTables (LedgerState TestBlock) ValuesMK
  -> LedgerTables (LedgerState TestBlock) ValuesMK
onValues f (LedgerTables testUtxtokTable) = LedgerTables $ updateMap testUtxtokTable
  where
    updateMap :: ValuesMK Token TValue -> ValuesMK Token TValue
    updateMap (ValuesMK utxovals) =
      ValuesMK $ f utxovals

queryKeys ::
     (Map Token TValue -> a)
  -> LedgerTables (LedgerState TestBlock) ValuesMK
  -> a
queryKeys f (LedgerTables (ValuesMK utxovals)) = f utxovals

{-------------------------------------------------------------------------------
  Instances required for on-disk storage of ledger state tables
-------------------------------------------------------------------------------}

type instance Ledger.Key   (LedgerState TestBlock) = Token
type instance Ledger.Value (LedgerState TestBlock) = TValue

instance HasLedgerTables (LedgerState TestBlock) where
  projectLedgerTables st       = utxtoktables $ payloadDependentState st
  withLedgerTables    st table = st { payloadDependentState =
                                        (payloadDependentState st) {utxtoktables = table}
                                    }

instance HasLedgerTables (Ticked1 (LedgerState TestBlock)) where
  projectLedgerTables (TickedTestLedger st)        =
    castLedgerTables $ projectLedgerTables st
  withLedgerTables    (TickedTestLedger st) tables =
    TickedTestLedger $ withLedgerTables st $ castLedgerTables tables

instance CanSerializeLedgerTables (LedgerState TestBlock)

instance Serialise (LedgerTables (LedgerState TestBlock) EmptyMK) where
  encode (LedgerTables (_ :: EmptyMK Token TValue))
         = CBOR.encodeNull
  decode = LedgerTables EmptyMK <$ CBOR.decodeNull

instance ToCBOR Token where
  toCBOR (Token pt) = S.encode pt

instance FromCBOR Token where
  fromCBOR = fmap Token S.decode

instance ToCBOR TValue where
  toCBOR (TValue v) = S.encode v

instance FromCBOR TValue where
  fromCBOR = fmap TValue S.decode

instance CanStowLedgerTables (LedgerState TestBlock) where
  stowLedgerTables     = stowErr "stowLedgerTables"
  unstowLedgerTables   = stowErr "unstowLedgerTables"

stowErr :: String -> a
stowErr fname = error $ "Function " <> fname <> " should not be used in these tests."

deriving anyclass instance (ToExpr k, ToExpr v) => ToExpr (DS.UtxoDiff k v)
deriving anyclass instance (ToExpr k, ToExpr v) => ToExpr (DS.RootMeasure k v)
deriving anyclass instance (ToExpr k, ToExpr v) => ToExpr (DS.InternalMeasure k v)
deriving anyclass instance (ToExpr v) => ToExpr (StrictMaybe v)
deriving anyclass instance (ToExpr k, ToExpr v) => ToExpr (DS.Element k v)
deriving anyclass instance ToExpr DS.Length
deriving anyclass instance ToExpr DS.SlotNoUB
deriving anyclass instance ToExpr DS.SlotNoLB
deriving anyclass instance ToExpr (mk Token TValue) => ToExpr (LedgerTables (LedgerState TestBlock) mk)
deriving instance ToExpr (LedgerTables (LedgerState TestBlock) mk) => ToExpr (PayloadDependentState Tx mk)

deriving newtype instance ToExpr (ValuesMK Token TValue)

instance ToExpr (ExtLedgerState TestBlock ValuesMK) where
  toExpr = genericToExpr

instance ToExpr (LedgerState (TestBlockWith Tx) ValuesMK) where
  toExpr = genericToExpr

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
  - The ledger state is a map from Point to SlotNo.
  - We start always in an initial state in which 'GenesisPoint' maps to slot 0.
  - When we generate a block for point p, the payload of the block will be:
      - input: point p - 1
      - ouptput: (point p, slot of point p)


  A consequence of adopting the strategy above is that the initial state is
  coupled to the generator's semantics.
 -------------------------------------------------------------------------------}

genesis :: ExtLedgerState TestBlock ValuesMK
genesis = testInitExtLedgerWithState initialTestLedgerState

initialTestLedgerState :: PayloadDependentState Tx ValuesMK
initialTestLedgerState = UTxTok {
    utxtoktables =   LedgerTables
                   $ ValuesMK
                   $ Map.singleton initialToken (pointTValue initialToken)
  , utxhist      = Set.singleton initialToken

  }
  where
    initialToken = Token GenesisPoint

-- | Get the token value associated to a given token. This is coupled to the
-- generators semantics.
pointTValue :: Token -> TValue
pointTValue = TValue . pointSlot . unToken

genBlocks ::
     Word64
  -> Point TestBlock
  -> [TestBlock]
genBlocks n pt0 = take (fromIntegral n) (go pt0)
  where
    go pt = let b = genBlock pt in b : go (blockPoint b)

genBlock ::
     Point TestBlock -> TestBlock
genBlock pt =
  mkBlockFrom pt Tx { consumed = Token pt
                    , produced = ( Token pt', TValue (pointSlot pt'))
                    }
  where
    mkBlockFrom :: Point (TestBlockWith ptype) -> ptype -> TestBlockWith ptype
    mkBlockFrom GenesisPoint           = firstBlockWithPayload 0
    mkBlockFrom (BlockPoint slot hash) = successorBlockWithPayload hash slot

    pt' :: Point (TestBlockWith Tx)
    pt' = castPoint (blockPoint dummyBlk)
      where
        -- This could be the new block itself; we merely wanted to avoid the loop.
        dummyBlk :: TestBlockWith ()
        dummyBlk = mkBlockFrom (castPoint pt) ()

extLedgerDbConfig :: SecurityParam -> LedgerDbCfg (ExtLedgerState TestBlock)
extLedgerDbConfig secParam = LedgerDbCfg {
      ledgerDbCfgSecParam = secParam
    , ledgerDbCfg         = ExtLedgerCfg $ singleNodeTestConfigWith TestBlockCodecConfig TestBlockStorageConfig secParam (GenesisWindow (2 * maxRollbacks secParam))
    }


-- | TODO: for the time being 'TestBlock' does not have any codec config
data instance CodecConfig TestBlock = TestBlockCodecConfig
  deriving (Show, Generic, NoThunks)

-- | TODO: for the time being 'TestBlock' does not have any storage config
data instance StorageConfig TestBlock = TestBlockStorageConfig
  deriving (Show, Generic, NoThunks)
