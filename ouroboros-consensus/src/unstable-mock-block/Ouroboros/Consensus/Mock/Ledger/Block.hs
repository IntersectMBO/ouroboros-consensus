{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Simple block to go with the mock ledger
--
-- None of the definitions in this module depend on, or even refer to, any
-- specific consensus protocols.
module Ouroboros.Consensus.Mock.Ledger.Block
  ( BlockQuery (..)
  , Header (..)
  , SimpleBlock
  , SimpleBlock' (..)
  , SimpleBody (..)
  , SimpleHash
  , SimpleHeader
  , SimpleStdHeader (..)

    -- * Working with 'SimpleBlock'
  , countSimpleGenTxs
  , matchesSimpleHeader
  , mkSimpleHeader

    -- * Configuration
  , BlockConfig (..)
  , CodecConfig (..)
  , SimpleLedgerConfig (..)
  , StorageConfig (..)

    -- * Protocol-specific part
  , MockProtocolSpecific (..)

    -- * 'UpdateLedger'
  , LedgerState (..)
  , Ticked (..)
  , genesisSimpleLedgerState
  , genesisSimpleLedgerTables
  , updateSimpleLedgerState

    -- * 'ApplyTx' (mempool support)
  , GenTx (..)
  , TxId (..)
  , Validated (..)
  , genTxSize
  , mkSimpleGenTx

    -- * Crypto
  , SimpleCrypto
  , SimpleMockCrypto
  , SimpleStandardCrypto

    -- * Serialisation
  , decodeSimpleHeader
  , encodeSimpleHeader
  , simpleBlockBinaryBlockInfo

    -- * For tests
  , simpleBlockCapacity
  ) where

import Cardano.Binary (ToCBOR (..))
import Cardano.Crypto.Hash (Hash, HashAlgorithm, SHA256, ShortHash)
import qualified Cardano.Crypto.Hash as Hash
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import Codec.Serialise (Serialise (..), serialise)
import Control.Monad.Except
import qualified Data.ByteString.Lazy as Lazy
import Data.Kind (Type)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable
import Data.Word
import GHC.Generics (Generic)
import GHC.TypeLits (KnownNat)
import NoThunks.Class (NoThunks (..))
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.HardFork.Abstract
import Ouroboros.Consensus.HardFork.Combinator.PartialConfig
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import Ouroboros.Consensus.HeaderValidation
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.CommonProtocolParams
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Ledger.Inspect
import Ouroboros.Consensus.Ledger.Query
import Ouroboros.Consensus.Ledger.SupportsMempool
import Ouroboros.Consensus.Ledger.SupportsPeerSelection
import Ouroboros.Consensus.Ledger.SupportsPeras (LedgerSupportsPeras)
import qualified Ouroboros.Consensus.Ledger.Tables.Diff as Diff
import Ouroboros.Consensus.Mock.Ledger.Address
import Ouroboros.Consensus.Mock.Ledger.State
import qualified Ouroboros.Consensus.Mock.Ledger.UTxO as Mock
import Ouroboros.Consensus.Node.Serialisation
import Ouroboros.Consensus.Storage.Common
  ( BinaryBlockInfo (..)
  , SizeInBytes
  )
import Ouroboros.Consensus.Util (ShowProxy (..), hashFromBytesShortE)
import Ouroboros.Consensus.Util.Condense
import Ouroboros.Network.Tx (HasRawTxId (..))
import Test.Util.Orphans.Serialise ()

{-------------------------------------------------------------------------------
  Definition of a block

  The primed versions allow to vary the @ext@ parameter independently of the
  previous block hash.
-------------------------------------------------------------------------------}

type SimpleBlock c ext = SimpleBlock' c ext ext
type SimpleHeader c ext = Header (SimpleBlock c ext)

data SimpleBlock' c ext ext' = SimpleBlock
  { simpleHeader :: Header (SimpleBlock' c ext ext')
  , simpleBody :: SimpleBody
  }
  deriving (Generic, Show, Eq)

instance
  (HashAlgorithm (SimpleHash c), Typeable c, Typeable ext, Serialise ext') =>
  Serialise (SimpleBlock' c ext ext')
  where
  encode (SimpleBlock hdr body) =
    mconcat
      [ CBOR.encodeListLen 2
      , encode hdr
      , encode body
      ]
  decode = do
    CBOR.decodeListLenOf 2
    hdr <- decode
    body <- decode
    return (SimpleBlock hdr body)

instance
  (Typeable c, Typeable ext, Typeable ext') =>
  ShowProxy (SimpleBlock' c ext ext')

data instance Header (SimpleBlock' c ext ext') = SimpleHeader
  { simpleHeaderHash :: HeaderHash (SimpleBlock' c ext ext')
  -- ^ The header hash
  --
  -- This is the hash of the header itself. This is a bit unpleasant,
  -- because it makes the hash look self-referential (when computing the
  -- hash we must ignore the 'simpleHeaderHash' field). However, the benefit
  -- is that we can give a 'HasHeader' instance that does not require
  -- a (static) 'Serialise' instance.
  , simpleHeaderStd :: SimpleStdHeader c ext
  -- ^ Fields required for the 'HasHeader' instance
  , simpleHeaderExt :: ext'
  -- ^ Header extension
  --
  -- This extension will be required when using 'SimpleBlock' for specific
  -- consensus protocols.
  }
  deriving (Generic, Show, Eq, NoThunks)

instance
  (Typeable c, Typeable ext, Typeable ext') =>
  ShowProxy (Header (SimpleBlock' c ext ext'))

instance
  (SimpleCrypto c, Typeable ext, Typeable ext') =>
  GetHeader (SimpleBlock' c ext ext')
  where
  getHeader = simpleHeader

  blockMatchesHeader = matchesSimpleHeader

  headerIsEBB = const Nothing

data SimpleStdHeader c ext = SimpleStdHeader
  { simplePrev :: ChainHash (SimpleBlock c ext)
  , simpleSlotNo :: SlotNo
  , simpleBlockNo :: BlockNo
  , simpleBodyHash :: Hash (SimpleHash c) SimpleBody
  , simpleBodySize :: SizeInBytes
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass NoThunks

deriving anyclass instance
  (HashAlgorithm (SimpleHash c), Typeable c, Typeable ext) =>
  Serialise (SimpleStdHeader c ext)

data SimpleBody = SimpleBody
  { simpleTxs :: [Mock.Tx]
  }
  deriving (Generic, Show, Eq)

instance Serialise SimpleBody where
  encode (SimpleBody txs) = encode txs
  decode = SimpleBody <$> decode

{-------------------------------------------------------------------------------
  Working with 'SimpleBlock'
-------------------------------------------------------------------------------}

-- | Create a header by hashing the header without hash and adding to the
-- resulting value.
mkSimpleHeader ::
  (HashAlgorithm (SimpleHash c), Typeable c, Typeable ext) =>
  (ext' -> CBOR.Encoding) ->
  SimpleStdHeader c ext ->
  ext' ->
  Header (SimpleBlock' c ext ext')
mkSimpleHeader encodeExt std ext =
  headerWithoutHash
    { simpleHeaderHash =
        Hash.hashWithSerialiser
          (encodeSimpleHeader encodeExt)
          headerWithoutHash
    }
 where
  headerWithoutHash =
    SimpleHeader
      { simpleHeaderHash = error "Serialise instances should ignore hash"
      , simpleHeaderStd = std
      , simpleHeaderExt = ext
      }

-- | Check whether the block matches the header
matchesSimpleHeader ::
  SimpleCrypto c =>
  Header (SimpleBlock' c ext ext') ->
  SimpleBlock' c ext ext'' ->
  Bool
matchesSimpleHeader SimpleHeader{..} SimpleBlock{..} =
  simpleBodyHash == Hash.hashWithSerialiser toCBOR simpleBody
 where
  SimpleStdHeader{..} = simpleHeaderStd

countSimpleGenTxs :: SimpleBlock c ext -> Word64
countSimpleGenTxs = fromIntegral . length . extractTxs

{-------------------------------------------------------------------------------
  HasHeader instance for SimpleHeader
-------------------------------------------------------------------------------}

instance
  (SimpleCrypto c, Typeable ext, Typeable ext') =>
  HasHeader (Header (SimpleBlock' c ext ext'))
  where
  getHeaderFields hdr =
    HeaderFields
      { headerFieldHash = simpleHeaderHash hdr
      , headerFieldSlot = simpleSlotNo . simpleHeaderStd $ hdr
      , headerFieldBlockNo = simpleBlockNo . simpleHeaderStd $ hdr
      }

{-------------------------------------------------------------------------------
  HasHeader instance for SimpleBlock
-------------------------------------------------------------------------------}

type instance
  HeaderHash (SimpleBlock' c ext ext') =
    Hash (SimpleHash c) (Header (SimpleBlock' c ext ext'))

instance
  (SimpleCrypto c, Typeable ext, Typeable ext') =>
  HasHeader (SimpleBlock' c ext ext')
  where
  getHeaderFields = getBlockHeaderFields

instance (SimpleCrypto c, Typeable ext) => GetPrevHash (SimpleBlock c ext) where
  headerPrevHash = simplePrev . simpleHeaderStd

instance
  (SimpleCrypto c, Typeable ext, Typeable ext') =>
  StandardHash (SimpleBlock' c ext ext')

instance
  (KnownNat (Hash.HashSize (SimpleHash c)), SimpleCrypto c) =>
  ConvertRawHash (SimpleBlock' c ext ext')
  where
  type HashSize (SimpleBlock' c ext ext') = Hash.HashSize (SimpleHash c)
  toShortRawHash _ = Hash.hashToBytesShort
  unsafeFromShortRawHash _ = hashFromBytesShortE

{-------------------------------------------------------------------------------
  HasMockTxs instance
-------------------------------------------------------------------------------}

instance Mock.HasMockTxs (SimpleBlock' c ext ext') where
  getMockTxs = Mock.getMockTxs . simpleBody

instance Mock.HasMockTxs SimpleBody where
  getMockTxs = simpleTxs

{-------------------------------------------------------------------------------
  Envelope validation
-------------------------------------------------------------------------------}

instance (SimpleCrypto c, Typeable ext) => HasAnnTip (SimpleBlock c ext)

-- Use defaults

instance (SimpleCrypto c, Typeable ext) => BasicEnvelopeValidation (SimpleBlock c ext)

-- Use defaults

instance (SimpleCrypto c, Typeable ext) => ValidateEnvelope (SimpleBlock c ext)

-- Use defaults

{-------------------------------------------------------------------------------
  Block config
-------------------------------------------------------------------------------}

data instance BlockConfig (SimpleBlock c ext) = SimpleBlockConfig
  deriving stock Generic
  deriving anyclass NoThunks

{-------------------------------------------------------------------------------
  Codec config
-------------------------------------------------------------------------------}

data instance CodecConfig (SimpleBlock c ext) = SimpleCodecConfig
  deriving stock Generic
  deriving anyclass NoThunks

{-------------------------------------------------------------------------------
  Storage config
-------------------------------------------------------------------------------}

data instance StorageConfig (SimpleBlock c ext) = SimpleStorageConfig SecurityParam
  deriving stock Generic
  deriving anyclass NoThunks

{-------------------------------------------------------------------------------
  Hard fork history
-------------------------------------------------------------------------------}

instance HasHardForkHistory (SimpleBlock c ext) where
  type HardForkIndices (SimpleBlock c ext) = '[SimpleBlock c ext]
  hardForkSummary = neverForksHardForkSummary simpleLedgerEraParams

{-------------------------------------------------------------------------------
  Protocol specific constraints
-------------------------------------------------------------------------------}

class
  ( SimpleCrypto c
  , Typeable ext
  , Show (MockLedgerConfig c ext)
  , NoThunks (MockLedgerConfig c ext)
  , Serialise (MockLedgerConfig c ext)
  ) =>
  MockProtocolSpecific c ext
  where
  type MockLedgerConfig c ext :: Type

{-------------------------------------------------------------------------------
  Update the ledger
-------------------------------------------------------------------------------}

data SimpleLedgerConfig c ext = SimpleLedgerConfig
  { simpleMockLedgerConfig :: !(MockLedgerConfig c ext)
  -- ^ Config required by the various kinds of mock block (PFT, Praos, ..)
  , simpleLedgerEraParams :: !HardFork.EraParams
  -- ^ Era parameters
  , simpleLedgerMockConfig :: !MockConfig
  }
  deriving Generic

deriving instance Show (MockLedgerConfig c ext) => Show (SimpleLedgerConfig c ext)
deriving instance Eq (MockLedgerConfig c ext) => Eq (SimpleLedgerConfig c ext)
deriving instance
  NoThunks (MockLedgerConfig c ext) =>
  NoThunks (SimpleLedgerConfig c ext)
deriving instance
  Serialise (MockLedgerConfig c ext) =>
  Serialise (SimpleLedgerConfig c ext)

type instance LedgerCfg LedgerState (SimpleBlock c ext) = SimpleLedgerConfig c ext

instance MockProtocolSpecific c ext => HasPartialLedgerConfig (SimpleBlock c ext)

instance
  Serialise (MockLedgerConfig c ext) =>
  SerialiseNodeToClient (SimpleBlock c ext) (SimpleLedgerConfig c ext)

instance GetTip (LedgerState (SimpleBlock c ext)) where
  getTip (SimpleLedgerState st) = castPoint $ mockTip st

instance GetTip (Ticked LedgerState (SimpleBlock c ext)) where
  getTip = castPoint . getTip . getTickedSimpleLedgerState

type instance AuxLedgerEvent (SimpleBlock c ext) = VoidLedgerEvent

instance
  MockProtocolSpecific c ext =>
  IsLedger LedgerState (SimpleBlock c ext)
  where
  type LedgerErr LedgerState (SimpleBlock c ext) = MockError (SimpleBlock c ext)

  applyChainTickLedgerResult _ _ _ st =
    pureLedgerResult (TickedSimpleLedgerState st, mempty)

instance
  MockProtocolSpecific c ext =>
  ApplyBlock LedgerState (SimpleBlock c ext)
  where
  applyBlockLedgerResultWithValidation _validation _events cfg blk values tickedSt = do
    -- Stow the read values into the mock state's UTxO, apply the block, then
    -- diff the resulting UTxO against the input and clear it back out (the
    -- @mk@-free analogue of stow → BBODY → diff → unstow).
    st' <- updateSimpleLedgerState cfg blk (stowValues values tickedSt)
    let diff = Diff.diff values (mockUtxo (simpleLedgerState st'))
    pure $ pureLedgerResult (clearValues st', diff)

  applyBlockLedgerResult = defaultApplyBlockLedgerResult
  reapplyBlockLedgerResult =
    defaultReapplyBlockLedgerResult (error . ("reapplyBlockLedgerResult: unexpected error: " <>) . show)

-- | Stow the given values into the ticked mock state's UTxO field.
stowValues ::
  Mock.Utxo ->
  TickedLedgerState (SimpleBlock c ext) ->
  TickedLedgerState (SimpleBlock c ext)
stowValues values (TickedSimpleLedgerState (SimpleLedgerState st)) =
  TickedSimpleLedgerState (SimpleLedgerState st{mockUtxo = values})

-- | Clear the mock state's UTxO field, returning a canonical (table-free)
-- ledger state.
clearValues ::
  LedgerState (SimpleBlock c ext) ->
  LedgerState (SimpleBlock c ext)
clearValues (SimpleLedgerState st) = SimpleLedgerState st{mockUtxo = mempty}

data instance LedgerState (SimpleBlock c ext) = SimpleLedgerState
  { simpleLedgerState :: MockState (SimpleBlock c ext)
  }
  deriving stock Generic

deriving instance
  (SimpleCrypto c, Typeable ext) =>
  Eq (LedgerState (SimpleBlock c ext))
deriving instance
  (SimpleCrypto c, Typeable ext) =>
  NoThunks (LedgerState (SimpleBlock c ext))
deriving instance
  (SimpleCrypto c, Typeable ext) =>
  Show (LedgerState (SimpleBlock c ext))

-- Ticking has no effect on the simple ledger state
newtype instance Ticked LedgerState (SimpleBlock c ext) = TickedSimpleLedgerState
  { getTickedSimpleLedgerState :: LedgerState (SimpleBlock c ext)
  }
  deriving Generic

deriving anyclass instance
  ( SimpleCrypto c
  , Typeable ext
  ) =>
  NoThunks (Ticked LedgerState (SimpleBlock c ext))
deriving instance
  ( SimpleCrypto c
  , Typeable ext
  ) =>
  Show (Ticked LedgerState (SimpleBlock c ext))

instance MockProtocolSpecific c ext => UpdateLedger (SimpleBlock c ext)

updateSimpleLedgerState ::
  (SimpleCrypto c, Typeable ext) =>
  LedgerConfig (SimpleBlock c ext) ->
  SimpleBlock c ext ->
  TickedLedgerState (SimpleBlock c ext) ->
  Except
    (MockError (SimpleBlock c ext))
    (LedgerState (SimpleBlock c ext))
updateSimpleLedgerState cfg b (TickedSimpleLedgerState (SimpleLedgerState st)) =
  SimpleLedgerState <$> updateMockState (simpleLedgerMockConfig cfg) b st

updateSimpleUTxO ::
  Mock.HasMockTxs a =>
  LedgerConfig (SimpleBlock c ext) ->
  SlotNo ->
  a ->
  TickedLedgerState (SimpleBlock c ext) ->
  Except
    (MockError (SimpleBlock c ext))
    (TickedLedgerState (SimpleBlock c ext))
updateSimpleUTxO cfg slot x (TickedSimpleLedgerState (SimpleLedgerState st)) =
  TickedSimpleLedgerState . SimpleLedgerState
    <$> updateMockUTxO (simpleLedgerMockConfig cfg) slot x st

-- | The genesis ledger state, with an empty (stowed-out) UTxO; the genesis
-- UTxO values are obtained separately via 'genesisSimpleLedgerTables'.
genesisSimpleLedgerState :: AddrDist -> LedgerState (SimpleBlock c ext)
genesisSimpleLedgerState =
  clearValues . SimpleLedgerState . genesisMockState

-- | The genesis UTxO values, threaded alongside 'genesisSimpleLedgerState' now
-- that the state is @mk@-free.
genesisSimpleLedgerTables :: AddrDist -> Values (SimpleBlock c ext)
genesisSimpleLedgerTables = mockUtxo . genesisMockState

-- | Dummy values
instance MockProtocolSpecific c ext => CommonProtocolParams (SimpleBlock c ext) where
  maxHeaderSize = const 2000000
  maxTxSize = const 2000000

instance LedgerSupportsPeerSelection (SimpleBlock c ext) where
  getPeers = const []

instance LedgerSupportsPeras (SimpleBlock c ext)

{-------------------------------------------------------------------------------
  LedgerTables
-------------------------------------------------------------------------------}

type instance TxIn (SimpleBlock c ext) = Mock.TxIn
type instance TxOut (SimpleBlock c ext) = Mock.TxOut

instance BlockSupportsUTxOHD (SimpleBlock c ext) where
  type Keys (SimpleBlock c ext) = Set Mock.TxIn
  type Values (SimpleBlock c ext) = Map Mock.TxIn Mock.TxOut
  type Diff (SimpleBlock c ext) = Diff.Diff Mock.TxIn Mock.TxOut

  blockKeys SimpleBlock{simpleBody = SimpleBody txs} = Mock.txIns txs
  forward diffs vals = Diff.applyDiff vals (mconcat diffs)
  restrictValues keys vals = vals `Map.restrictKeys` keys
  valuesSize = Map.size
  encodeValues = encode
  decodeValues _ = decode

instance SingleEraUTxOHDBlock (SimpleBlock c ext) where
  emptyValues = Map.empty
  emptyDiffs = mempty

instance SingleEraBlockSupportsUTxOHD (SimpleBlock c ext) where
  rangeReadValues (mbPrev, n) vals =
    let toRead = case mbPrev of
          Nothing -> vals
          Just prev -> snd (Map.split prev vals)
        page = Map.take n toRead
     in (page, fst <$> Map.lookupMax page)
  keysToList = Set.toList
  valuesToList = Map.toList
  valuesFromList = Map.fromList
  diffToList (Diff.Diff m) = Map.toList m

{-------------------------------------------------------------------------------
  Support for the mempool
-------------------------------------------------------------------------------}

data instance GenTx (SimpleBlock c ext) = SimpleGenTx
  { simpleGenTx :: !Mock.Tx
  , simpleGenTxId :: !Mock.TxId
  }
  deriving stock (Generic, Eq, Ord)
  deriving anyclass Serialise

newtype instance Validated (GenTx (SimpleBlock c ext)) = ValidatedSimpleGenTx
  { forgetValidatedSimpleGenTx :: GenTx (SimpleBlock c ext)
  }
  deriving newtype (Generic, Eq, Ord)

instance
  (Typeable c, Typeable ext) =>
  ShowProxy (GenTx (SimpleBlock c ext))

type instance ApplyTxErr (SimpleBlock c ext) = MockError (SimpleBlock c ext)

instance
  MockProtocolSpecific c ext =>
  LedgerSupportsMempool (SimpleBlock c ext)
  where
  applyTx cfg _wti slot tx values tickedSt = do
    TickedSimpleLedgerState st' <-
      updateSimpleUTxO cfg slot tx (stowValues values tickedSt)
    let diff = Diff.diff values (mockUtxo (simpleLedgerState st'))
    return
      ( TickedSimpleLedgerState (clearValues st')
      , diff
      , ValidatedSimpleGenTx tx
      )

  reapplyTx cfg slot vtx values tickedSt = do
    (st', diff, _vtx) <-
      applyTx cfg DoNotIntervene slot (forgetValidatedSimpleGenTx vtx) values tickedSt
    pure (st', diff)

  txForgetValidated = forgetValidatedSimpleGenTx

  getTransactionKeySets = Mock.txIns . simpleGenTx

  mkMempoolApplyTxError _tls txt = Just $ MockMempoolError txt

instance TxLimits (SimpleBlock c ext) where
  type TxMeasurePhase1 (SimpleBlock c ext) = IgnoringOverflow ByteSize32
  type TxMeasurePhase2 (SimpleBlock c ext) = TrivialTxMeasurePhase2

  txWireSize = fromIntegral . unByteSize32 . genTxSize

  -- Large value so that the Mempool tests never run out of capacity when they
  -- don't override it.
  --
  -- But not 'maxbound'!, since the mempool sometimes holds multiple blocks worth.
  blockCapacityTxMeasure _cfg _st = TxMeasure (IgnoringOverflow simpleBlockCapacity) TrivialTxMeasurePhase2

  txMeasurePhase1 cfg _st =
    fmap IgnoringOverflow
      . checkTxSize (simpleLedgerMockConfig cfg)
      . simpleGenTx

  txMeasurePhase2 _cfg _values _st _tx = pure TrivialTxMeasurePhase2

simpleBlockCapacity :: ByteSize32
simpleBlockCapacity = ByteSize32 512

newtype instance TxId (GenTx (SimpleBlock c ext)) = SimpleGenTxId
  { unSimpleGenTxId :: Mock.TxId
  }
  deriving stock Generic
  deriving newtype (Show, Eq, Ord, Serialise, NoThunks)

instance
  (Typeable c, Typeable ext) =>
  ShowProxy (TxId (GenTx (SimpleBlock c ext)))

instance HasTxId (GenTx (SimpleBlock c ext)) where
  txId = SimpleGenTxId . simpleGenTxId

instance HasRawTxId (TxId (GenTx (SimpleBlock c ext))) where
  type RawTxId (TxId (GenTx (SimpleBlock c ext))) = Mock.TxId
  getRawTxId = unSimpleGenTxId

instance (Typeable p, Typeable c) => NoThunks (GenTx (SimpleBlock p c)) where
  showTypeOf _ = show $ typeRep (Proxy @(GenTx (SimpleBlock p c)))

instance (Typeable p, Typeable c) => NoThunks (Validated (GenTx (SimpleBlock p c))) where
  showTypeOf _ = show $ typeRep (Proxy @(Validated (GenTx (SimpleBlock p c))))

instance HasTxs (SimpleBlock c ext) where
  extractTxs = map mkSimpleGenTx . simpleTxs . simpleBody

instance Mock.HasMockTxs (GenTx (SimpleBlock p c)) where
  getMockTxs = Mock.getMockTxs . simpleGenTx

instance Condense (GenTx (SimpleBlock p c)) where
  condense = condense . simpleGenTx

instance Show (GenTx (SimpleBlock p c)) where
  show = show . simpleGenTx

instance Show (Validated (GenTx (SimpleBlock p c))) where
  show = show . forgetValidatedSimpleGenTx

instance Condense (GenTxId (SimpleBlock p c)) where
  condense = condense . unSimpleGenTxId

mkSimpleGenTx :: Mock.Tx -> GenTx (SimpleBlock c ext)
mkSimpleGenTx tx =
  SimpleGenTx
    { simpleGenTx = tx
    , simpleGenTxId = Hash.hashWithSerialiser toCBOR tx
    }

genTxSize :: GenTx (SimpleBlock c ext) -> ByteSize32
genTxSize = txSize . simpleGenTx

{-------------------------------------------------------------------------------
  Support for BlockSupportsLedgerQuery
-------------------------------------------------------------------------------}

data instance BlockQuery (SimpleBlock c ext) fp result where
  QueryLedgerTip :: BlockQuery (SimpleBlock c ext) QFNoTables (Point (SimpleBlock c ext))

instance MockProtocolSpecific c ext => BlockSupportsLedgerQuery (SimpleBlock c ext) where
  answerPureBlockQuery _cfg QueryLedgerTip =
    castPoint
      . ledgerTipPoint
      . ledgerState
  answerBlockQueryLookup _cfg q = case q of {}
  answerBlockQueryTraverse _cfg q = case q of {}
  blockQueryIsSupportedOnVersion QueryLedgerTip = const True

instance SameDepIndex2 (BlockQuery (SimpleBlock c ext)) where
  sameDepIndex2 QueryLedgerTip QueryLedgerTip = Just Refl

deriving instance Show (BlockQuery (SimpleBlock c ext) fp result)

instance
  (Typeable c, Typeable ext) =>
  ShowProxy (BlockQuery (SimpleBlock c ext))

instance
  (SimpleCrypto c, Typeable ext) =>
  ShowQuery (BlockQuery (SimpleBlock c ext) fp)
  where
  showResult QueryLedgerTip = show

{-------------------------------------------------------------------------------
  Inspection
-------------------------------------------------------------------------------}

instance InspectLedger (SimpleBlock c ext)

-- Use defaults

{-------------------------------------------------------------------------------
  Crypto needed for simple blocks
-------------------------------------------------------------------------------}

class (HashAlgorithm (SimpleHash c), Typeable c) => SimpleCrypto c where
  type SimpleHash c :: Type

data SimpleStandardCrypto
data SimpleMockCrypto

instance SimpleCrypto SimpleStandardCrypto where
  type SimpleHash SimpleStandardCrypto = SHA256

instance SimpleCrypto SimpleMockCrypto where
  type SimpleHash SimpleMockCrypto = ShortHash

{-------------------------------------------------------------------------------
  Condense instances
-------------------------------------------------------------------------------}

instance Condense ext' => Condense (Header (SimpleBlock' c ext ext')) where
  condense SimpleHeader{..} =
    mconcat
      [ "("
      , condense simplePrev
      , "->"
      , condense simpleHeaderHash
      , ","
      , condense simpleSlotNo
      , ","
      , condense simpleHeaderExt
      , ")"
      ]
   where
    SimpleStdHeader{..} = simpleHeaderStd

instance Condense ext' => Condense (SimpleBlock' c ext ext') where
  condense SimpleBlock{..} =
    mconcat
      [ "("
      , condense simplePrev
      , "->"
      , condense simpleHeaderHash
      , ","
      , condense simpleSlotNo
      , ","
      , condense simpleHeaderExt
      , ","
      , condense simpleTxs
      , ")"
      ]
   where
    SimpleHeader{..} = simpleHeader
    SimpleStdHeader{..} = simpleHeaderStd
    SimpleBody{..} = simpleBody

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

instance ToCBOR SimpleBody where
  toCBOR = encode

encodeSimpleHeader ::
  (HashAlgorithm (SimpleHash c), Typeable c, Typeable ext) =>
  (ext' -> CBOR.Encoding) ->
  Header (SimpleBlock' c ext ext') ->
  CBOR.Encoding
encodeSimpleHeader encodeExt SimpleHeader{..} =
  mconcat
    [ CBOR.encodeListLen 2
    , encode simpleHeaderStd
    , encodeExt simpleHeaderExt
    ]

decodeSimpleHeader ::
  (HashAlgorithm (SimpleHash c), Typeable c, Typeable ext) =>
  (ext' -> CBOR.Encoding) ->
  (forall s. CBOR.Decoder s ext') ->
  forall s.
  CBOR.Decoder s (Header (SimpleBlock' c ext ext'))
decodeSimpleHeader encodeExt decodeExt = do
  CBOR.decodeListLenOf 2
  mkSimpleHeader encodeExt <$> decode <*> decodeExt

-- | Custom 'Serialise' instance that doesn't serialise the hash
instance
  (HashAlgorithm (SimpleHash c), Typeable c, Typeable ext, Serialise ext') =>
  Serialise (Header (SimpleBlock' c ext ext'))
  where
  encode = encodeSimpleHeader encode
  decode = decodeSimpleHeader encode decode

simpleBlockBinaryBlockInfo ::
  (SimpleCrypto c, Serialise ext', Typeable ext, Typeable ext') =>
  SimpleBlock' c ext ext' -> BinaryBlockInfo
simpleBlockBinaryBlockInfo b =
  BinaryBlockInfo
    { headerOffset = 1 -- For the 'encodeListLen'
    , headerSize = fromIntegral $ Lazy.length $ serialise (getHeader b)
    }
