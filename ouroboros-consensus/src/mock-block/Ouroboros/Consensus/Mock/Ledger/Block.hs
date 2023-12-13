{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE EmptyCase                  #-}
{-# LANGUAGE EmptyDataDeriving          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | Simple block to go with the mock ledger
--
-- None of the definitions in this module depend on, or even refer to, any
-- specific consensus protocols.
module Ouroboros.Consensus.Mock.Ledger.Block (
    BlockQuery (..)
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
  , LedgerTables (..)
  , Ticked (..)
  , Ticked1 (..)
  , genesisSimpleLedgerState
  , updateSimpleLedgerState
    -- * 'ApplyTx' (mempool support)
  , GenTx (..)
  , TxId (..)
  , Validated (..)
  , mkSimpleGenTx
  , txSize
    -- * Crypto
  , SimpleCrypto
  , SimpleMockCrypto
  , SimpleStandardCrypto
    -- * Serialisation
  , decodeSimpleHeader
  , encodeSimpleHeader
  , simpleBlockBinaryBlockInfo
  ) where

import           Cardano.Binary (ToCBOR (..))
import           Cardano.Crypto.Hash (Hash, HashAlgorithm, SHA256, ShortHash)
import qualified Cardano.Crypto.Hash as Hash
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import           Codec.Serialise (Serialise (..), serialise)
import           Control.Monad.Except
import qualified Data.ByteString.Lazy as Lazy
import           Data.Foldable (foldMap')
import           Data.Kind (Type)
import           Data.Proxy
import           Data.Typeable
import           Data.Word
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks (..))
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.HardFork.Abstract
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.CommonProtocolParams
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Inspect
import           Ouroboros.Consensus.Ledger.Query
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Ledger.SupportsPeerSelection
import           Ouroboros.Consensus.Ledger.Tables.Utils
import           Ouroboros.Consensus.Mock.Ledger.Address
import           Ouroboros.Consensus.Mock.Ledger.State
import qualified Ouroboros.Consensus.Mock.Ledger.UTxO as Mock
import           Ouroboros.Consensus.Storage.Common (BinaryBlockInfo (..))
import           Ouroboros.Consensus.Util (ShowProxy (..), hashFromBytesShortE)
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.Orphans ()

{-------------------------------------------------------------------------------
  Definition of a block

  The primed versions allow to vary the @ext@ parameter independently of the
  previous block hash.
-------------------------------------------------------------------------------}

type SimpleBlock  c ext = SimpleBlock'  c ext ext
type SimpleHeader c ext = Header (SimpleBlock c ext)

data SimpleBlock' c ext ext' = SimpleBlock {
      simpleHeader :: Header (SimpleBlock' c ext ext')
    , simpleBody   :: SimpleBody
    }
  deriving (Generic, Show, Eq)

instance (SimpleCrypto c, Serialise ext') => Serialise (SimpleBlock' c ext ext') where
  encode (SimpleBlock hdr body) = mconcat [
        CBOR.encodeListLen 2
      , encode hdr
      , encode body
      ]
  decode = do
      CBOR.decodeListLenOf 2
      hdr  <- decode
      body <- decode
      return (SimpleBlock hdr body)

instance (Typeable c, Typeable ext, Typeable ext')
    => ShowProxy (SimpleBlock' c ext ext') where

data instance Header (SimpleBlock' c ext ext') = SimpleHeader {
      -- | The header hash
      --
      -- This is the hash of the header itself. This is a bit unpleasant,
      -- because it makes the hash look self-referential (when computing the
      -- hash we must ignore the 'simpleHeaderHash' field). However, the benefit
      -- is that we can give a 'HasHeader' instance that does not require
      -- a (static) 'Serialise' instance.
      simpleHeaderHash :: HeaderHash (SimpleBlock' c ext ext')

      -- | Fields required for the 'HasHeader' instance
    , simpleHeaderStd  :: SimpleStdHeader c ext

      -- | Header extension
      --
      -- This extension will be required when using 'SimpleBlock' for specific
      -- consensus protocols.
    , simpleHeaderExt  :: ext'
    }
  deriving (Generic, Show, Eq, NoThunks)

instance (Typeable c, Typeable ext, Typeable ext')
    => ShowProxy (Header (SimpleBlock' c ext ext')) where

instance (SimpleCrypto c, Typeable ext, Typeable ext')
      => GetHeader (SimpleBlock' c ext ext') where
  getHeader = simpleHeader

  blockMatchesHeader = matchesSimpleHeader

  headerIsEBB = const Nothing

data SimpleStdHeader c ext = SimpleStdHeader {
      simplePrev     :: ChainHash (SimpleBlock c ext)
    , simpleSlotNo   :: SlotNo
    , simpleBlockNo  :: BlockNo
    , simpleBodyHash :: Hash (SimpleHash c) SimpleBody
      -- TODO: use SizeInBytes
    , simpleBodySize :: Word32
    }
  deriving stock    (Generic, Show, Eq)
  deriving anyclass (Serialise, NoThunks)

data SimpleBody = SimpleBody {
      simpleTxs :: [Mock.Tx]
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
mkSimpleHeader :: SimpleCrypto c
               => (ext' -> CBOR.Encoding)
               -> SimpleStdHeader c ext
               -> ext'
               -> Header (SimpleBlock' c ext ext')
mkSimpleHeader encodeExt std ext =
    headerWithoutHash {
        simpleHeaderHash = Hash.hashWithSerialiser
                             (encodeSimpleHeader encodeExt)
                             headerWithoutHash
      }
  where
    headerWithoutHash = SimpleHeader {
        simpleHeaderHash = error "Serialise instances should ignore hash"
      , simpleHeaderStd  = std
      , simpleHeaderExt  = ext
      }

-- | Check whether the block matches the header
matchesSimpleHeader :: SimpleCrypto c
                    => Header (SimpleBlock' c ext ext')
                    -> SimpleBlock'  c ext ext''
                    -> Bool
matchesSimpleHeader SimpleHeader{..} SimpleBlock {..} =
    simpleBodyHash == Hash.hashWithSerialiser toCBOR simpleBody
  where
    SimpleStdHeader{..} = simpleHeaderStd

countSimpleGenTxs :: SimpleBlock c ext -> Word64
countSimpleGenTxs = fromIntegral . length . extractTxs

{-------------------------------------------------------------------------------
  HasHeader instance for SimpleHeader
-------------------------------------------------------------------------------}

instance (SimpleCrypto c, Typeable ext, Typeable ext')
      => HasHeader (Header (SimpleBlock' c ext ext')) where
  getHeaderFields hdr = HeaderFields {
        headerFieldHash    = simpleHeaderHash hdr
      , headerFieldSlot    = simpleSlotNo  . simpleHeaderStd $ hdr
      , headerFieldBlockNo = simpleBlockNo . simpleHeaderStd $ hdr
      }

{-------------------------------------------------------------------------------
  HasHeader instance for SimpleBlock
-------------------------------------------------------------------------------}

type instance HeaderHash (SimpleBlock' c ext ext') =
  Hash (SimpleHash c) (Header (SimpleBlock' c ext ext'))

instance (SimpleCrypto c, Typeable ext, Typeable ext')
      => HasHeader (SimpleBlock' c ext ext') where
  getHeaderFields = getBlockHeaderFields

instance (SimpleCrypto c, Typeable ext) => GetPrevHash (SimpleBlock c ext) where
  headerPrevHash = simplePrev . simpleHeaderStd

instance (SimpleCrypto c, Typeable ext, Typeable ext')
      => StandardHash (SimpleBlock' c ext ext')

instance SimpleCrypto c => ConvertRawHash (SimpleBlock' c ext ext') where
  toShortRawHash   _ = Hash.hashToBytesShort
  fromShortRawHash _ = hashFromBytesShortE
  hashSize         _ = fromIntegral $ Hash.sizeHash (Proxy @(SimpleHash c))

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
  deriving stock   (Generic)
  deriving anyclass (NoThunks)

{-------------------------------------------------------------------------------
  Codec config
-------------------------------------------------------------------------------}

data instance CodecConfig (SimpleBlock c ext) = SimpleCodecConfig
  deriving stock    (Generic)
  deriving anyclass (NoThunks)

{-------------------------------------------------------------------------------
  Storage config
-------------------------------------------------------------------------------}

data instance StorageConfig (SimpleBlock c ext) = SimpleStorageConfig SecurityParam
  deriving stock    (Generic)
  deriving anyclass (NoThunks)

{-------------------------------------------------------------------------------
  Hard fork history
-------------------------------------------------------------------------------}

instance HasHardForkHistory (SimpleBlock c ext) where
  type HardForkIndices (SimpleBlock c ext) = '[SimpleBlock c ext]
  hardForkSummary = neverForksHardForkSummary simpleLedgerEraParams

{-------------------------------------------------------------------------------
  Protocol specific constraints
-------------------------------------------------------------------------------}

class ( SimpleCrypto c
      , Typeable ext
      , Show     (MockLedgerConfig c ext)
      , NoThunks (MockLedgerConfig c ext)
      ) => MockProtocolSpecific c ext where
  type family MockLedgerConfig c ext :: Type

{-------------------------------------------------------------------------------
  Update the ledger
-------------------------------------------------------------------------------}

data SimpleLedgerConfig c ext = SimpleLedgerConfig {
      -- | Config required by the various kinds of mock block (PFT, Praos, ..)
      simpleMockLedgerConfig :: !(MockLedgerConfig c ext)

      -- | Era parameters
    , simpleLedgerEraParams  :: !HardFork.EraParams
    }
  deriving (Generic)

deriving instance Show (MockLedgerConfig c ext) => Show (SimpleLedgerConfig c ext)
deriving instance NoThunks (MockLedgerConfig c ext)
               => NoThunks (SimpleLedgerConfig c ext)

type instance LedgerCfg (LedgerState (SimpleBlock c ext)) = SimpleLedgerConfig c ext

instance GetTip (LedgerState (SimpleBlock c ext)) where
  getTip (SimpleLedgerState st _) = castPoint $ mockTip st

instance GetTip (Ticked1 (LedgerState (SimpleBlock c ext))) where
  getTip = castPoint . getTip . getTickedSimpleLedgerState

instance MockProtocolSpecific c ext
      => IsLedger (LedgerState (SimpleBlock c ext)) where
  type LedgerErr (LedgerState (SimpleBlock c ext)) = MockError (SimpleBlock c ext)

  type AuxLedgerEvent (LedgerState (SimpleBlock c ext)) = VoidLedgerEvent (LedgerState (SimpleBlock c ext))

  applyChainTickLedgerResult _ _ = pureLedgerResult
                                 . TickedSimpleLedgerState
                                 . flip SimpleLedgerState emptyLedgerTables
                                 . simpleLedgerState

instance MockProtocolSpecific c ext
      => ApplyBlock (LedgerState (SimpleBlock c ext)) (SimpleBlock c ext) where
  applyBlockLedgerResult _ blk st =
      fmap ( pureLedgerResult
           . forgetTrackingValues
           . calculateDifference st
           . unstowLedgerTables
           )
      . updateSimpleLedgerState blk
      . TickedSimpleLedgerState
      . stowLedgerTables
      $ getTickedSimpleLedgerState st

  reapplyBlockLedgerResult =
      (mustSucceed . runExcept) ..: applyBlockLedgerResult
    where
      mustSucceed (Left  err) = error ("reapplyBlockLedgerResult: unexpected error: " <> show err)
      mustSucceed (Right st)  = st

  getBlockKeySets SimpleBlock{simpleBody = SimpleBody txs} =
    foldMap' id
     [ LedgerTables $ KeysMK ins | Mock.Tx _ ins _ <- txs ]

data instance LedgerState (SimpleBlock c ext) mk  = SimpleLedgerState {
      simpleLedgerState :: MockState (SimpleBlock c ext)
    , simpleLedgerTables :: LedgerTables (LedgerState (SimpleBlock c ext)) mk
    }
  deriving stock   (Generic)

deriving instance ( SimpleCrypto c
                  , Typeable ext
                  , Eq (mk Mock.TxIn Mock.TxOut)
                  )
                  => Eq (LedgerState (SimpleBlock c ext) mk)
deriving instance ( SimpleCrypto c
                  , Typeable ext
                  , NoThunks (mk Mock.TxIn Mock.TxOut)
                  )
                  => NoThunks (LedgerState (SimpleBlock c ext) mk)
deriving instance ( SimpleCrypto c
                  , Typeable ext
                  , Show (mk Mock.TxIn Mock.TxOut)
                  )
                  => Show (LedgerState (SimpleBlock c ext) mk)

-- Ticking has no effect on the simple ledger state
newtype instance Ticked1 (LedgerState (SimpleBlock c ext)) mk = TickedSimpleLedgerState {
      getTickedSimpleLedgerState :: LedgerState (SimpleBlock c ext) mk
    }
  deriving (Generic)

deriving anyclass instance ( SimpleCrypto c
                           , Typeable ext
                           )
                           => NoThunks (Ticked1 (LedgerState (SimpleBlock c ext)) TrackingMK)

instance MockProtocolSpecific c ext => UpdateLedger (SimpleBlock c ext)

updateSimpleLedgerState :: (SimpleCrypto c, Typeable ext)
                        => SimpleBlock c ext
                        -> TickedLedgerState (SimpleBlock c ext) mk1
                        -> Except (MockError (SimpleBlock c ext))
                                  (LedgerState (SimpleBlock c ext) mk1)
updateSimpleLedgerState b (TickedSimpleLedgerState (SimpleLedgerState st tbs)) =
    flip SimpleLedgerState tbs <$> updateMockState b st

updateSimpleUTxO :: Mock.HasMockTxs a
                 => SlotNo
                 -> a
                 -> TickedLedgerState (SimpleBlock c ext) EmptyMK
                 -> Except (MockError (SimpleBlock c ext))
                           (TickedLedgerState (SimpleBlock c ext) EmptyMK)
updateSimpleUTxO slot x (TickedSimpleLedgerState (SimpleLedgerState st tbs)) =
    TickedSimpleLedgerState . flip SimpleLedgerState tbs <$> updateMockUTxO slot x st

genesisSimpleLedgerState :: AddrDist -> LedgerState (SimpleBlock c ext) ValuesMK
genesisSimpleLedgerState =
    unstowLedgerTables
  . flip SimpleLedgerState emptyLedgerTables
  . genesisMockState

-- | Dummy values
instance MockProtocolSpecific c ext => CommonProtocolParams (SimpleBlock c ext) where
  maxHeaderSize = const 2000000
  maxTxSize     = const 2000000

instance LedgerSupportsPeerSelection (SimpleBlock c ext) where
  getPeers = const []

{-------------------------------------------------------------------------------
  LedgerTables
-------------------------------------------------------------------------------}

type instance Key   (LedgerState (SimpleBlock c ext)) = Mock.TxIn
type instance Value (LedgerState (SimpleBlock c ext)) = Mock.TxOut

instance HasLedgerTables (LedgerState (SimpleBlock c ext)) where
  projectLedgerTables = simpleLedgerTables
  withLedgerTables (SimpleLedgerState s _) = SimpleLedgerState s

instance HasLedgerTables (Ticked1 (LedgerState (SimpleBlock c ext))) where
  projectLedgerTables = castLedgerTables
                      . simpleLedgerTables
                      . getTickedSimpleLedgerState
  withLedgerTables   (TickedSimpleLedgerState st) tables =
      TickedSimpleLedgerState $ withLedgerTables st $ castLedgerTables tables

instance CanSerializeLedgerTables (LedgerState (SimpleBlock c ext))

instance CanStowLedgerTables (LedgerState (SimpleBlock c ext)) where
  stowLedgerTables st =
      SimpleLedgerState {
        simpleLedgerState  = simpleLedgerState { mockUtxo = m }
      , simpleLedgerTables = emptyLedgerTables
      }
    where
      SimpleLedgerState {
          simpleLedgerState
        , simpleLedgerTables = LedgerTables (ValuesMK m)
        } = st

  unstowLedgerTables st =
    SimpleLedgerState {
        simpleLedgerState = simpleLedgerState { mockUtxo = mempty }
      , simpleLedgerTables =
          LedgerTables (ValuesMK (mockUtxo simpleLedgerState))
      }
    where
      SimpleLedgerState {
          simpleLedgerState
        } = st

deriving newtype instance CanStowLedgerTables (Ticked1 (LedgerState (SimpleBlock c ext)))

{-------------------------------------------------------------------------------
  Support for the mempool
-------------------------------------------------------------------------------}

data instance GenTx (SimpleBlock c ext) = SimpleGenTx {
      simpleGenTx   :: !Mock.Tx
    , simpleGenTxId :: !Mock.TxId
    }
  deriving stock    (Generic, Eq, Ord)
  deriving anyclass (Serialise)

newtype instance Validated (GenTx (SimpleBlock c ext)) = ValidatedSimpleGenTx {
      forgetValidatedSimpleGenTx :: GenTx (SimpleBlock c ext)
    }
  deriving newtype (Generic, Eq, Ord)

instance (Typeable c, Typeable ext)
    => ShowProxy (GenTx (SimpleBlock c ext)) where

type instance ApplyTxErr (SimpleBlock c ext) = MockError (SimpleBlock c ext)

instance MockProtocolSpecific c ext
      => LedgerSupportsMempool (SimpleBlock c ext) where
  applyTx _cfg _wti slot tx st = do
     let st' = stowLedgerTables st
     st'' <- unstowLedgerTables
             <$> updateSimpleUTxO slot tx st'
     return ( forgetTrackingValues $ calculateDifference st st''
             , ValidatedSimpleGenTx tx )

  reapplyTx _cfg slot vtx st = applyDiffs st . fst
    <$> applyTx _cfg DoNotIntervene slot (forgetValidatedSimpleGenTx vtx) st

  -- Large value so that the Mempool tests never run out of capacity when they
  -- don't override it.
  txsMaxBytes   = const 1000000000
  txInBlockSize = txSize

  txForgetValidated = forgetValidatedSimpleGenTx

  getTransactionKeySets tx =
    let Mock.Tx _ ins _ = simpleGenTx tx
    in LedgerTables $ KeysMK ins

newtype instance TxId (GenTx (SimpleBlock c ext)) = SimpleGenTxId {
      unSimpleGenTxId :: Mock.TxId
    }
  deriving stock   (Generic)
  deriving newtype (Show, Eq, Ord, Serialise, NoThunks)

instance (Typeable c, Typeable ext)
    => ShowProxy (TxId (GenTx (SimpleBlock c ext))) where

instance HasTxId (GenTx (SimpleBlock c ext)) where
  txId = SimpleGenTxId . simpleGenTxId

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
mkSimpleGenTx tx = SimpleGenTx
    { simpleGenTx   = tx
    , simpleGenTxId = Hash.hashWithSerialiser toCBOR tx
    }

txSize :: GenTx (SimpleBlock c ext) -> Word32
txSize = fromIntegral . Lazy.length . serialise

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

instance SameDepIndex2 (BlockQuery (SimpleBlock c ext)) where
  sameDepIndex2 QueryLedgerTip QueryLedgerTip = Just Refl

deriving instance Show (BlockQuery (SimpleBlock c ext) fp result)

instance (Typeable c, Typeable ext)
    => ShowProxy (BlockQuery (SimpleBlock c ext)) where

instance (SimpleCrypto c, Typeable ext)
      => ShowQuery (BlockQuery (SimpleBlock c ext) fp) where
  showResult QueryLedgerTip = show

{-------------------------------------------------------------------------------
  Inspection
-------------------------------------------------------------------------------}

instance InspectLedger (SimpleBlock c ext) where
  -- Use defaults

{-------------------------------------------------------------------------------
  Crypto needed for simple blocks
-------------------------------------------------------------------------------}

class (HashAlgorithm (SimpleHash c), Typeable c) => SimpleCrypto c where
  type family SimpleHash c :: Type

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
  condense SimpleHeader{..} = mconcat [
        "("
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
  condense SimpleBlock{..} = mconcat [
        "("
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
      SimpleHeader{..}    = simpleHeader
      SimpleStdHeader{..} = simpleHeaderStd
      SimpleBody{..}      = simpleBody

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

instance ToCBOR SimpleBody where
  toCBOR = encode

encodeSimpleHeader :: (ext' -> CBOR.Encoding)
                   -> Header (SimpleBlock' c ext ext')
                   -> CBOR.Encoding
encodeSimpleHeader encodeExt SimpleHeader{..} =  mconcat [
      CBOR.encodeListLen 2
    , encode simpleHeaderStd
    , encodeExt simpleHeaderExt
    ]

decodeSimpleHeader :: SimpleCrypto c
                   => (ext' -> CBOR.Encoding)
                   -> (forall s. CBOR.Decoder s ext')
                   -> forall s. CBOR.Decoder s (Header (SimpleBlock' c ext ext'))
decodeSimpleHeader encodeExt decodeExt = do
    CBOR.decodeListLenOf 2
    mkSimpleHeader encodeExt <$> decode <*> decodeExt

-- | Custom 'Serialise' instance that doesn't serialise the hash
instance (SimpleCrypto c, Serialise ext')
      => Serialise (Header (SimpleBlock' c ext ext')) where
  encode = encodeSimpleHeader encode
  decode = decodeSimpleHeader encode decode

simpleBlockBinaryBlockInfo ::
     (SimpleCrypto c, Serialise ext', Typeable ext, Typeable ext')
  => SimpleBlock' c ext ext' -> BinaryBlockInfo
simpleBlockBinaryBlockInfo b = BinaryBlockInfo
    { headerOffset = 1 -- For the 'encodeListLen'
    , headerSize   = fromIntegral $ Lazy.length $ serialise (getHeader b)
    }