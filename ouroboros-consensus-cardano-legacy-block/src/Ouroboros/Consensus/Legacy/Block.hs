{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

module Ouroboros.Consensus.Legacy.Block (
    BlockConfig (..)
  , BlockQuery (..)
  , CodecConfig (..)
  , FromLegacyBlock
  , GenTx (..)
  , Header (..)
  , LedgerState (..)
  , LedgerTables (..)
  , LegacyBlock (..)
  , NestedCtxt_ (..)
  , StorageConfig (..)
  , Ticked1 (..)
  , ToLegacyBlock
  , TxId (..)
  , Validated (..)
  ) where

import           Cardano.Prelude (Bifunctor (..), ByteString, Coercible,
                     NonEmpty, Word32)
import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import           Codec.Serialise (Serialise)
import qualified Data.ByteString.Lazy as BL
import           Data.ByteString.Short (ShortByteString)
import           Data.Coerce (coerce)
import           Data.Data (Typeable)
import           Data.Kind (Type)
import           Data.Map.Strict (Map)
import           Data.Void (Void)
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)
import           NoThunks.Class (NoThunks)
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Byron.Node ()
import           Ouroboros.Consensus.Config (TopLevelConfig, castTopLevelConfig)
import           Ouroboros.Consensus.Forecast (Forecast)
import           Ouroboros.Consensus.HardFork.Abstract (HasHardForkHistory (..))
import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.Serialisation.Common
                     (SerialiseConstraintsHFC)
import           Ouroboros.Consensus.HardFork.History.EraParams (EraParams)
import           Ouroboros.Consensus.HardFork.History.Summary (Bound, Summary)
import           Ouroboros.Consensus.HeaderValidation (AnnTip,
                     BasicEnvelopeValidation (..), HasAnnTip (..),
                     ValidateEnvelope (..), castAnnTip)
import           Ouroboros.Consensus.Ledger.Abstract (ApplyBlock, UpdateLedger)
import           Ouroboros.Consensus.Ledger.Basics
import           Ouroboros.Consensus.Ledger.CommonProtocolParams
                     (CommonProtocolParams (..))
import           Ouroboros.Consensus.Ledger.Extended
                     (Ticked1 (TickedExtLedgerState, tickedHeaderState, tickedLedgerState, tickedLedgerView))
import           Ouroboros.Consensus.Ledger.Inspect (InspectLedger (..),
                     LedgerEvent, castLedgerEvent)
import           Ouroboros.Consensus.Ledger.Query (BlockSupportsLedgerQuery,
                     ConfigSupportsNode, QueryFootprint (QFNoTables),
                     ShowQuery (..), SomeBlockQuery (..))
import           Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr,
                     GenTxId, HasTxId (..), HasTxs (..),
                     LedgerSupportsMempool (..), WhetherToIntervene)
import           Ouroboros.Consensus.Ledger.SupportsPeerSelection
                     (LedgerSupportsPeerSelection (..), PoolStake,
                     StakePoolRelay)
import           Ouroboros.Consensus.Ledger.SupportsProtocol
                     (LedgerSupportsProtocol (..))
import           Ouroboros.Consensus.Ledger.Tables.Utils (emptyLedgerTables)
import           Ouroboros.Consensus.Node.InitStorage (NodeInitStorage (..))
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
                     (HasNetworkProtocolVersion (..), NodeToClientVersion,
                     NodeToNodeVersion, SupportedNetworkProtocolVersion (..))
import           Ouroboros.Consensus.Node.Run (RunNode,
                     SerialiseDiskConstraints, SerialiseNodeToClientConstraints,
                     SerialiseNodeToNodeConstraints (..))
import           Ouroboros.Consensus.Node.Serialisation
                     (SerialiseNodeToClient (..), SerialiseNodeToNode (..),
                     SerialiseResult' (..))
import           Ouroboros.Consensus.Protocol.Abstract
                     (ConsensusProtocol (ChainDepState, LedgerView, SelectView, ValidateView))
import           Ouroboros.Consensus.Storage.ChainDB.Init (InitChainDB (..))
import           Ouroboros.Consensus.Storage.ImmutableDB.Chunks (ChunkInfo)
import           Ouroboros.Consensus.Storage.Serialisation (BinaryBlockInfo,
                     DecodeDisk (..), DecodeDiskDep (..),
                     DecodeDiskDepIx (decodeDiskDepIx), EncodeDisk (..),
                     EncodeDiskDep (..), EncodeDiskDepIx (encodeDiskDepIx),
                     HasBinaryBlockInfo (..), PrefixLen,
                     ReconstructNestedCtxt (..), SerialisedHeader, SizeInBytes,
                     castSerialisedHeader)
import           Ouroboros.Consensus.Util.IOLike (IOLike)
import           Ouroboros.Consensus.Util.Singletons (SingI)
import           Ouroboros.Network.Block (Serialised)
import           Ouroboros.Network.Util.ShowProxy (ShowProxy (..))

{-------------------------------------------------------------------------------
  LegacyBlock
-------------------------------------------------------------------------------}

newtype LegacyBlock (blk :: Type) = LegacyBlock { getLegacyBlock :: blk }
  deriving newtype Show
  deriving newtype (BlockSupportsMetrics, ConfigSupportsNode)

class blk' ~ LegacyBlock blk => ToLegacyBlock blk blk'
instance blk' ~ LegacyBlock blk => ToLegacyBlock blk blk'

class blk ~ LegacyBlock blk' => FromLegacyBlock blk blk'
instance blk ~ LegacyBlock blk' => FromLegacyBlock blk blk'

{-------------------------------------------------------------------------------
  LegacyBlock: data/type family instances
-------------------------------------------------------------------------------}

type instance BlockProtocol (LegacyBlock blk) = BlockProtocol blk

newtype instance BlockConfig (LegacyBlock blk) = LegacyBlockConfig (BlockConfig blk)
deriving newtype instance NoThunks (BlockConfig blk)
                       => NoThunks (BlockConfig (LegacyBlock blk))

newtype instance CodecConfig (LegacyBlock blk) = LegacyCodecConfig (CodecConfig blk)
deriving newtype instance NoThunks (CodecConfig blk)
                       => NoThunks (CodecConfig (LegacyBlock blk))

newtype instance StorageConfig (LegacyBlock blk) = LegacyStorageConfig (StorageConfig blk)
deriving newtype instance NoThunks (StorageConfig blk)
                       => NoThunks (StorageConfig (LegacyBlock blk))

newtype instance GenTx (LegacyBlock blk) = LegacyGenTx {getLegacyGenTx :: GenTx blk}

deriving newtype instance Show (GenTx blk) => Show (GenTx (LegacyBlock blk))
deriving newtype instance Eq (GenTx blk) => Eq (GenTx (LegacyBlock blk))
deriving newtype instance NoThunks (GenTx blk) => NoThunks (GenTx (LegacyBlock blk))

newtype instance Validated (GenTx (LegacyBlock blk)) = LegacyValidatedGenTx {getLegacyValidatedGenTx :: Validated (GenTx blk)}
deriving newtype instance NoThunks (Validated (GenTx blk))
                       => NoThunks (Validated (GenTx (LegacyBlock blk)))
deriving newtype instance Eq (Validated (GenTx blk))
                       => Eq (Validated (GenTx (LegacyBlock blk)))
deriving newtype instance Show (Validated (GenTx blk))
                       => Show (Validated (GenTx (LegacyBlock blk)))

data instance BlockQuery (LegacyBlock blk) fp a where
  LegacyBlockQuery :: SingI fp' => BlockQuery blk fp' a -> BlockQuery (LegacyBlock blk) QFNoTables a

instance (forall fp'. Show (BlockQuery blk fp' a)) => Show (BlockQuery (LegacyBlock blk) fp a) where
  show (LegacyBlockQuery q) = show q

instance (forall fp'. ShowQuery (BlockQuery blk fp')) => ShowQuery (BlockQuery (LegacyBlock blk) fp) where
  showResult (LegacyBlockQuery q) = showResult q

type instance ApplyTxErr (LegacyBlock blk) = ApplyTxErr blk

newtype instance TxId (GenTx (LegacyBlock blk)) = LegacyGenTxId {
    getLegacyGenTxId :: TxId (GenTx blk)
  }
deriving newtype instance NoThunks (TxId (GenTx blk))
                       => NoThunks (TxId (GenTx (LegacyBlock blk)))
deriving newtype instance Eq (TxId (GenTx blk))
                       => Eq (TxId (GenTx (LegacyBlock blk)))
deriving newtype instance Ord (TxId (GenTx blk))
                       => Ord (TxId (GenTx (LegacyBlock blk)))
deriving newtype instance Show (TxId (GenTx blk))
                       => Show (TxId (GenTx (LegacyBlock blk)))

newtype instance Header (LegacyBlock blk) = LegacyHeader { getLegacyHeader :: Header blk}
deriving newtype instance NoThunks (Header blk)
                       => NoThunks (Header (LegacyBlock blk))
deriving newtype instance Show (Header blk)
                       => Show (Header (LegacyBlock blk))

type instance HeaderHash (LegacyBlock blk) = HeaderHash blk

type instance CannotForge (LegacyBlock blk) = CannotForge blk

type instance ForgeStateInfo (LegacyBlock blk) = ForgeStateInfo blk

type instance ForgeStateUpdateError (LegacyBlock blk) = ForgeStateUpdateError blk

newtype instance NestedCtxt_ (LegacyBlock blk) a b = LegacyNestedCtxt_ (NestedCtxt_ blk a b)
deriving newtype instance Show (NestedCtxt_ blk a b)
                       => Show (NestedCtxt_ (LegacyBlock blk) a b)
deriving newtype instance SameDepIndex (NestedCtxt_ blk f)
                       => SameDepIndex (NestedCtxt_ (LegacyBlock blk) f)

{-------------------------------------------------------------------------------
  LegacyBlock: class instances
-------------------------------------------------------------------------------}

--
-- InspectLedger
--

instance InspectLedger blk => InspectLedger (LegacyBlock blk) where
  type LedgerWarning (LegacyBlock blk) = LedgerWarning blk
  type LedgerUpdate (LegacyBlock blk) = LedgerUpdate blk

  inspectLedger ::
       TopLevelConfig (LegacyBlock blk)
    -> LedgerState (LegacyBlock blk) mk1
    -> LedgerState (LegacyBlock blk) mk2
    -> [LedgerEvent (LegacyBlock blk)]
  inspectLedger tlCfg (coerce -> stBefore) (coerce -> stAfter) =
      castLedgerEvent <$>
        inspectLedger @blk (castTopLevelConfig tlCfg) stBefore stAfter

--
-- ConvertRawHash
--

instance ConvertRawHash blk => ConvertRawHash (LegacyBlock blk) where
  hashSize :: proxy (LegacyBlock blk) -> Word32
  hashSize _ = hashSize (Proxy @blk)

  toRawHash ::
       proxy (LegacyBlock blk)
    -> HeaderHash (LegacyBlock blk)
    -> ByteString
  toRawHash _ = toRawHash (Proxy @blk)

  fromRawHash ::
       proxy (LegacyBlock blk)
    -> ByteString
    -> HeaderHash (LegacyBlock blk)
  fromRawHash _ = fromRawHash (Proxy @blk)

--
-- LedgerSupportsPeerSelection
--

instance LedgerSupportsPeerSelection blk
      => LedgerSupportsPeerSelection (LegacyBlock blk) where
  getPeers ::
       LedgerState (LegacyBlock blk) mk
    -> [(PoolStake, NonEmpty StakePoolRelay)]
  getPeers = coerce $ getPeers @blk

--
-- NodeInitStorage
--

instance NodeInitStorage blk => NodeInitStorage (LegacyBlock blk) where
  nodeImmutableDbChunkInfo :: StorageConfig (LegacyBlock blk) -> ChunkInfo
  nodeImmutableDbChunkInfo = coerce $ nodeImmutableDbChunkInfo @blk

  nodeCheckIntegrity :: StorageConfig (LegacyBlock blk) -> LegacyBlock blk -> Bool
  nodeCheckIntegrity = coerce $ nodeCheckIntegrity @blk

  nodeInitChainDB ::
       forall m. IOLike m
    => StorageConfig (LegacyBlock blk)
    -> InitChainDB m (LegacyBlock blk)
    -> m ()
  nodeInitChainDB scfg initCDB =
      nodeInitChainDB @blk @m (coerce scfg) (conv initCDB)
    where
      conv :: InitChainDB m (LegacyBlock blk) -> InitChainDB m blk
      conv InitChainDB{addBlock, getCurrentLedger} = InitChainDB {
            addBlock = addBlock . LegacyBlock
          , getCurrentLedger = getLegacyLedgerState <$> getCurrentLedger
          }

--
-- LedgerSupportsMempool
--

instance ( LedgerSupportsMempool blk
         , ApplyBlock (LedgerState (LegacyBlock blk)) (LegacyBlock blk)
         ) => LedgerSupportsMempool (LegacyBlock blk) where
  txInvariant :: GenTx (LegacyBlock blk) -> Bool
  txInvariant = coerce $ txInvariant @blk

  applyTx ::
       LedgerConfig (LegacyBlock blk)
    -> WhetherToIntervene
    -> SlotNo
    -> GenTx (LegacyBlock blk)
    -> TickedLedgerState (LegacyBlock blk) ValuesMK
    -> Except
         (ApplyTxErr (LegacyBlock blk))
         (TickedLedgerState (LegacyBlock blk) DiffMK,
         Validated (GenTx (LegacyBlock blk)))
  applyTx lcfg toIntervene sl tx tst =
      bimap (coerce . flip withLedgerTables emptyLedgerTables) coerce <$>
        applyTx @blk
          (coerce lcfg)
          toIntervene
          sl
          (coerce tx)
          (flip withLedgerTables emptyLedgerTables . coerce $ tst)

  reapplyTx ::
       HasCallStack
    => LedgerConfig (LegacyBlock blk)
    -> SlotNo
    -> Validated (GenTx (LegacyBlock blk))
    -> TickedLedgerState (LegacyBlock blk) ValuesMK
    -> Except
         (ApplyTxErr (LegacyBlock blk))
         (TickedLedgerState (LegacyBlock blk) ValuesMK)
  reapplyTx lcfg sl tx tst =
      coerce . flip withLedgerTables emptyLedgerTables <$>
        reapplyTx @blk
          (coerce lcfg)
          sl
          (coerce tx)
          (flip withLedgerTables emptyLedgerTables . coerce $ tst)

  txsMaxBytes :: TickedLedgerState (LegacyBlock blk) mk -> Word32
  txsMaxBytes = coerce $ txsMaxBytes @blk

  txInBlockSize :: GenTx (LegacyBlock blk) -> Word32
  txInBlockSize = coerce $ txInBlockSize @blk

  txForgetValidated :: Validated (GenTx (LegacyBlock blk)) -> GenTx (LegacyBlock blk)
  txForgetValidated = coerce $ txForgetValidated @blk

  getTransactionKeySets ::
       GenTx (LegacyBlock blk)
    -> LedgerTables (LedgerState (LegacyBlock blk)) KeysMK
  getTransactionKeySets = const emptyLedgerTables

--
-- ValidateEnvelope
--

instance ValidateEnvelope blk => ValidateEnvelope (LegacyBlock blk) where
  type OtherHeaderEnvelopeError (LegacyBlock blk) = OtherHeaderEnvelopeError blk

  additionalEnvelopeChecks ::
       TopLevelConfig (LegacyBlock blk)
    -> Ticked (LedgerView (BlockProtocol (LegacyBlock blk)))
    -> Header (LegacyBlock blk)
    -> Except (OtherHeaderEnvelopeError (LegacyBlock blk)) ()
  additionalEnvelopeChecks tcfg = coerce $
      additionalEnvelopeChecks @blk (castTopLevelConfig tcfg)

--
-- HasAnnTip
--

instance HasAnnTip blk => HasAnnTip (LegacyBlock blk) where
  type TipInfo (LegacyBlock blk) = TipInfo blk

  getTipInfo :: Header (LegacyBlock blk) -> TipInfo (LegacyBlock blk)
  getTipInfo = coerce $ getTipInfo @blk

  tipInfoHash ::
       proxy (LegacyBlock blk)
    -> TipInfo (LegacyBlock blk)
    -> HeaderHash (LegacyBlock blk)
  tipInfoHash _ = coerce $ tipInfoHash (Proxy @blk)

--
-- BasicEnvelopeValidation
--

instance (BasicEnvelopeValidation blk, HasHeader blk)
      => BasicEnvelopeValidation (LegacyBlock blk) where
  expectedFirstBlockNo :: proxy (LegacyBlock blk) -> BlockNo
  expectedFirstBlockNo _ = expectedFirstBlockNo (Proxy @blk)

  expectedNextBlockNo ::
       proxy (LegacyBlock blk)
    -> TipInfo (LegacyBlock blk)
    -> TipInfo (LegacyBlock blk)
    -> BlockNo
    -> BlockNo
  expectedNextBlockNo _ = expectedNextBlockNo (Proxy @blk)

  minimumPossibleSlotNo :: Proxy (LegacyBlock blk) -> SlotNo
  minimumPossibleSlotNo _ = minimumPossibleSlotNo (Proxy @blk)

  minimumNextSlotNo ::
       proxy (LegacyBlock blk)
    -> TipInfo (LegacyBlock blk)
    -> TipInfo (LegacyBlock blk)
    -> SlotNo
    -> SlotNo
  minimumNextSlotNo _ = minimumNextSlotNo (Proxy @blk)

--
-- GetPrevHash
--

instance GetPrevHash blk
      => GetPrevHash (LegacyBlock blk) where
  headerPrevHash :: Header (LegacyBlock blk) -> ChainHash (LegacyBlock blk)
  headerPrevHash = castHash . headerPrevHash @blk . coerce

--
-- LedgerSupportsProtocol
--

instance ( ApplyBlock (LedgerState (LegacyBlock blk)) (LegacyBlock blk)
         , LedgerSupportsProtocol blk
         ) => LedgerSupportsProtocol (LegacyBlock blk) where
  protocolLedgerView ::
       LedgerConfig (LegacyBlock blk)
    -> TickedLedgerState (LegacyBlock blk) mk
    -> Ticked (LedgerView (BlockProtocol (LegacyBlock blk)))
  protocolLedgerView = coerce $ protocolLedgerView @blk

  ledgerViewForecastAt ::
       HasCallStack
    => LedgerConfig (LegacyBlock blk)
    -> LedgerState (LegacyBlock blk) mk
    -> Forecast (LedgerView (BlockProtocol (LegacyBlock blk)))
  ledgerViewForecastAt = coerce $ ledgerViewForecastAt @blk

--
-- UpdateLedger
--

deriving newtype instance ApplyBlock (LedgerState (LegacyBlock blk)) (LegacyBlock blk)
                       => UpdateLedger (LegacyBlock blk)

--
-- GetHeader
--

deriving newtype instance (HasHeader blk, GetHeader blk)
                       => GetHeader (LegacyBlock blk)

--
-- BlockSupportsProtocol
--

instance BlockSupportsProtocol blk
      => BlockSupportsProtocol (LegacyBlock blk) where
  validateView ::
       BlockConfig (LegacyBlock blk)
    -> Header (LegacyBlock blk)
    -> ValidateView (BlockProtocol (LegacyBlock blk))
  validateView = coerce $ validateView @blk

  selectView ::
       BlockConfig (LegacyBlock blk)
    -> Header (LegacyBlock blk)
    -> SelectView (BlockProtocol (LegacyBlock blk))
  selectView = coerce $ selectView @blk

--
-- HasPartialLedgerConfig
--

instance ( HasPartialLedgerConfig blk
         , ApplyBlock (LedgerState (LegacyBlock blk)) (LegacyBlock blk)
         ) => HasPartialLedgerConfig (LegacyBlock blk) where
  type PartialLedgerConfig (LegacyBlock blk) = PartialLedgerConfig blk

  completeLedgerConfig ::
       proxy (LegacyBlock blk)
    -> EpochInfo (Except PastHorizonException)
    -> PartialLedgerConfig (LegacyBlock blk)
    -> LedgerConfig (LegacyBlock blk)
  completeLedgerConfig _ = completeLedgerConfig (Proxy @blk)

--
-- SingleEraBlock
--

instance ( SingleEraBlock blk
         , ApplyBlock (LedgerState (LegacyBlock blk)) (LegacyBlock blk)
         , BlockSupportsLedgerQuery (LegacyBlock blk)
         ) => SingleEraBlock (LegacyBlock blk) where
  singleEraTransition ::
       PartialLedgerConfig (LegacyBlock blk)
    -> EraParams
    -> Bound
    -> LedgerState (LegacyBlock blk) mk
    -> Maybe EpochNo
  singleEraTransition = coerce $ singleEraTransition @blk

  singleEraInfo :: proxy (LegacyBlock blk) -> SingleEraInfo (LegacyBlock blk)
  singleEraInfo _ = coerce $ singleEraInfo (Proxy @blk)

--
-- CommonProtocolParams
--

instance ( ApplyBlock (LedgerState (LegacyBlock blk)) (LegacyBlock blk)
         , CommonProtocolParams blk
         ) => CommonProtocolParams (LegacyBlock blk) where
  maxHeaderSize :: LedgerState (LegacyBlock blk) mk -> Word32
  maxHeaderSize = coerce $ maxHeaderSize @blk

  maxTxSize :: LedgerState (LegacyBlock blk) mk -> Word32
  maxTxSize = coerce $ maxTxSize @blk

--
-- HasNestedContent
--

instance ( HasNestedContent f blk
         , Coercible (f blk) (f (LegacyBlock blk))
         ) => HasNestedContent f (LegacyBlock blk) where
  nest :: DepPair (NestedCtxt f (LegacyBlock blk)) -> f (LegacyBlock blk)
  nest = coerce $ nest @f @blk

  unnest :: f (LegacyBlock blk) -> DepPair (NestedCtxt f (LegacyBlock blk))
  unnest = coerce $ unnest @f @blk

--
-- ReconstructNestedCtxt
--

instance ( ReconstructNestedCtxt f blk
         , Coercible (f blk) (f (LegacyBlock blk))
         ) => ReconstructNestedCtxt f (LegacyBlock blk) where
  reconstructPrefixLen :: proxy (f (LegacyBlock blk)) -> PrefixLen
  reconstructPrefixLen _ = reconstructPrefixLen (Proxy @(f blk))

  reconstructNestedCtxt ::
       proxy (f (LegacyBlock blk))
    -> ShortByteString
    -> SizeInBytes
    -> SomeSecond (NestedCtxt f) (LegacyBlock blk)
  reconstructNestedCtxt _ =
      castSomeNestedCtxt coerce .: reconstructNestedCtxt (Proxy @(f blk))

--
-- HasTxId
--

instance HasTxId (GenTx blk) => HasTxId (GenTx (LegacyBlock blk)) where
  txId :: GenTx (LegacyBlock blk) -> TxId (GenTx (LegacyBlock blk))
  txId = coerce $ txId @(GenTx blk)

--
-- RunNode
--

instance ( BlockSupportsMetrics blk
         , CommonProtocolParams blk
         , ApplyBlock (LedgerState (LegacyBlock blk)) (LegacyBlock blk)
         , ConfigSupportsNode blk
         , HasTxId (GenTx blk)
         , InspectLedger blk
         , LedgerSupportsMempool blk
         , LedgerSupportsPeerSelection blk
         , LedgerSupportsProtocol blk
         , NodeInitStorage blk
         , HasHardForkHistory blk
         , ShowProxy (ApplyTxErr blk)
         , ReconstructNestedCtxt Header blk
         , Serialise (HeaderHash blk)
         , EncodeDisk (LegacyBlock blk) (ChainDepState (BlockProtocol blk))
         , DecodeDisk (LegacyBlock blk) (ChainDepState (BlockProtocol blk))
         , SerialiseNodeToClient (LegacyBlock blk) (ApplyTxErr blk)
         , SerialiseNodeToNodeConstraints blk
         , Show (CannotForge blk)
         , Show (ForgeStateInfo blk)
         , Show (ForgeStateUpdateError blk)
         , SupportedNetworkProtocolVersion blk
         , BlockSupportsLedgerQuery (LegacyBlock blk)
         , SerialiseResult' blk BlockQuery
         , SerialiseNodeToClient blk (GenTx blk)
         , SerialiseNodeToClient blk (GenTxId blk)
         , SerialiseNodeToClient blk blk
         , SerialiseNodeToClient blk (Serialised blk)
         , EncodeDisk blk blk
         , EncodeDisk blk (LedgerState blk EmptyMK)
         , EncodeDisk blk (AnnTip blk)
         , DecodeDisk blk (BL.ByteString -> blk)
         , DecodeDisk blk (LedgerState blk EmptyMK)
         , DecodeDisk blk (AnnTip blk)
         , SerialiseNodeToClient blk (SomeBlockQuery (BlockQuery blk))
         , SerialiseNodeToClient blk SlotNo
         , ShowProxy (BlockQuery blk)
         , ShowProxy (GenTx blk)
         , ShowProxy (Header blk)
         , ShowProxy (TxId (GenTx blk))
         , ShowProxy blk
         , HasBinaryBlockInfo blk
         , EncodeDiskDep (NestedCtxt Header) blk
         , DecodeDiskDep (NestedCtxt Header) blk
         ) => RunNode (LegacyBlock blk)

--
-- HasHardForkHistory
--

instance HasHardForkHistory blk => HasHardForkHistory (LegacyBlock blk) where
  type instance HardForkIndices (LegacyBlock blk) = HardForkIndices blk

  hardForkSummary ::
       LedgerConfig (LegacyBlock blk)
    -> LedgerState (LegacyBlock blk) mk
    -> Summary (HardForkIndices (LegacyBlock blk))
  hardForkSummary = coerce $ hardForkSummary @blk

--
-- SupportedNetworkProtocolVersion
--

instance SupportedNetworkProtocolVersion blk
      => SupportedNetworkProtocolVersion (LegacyBlock blk) where
  supportedNodeToNodeVersions ::
       Proxy (LegacyBlock blk)
    -> Map NodeToNodeVersion (BlockNodeToNodeVersion (LegacyBlock blk))
  supportedNodeToNodeVersions _ = supportedNodeToNodeVersions (Proxy @blk)

  supportedNodeToClientVersions ::
       Proxy (LegacyBlock blk)
    -> Map NodeToClientVersion (BlockNodeToClientVersion (LegacyBlock blk))
  supportedNodeToClientVersions _ = supportedNodeToClientVersions (Proxy @blk)

  latestReleasedNodeVersion ::
       Proxy (LegacyBlock blk)
    -> (Maybe NodeToNodeVersion, Maybe NodeToClientVersion)
  latestReleasedNodeVersion _ = latestReleasedNodeVersion (Proxy @blk)

--
-- HasNetworkProtocolVersion
--

instance HasNetworkProtocolVersion blk
      => HasNetworkProtocolVersion (LegacyBlock blk) where
  type instance BlockNodeToNodeVersion (LegacyBlock blk) =
      BlockNodeToNodeVersion blk
  type instance BlockNodeToClientVersion (LegacyBlock blk) =
      BlockNodeToClientVersion blk

--
-- SerialiseNodeToNodeConstraints
--

instance SerialiseNodeToNodeConstraints blk
      => SerialiseNodeToNodeConstraints (LegacyBlock blk) where
  estimateBlockSize :: Header (LegacyBlock blk) -> SizeInBytes
  estimateBlockSize = coerce $ estimateBlockSize @blk

--
-- SerialiseNodeToNode
--

instance SerialiseNodeToNode blk (GenTxId blk)
      => SerialiseNodeToNode (LegacyBlock blk) (GenTxId (LegacyBlock blk)) where
  encodeNodeToNode ::
       CodecConfig (LegacyBlock blk)
    -> BlockNodeToNodeVersion (LegacyBlock blk)
    -> GenTxId (LegacyBlock blk)
    -> Encoding
  encodeNodeToNode = coerce $ encodeNodeToNode @blk @(GenTxId blk)

  decodeNodeToNode ::
       CodecConfig (LegacyBlock blk)
    -> BlockNodeToNodeVersion (LegacyBlock blk)
    -> forall s. Decoder s (GenTxId (LegacyBlock blk))
  decodeNodeToNode ccfg version = coerce $
      decodeNodeToNode @blk @(GenTxId blk) (coerce ccfg) version

instance SerialiseNodeToNode blk (SerialisedHeader blk)
      => SerialiseNodeToNode (LegacyBlock blk) (SerialisedHeader (LegacyBlock blk)) where
  encodeNodeToNode ::
       CodecConfig (LegacyBlock blk)
    -> BlockNodeToNodeVersion (LegacyBlock blk)
    -> SerialisedHeader (LegacyBlock blk)
    -> Encoding
  encodeNodeToNode ccfg version = conv $
      encodeNodeToNode @blk @(SerialisedHeader blk) (coerce ccfg) version
    where
      conv ::
           (SerialisedHeader blk -> Encoding)
        -> (SerialisedHeader (LegacyBlock blk) -> Encoding)
      conv f x = f $ castSerialisedHeader coerce x

  decodeNodeToNode ::
       CodecConfig (LegacyBlock blk)
    -> BlockNodeToNodeVersion (LegacyBlock blk)
    -> forall s. Decoder s (SerialisedHeader (LegacyBlock blk))
  decodeNodeToNode ccfg version = conv $
      decodeNodeToNode @blk @(SerialisedHeader blk) (coerce ccfg) version
    where
      conv ::
           Decoder s (SerialisedHeader blk)
        -> Decoder s (SerialisedHeader (LegacyBlock blk))
      conv = fmap (castSerialisedHeader coerce)

instance SerialiseNodeToNode blk (Serialised blk)
      => SerialiseNodeToNode (LegacyBlock blk) (Serialised (LegacyBlock blk)) where
  encodeNodeToNode ::
       CodecConfig (LegacyBlock blk)
    -> BlockNodeToNodeVersion (LegacyBlock blk)
    -> Serialised (LegacyBlock blk)
    -> Encoding
  encodeNodeToNode ccfg version = coerce $
      encodeNodeToNode @blk @(Serialised blk) (coerce ccfg) version

  decodeNodeToNode ::
       CodecConfig (LegacyBlock blk)
    -> BlockNodeToNodeVersion (LegacyBlock blk)
    -> forall s. Decoder s (Serialised (LegacyBlock blk))
  decodeNodeToNode ccfg version = coerce $
      decodeNodeToNode @blk @(Serialised blk) (coerce ccfg) version

instance SerialiseNodeToNode blk (GenTx blk)
      => SerialiseNodeToNode (LegacyBlock blk) (GenTx (LegacyBlock blk)) where
  encodeNodeToNode ::
       CodecConfig (LegacyBlock blk)
    -> BlockNodeToNodeVersion (LegacyBlock blk)
    -> GenTx (LegacyBlock blk)
    -> Encoding
  encodeNodeToNode ccfg version = coerce $
      encodeNodeToNode @blk @(GenTx blk) (coerce ccfg) version

  decodeNodeToNode ::
       CodecConfig (LegacyBlock blk)
    -> BlockNodeToNodeVersion (LegacyBlock blk)
    -> forall s. Decoder s (GenTx (LegacyBlock blk))
  decodeNodeToNode ccfg version = coerce $
      decodeNodeToNode @blk @(GenTx blk) (coerce ccfg) version

instance SerialiseNodeToNode blk (Header blk)
      => SerialiseNodeToNode (LegacyBlock blk) (Header (LegacyBlock blk)) where
  encodeNodeToNode ::
       CodecConfig (LegacyBlock blk)
    -> BlockNodeToNodeVersion (LegacyBlock blk)
    -> Header (LegacyBlock blk)
    -> Encoding
  encodeNodeToNode ccfg version = coerce $
      encodeNodeToNode @blk @(Header blk) (coerce ccfg) version

  decodeNodeToNode ::
       CodecConfig (LegacyBlock blk)
    -> BlockNodeToNodeVersion (LegacyBlock blk)
    -> forall s. Decoder s (Header (LegacyBlock blk))
  decodeNodeToNode ccfg version = coerce $
      decodeNodeToNode @blk @(Header blk) (coerce ccfg) version

instance SerialiseNodeToNode blk blk
      => SerialiseNodeToNode (LegacyBlock blk) (LegacyBlock blk) where
  encodeNodeToNode ::
       CodecConfig (LegacyBlock blk)
    -> BlockNodeToNodeVersion (LegacyBlock blk)
    -> LegacyBlock blk
    -> Encoding
  encodeNodeToNode ccfg version = coerce $
      encodeNodeToNode @blk @blk (coerce ccfg) version

  decodeNodeToNode ::
       CodecConfig (LegacyBlock blk)
    -> BlockNodeToNodeVersion (LegacyBlock blk)
    -> forall s. Decoder s (LegacyBlock blk)
  decodeNodeToNode ccfg version = coerce $
      decodeNodeToNode @blk @blk (coerce ccfg) version

--
-- SerialiseNodeToClientConstraints
--

instance ( ConvertRawHash blk
         , SerialiseNodeToClient (LegacyBlock blk) (ApplyTxErr (LegacyBlock blk))
         , Typeable blk
         , SerialiseResult' blk BlockQuery
         , SerialiseNodeToClient blk (GenTx blk)
         , SerialiseNodeToClient blk (GenTxId blk)
         , SerialiseNodeToClient blk blk
         , SerialiseNodeToClient blk (Serialised blk)
         , SerialiseNodeToClient blk (SomeBlockQuery (BlockQuery blk))
         , SerialiseNodeToClient blk SlotNo
         ) => SerialiseNodeToClientConstraints (LegacyBlock blk) where

--
-- SerialiseResult
--

instance SerialiseResult' blk BlockQuery
      => SerialiseResult' (LegacyBlock blk) BlockQuery where
  encodeResult' ::
       forall fp result. CodecConfig (LegacyBlock blk)
    -> BlockNodeToClientVersion (LegacyBlock blk)
    -> BlockQuery (LegacyBlock blk) fp result
    -> result
    -> Encoding
  encodeResult' ccfg version (LegacyBlockQuery q) =
      encodeResult' @QueryFootprint @blk (coerce ccfg) version q

  decodeResult' ::
       CodecConfig (LegacyBlock blk)
    -> BlockNodeToClientVersion (LegacyBlock blk)
    -> BlockQuery (LegacyBlock blk) fp result
    -> forall s. Decoder s result
  decodeResult' ccfg version (LegacyBlockQuery q) =
      decodeResult' @QueryFootprint @blk (coerce ccfg) version q

--
-- SerialiseNodeToClient
--

instance SerialiseNodeToClient blk (GenTx blk)
      => SerialiseNodeToClient (LegacyBlock blk) (GenTx (LegacyBlock blk)) where
  encodeNodeToClient ::
       CodecConfig (LegacyBlock blk)
    -> BlockNodeToClientVersion (LegacyBlock blk)
    -> GenTx (LegacyBlock blk)
    -> Encoding
  encodeNodeToClient ccfg version = coerce $
      encodeNodeToClient @blk @(GenTx blk) (coerce ccfg) version

  decodeNodeToClient ::
       CodecConfig (LegacyBlock blk)
    -> BlockNodeToClientVersion (LegacyBlock blk)
    -> forall s. Decoder s (GenTx (LegacyBlock blk))
  decodeNodeToClient ccfg version = coerce $
      decodeNodeToClient @blk @(GenTx blk) (coerce ccfg) version

instance SerialiseNodeToClient blk (GenTxId blk)
      => SerialiseNodeToClient (LegacyBlock blk) (GenTxId (LegacyBlock blk)) where
  encodeNodeToClient ::
       CodecConfig (LegacyBlock blk)
    -> BlockNodeToClientVersion (LegacyBlock blk)
    -> GenTxId (LegacyBlock blk)
    -> Encoding
  encodeNodeToClient ccfg version = coerce $
      encodeNodeToClient @blk @(GenTxId blk) (coerce ccfg) version

  decodeNodeToClient ::
       CodecConfig (LegacyBlock blk)
    -> BlockNodeToClientVersion (LegacyBlock blk)
    -> forall s. Decoder s (GenTxId (LegacyBlock blk))
  decodeNodeToClient ccfg version = coerce $
      decodeNodeToClient @blk @(GenTxId blk) (coerce ccfg) version

instance SerialiseNodeToClient blk blk
      => SerialiseNodeToClient (LegacyBlock blk) (LegacyBlock blk) where
  encodeNodeToClient ::
       CodecConfig (LegacyBlock blk)
    -> BlockNodeToClientVersion (LegacyBlock blk)
    -> LegacyBlock blk
    -> Encoding
  encodeNodeToClient ccfg version = coerce $
      encodeNodeToClient @blk @blk (coerce ccfg) version

  decodeNodeToClient ::
       CodecConfig (LegacyBlock blk)
    -> BlockNodeToClientVersion (LegacyBlock blk)
    -> forall s. Decoder s (LegacyBlock blk)
  decodeNodeToClient ccfg version = coerce $
      decodeNodeToClient @blk @blk (coerce ccfg) version

instance SerialiseNodeToClient blk (Serialised blk)
      => SerialiseNodeToClient (LegacyBlock blk) (Serialised (LegacyBlock blk)) where
  encodeNodeToClient ::
       CodecConfig (LegacyBlock blk)
    -> BlockNodeToClientVersion (LegacyBlock blk)
    -> Serialised (LegacyBlock blk)
    -> Encoding
  encodeNodeToClient ccfg version = coerce $
      encodeNodeToClient @blk @(Serialised blk) (coerce ccfg) version

  decodeNodeToClient ::
       CodecConfig (LegacyBlock blk)
    -> BlockNodeToClientVersion (LegacyBlock blk)
    -> forall s. Decoder s (Serialised (LegacyBlock blk))
  decodeNodeToClient ccfg version = coerce $
      decodeNodeToClient @blk @(Serialised blk) (coerce ccfg) version

instance SerialiseNodeToClient blk (SomeBlockQuery (BlockQuery blk))
      => SerialiseNodeToClient (LegacyBlock blk) (SomeBlockQuery (BlockQuery (LegacyBlock blk))) where
  encodeNodeToClient ::
       CodecConfig (LegacyBlock blk)
    -> BlockNodeToClientVersion (LegacyBlock blk)
    -> SomeBlockQuery (BlockQuery (LegacyBlock blk))
    -> Encoding
  encodeNodeToClient ccfg version = conv $
      encodeNodeToClient @blk @(SomeBlockQuery (BlockQuery blk)) (coerce ccfg) version
    where
      conv ::
           (SomeBlockQuery (BlockQuery blk) -> Encoding)
        -> (SomeBlockQuery (BlockQuery (LegacyBlock blk)) -> Encoding)
      conv f (SomeBlockQuery (LegacyBlockQuery q)) = f (SomeBlockQuery q)

  decodeNodeToClient ::
       CodecConfig (LegacyBlock blk)
    -> BlockNodeToClientVersion (LegacyBlock blk)
    -> forall s. Decoder s (SomeBlockQuery (BlockQuery (LegacyBlock blk)))
  decodeNodeToClient ccfg version = conv $
      decodeNodeToClient @blk @(SomeBlockQuery (BlockQuery blk)) (coerce ccfg) version
    where
      conv ::
           Decoder s (SomeBlockQuery (BlockQuery blk))
        -> Decoder s (SomeBlockQuery (BlockQuery (LegacyBlock blk)))
      conv = fmap (\(SomeBlockQuery q) -> SomeBlockQuery (LegacyBlockQuery q))

instance SerialiseNodeToClient blk SlotNo
      => SerialiseNodeToClient (LegacyBlock blk) SlotNo where
  encodeNodeToClient ::
       CodecConfig (LegacyBlock blk)
    -> BlockNodeToClientVersion (LegacyBlock blk)
    -> SlotNo
    -> Encoding
  encodeNodeToClient ccfg = encodeNodeToClient @blk (coerce ccfg)

  decodeNodeToClient ::
       CodecConfig (LegacyBlock blk)
    -> BlockNodeToClientVersion (LegacyBlock blk)
    -> forall s. Decoder s SlotNo
  decodeNodeToClient ccfg = decodeNodeToClient @blk (coerce ccfg)

--
-- SerialiseDiskConstraints
--

instance ( ReconstructNestedCtxt Header blk
         , Serialise (HeaderHash blk)
         , EncodeDisk (LegacyBlock blk) (ChainDepState (BlockProtocol blk))
         , DecodeDisk (LegacyBlock blk) (ChainDepState (BlockProtocol blk))
         , EncodeDisk blk blk
         , EncodeDisk blk (LedgerState blk EmptyMK)
         , EncodeDisk blk (AnnTip blk)
         , DecodeDisk blk (BL.ByteString -> blk)
         , DecodeDisk blk (LedgerState blk EmptyMK)
         , DecodeDisk blk (AnnTip blk)
         , HasBinaryBlockInfo blk
         , EncodeDiskDep (NestedCtxt Header) blk
         , DecodeDiskDep (NestedCtxt Header) blk
         ) => SerialiseDiskConstraints (LegacyBlock blk)

--
-- EncodeDisk
--

instance EncodeDisk blk blk
      => EncodeDisk (LegacyBlock blk) (LegacyBlock blk) where
  encodeDisk :: CodecConfig (LegacyBlock blk) -> LegacyBlock blk -> Encoding
  encodeDisk ccfg = coerce $ encodeDisk @blk @blk (coerce ccfg)

instance EncodeDisk blk (LedgerState blk EmptyMK)
      => EncodeDisk (LegacyBlock blk) (LedgerState (LegacyBlock blk) EmptyMK) where
  encodeDisk :: CodecConfig (LegacyBlock blk) -> LedgerState (LegacyBlock blk) EmptyMK -> Encoding
  encodeDisk ccfg = coerce $ encodeDisk @blk @(LedgerState blk EmptyMK) (coerce ccfg)

instance EncodeDisk blk (AnnTip blk)
      => EncodeDisk (LegacyBlock blk) (AnnTip (LegacyBlock blk)) where
  encodeDisk :: CodecConfig (LegacyBlock blk) -> AnnTip (LegacyBlock blk) -> Encoding
  encodeDisk ccfg = encodeDisk @blk @(AnnTip blk) (coerce ccfg) . castAnnTip

--
-- DecodeDisk
--

instance DecodeDisk blk (BL.ByteString -> blk)
      => DecodeDisk (LegacyBlock blk) (BL.ByteString -> LegacyBlock blk) where
  decodeDisk :: CodecConfig (LegacyBlock blk) -> forall s. Decoder s (BL.ByteString -> LegacyBlock blk)
  decodeDisk ccfg = coerce $ decodeDisk @blk @(BL.ByteString -> blk) (coerce ccfg)


instance DecodeDisk blk (LedgerState blk EmptyMK)
      => DecodeDisk (LegacyBlock blk) (LedgerState (LegacyBlock blk) EmptyMK) where
  decodeDisk :: CodecConfig (LegacyBlock blk) -> forall s. Decoder s (LedgerState (LegacyBlock blk) EmptyMK)
  decodeDisk ccfg = coerce $ decodeDisk @blk @(LedgerState blk EmptyMK) (coerce ccfg)

instance DecodeDisk blk (AnnTip blk)
      => DecodeDisk (LegacyBlock blk) (AnnTip (LegacyBlock blk)) where
  decodeDisk :: CodecConfig (LegacyBlock blk) -> forall s. Decoder s (AnnTip (LegacyBlock blk))
  decodeDisk ccfg = castAnnTip <$> decodeDisk @blk @(AnnTip blk) (coerce ccfg)

--
-- EncodeDiskDep
--

instance EncodeDiskDep (NestedCtxt Header) blk
      => EncodeDiskDep (NestedCtxt Header) (LegacyBlock blk) where
  encodeDiskDep ::
       CodecConfig (LegacyBlock blk)
    -> NestedCtxt Header (LegacyBlock blk) a
    -> a
    -> Encoding
  encodeDiskDep ccfg ctxt = encodeDiskDep @(NestedCtxt Header) @blk (coerce ccfg) (castNestedCtxt coerce ctxt)

--
-- DecodeDiskDep
--

instance DecodeDiskDep (NestedCtxt Header) blk
      => DecodeDiskDep (NestedCtxt Header) (LegacyBlock blk) where
  decodeDiskDep ::
       CodecConfig (LegacyBlock blk)
    -> NestedCtxt Header (LegacyBlock blk) a
    -> forall s. Decoder s (BL.ByteString -> a)
  decodeDiskDep ccfg ctxt = decodeDiskDep @(NestedCtxt Header) @blk (coerce ccfg) (castNestedCtxt coerce ctxt)

--
-- EncodeDiskDepIx
--

instance EncodeDiskDepIx (NestedCtxt Header) blk
      => EncodeDiskDepIx (NestedCtxt Header) (LegacyBlock blk) where
  encodeDiskDepIx ::
       CodecConfig (LegacyBlock blk)
    -> SomeSecond (NestedCtxt Header) (LegacyBlock blk)
    -> Encoding
  encodeDiskDepIx ccfg someCtxt = encodeDiskDepIx @(NestedCtxt Header) @blk (coerce ccfg) (conv someCtxt)
    where
      conv :: SomeSecond (NestedCtxt Header) (LegacyBlock blk) -> SomeSecond (NestedCtxt Header) blk
      conv (SomeSecond ctxt) = SomeSecond $ castNestedCtxt coerce ctxt

--
-- DecodeDiskDepIx
--

instance DecodeDiskDepIx (NestedCtxt Header) blk
      => DecodeDiskDepIx (NestedCtxt Header) (LegacyBlock blk) where
  decodeDiskDepIx ::
       CodecConfig (LegacyBlock blk)
    -> Decoder s (SomeSecond (NestedCtxt Header) (LegacyBlock blk))
  decodeDiskDepIx ccfg = conv <$> decodeDiskDepIx @(NestedCtxt Header) @blk (coerce ccfg)
    where
      conv ::  SomeSecond (NestedCtxt Header) blk -> SomeSecond (NestedCtxt Header) (LegacyBlock blk)
      conv (SomeSecond ctxt) = SomeSecond $ castNestedCtxt coerce ctxt

--
-- HasBinaryBlockInfo
--

instance HasBinaryBlockInfo blk
      => HasBinaryBlockInfo (LegacyBlock blk) where
  getBinaryBlockInfo :: LegacyBlock blk -> BinaryBlockInfo
  getBinaryBlockInfo = coerce $ getBinaryBlockInfo @blk

--
-- ShowProxy
--

instance ShowProxy (BlockQuery blk)
      => ShowProxy (BlockQuery (LegacyBlock blk)) where
  showProxy :: Proxy (BlockQuery (LegacyBlock blk)) -> String
  showProxy _ = showProxy (Proxy @(BlockQuery blk))

instance ShowProxy (GenTx blk)
      => ShowProxy (GenTx (LegacyBlock blk)) where
  showProxy :: Proxy (GenTx (LegacyBlock blk)) -> String
  showProxy _ = showProxy (Proxy @(GenTx blk))

instance ShowProxy (Header blk)
      => ShowProxy (Header (LegacyBlock blk)) where
  showProxy _ = showProxy (Proxy @(Header blk))

instance ShowProxy blk
      => ShowProxy (LegacyBlock blk) where
  showProxy _ = showProxy (Proxy @blk)

instance ShowProxy (TxId (GenTx blk))
      => ShowProxy (TxId (GenTx (LegacyBlock blk))) where
  showProxy _ = showProxy (Proxy @(TxId (GenTx blk)))

--
-- SerialiseConstraintsHFC
--

instance ( HasNetworkProtocolVersion blk
         , Serialise (HeaderHash blk)
         , EncodeDisk (LegacyBlock blk) (ChainDepState (BlockProtocol blk))
         , DecodeDisk (LegacyBlock blk) (ChainDepState (BlockProtocol blk))
         , SerialiseNodeToClient (LegacyBlock blk) (ApplyTxErr blk)
         , SerialiseNodeToNodeConstraints blk
         , SingleEraBlock blk
         , ApplyBlock (LedgerState (LegacyBlock blk)) (LegacyBlock blk)
         , BlockSupportsLedgerQuery (LegacyBlock blk)
         , SerialiseResult' blk BlockQuery
         , SerialiseNodeToClient blk (GenTx blk)
         , SerialiseNodeToClient blk (GenTxId blk)
         , SerialiseNodeToClient blk blk
         , SerialiseNodeToClient blk (Serialised blk)
         , EncodeDisk blk (AnnTip blk)
         , EncodeDisk blk blk
         , EncodeDisk blk (LedgerState blk EmptyMK)
         , DecodeDisk blk (BL.ByteString -> blk)
         , DecodeDisk blk (LedgerState blk EmptyMK)
         , DecodeDisk blk (AnnTip blk)
         , SerialiseNodeToClient blk (SomeBlockQuery (BlockQuery blk))
         , SerialiseNodeToClient blk SlotNo
         , HasBinaryBlockInfo blk
         , EncodeDiskDep (NestedCtxt Header) blk
         , DecodeDiskDep (NestedCtxt Header) blk
         ) => SerialiseConstraintsHFC (LegacyBlock blk)

--
-- HasTxs
--

instance HasTxs blk => HasTxs (LegacyBlock blk) where
  extractTxs :: LegacyBlock blk -> [GenTx (LegacyBlock blk)]
  extractTxs = coerce $ extractTxs @blk

--
-- SameDepIndex2
--

instance SameDepIndex2 (BlockQuery blk)
      => SameDepIndex2 (BlockQuery (LegacyBlock blk)) where
  sameDepIndex2 ::
       BlockQuery (LegacyBlock blk) x a
    -> BlockQuery (LegacyBlock blk) y b
    -> Maybe ('(x, a) :~: '(y, b))
  sameDepIndex2 (LegacyBlockQuery x) (LegacyBlockQuery y) = do
    Refl <- sameDepIndex2 x y
    pure Refl

{-------------------------------------------------------------------------------
  LedgerState
-------------------------------------------------------------------------------}

newtype instance LedgerState (LegacyBlock blk) mk = LegacyLedgerState {
    getLegacyLedgerState :: LedgerState blk EmptyMK
  }
  deriving (Generic)

deriving newtype instance Eq (LedgerState blk EmptyMK)
                       => Eq (LedgerState (LegacyBlock blk) mk)
deriving newtype instance Show (LedgerState blk EmptyMK)
                       => Show (LedgerState (LegacyBlock blk) mk)
deriving newtype instance NoThunks (LedgerState blk EmptyMK)
                      => NoThunks (LedgerState (LegacyBlock blk) mk)

newtype instance Ticked1 (LedgerState (LegacyBlock blk)) mk = TickedLegacyLedgerState {
    getTickedLegacyLedgerState :: Ticked1 (LedgerState blk) EmptyMK
  }
  deriving (Generic)

{-------------------------------------------------------------------------------
  LedgerState: instances
-------------------------------------------------------------------------------}

type instance LedgerCfg (LedgerState (LegacyBlock blk)) = LedgerCfg (LedgerState blk)

instance GetTip (LedgerState blk)
      => GetTip (LedgerState (LegacyBlock blk)) where
  getTip :: LedgerState (LegacyBlock blk) mk -> Point (LedgerState (LegacyBlock blk))
  getTip = castPoint . getTip . getLegacyLedgerState

instance GetTip (Ticked1 (LedgerState blk))
      => GetTip (Ticked1 (LedgerState (LegacyBlock blk))) where
  getTip ::
       Ticked1 (LedgerState (LegacyBlock blk)) mk
    -> Point (Ticked1 (LedgerState (LegacyBlock blk)))
  getTip = castPoint . getTip . getTickedLegacyLedgerState

instance StandardHash blk => StandardHash (LegacyBlock blk)

instance HasHeader blk => HasHeader (LegacyBlock blk) where
  getHeaderFields ::  LegacyBlock blk -> HeaderFields (LegacyBlock blk)
  getHeaderFields = castHeaderFieldsBlock . getHeaderFields . getLegacyBlock

castHeaderFieldsBlock ::
     HeaderFields blk
  -> HeaderFields (LegacyBlock blk)
castHeaderFieldsBlock HeaderFields{..} = HeaderFields{..}

instance (HasHeader blk, HasHeader (Header blk))
      => HasHeader (Header (LegacyBlock blk)) where
  getHeaderFields ::
       Header (LegacyBlock blk)
    -> HeaderFields (Header (LegacyBlock blk))
  getHeaderFields = castHeaderFieldsHeader . getHeaderFields . getLegacyHeader

castHeaderFieldsHeader ::
     HeaderFields (Header blk)
  -> HeaderFields (Header (LegacyBlock blk))
castHeaderFieldsHeader HeaderFields{..} = HeaderFields{..}

{-------------------------------------------------------------------------------
  LedgerState: tables
-------------------------------------------------------------------------------}

type instance Key   (LedgerState (LegacyBlock blk)) = Void
type instance Value (LedgerState (LegacyBlock blk)) = Void

instance HasLedgerTables (LedgerState (LegacyBlock blk))
instance HasLedgerTables (Ticked1 (LedgerState (LegacyBlock blk)))

instance LedgerTablesAreTrivial (LedgerState (LegacyBlock blk)) where
  convertMapKind (LegacyLedgerState x) = LegacyLedgerState x

instance LedgerTablesAreTrivial (Ticked1 (LedgerState (LegacyBlock blk))) where
  convertMapKind (TickedLegacyLedgerState x) = TickedLegacyLedgerState x

instance CanSerializeLedgerTables (LedgerState (LegacyBlock blk))

instance CanStowLedgerTables (LedgerState (LegacyBlock blk))
