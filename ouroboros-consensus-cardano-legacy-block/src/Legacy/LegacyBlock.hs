{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

module Legacy.LegacyBlock (
    GenTx (..)
  , LedgerState (..)
  , LedgerTables (..)
  , LegacyBlock (..)
  , Ticked1 (..)
  , Validated (..)
  ) where

import           Cardano.Prelude (Bifunctor (..), ByteString, Coercible, Word32)
import           Data.ByteString.Short (ShortByteString)
import           Data.Coerce (coerce)
import           Data.Kind (Type)
import           Data.Void (Void)
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)
import           NoThunks.Class (NoThunks)
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config (TopLevelConfig, castTopLevelConfig)
import           Ouroboros.Consensus.Forecast (Forecast)
import           Ouroboros.Consensus.HardFork.Combinator (BlockQuery, EpochInfo,
                     Except, GenTx, HasPartialLedgerConfig (..),
                     PastHorizonException, SingleEraInfo, Ticked, Validated)
import           Ouroboros.Consensus.HardFork.Combinator.Abstract.SingleEraBlock
                     (SingleEraBlock (..))
import           Ouroboros.Consensus.HardFork.History.EraParams (EraParams)
import           Ouroboros.Consensus.HardFork.History.Summary (Bound)
import           Ouroboros.Consensus.HeaderValidation (AnnTip (..),
                     BasicEnvelopeValidation (..), HasAnnTip (..),
                     HeaderState (HeaderState), ValidateEnvelope (..))
import           Ouroboros.Consensus.Ledger.Abstract (ApplyBlock, UpdateLedger)
import           Ouroboros.Consensus.Ledger.Basics
import           Ouroboros.Consensus.Ledger.CommonProtocolParams
                     (CommonProtocolParams (..))
import           Ouroboros.Consensus.Ledger.Extended (ExtLedgerState (..))
import           Ouroboros.Consensus.Ledger.Inspect (InspectLedger (..),
                     LedgerEvent, castLedgerEvent)
import           Ouroboros.Consensus.Ledger.Query (ConfigSupportsNode,
                     QueryLedger (..), ShowQuery, TraversingQueryHandler (..))
import           Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr,
                     HasTxId (..), LedgerSupportsMempool (..), TxId,
                     WhetherToIntervene)
import           Ouroboros.Consensus.Ledger.SupportsPeerSelection
                     (LedgerSupportsPeerSelection (..))
import           Ouroboros.Consensus.Ledger.SupportsProtocol
                     (LedgerSupportsProtocol (..))
import           Ouroboros.Consensus.Ledger.Tables.Utils
import           Ouroboros.Consensus.Node.InitStorage (NodeInitStorage (..))
import           Ouroboros.Consensus.Protocol.Abstract
                     (ConsensusProtocol (LedgerView, SelectView, ValidateView))
import           Ouroboros.Consensus.Storage.ChainDB.Init (InitChainDB)
import           Ouroboros.Consensus.Storage.ImmutableDB.Chunks (ChunkInfo)
import           Ouroboros.Consensus.Storage.Serialisation (PrefixLen,
                     ReconstructNestedCtxt (..), SizeInBytes)
import           Ouroboros.Consensus.Ticked (Ticked1)
import           Ouroboros.Consensus.Util.IOLike (IOLike)

{-------------------------------------------------------------------------------
  LegacyBlock
-------------------------------------------------------------------------------}

newtype LegacyBlock (blk :: Type) = LegacyBlock { getLegacyBlock :: blk }
  deriving newtype Show
  deriving newtype (BlockSupportsMetrics, ConfigSupportsNode)

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

newtype instance BlockQuery (LegacyBlock blk) a = LegacyBlockQuery (BlockQuery blk a)
deriving newtype instance Show (BlockQuery blk a)
                       => Show (BlockQuery (LegacyBlock blk) a)
deriving newtype instance ShowQuery (BlockQuery blk)
                       => ShowQuery (BlockQuery (LegacyBlock blk))


type instance ApplyTxErr (LegacyBlock blk) = ApplyTxErr blk

newtype instance TxId (GenTx (LegacyBlock blk)) = LegacyTxId (TxId (GenTx blk))
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
        inspectLedger (castTopLevelConfig tlCfg) stBefore stAfter

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
  getPeers = getPeers . coerce

--
-- NodeInitStorage
--

instance NodeInitStorage blk => NodeInitStorage (LegacyBlock blk) where
  nodeImmutableDbChunkInfo :: StorageConfig (LegacyBlock blk) -> ChunkInfo
  nodeImmutableDbChunkInfo = nodeImmutableDbChunkInfo . coerce

  nodeCheckIntegrity :: StorageConfig (LegacyBlock blk) -> LegacyBlock blk -> Bool
  nodeCheckIntegrity = nodeCheckIntegrity . coerce

  nodeInitChainDB ::
       IOLike m
    => StorageConfig (LegacyBlock blk)
    -> InitChainDB m (LegacyBlock blk)
    -> m ()
  nodeInitChainDB = nodeInitChainDB . coerce

--
-- LedgerSupportsMempool
--

instance ( LedgerSupportsMempool blk
         , ApplyBlock (LedgerState (LegacyBlock blk)) (LegacyBlock blk)
         , NoThunks (Ticked1 (LedgerState blk) EmptyMK)
         ) => LedgerSupportsMempool (LegacyBlock blk) where
  txInvariant :: GenTx (LegacyBlock blk) -> Bool
  txInvariant = txInvariant . coerce

  applyTx ::
       LedgerConfig (LegacyBlock blk)
    -> WhetherToIntervene
    -> SlotNo
    -> GenTx (LegacyBlock blk)
    -> TickedLedgerState (LegacyBlock blk) ValuesMK
    -> Except
         (ApplyTxErr (LegacyBlock blk))
         (TickedLedgerState (LegacyBlock blk) TrackingMK,
         Validated (GenTx (LegacyBlock blk)))
  applyTx lcfg toIntervene sl tx tst =
      bimap (coerce . flip withLedgerTables emptyLedgerTables) coerce <$>
        applyTx
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
         (TickedLedgerState (LegacyBlock blk) TrackingMK)
  reapplyTx lcfg sl tx tst =
      coerce . flip withLedgerTables emptyLedgerTables <$>
        reapplyTx
          (coerce lcfg)
          sl
          (coerce tx)
          (flip withLedgerTables emptyLedgerTables . coerce $ tst)

  txsMaxBytes :: TickedLedgerState (LegacyBlock blk) mk -> Word32
  txsMaxBytes = txsMaxBytes . coerce

  txInBlockSize :: GenTx (LegacyBlock blk) -> Word32
  txInBlockSize = txInBlockSize . coerce

  txForgetValidated :: Validated (GenTx (LegacyBlock blk)) -> GenTx (LegacyBlock blk)
  txForgetValidated = coerce . txForgetValidated . coerce

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
  additionalEnvelopeChecks tcfg tlv h =
      additionalEnvelopeChecks (castTopLevelConfig tcfg) (coerce tlv) (coerce h)

--
-- HasAnnTip
--

instance HasAnnTip blk => HasAnnTip (LegacyBlock blk) where
  type TipInfo (LegacyBlock blk) = TipInfo blk

  getTipInfo :: Header (LegacyBlock blk) -> TipInfo (LegacyBlock blk)
  getTipInfo = getTipInfo . coerce

  tipInfoHash ::
       proxy (LegacyBlock blk)
    -> TipInfo (LegacyBlock blk)
    -> HeaderHash (LegacyBlock blk)
  tipInfoHash _ = coerce . tipInfoHash (Proxy @blk) . coerce

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
  headerPrevHash = castHash . headerPrevHash . getLegacyHeader

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
  protocolLedgerView lcfg tst =
      coerce $ protocolLedgerView (coerce lcfg) (coerce tst)

  ledgerViewForecastAt ::
       HasCallStack
    => LedgerConfig (LegacyBlock blk)
    -> LedgerState (LegacyBlock blk) mk
    -> Forecast (LedgerView (BlockProtocol (LegacyBlock blk)))
  ledgerViewForecastAt lcfg st =
      coerce $ ledgerViewForecastAt (coerce lcfg) (coerce st)

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
  validateView bcfg h = validateView (coerce bcfg) (coerce h)

  selectView ::
       BlockConfig (LegacyBlock blk)
    -> Header (LegacyBlock blk)
    -> SelectView (BlockProtocol (LegacyBlock blk))
  selectView bcfg h = selectView (coerce bcfg) (coerce h)

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
         , NoThunks (Ticked1 (LedgerState blk) EmptyMK)
         ) => SingleEraBlock (LegacyBlock blk) where
  singleEraTransition ::
       PartialLedgerConfig (LegacyBlock blk)
    -> EraParams
    -> Bound
    -> LedgerState (LegacyBlock blk) mk
    -> Maybe EpochNo
  singleEraTransition plcfg eps b st =
    singleEraTransition (coerce plcfg) eps b (coerce st)

  singleEraInfo :: proxy (LegacyBlock blk) -> SingleEraInfo (LegacyBlock blk)
  singleEraInfo _ = coerce $ singleEraInfo (Proxy @blk)

--
-- CommonProtocolParams
--

instance ( ApplyBlock (LedgerState (LegacyBlock blk)) (LegacyBlock blk)
         , CommonProtocolParams blk
         ) => CommonProtocolParams (LegacyBlock blk) where
  maxHeaderSize :: LedgerState (LegacyBlock blk) mk -> Word32
  maxHeaderSize = maxHeaderSize . coerce

  maxTxSize :: LedgerState (LegacyBlock blk) mk -> Word32
  maxTxSize = maxTxSize . coerce

--
-- SameDepIndex
--

deriving newtype instance SameDepIndex (BlockQuery blk)
                       => SameDepIndex (BlockQuery (LegacyBlock blk))

--
-- QueryLedger
--

instance QueryLedger blk => QueryLedger (LegacyBlock blk) where
  answerBlockQuery =
      error "answerBlockQuery: LegacyBlock does not support ledger queries"

  getQueryKeySets ::
       BlockQuery (LegacyBlock blk) result
    -> LedgerTables (LedgerState (LegacyBlock blk)) KeysMK
  getQueryKeySets = const trivialLedgerTables

  tableTraversingQuery ::
       BlockQuery (LegacyBlock blk) result
    -> Maybe (TraversingQueryHandler (LegacyBlock blk) result)
  tableTraversingQuery = fmap fixup . tableTraversingQuery . coerce
    where
      fixup ::
           TraversingQueryHandler blk result
        -> TraversingQueryHandler (LegacyBlock blk) result
      fixup (TraversingQueryHandler partial empty comb post) = TraversingQueryHandler
        (\(ExtLedgerState lst (HeaderState tip chaindep)) ->
            partial (ExtLedgerState (coerce lst) (HeaderState (fmap castAnnTip tip) chaindep)))
        empty
        comb
        post

        where
          castAnnTip AnnTip{..} = AnnTip {
                annTipSlotNo
              , annTipBlockNo
              , annTipInfo
              }

--
-- HasNestedContent
--

instance ( HasNestedContent f blk
         , Coercible (f blk) (f (LegacyBlock blk))
         ) => HasNestedContent f (LegacyBlock blk) where
  nest :: DepPair (NestedCtxt f (LegacyBlock blk)) -> f (LegacyBlock blk)
  nest = coerce . nest . depPairFirst coerce

  unnest :: f (LegacyBlock blk) -> DepPair (NestedCtxt f (LegacyBlock blk))
  unnest = depPairFirst coerce . unnest . coerce

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
  txId = txId . coerce

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

deriving anyclass instance NoThunks (Ticked1 (LedgerState blk) EmptyMK)
                        => NoThunks (Ticked1 (LedgerState (LegacyBlock blk)) mk)

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

instance HasTickedLedgerTables (LedgerState (LegacyBlock blk))

instance LedgerTablesAreTrivial (LedgerState (LegacyBlock blk)) where
  convertMapKind (LegacyLedgerState x) = LegacyLedgerState x

instance LedgerTablesAreTrivial (Ticked1 (LedgerState (LegacyBlock blk))) where
  convertMapKind (TickedLegacyLedgerState x) = TickedLegacyLedgerState x

instance CanSerializeLedgerTables (LedgerState (LegacyBlock blk))

instance CanStowLedgerTables (LedgerState (LegacyBlock blk))
