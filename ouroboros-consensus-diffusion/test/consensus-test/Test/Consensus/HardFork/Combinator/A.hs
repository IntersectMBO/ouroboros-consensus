{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Consensus.HardFork.Combinator.A
  ( BlockA (..)
  , ProtocolA
  , blockForgingA
  , safeFromTipA
  , stabilityWindowA

    -- * Additional types
  , PartialLedgerConfigA (..)
  , TxPayloadA (..)

    -- * Type family instances
  , BlockConfig (..)
  , CodecConfig (..)
  , ConsensusConfig (..)
  , GenTx (..)
  , Header (..)
  , LedgerState (..)
  , LedgerTables (..)
  , NestedCtxt_ (..)
  , StorageConfig (..)
  , TxId (..)
  ) where

import Cardano.Ledger.BaseTypes.NonZero
import Cardano.Slotting.EpochInfo
import Codec.Serialise
import Control.Monad (guard)
import qualified Data.Binary as B
import Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Short as SBS
import Data.Functor.Identity
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Void
import Data.Word
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks, OnlyCheckWhnfNamed (..))
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.BlockchainTime
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.Config.SupportsNode
import Ouroboros.Consensus.Forecast
import Ouroboros.Consensus.HardFork.Combinator
import Ouroboros.Consensus.HardFork.Combinator.Condense
import Ouroboros.Consensus.HardFork.Combinator.Serialisation.Common
import Ouroboros.Consensus.HardFork.History
  ( Bound (..)
  , EraParams (..)
  )
import qualified Ouroboros.Consensus.HardFork.History as History
import Ouroboros.Consensus.HeaderValidation
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.CommonProtocolParams
import Ouroboros.Consensus.Ledger.Inspect
import Ouroboros.Consensus.Ledger.Query
import Ouroboros.Consensus.Ledger.SupportsMempool
import Ouroboros.Consensus.Ledger.SupportsPeerSelection
import Ouroboros.Consensus.Ledger.SupportsProtocol
import Ouroboros.Consensus.Ledger.Tables.Utils
import Ouroboros.Consensus.Node.InitStorage
import Ouroboros.Consensus.Node.NetworkProtocolVersion
import Ouroboros.Consensus.Node.Run
import Ouroboros.Consensus.Node.Serialisation
import Ouroboros.Consensus.Protocol.Abstract
import Ouroboros.Consensus.Storage.ImmutableDB (simpleChunkInfo)
import Ouroboros.Consensus.Storage.LedgerDB
import Ouroboros.Consensus.Storage.Serialisation
import Ouroboros.Consensus.Util (repeatedlyM)
import Ouroboros.Consensus.Util.Condense
import Ouroboros.Consensus.Util.IndexedMemPack
import Ouroboros.Consensus.Util.Orphans ()
import Ouroboros.Network.Block
  ( Serialised
  , unwrapCBORinCBOR
  , wrapCBORinCBOR
  )
import Ouroboros.Network.Magic
import Test.Cardano.Slotting.Numeric ()
import Test.Util.Time (dawnOfTime)

{-------------------------------------------------------------------------------
  BlockA
-------------------------------------------------------------------------------}

data ProtocolA

data instance ConsensusConfig ProtocolA = CfgA
  { cfgA_k :: SecurityParam
  , cfgA_leadInSlots :: Set SlotNo
  }
  deriving NoThunks via OnlyCheckWhnfNamed "CfgA" (ConsensusConfig ProtocolA)

instance ConsensusProtocol ProtocolA where
  type ChainDepState ProtocolA = ()
  type LedgerView ProtocolA = ()
  type IsLeader ProtocolA = ()
  type CanBeLeader ProtocolA = ()
  type ValidateView ProtocolA = ()
  type ValidationErr ProtocolA = Void

  checkIsLeader CfgA{..} () slot _ =
    if slot `Set.member` cfgA_leadInSlots
      then Just ()
      else Nothing

  protocolSecurityParam = cfgA_k

  tickChainDepState _ _ _ _ = TickedTrivial
  updateChainDepState _ _ _ _ = return ()
  reupdateChainDepState _ _ _ _ = ()

data BlockA = BlkA
  { blkA_header :: Header BlockA
  , blkA_body :: [GenTx BlockA]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass Serialise
  deriving NoThunks via OnlyCheckWhnfNamed "BlkA" BlockA

data instance Header BlockA = HdrA
  { hdrA_fields :: HeaderFields BlockA
  , hdrA_prev :: ChainHash BlockA
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass Serialise
  deriving NoThunks via OnlyCheckWhnfNamed "HdrA" (Header BlockA)

instance GetHeader BlockA where
  getHeader = blkA_header
  blockMatchesHeader = \_ _ -> True -- We are not interested in integrity here
  headerIsEBB = const Nothing

data instance BlockConfig BlockA = BCfgA
  deriving (Generic, NoThunks)

type instance BlockProtocol BlockA = ProtocolA
type instance HeaderHash BlockA = Strict.ByteString

data instance CodecConfig BlockA = CCfgA
  deriving (Generic, NoThunks)

data instance StorageConfig BlockA = SCfgA
  deriving (Generic, NoThunks)

instance ConfigSupportsNode BlockA where
  getSystemStart _ = SystemStart dawnOfTime
  getNetworkMagic _ = NetworkMagic 0

instance StandardHash BlockA

instance HasHeader BlockA where
  getHeaderFields = getBlockHeaderFields

instance HasHeader (Header BlockA) where
  getHeaderFields = castHeaderFields . hdrA_fields

instance GetPrevHash BlockA where
  headerPrevHash = hdrA_prev

instance HasAnnTip BlockA

instance BasicEnvelopeValidation BlockA

-- Use defaults

instance ValidateEnvelope BlockA

data instance LedgerState BlockA mk = LgrA
  { lgrA_tip :: Point BlockA
  , lgrA_transition :: Maybe SlotNo
  -- ^ The 'SlotNo' of the block containing the 'InitiateAtoB' transaction
  }
  deriving (Show, Eq, Generic, Serialise)
  deriving NoThunks via OnlyCheckWhnfNamed "LgrA" (LedgerState BlockA mk)

-- | Ticking has no state on the A ledger state
newtype instance Ticked (LedgerState BlockA) mk = TickedLedgerStateA
  { getTickedLedgerStateA :: LedgerState BlockA mk
  }
  deriving stock (Generic, Show, Eq)
  deriving NoThunks via OnlyCheckWhnfNamed "TickedLgrA" (Ticked (LedgerState BlockA) mk)

{-------------------------------------------------------------------------------
  Ledger Tables
-------------------------------------------------------------------------------}

type instance TxIn (LedgerState BlockA) = Void
type instance TxOut (LedgerState BlockA) = Void

instance LedgerTablesAreTrivial (LedgerState BlockA) where
  convertMapKind (LgrA x y) = LgrA x y
instance LedgerTablesAreTrivial (Ticked (LedgerState BlockA)) where
  convertMapKind (TickedLedgerStateA x) = TickedLedgerStateA (convertMapKind x)
deriving via
  Void
  instance
    IndexedMemPack (LedgerState BlockA EmptyMK) Void
deriving via
  TrivialLedgerTables (LedgerState BlockA)
  instance
    HasLedgerTables (LedgerState BlockA)
deriving via
  TrivialLedgerTables (Ticked (LedgerState BlockA))
  instance
    HasLedgerTables (Ticked (LedgerState BlockA))
deriving via
  TrivialLedgerTables (LedgerState BlockA)
  instance
    CanStowLedgerTables (LedgerState BlockA)
deriving via
  TrivialLedgerTables (LedgerState BlockA)
  instance
    CanUpgradeLedgerTables (LedgerState BlockA)
deriving via
  TrivialLedgerTables (LedgerState BlockA)
  instance
    SerializeTablesWithHint (LedgerState BlockA)

data PartialLedgerConfigA = LCfgA
  { lcfgA_k :: SecurityParam
  , lcfgA_systemStart :: SystemStart
  , lcfgA_forgeTxs :: Map SlotNo [GenTx BlockA]
  }
  deriving Show
  deriving NoThunks via OnlyCheckWhnfNamed "LCfgA" PartialLedgerConfigA
  deriving Generic
  deriving Serialise

deriving newtype instance Serialise SecurityParam
instance (HasZero a, Serialise a) => Serialise (NonZero a) where
  encode = encode . unNonZero
  decode = do
    a <- decode
    case nonZero a of
      Nothing -> fail "Expected non zero but found zero!"
      Just a' -> pure a'

type instance
  LedgerCfg (LedgerState BlockA) =
    (EpochInfo Identity, PartialLedgerConfigA)

instance GetTip (LedgerState BlockA) where
  getTip = castPoint . lgrA_tip

instance GetTip (Ticked (LedgerState BlockA)) where
  getTip = castPoint . getTip . getTickedLedgerStateA

instance IsLedger (LedgerState BlockA) where
  type LedgerErr (LedgerState BlockA) = Void

  type
    AuxLedgerEvent (LedgerState BlockA) =
      VoidLedgerEvent (LedgerState BlockA)

  applyChainTickLedgerResult _ _ _ =
    pureLedgerResult
      . TickedLedgerStateA
      . noNewTickingDiffs

instance ApplyBlock (LedgerState BlockA) BlockA where
  applyBlockLedgerResultWithValidation _ _ cfg blk =
    fmap (pureLedgerResult . convertMapKind . setTip)
      . repeatedlyM
        (fmap (convertMapKind . fst) .: applyTx cfg DoNotIntervene (blockSlot blk))
        (blkA_body blk)
   where
    setTip :: TickedLedgerState BlockA mk -> LedgerState BlockA mk
    setTip (TickedLedgerStateA st) = st{lgrA_tip = blockPoint blk}

  applyBlockLedgerResult = defaultApplyBlockLedgerResult
  reapplyBlockLedgerResult =
    defaultReapplyBlockLedgerResult absurd

  getBlockKeySets _blk = trivialLedgerTables

instance UpdateLedger BlockA

instance CommonProtocolParams BlockA where
  maxHeaderSize _ = maxBound
  maxTxSize _ = maxBound

instance BlockSupportsProtocol BlockA where
  validateView _ _ = ()

instance LedgerSupportsProtocol BlockA where
  protocolLedgerView _ _ = ()
  ledgerViewForecastAt _ = trivialForecast

instance HasPartialConsensusConfig ProtocolA

instance HasPartialLedgerConfig BlockA where
  type PartialLedgerConfig BlockA = PartialLedgerConfigA

  completeLedgerConfig _ ei pcfg = (History.toPureEpochInfo ei, pcfg)

data TxPayloadA = InitiateAtoB
  deriving (Show, Eq, Generic, NoThunks, Serialise)

type instance CannotForge BlockA = Void
type instance ForgeStateInfo BlockA = ()
type instance ForgeStateUpdateError BlockA = Void

forgeBlockA ::
  TopLevelConfig BlockA ->
  BlockNo ->
  SlotNo ->
  TickedLedgerState BlockA mk ->
  [GenTx BlockA] ->
  IsLeader (BlockProtocol BlockA) ->
  BlockA
forgeBlockA tlc bno sno (TickedLedgerStateA st) _txs _ =
  BlkA
    { blkA_header =
        HdrA
          { hdrA_fields =
              HeaderFields
                { headerFieldHash = Lazy.toStrict . B.encode $ unSlotNo sno
                , headerFieldSlot = sno
                , headerFieldBlockNo = bno
                }
          , hdrA_prev = ledgerTipHash st
          }
    , blkA_body = Map.findWithDefault [] sno (lcfgA_forgeTxs ledgerConfig)
    }
 where
  ledgerConfig :: PartialLedgerConfig BlockA
  ledgerConfig = snd $ configLedger tlc

blockForgingA :: Monad m => BlockForging m BlockA
blockForgingA =
  BlockForging
    { forgeLabel = "BlockA"
    , canBeLeader = ()
    , updateForgeState = \_ _ _ -> return $ ForgeStateUpdated ()
    , checkCanForge = \_ _ _ _ _ -> return ()
    , forgeBlock = \cfg bno slot st txs proof ->
        return $
          forgeBlockA cfg bno slot st (fmap txForgetValidated txs) proof
    }

-- | See 'Ouroboros.Consensus.HardFork.History.EraParams.safeFromTip'
safeFromTipA :: SecurityParam -> Word64
safeFromTipA (SecurityParam k) = unNonZero k

-- | This mock ledger assumes that every node is honest and online, every slot
-- has a single leader, and ever message arrives before the next slot. So a run
-- of @k@ slots is guaranteed to extend the chain by @k@ blocks.
stabilityWindowA :: SecurityParam -> Word64
stabilityWindowA (SecurityParam k) = unNonZero k

data instance GenTx BlockA = TxA
  { txA_id :: TxId (GenTx BlockA)
  , txA_payload :: TxPayloadA
  }
  deriving (Show, Eq, Generic, Serialise)
  deriving NoThunks via OnlyCheckWhnfNamed "TxA" (GenTx BlockA)

newtype instance Validated (GenTx BlockA) = ValidatedGenTxA {forgetValidatedGenTxA :: GenTx BlockA}
  deriving stock Show
  deriving newtype (Generic, Eq)
  deriving anyclass NoThunks

type instance ApplyTxErr BlockA = Void

instance LedgerSupportsMempool BlockA where
  applyTx _ _wti sno tx@(TxA _ payload) (TickedLedgerStateA st) =
    case payload of
      InitiateAtoB -> do
        return (TickedLedgerStateA $ st{lgrA_transition = Just sno}, ValidatedGenTxA tx)

  reapplyTx _ cfg slot tx st =
    attachAndApplyDiffs st . fst <$> applyTx cfg DoNotIntervene slot (forgetValidatedGenTxA tx) st

  txForgetValidated = forgetValidatedGenTxA

  getTransactionKeySets _tx = trivialLedgerTables

instance TxLimits BlockA where
  type TxMeasure BlockA = IgnoringOverflow ByteSize32
  blockCapacityTxMeasure _cfg _st = IgnoringOverflow $ ByteSize32 $ 100 * 1024 -- arbitrary
  txMeasure _cfg _st _tx = pure $ IgnoringOverflow $ ByteSize32 0

newtype instance TxId (GenTx BlockA) = TxIdA Int
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (NoThunks, Serialise)

instance HasTxId (GenTx BlockA) where
  txId = txA_id

instance ConvertRawTxId (GenTx BlockA) where
  toRawTxIdHash = SBS.toShort . Lazy.toStrict . serialise

instance ShowQuery (BlockQuery BlockA fp) where
  showResult qry = case qry of {}

data instance BlockQuery BlockA fp result
  deriving Show

instance BlockSupportsLedgerQuery BlockA where
  answerPureBlockQuery _ qry = case qry of {}
  answerBlockQueryLookup _ qry = case qry of {}
  answerBlockQueryTraverse _ qry = case qry of {}
  blockQueryIsSupportedOnVersion qry _ = case qry of {}

instance SameDepIndex2 (BlockQuery BlockA) where
  sameDepIndex2 qry _qry' = case qry of {}

instance ConvertRawHash BlockA where
  toRawHash _ = id
  fromRawHash _ = id
  hashSize _ = 8 -- We use the SlotNo as the hash, which is Word64

data instance NestedCtxt_ BlockA f a where
  CtxtA :: NestedCtxt_ BlockA f (f BlockA)

deriving instance Show (NestedCtxt_ BlockA f a)
instance SameDepIndex (NestedCtxt_ BlockA f)

instance TrivialDependency (NestedCtxt_ BlockA f) where
  type TrivialIndex (NestedCtxt_ BlockA f) = f BlockA
  hasSingleIndex CtxtA CtxtA = Refl
  indexIsTrivial = CtxtA

instance EncodeDisk BlockA (Header BlockA)
instance DecodeDisk BlockA (Lazy.ByteString -> Header BlockA) where
  decodeDisk _ = const <$> decode

instance EncodeDiskDepIx (NestedCtxt Header) BlockA
instance EncodeDiskDep (NestedCtxt Header) BlockA

instance DecodeDiskDepIx (NestedCtxt Header) BlockA
instance DecodeDiskDep (NestedCtxt Header) BlockA

instance HasNestedContent Header BlockA

-- Use defaults

instance ReconstructNestedCtxt Header BlockA

-- Use defaults

instance LedgerSupportsPeerSelection BlockA where
  getPeers = const []

data UpdateA
  = ProposalSubmitted
  | ProposalStable
  deriving (Show, Eq)

instance Condense UpdateA where
  condense = show

instance InspectLedger BlockA where
  type LedgerWarning BlockA = Void
  type LedgerUpdate BlockA = UpdateA

  inspectLedger cfg before after =
    case (getConfirmationDepth before, getConfirmationDepth after) of
      (Nothing, Just _) ->
        return $ LedgerUpdate ProposalSubmitted
      (Just (_, d), Just (_, d')) -> do
        guard $ d < k && d' >= k
        return $ LedgerUpdate ProposalStable
      _otherwise ->
        []
   where
    k = stabilityWindowA (lcfgA_k (snd (configLedger cfg)))

getConfirmationDepth :: LedgerState BlockA mk -> Maybe (SlotNo, Word64)
getConfirmationDepth st = do
  confirmedInSlot <- lgrA_transition st
  return $ case ledgerTipSlot st of
    Origin -> error "impossible"
    NotOrigin s ->
      if s < confirmedInSlot
        then error "impossible"
        else
          ( confirmedInSlot
          , History.countSlots s confirmedInSlot
          )

instance NodeInitStorage BlockA where
  nodeCheckIntegrity _ _ = True

  -- Pick some chunk size
  nodeImmutableDbChunkInfo _ = simpleChunkInfo 10

instance BlockSupportsMetrics BlockA where
  isSelfIssued = isSelfIssuedConstUnknown

deriving via
  SelectViewDiffusionPipelining BlockA
  instance
    BlockSupportsDiffusionPipelining BlockA

instance SingleEraBlock BlockA where
  singleEraInfo _ = SingleEraInfo "A"

  singleEraTransition cfg EraParams{..} eraStart st = do
    (confirmedInSlot, confirmationDepth) <- getConfirmationDepth st

    -- The ledger must report the scheduled transition to the next era as soon
    -- as the block containing this transaction is immutable (that is, at
    -- least @k@ blocks have come after) -- this happens elsewhere in the
    -- corresponding 'SingleEraBlock' instance. It must not report it sooner
    -- than that because the consensus layer requires that conversions about
    -- time (when successful) must not be subject to rollback.
    guard $ confirmationDepth >= stabilityWindowA (lcfgA_k cfg)

    -- Consensus /also/ insists that as long as the transition to the next era
    -- is not yet known (ie not yet determined by an immutable block), there
    -- is a safe zone that extends past the tip of the ledger in which we
    -- guarantee the next era will not begin. This means that we must have an
    -- additional @safeFromTipA k@ blocks /after/ reporting the transition and
    -- /before/ the start of the next era.
    --
    -- Thus, we schedule the next era to begin with the first upcoming epoch
    -- that starts /after/ we're guaranteed to see both the aforementioned @k@
    -- additional blocks and also a further @safeFromTipA k@ slots after the
    -- last of those.

    let
      -- The last slot that must be in the current era
      firstPossibleLastSlotThisEra =
        History.addSlots
          (stabilityWindowA k + safeFromTipA k)
          confirmedInSlot

      -- The 'EpochNo' corresponding to 'firstPossibleLastSlotThisEra'
      lastEpochThisEra = slotToEpoch firstPossibleLastSlotThisEra

      -- The first epoch that may be in the next era
      -- (recall: eras are epoch-aligned)
      firstEpochNextEra = succ lastEpochThisEra

    return firstEpochNextEra
   where
    k = lcfgA_k cfg

    -- Slot conversion (valid for slots in this era only)
    slotToEpoch :: SlotNo -> EpochNo
    slotToEpoch s =
      History.addEpochs
        (History.countSlots s (boundSlot eraStart) `div` unEpochSize eraEpochSize)
        (boundEpoch eraStart)

instance HasTxs BlockA where
  extractTxs = blkA_body

{-------------------------------------------------------------------------------
  Condense
-------------------------------------------------------------------------------}

instance CondenseConstraints BlockA

instance Condense BlockA where condense = show
instance Condense (Header BlockA) where condense = show
instance Condense (GenTx BlockA) where condense = show
instance Condense (TxId (GenTx BlockA)) where condense = show

{-------------------------------------------------------------------------------
  Top-level serialisation constraints
-------------------------------------------------------------------------------}

instance HasBinaryBlockInfo BlockA where
  -- Standard cborg generic serialisation is:
  --
  -- > [number of fields in the product]
  -- >   [tag of the constructor]
  -- >   field1
  -- >   ..
  -- >   fieldN
  getBinaryBlockInfo BlkA{..} =
    BinaryBlockInfo
      { headerOffset = 2
      , headerSize = fromIntegral $ Lazy.length (serialise blkA_header)
      }

instance SerialiseNodeToClient BlockA PartialLedgerConfigA

-- NOTE: we will never use BlockA as a SingleEraBlock, however in order to fulfill the
-- constraints we need to be able to provide this instance.
--
-- We could follow what is done for Shelley and serialise a fixed EpochInfo, but it is
-- not worth the effort, we will never call these methods.
instance SerialiseNodeToClient BlockA (EpochInfo Identity, PartialLedgerConfigA) where
  encodeNodeToClient = error "BlockA being used as a SingleEraBlock"
  decodeNodeToClient = error "BlockA being used as a SingleEraBlock"

instance SerialiseConstraintsHFC BlockA
instance SerialiseDiskConstraints BlockA
instance SerialiseNodeToClientConstraints BlockA
instance SerialiseNodeToNodeConstraints BlockA where
  estimateBlockSize = const 0

{-------------------------------------------------------------------------------
  SerialiseDiskConstraints
-------------------------------------------------------------------------------}

deriving instance Serialise (AnnTip BlockA)

instance EncodeDisk BlockA (LedgerState BlockA EmptyMK)
instance DecodeDisk BlockA (LedgerState BlockA EmptyMK)

instance EncodeDisk BlockA BlockA
instance DecodeDisk BlockA (Lazy.ByteString -> BlockA) where
  decodeDisk _ = const <$> decode

instance EncodeDisk BlockA (AnnTip BlockA)
instance DecodeDisk BlockA (AnnTip BlockA)

instance EncodeDisk BlockA ()
instance DecodeDisk BlockA ()

instance HasNetworkProtocolVersion BlockA

{-------------------------------------------------------------------------------
  SerialiseNodeToNode
-------------------------------------------------------------------------------}

instance SerialiseNodeToNode BlockA BlockA
instance SerialiseNodeToNode BlockA Strict.ByteString
instance SerialiseNodeToNode BlockA (Serialised BlockA)
instance SerialiseNodeToNode BlockA (SerialisedHeader BlockA)
instance SerialiseNodeToNode BlockA (GenTx BlockA)
instance SerialiseNodeToNode BlockA (GenTxId BlockA)

-- Must be compatible with @(SerialisedHeader BlockA)@, which uses
-- the @Serialise (SerialisedHeader BlockA)@ instance below
instance SerialiseNodeToNode BlockA (Header BlockA) where
  encodeNodeToNode _ _ = wrapCBORinCBOR encode
  decodeNodeToNode _ _ = unwrapCBORinCBOR (const <$> decode)

instance Serialise (SerialisedHeader BlockA) where
  encode = encodeTrivialSerialisedHeader
  decode = decodeTrivialSerialisedHeader

{-------------------------------------------------------------------------------
  SerialiseNodeToClient
-------------------------------------------------------------------------------}

instance SerialiseNodeToClient BlockA BlockA
instance SerialiseNodeToClient BlockA (Serialised BlockA)
instance SerialiseNodeToClient BlockA (GenTx BlockA)
instance SerialiseNodeToClient BlockA (GenTxId BlockA)
instance SerialiseNodeToClient BlockA SlotNo

instance SerialiseNodeToClient BlockA Void where
  encodeNodeToClient _ _ = absurd
  decodeNodeToClient _ _ = fail "no ApplyTxErr to be decoded"

instance SerialiseNodeToClient BlockA (SomeBlockQuery (BlockQuery BlockA)) where
  encodeNodeToClient _ _ = \case {}
  decodeNodeToClient _ _ = fail "there are no queries to be decoded"

instance SerialiseBlockQueryResult BlockA BlockQuery where
  encodeBlockQueryResult _ _ = \case {}
  decodeBlockQueryResult _ _ = \case {}
