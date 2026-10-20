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
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | A test block whose headers can carry Leios announcements.
--
-- Its protocol enforces no leader schedule and checks no crypto, so a test is
-- free to mint any chain shape and any announcement. A header can be marked
-- invalid, which its protocol then rejects, both in ordinary header validation
-- and in the out-of-context validation an announcement gets. Its ledger view is
-- forecastable only within a configured range, just like the Cardano ledger's,
-- so a test can reach the forecast horizon on demand.
--
-- No chain may exceed 'maxLeiosTestChainLength' blocks, since the block hash is
-- a path from genesis and must fit a fixed size.
module Test.Consensus.Leios.TestBlock
  ( -- * Block
    Header (..)
  , LeiosTestBlock (..)
  , maxLeiosTestChainLength

    -- * Building chains
  , firstLeiosBlock
  , forkLeiosBlock
  , setAnnouncement
  , setCounterBelowImmTips
  , setInvalid
  , setIssuer
  , setLeiosCert
  , setSlot
  , successorLeiosBlock

    -- * Ledger
  , LedgerState (..)
  , LeiosTestBlockError (..)
  , LeiosTestLedgerConfig (..)
  , leiosTestInitExtLedger

    -- * Protocol
  , LeiosTestInvalidHeader (..)
  , LeiosTestProtocol

    -- * Node configuration
  , BlockConfig (..)
  , singleNodeLeiosTestConfig
  ) where

import Cardano.Binary (DecoderError, decodeMaybe, encodeMaybe, enforceSize, fromCBOR, toCBOR)
import qualified Cardano.Crypto.Hash as Hash
import Cardano.Ledger.BaseTypes (BoundedRational (..), Milliseconds32 (..))
import Cardano.Ledger.Binary
  ( DecCBOR (..)
  , EncCBOR (..)
  , Version
  , natVersion
  , toPlainDecoder
  , toPlainEncoding
  )
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (BlockIssuer))
import Codec.CBOR.Decoding (Decoder)
import qualified Codec.CBOR.Decoding as CBOR.Dec
import Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Encoding as CBOR
import Codec.Serialise (Serialise (..), serialise)
import Control.Monad (guard, replicateM, replicateM_)
import Control.Monad.Except (Except, throwError)
import qualified Data.Binary.Get as Get
import qualified Data.Binary.Put as Put
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Short as SBS
import Data.Foldable (for_)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Measure as Measure
import Data.String (fromString)
import Data.Time.Clock (UTCTime (..))
import Data.Time.Clock.System (systemEpochDay)
import Data.Void (Void)
import Data.Word (Word64)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.BlockchainTime
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.Config.SupportsNode
import Ouroboros.Consensus.Forecast
import Ouroboros.Consensus.HardFork.Abstract
import Ouroboros.Consensus.HardFork.Combinator.Abstract
  ( ImmutableEraParams (immutableEraParams)
  )
import Ouroboros.Consensus.HardFork.History (EpochToPerasRoundInfo, forgetEraIndex)
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import Ouroboros.Consensus.HeaderValidation
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.CommonProtocolParams
  ( CommonProtocolParams (..)
  )
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Ledger.Inspect
import Ouroboros.Consensus.Ledger.Peras (initPerasState)
import Ouroboros.Consensus.Ledger.Query
import Ouroboros.Consensus.Ledger.SupportsMempool
import Ouroboros.Consensus.Ledger.SupportsPeerSelection
  ( LedgerSupportsPeerSelection (..)
  )
import Ouroboros.Consensus.Ledger.SupportsPeras (LedgerStateSupportsPeras)
import Ouroboros.Consensus.Ledger.SupportsProtocol
import Ouroboros.Consensus.Ledger.Tables.Utils
import Ouroboros.Consensus.Leios.Protocol
import Ouroboros.Consensus.Leios.Types (BytesSize, EbHash, decodeEbHash, encodeEbHash)
import Ouroboros.Consensus.Node.InitStorage (NodeInitStorage (..))
import Ouroboros.Consensus.Node.NetworkProtocolVersion
import Ouroboros.Consensus.Node.Run
  ( RunNode
  , SerialiseNodeToClientConstraints
  , SerialiseNodeToNodeConstraints (..)
  )
import Ouroboros.Consensus.Node.Serialisation
import Ouroboros.Consensus.Peras.Cert.Mock (MockPerasCert (..))
import Ouroboros.Consensus.Peras.Context
  ( StateSupportsPerasEpochContext (..)
  , mkBoundedPerasEpochContextWith
  )
import Ouroboros.Consensus.Peras.Crypto.Mock
  ( MockPerasCrypto
  , MockPerasVotingCommitteeScheme
  )
import Ouroboros.Consensus.Peras.Error.Mock (MockPerasError)
import Ouroboros.Consensus.Peras.Vote.Mock (MockPerasVote (..))
import Ouroboros.Consensus.Peras.Voting.Mock (mkMockPerasVotingCommitteeInput)
import Ouroboros.Consensus.Protocol.Abstract
import Ouroboros.Consensus.Storage.ChainDB (SerialiseDiskConstraints)
import Ouroboros.Consensus.Storage.ImmutableDB (simpleChunkInfo)
import Ouroboros.Consensus.Storage.Serialisation
import Ouroboros.Consensus.Util (ShowProxy (..))
import Ouroboros.Consensus.Util.Condense
import Ouroboros.Consensus.Util.IndexedMemPack
import Ouroboros.Consensus.Util.Orphans ()
import Ouroboros.Network.Block (Serialised)
import Ouroboros.Network.Magic (NetworkMagic (..))
import Ouroboros.Network.Tx (HasRawTxId (..))
import Test.Util.Orphans.ToExpr ()
import Test.Util.TestBlock (TestHash (..), Validity (..), unTestHash)

{-------------------------------------------------------------------------------
  Block
-------------------------------------------------------------------------------}

-- | A block is just its header: nothing here fetches or applies a body.
newtype LeiosTestBlock = LeiosTestBlock
  { ltbHeader :: Header LeiosTestBlock
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass NoThunks

-- | The hash is a path through the tree of forks, exactly as
-- 'Test.Util.TestBlock.TestHash': an identity assigned by the test rather than
-- a digest of the contents. So two headers a test needs to be distinct must sit
-- at different positions in the tree.
--
-- LIMIT: no chain may exceed 'maxLeiosTestChainLength' blocks. The path is as
-- long as the chain, and 'ConvertRawHash' requires a fixed-size hash, so
-- 'toRawHash' throws beyond that.
data instance Header LeiosTestBlock = LeiosTestHeader
  { lthHash :: !TestHash
  , lthSlot :: !SlotNo
  , lthIssuer :: !Word64
  -- ^ Stands in for the block issuer, so that the election is a real identity.
  , lthAnnouncement :: !(Maybe (EbHash, BytesSize))
  , lthValidity :: !Validity
  -- ^ Whether the protocol accepts this header; see 'setInvalid'.
  , lthCounter :: !CounterVsImmTip
  -- ^ Whether this header's operational certificate counter is below the one
  -- the immutable tip records for its issuer; see 'setCounterBelowImmTips'.
  , lthLeiosCert :: !LeiosTestCert
  -- ^ Whether this block contains a Leios certificate; see 'setLeiosCert'.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass NoThunks

data LeiosTestCert = NoLeiosTestCert | HasLeiosTestCert
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NoThunks, Serialise)

data CounterVsImmTip = HeaderCounterNotBelowImmTips | HeaderCounterBelowImmTips
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NoThunks, Serialise)

type instance HeaderHash LeiosTestBlock = TestHash

instance StandardHash LeiosTestBlock

instance ShowProxy LeiosTestBlock
instance ShowProxy (Header LeiosTestBlock)

instance HasHeader (Header LeiosTestBlock) where
  getHeaderFields hdr =
    HeaderFields
      { headerFieldHash = lthHash hdr
      , headerFieldSlot = lthSlot hdr
      , -- One less than the path length, so the first block after genesis is
        -- 'BlockNo' 0, as 'expectedFirstBlockNo' defaults to.
        headerFieldBlockNo =
          fromIntegral . subtract 1 . NE.length . unTestHash $ lthHash hdr
      }

instance HasHeader LeiosTestBlock where
  getHeaderFields = getBlockHeaderFields

instance GetHeader LeiosTestBlock where
  getHeader = ltbHeader
  blockMatchesHeader hdr blk = hdr == ltbHeader blk
  headerIsEBB = const Nothing

instance GetPrevHash LeiosTestBlock where
  headerPrevHash hdr =
    case NE.nonEmpty . NE.tail . unTestHash . lthHash $ hdr of
      Nothing -> GenesisHash
      Just prevHash -> BlockHash (TestHash prevHash)

instance Condense LeiosTestBlock where
  condense blk =
    mconcat
      [ "(H:"
      , condense (blockHash blk)
      , ",S:"
      , condense (blockSlot blk)
      , ",B:"
      , condense (unBlockNo (blockNo blk))
      , ")"
      ]

instance Condense (Header LeiosTestBlock) where
  condense = condense . LeiosTestBlock

{-------------------------------------------------------------------------------
  Building chains
-------------------------------------------------------------------------------}

-- | The first block of the given fork, in slot 1.
firstLeiosBlock :: Word64 -> LeiosTestBlock
firstLeiosBlock forkNo = mkLeiosTestBlock (TestHash (forkNo :| [])) 1

-- | The successor of the given block: @b -> b ++ [0]@, one slot later.
successorLeiosBlock :: LeiosTestBlock -> LeiosTestBlock
successorLeiosBlock blk =
  mkLeiosTestBlock
    (TestHash (NE.cons 0 (unTestHash (blockHash blk))))
    (succ (blockSlot blk))

-- | A sibling of the given block: same slot and same predecessor, different
-- hash.
forkLeiosBlock :: LeiosTestBlock -> LeiosTestBlock
forkLeiosBlock blk =
  mkLeiosTestBlock
    (TestHash (succ h :| hs))
    (blockSlot blk)
 where
  h :| hs = unTestHash (blockHash blk)

mkLeiosTestBlock :: TestHash -> SlotNo -> LeiosTestBlock
mkLeiosTestBlock hash slot =
  LeiosTestBlock
    LeiosTestHeader
      { lthHash = hash
      , lthSlot = slot
      , lthIssuer = 0
      , lthAnnouncement = Nothing
      , lthValidity = Valid
      , lthCounter = HeaderCounterNotBelowImmTips
      , lthLeiosCert = NoLeiosTestCert
      }

onHeader :: (Header LeiosTestBlock -> Header LeiosTestBlock) -> LeiosTestBlock -> LeiosTestBlock
onHeader f = LeiosTestBlock . f . ltbHeader

-- | Announce the endorser block with this hash, claiming this size.
setAnnouncement :: EbHash -> BytesSize -> LeiosTestBlock -> LeiosTestBlock
setAnnouncement ebHash size = onHeader $ \hdr -> hdr{lthAnnouncement = Just (ebHash, size)}

-- | Move this block to another slot.
setSlot :: SlotNo -> LeiosTestBlock -> LeiosTestBlock
setSlot slot = onHeader $ \hdr -> hdr{lthSlot = slot}

setIssuer :: Word64 -> LeiosTestBlock -> LeiosTestBlock
setIssuer issuer = onHeader $ \hdr -> hdr{lthIssuer = issuer}

-- | Make this block contain a Leios certificate, which certifies the endorser
-- block its predecessor announced.
setLeiosCert :: LeiosTestBlock -> LeiosTestBlock
setLeiosCert = onHeader $ \hdr -> hdr{lthLeiosCert = HasLeiosTestCert}

-- | Make the protocol reject this header.
setInvalid :: LeiosTestBlock -> LeiosTestBlock
setInvalid = onHeader $ \hdr -> hdr{lthValidity = Invalid}

-- | Put this header's operational certificate counter below the one the
-- immutable tip records for its issuer, so that its announcement is accepted
-- but not processed.
setCounterBelowImmTips :: LeiosTestBlock -> LeiosTestBlock
setCounterBelowImmTips = onHeader $ \hdr -> hdr{lthCounter = HeaderCounterBelowImmTips}

{-------------------------------------------------------------------------------
  Protocol
-------------------------------------------------------------------------------}

-- | No leader schedule and no header crypto: a header is valid exactly when the
-- test has not marked it invalid.
data LeiosTestProtocol

data LeiosTestInvalidHeader = LeiosTestInvalidHeader
  deriving stock (Eq, Show, Generic)
  deriving anyclass NoThunks

data instance ConsensusConfig LeiosTestProtocol = LeiosTestProtocolConfig
  { ltpcSecurityParam :: !SecurityParam
  }
  deriving stock Generic
  deriving anyclass NoThunks

instance ConsensusProtocol LeiosTestProtocol where
  type ChainDepState LeiosTestProtocol = ()
  type IsLeader LeiosTestProtocol = ()
  type CanBeLeader LeiosTestProtocol = ()
  type LedgerView LeiosTestProtocol = LeiosLedgerView
  type ValidationErr LeiosTestProtocol = LeiosTestInvalidHeader
  type ValidateView LeiosTestProtocol = Header LeiosTestBlock

  protocolSecurityParam = ltpcSecurityParam

  -- Every node leads in every slot; the test itself decides what gets minted.
  checkIsLeader _ _ _ _ = Just ()

  tickChainDepState _ _ _ () = TickedTrivial
  updateChainDepState _ hdr _ _ = checkValidity hdr
  reupdateChainDepState _ _ _ _ = ()

instance LeiosProtocol LeiosTestProtocol where
  getLeiosAnnouncement _proxy hdr =
    lthAnnouncement hdr >>= \(ebHash, size) ->
      pure
        MkLeiosAnnouncement
          { announcementElectee = issuerKeyHash (lthIssuer hdr)
          , announcementEbHash = ebHash
          , announcementEbSize = size
          }
  getContainsLeiosCert _proxy hdr = case lthLeiosCert hdr of
    NoLeiosTestCert -> False
    HasLeiosTestCert -> True
  validateLeiosAnnouncement _cfg hdr _slot _st = do
    checkValidity hdr
    pure $ case lthCounter hdr of
      HeaderCounterNotBelowImmTips -> FreshOCIN
      HeaderCounterBelowImmTips -> StaleOCIN
  getLeiosLedgerView _proxy = Just

-- | A test issuer's stand-in for the hash of a pool's cold verification key.
issuerKeyHash :: Word64 -> KeyHash BlockIssuer
issuerKeyHash = KeyHash . Hash.castHash . Hash.hashWith (BL.toStrict . serialise)

checkValidity :: Header LeiosTestBlock -> Except LeiosTestInvalidHeader ()
checkValidity hdr = case lthValidity hdr of
  Valid -> pure ()
  Invalid -> throwError LeiosTestInvalidHeader

type instance BlockProtocol LeiosTestBlock = LeiosTestProtocol

instance BlockSupportsProtocol LeiosTestBlock where
  validateView _ hdr = hdr

{-------------------------------------------------------------------------------
  Ledger
-------------------------------------------------------------------------------}

newtype instance LedgerState LeiosTestBlock mk = LeiosTestLedger
  { ltlsTip :: Point LeiosTestBlock
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NoThunks, Serialise)

newtype instance Ticked LedgerState LeiosTestBlock mk = TickedLeiosTestLedger
  { getTickedLeiosTestLedger :: LedgerState LeiosTestBlock mk
  }
  deriving stock Generic
  deriving anyclass NoThunks

-- | The block does not fit onto the ledger's tip.
data LeiosTestBlockError
  = LeiosTestInvalidHash
      (ChainHash LeiosTestBlock)
      (ChainHash LeiosTestBlock)
  deriving stock (Eq, Show, Generic)
  deriving anyclass NoThunks

-- | The forecast range is finite, so a test can reach 'OutsideForecastRange'
-- on demand.
data LeiosTestLedgerConfig = LeiosTestLedgerConfig
  { ltlcHardForkParams :: !HardFork.EraParams
  , ltlcForecastRange :: !SlotNo
  , ltlcLeios :: !LeiosLedgerView
  -- ^ the ledger view, the same in every slot
  }
  deriving stock (Show, Generic)
  deriving anyclass NoThunks

instance Serialise LeiosTestLedgerConfig where
  encode cfg =
    CBOR.encodeListLen 2
      <> encode
        ( ltlcHardForkParams cfg
        , ltlcForecastRange cfg
        , unboundRational (llvQuorumStakeThreshold llv)
        ,
          ( unMilliseconds32 (llvAnnouncementPeriod llv)
          , unMilliseconds32 (llvVotePeriod llv)
          , unMilliseconds32 (llvDiffusionPeriod llv)
          )
        , (llvMaxEbBodySize llv, llvMaxEbTxsSize llv)
        )
      <> toPlainEncoding leiosCodecVersion (encCBOR (llvCommittee llv))
   where
    llv = ltlcLeios cfg
  decode = do
    CBOR.Dec.decodeListLenOf 2
    ( hardForkParams
      , forecastRange
      , quorum
      , (announcementPeriod, votePeriod, diffusionPeriod)
      , (maxEbBodySize, maxEbTxsSize)
      ) <-
      decode
    committee <- toPlainDecoder Nothing leiosCodecVersion decCBOR
    quorumStakeThreshold <-
      maybe (fail "LeiosTestLedgerConfig: quorum out of range") pure $ boundRational quorum
    pure
      LeiosTestLedgerConfig
        { ltlcHardForkParams = hardForkParams
        , ltlcForecastRange = forecastRange
        , ltlcLeios =
            MkLeiosLedgerView
              { llvCommittee = committee
              , llvQuorumStakeThreshold = quorumStakeThreshold
              , llvAnnouncementPeriod = Milliseconds32 announcementPeriod
              , llvVotePeriod = Milliseconds32 votePeriod
              , llvDiffusionPeriod = Milliseconds32 diffusionPeriod
              , llvMaxEbBodySize = maxEbBodySize
              , llvMaxEbTxsSize = maxEbTxsSize
              }
        }

-- | Dijkstra's protocol version, since Leios arrives in Dijkstra.
--
-- The test ledger config uses it to encode the committee, which has only a
-- versioned codec.
leiosCodecVersion :: Version
leiosCodecVersion = natVersion @12

type instance LedgerCfg LedgerState LeiosTestBlock = LeiosTestLedgerConfig

leiosTestInitExtLedger :: LeiosTestLedgerConfig -> ExtLedgerState LeiosTestBlock ValuesMK
leiosTestInitExtLedger cfg =
  ExtLedgerState
    { ledgerState
    , headerState
    , perasState = initPerasState cfg ledgerState headerState
    }
 where
  ledgerState = LeiosTestLedger GenesisPoint
  headerState = genesisHeaderState ()

type instance TxIn LeiosTestBlock = Void
type instance TxOut LeiosTestBlock = Void

instance LedgerTablesAreTrivial LedgerState LeiosTestBlock where
  convertMapKind (LeiosTestLedger tip) = LeiosTestLedger tip
instance LedgerTablesAreTrivial (Ticked LedgerState) LeiosTestBlock where
  convertMapKind (TickedLeiosTestLedger x) = TickedLeiosTestLedger $ convertMapKind x
deriving via
  Void
  instance
    IndexedMemPack LedgerState LeiosTestBlock Void
instance HasLedgerTables LedgerState LeiosTestBlock where
  projectLedgerTables _ = emptyLedgerTables
  withLedgerTables st _ = convertMapKind st
instance HasLedgerTables (Ticked LedgerState) LeiosTestBlock where
  projectLedgerTables _ = emptyLedgerTables
  withLedgerTables st _ = convertMapKind st
instance CanStowLedgerTables (LedgerState LeiosTestBlock) where
  stowLedgerTables = convertMapKind
  unstowLedgerTables = convertMapKind
instance CanUpgradeLedgerTables LedgerState LeiosTestBlock where
  upgradeTables _ _ = id
instance SerializeTablesWithHint LedgerState LeiosTestBlock where
  decodeTablesWithHint _ = do
    _ <- CBOR.Dec.decodeMapLen
    pure (LedgerTables $ ValuesMK Map.empty)
  encodeTablesWithHint _ _ = CBOR.encodeMapLen 0

instance GetTip (LedgerState LeiosTestBlock) where
  getTip = castPoint . ltlsTip

instance GetTip (Ticked LedgerState LeiosTestBlock) where
  getTip = castPoint . ltlsTip . getTickedLeiosTestLedger

type instance AuxLedgerEvent LeiosTestBlock = VoidLedgerEvent

instance IsLedger LedgerState LeiosTestBlock where
  type LedgerErr LedgerState LeiosTestBlock = LeiosTestBlockError

  applyChainTickLedgerResult _events _cfg _slot =
    pureLedgerResult . TickedLeiosTestLedger . noNewTickingDiffs

instance ApplyBlock LedgerState LeiosTestBlock where
  applyBlockLedgerResultWithValidation
    _validation
    _events
    _cfg
    blk
    (TickedLeiosTestLedger LeiosTestLedger{ltlsTip})
      | blockPrevHash blk /= pointHash ltlsTip =
          throwError $ LeiosTestInvalidHash (pointHash ltlsTip) (blockPrevHash blk)
      | otherwise =
          pure $ pureLedgerResult $ trackingToDiffs $ LeiosTestLedger (blockPoint blk)

  applyBlockLedgerResult = defaultApplyBlockLedgerResult
  reapplyBlockLedgerResult =
    defaultReapplyBlockLedgerResult
      (error . ("reapplying a block failed: " ++) . show)

instance GetBlockKeySets LeiosTestBlock where
  getBlockKeySets = const emptyLedgerTables

instance UpdateLedger LeiosTestBlock

instance InspectLedger LeiosTestBlock

instance HasAnnTip LeiosTestBlock

instance BasicEnvelopeValidation LeiosTestBlock

instance ValidateEnvelope LeiosTestBlock

instance LedgerSupportsProtocol LeiosTestBlock where
  protocolLedgerView cfg _st = ltlcLeios cfg
  ledgerViewAtTip cfg _st = ltlcLeios cfg
  ledgerViewForecastAt cfg st =
    constantForecastInRange (Just (ltlcForecastRange cfg)) (ltlcLeios cfg) (getTipSlot st)

instance LedgerStateSupportsPeras (LedgerState LeiosTestBlock)

instance LedgerStateSupportsPeras (Ticked LedgerState LeiosTestBlock)

instance StateSupportsPerasEpochContext LeiosTestBlock where
  type MaybeEraIndexedEpochToPerasRoundInfo LeiosTestBlock = EpochToPerasRoundInfo
  toMaybeEraIndexedEpochToPerasRoundInfo _ = forgetEraIndex
  fromMaybeEraIndexedEpochToPerasRoundInfo _ = id
  mkBoundedPerasEpochContext = mkBoundedPerasEpochContextWith mkMockPerasVotingCommitteeInput

-- | A mock, as for 'Test.Util.TestBlock.TestBlock'.
instance BlockSupportsPeras LeiosTestBlock where
  type PerasCrypto LeiosTestBlock = MockPerasCrypto LeiosTestBlock
  type PerasVotingCommitteeScheme LeiosTestBlock = MockPerasVotingCommitteeScheme LeiosTestBlock
  type PerasVote LeiosTestBlock = MockPerasVote LeiosTestBlock
  type PerasCert LeiosTestBlock = MockPerasCert LeiosTestBlock
  type PerasError LeiosTestBlock = MockPerasError LeiosTestBlock
  forgePerasVoteIfEligible = defaultForgePerasVoteIfEligible
  verifyPerasVote = defaultVerifyPerasVote
  forgePerasCert = defaultForgePerasCert
  verifyPerasCert = defaultVerifyPerasCert
  getPerasCertInBlock _ = Right Nothing

instance HasHardForkHistory LeiosTestBlock where
  type HardForkIndices LeiosTestBlock = '[LeiosTestBlock]
  hardForkSummary = neverForksHardForkSummary ltlcHardForkParams

instance ImmutableEraParams LeiosTestBlock where
  immutableEraParams = ltlcHardForkParams . topLevelConfigLedger

{-------------------------------------------------------------------------------
  Configuration
-------------------------------------------------------------------------------}

data instance BlockConfig LeiosTestBlock = LeiosTestBlockConfig
  deriving stock (Show, Generic)
  deriving anyclass NoThunks

data instance CodecConfig LeiosTestBlock = LeiosTestCodecConfig
  deriving stock (Show, Generic)
  deriving anyclass NoThunks

data instance StorageConfig LeiosTestBlock = LeiosTestStorageConfig
  deriving stock (Show, Generic)
  deriving anyclass NoThunks

instance HasNetworkProtocolVersion LeiosTestBlock

instance ConfigSupportsNode LeiosTestBlock where
  -- Where io-sim starts its clock, so that slot 0 begins as a test does. An
  -- earlier date would put the node many slots past its empty chain at the
  -- outset, and a later one would leave its clock not yet started.
  getSystemStart = const (SystemStart (UTCTime systemEpochDay 0))

  getNetworkMagic = const (NetworkMagic 42)

singleNodeLeiosTestConfig ::
  LeiosTestLedgerConfig ->
  SecurityParam ->
  TopLevelConfig LeiosTestBlock
singleNodeLeiosTestConfig ledgerConfig k =
  TopLevelConfig
    { topLevelConfigProtocol = LeiosTestProtocolConfig k
    , topLevelConfigLedger = ledgerConfig
    , topLevelConfigBlock = LeiosTestBlockConfig
    , topLevelConfigCodec = LeiosTestCodecConfig
    , topLevelConfigStorage = LeiosTestStorageConfig
    , topLevelConfigCheckpoints = emptyCheckpointsMap
    }

{-------------------------------------------------------------------------------
  NestedCtxt
-------------------------------------------------------------------------------}

data instance NestedCtxt_ LeiosTestBlock f a where
  CtxtLeiosTestBlock :: NestedCtxt_ LeiosTestBlock f (f LeiosTestBlock)

deriving instance Show (NestedCtxt_ LeiosTestBlock f a)

instance TrivialDependency (NestedCtxt_ LeiosTestBlock f) where
  type TrivialIndex (NestedCtxt_ LeiosTestBlock f) = f LeiosTestBlock
  hasSingleIndex CtxtLeiosTestBlock CtxtLeiosTestBlock = Refl
  indexIsTrivial = CtxtLeiosTestBlock

instance SameDepIndex (NestedCtxt_ LeiosTestBlock f)
instance HasNestedContent f LeiosTestBlock

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

instance Serialise LeiosTestBlock where
  encode (LeiosTestBlock hdr) = encode hdr
  decode = LeiosTestBlock <$> decode

instance Serialise (Header LeiosTestBlock) where
  encode LeiosTestHeader{..} =
    encode lthHash
      <> encode lthSlot
      <> encode lthIssuer
      <> encodeMaybe encodeAnnouncement lthAnnouncement
      <> encode lthValidity
      <> encode lthCounter
      <> encode lthLeiosCert
  decode = do
    lthHash <- decode
    lthSlot <- decode
    lthIssuer <- decode
    lthAnnouncement <- decodeMaybe decodeAnnouncement
    lthValidity <- decode
    lthCounter <- decode
    lthLeiosCert <- decode
    pure LeiosTestHeader{..}

encodeAnnouncement :: (EbHash, BytesSize) -> Encoding
encodeAnnouncement (ebHash, size) =
  CBOR.encodeListLen 2 <> encodeEbHash ebHash <> encode size

decodeAnnouncement :: Decoder s (EbHash, BytesSize)
decodeAnnouncement = do
  enforceSize (fromString "Announcement") 2
  (,) <$> decodeEbHash <*> decode

instance HasBinaryBlockInfo LeiosTestBlock where
  getBinaryBlockInfo blk =
    BinaryBlockInfo
      { headerOffset = 0
      , headerSize = fromIntegral . BL.length . serialise . ltbHeader $ blk
      }

-- | The cap this block's hash imposes on chain length: its 32 bytes hold the
-- path's length and 31 path components. See the LIMIT on 'Header'
-- 'LeiosTestBlock'.
maxLeiosTestChainLength :: Int
maxLeiosTestChainLength = 31

-- | The raw hash is 32 bytes, as a real header hash is, so that a message that
-- carries one has its real size. It holds the path's length and then one byte
-- per path component, so a path component, a fork number, must be below 256.
instance ConvertRawHash LeiosTestBlock where
  type HashSize LeiosTestBlock = 32
  toRawHash _ (TestHash h)
    | len > maxLeiosTestChainLength =
        error $
          "LeiosTestBlock: chain longer than "
            <> show maxLeiosTestChainLength
            <> " blocks"
    | any (> 255) h = error "LeiosTestBlock: fork number above 255"
    | otherwise = BL.toStrict . Put.runPut $ do
        Put.putWord8 (fromIntegral len)
        for_ h (Put.putWord8 . fromIntegral)
        replicateM_ (maxLeiosTestChainLength - len) $ Put.putWord8 0
   where
    len = length h
  unsafeFromRawHash _ bs = flip Get.runGet (BL.fromStrict bs) $ do
    len <- fromIntegral <$> Get.getWord8
    (NE.nonEmpty -> Just h, rs) <-
      splitAt len <$> replicateM maxLeiosTestChainLength Get.getWord8
    guard $ all (0 ==) rs
    pure $ TestHash (fromIntegral <$> h)

instance Serialise (AnnTip LeiosTestBlock) where
  encode = defaultEncodeAnnTip encode
  decode = defaultDecodeAnnTip decode

instance Serialise (ExtLedgerState LeiosTestBlock EmptyMK) where
  encode = encodeExtLedgerState encode encode encode toCBOR
  decode = decodeExtLedgerState decode decode decode fromCBOR

instance Serialise (RealPoint LeiosTestBlock) where
  encode = encodeRealPoint encode
  decode = decodeRealPoint decode

instance EncodeDisk LeiosTestBlock LeiosTestBlock
instance DecodeDisk LeiosTestBlock (BL.ByteString -> Either DecoderError LeiosTestBlock) where
  decodeDisk _ = const . Right <$> decode

instance EncodeDisk LeiosTestBlock (Header LeiosTestBlock)
instance DecodeDisk LeiosTestBlock (BL.ByteString -> Header LeiosTestBlock) where
  decodeDisk _ = const <$> decode

instance EncodeDisk LeiosTestBlock (AnnTip LeiosTestBlock)
instance DecodeDisk LeiosTestBlock (AnnTip LeiosTestBlock)

instance ReconstructNestedCtxt Header LeiosTestBlock

instance EncodeDisk LeiosTestBlock (LedgerState LeiosTestBlock EmptyMK)
instance DecodeDisk LeiosTestBlock (LedgerState LeiosTestBlock EmptyMK)

instance EncodeDiskDep (NestedCtxt Header) LeiosTestBlock
instance DecodeDiskDep (NestedCtxt Header) LeiosTestBlock

instance EncodeDisk LeiosTestBlock ()
instance DecodeDisk LeiosTestBlock ()

instance SerialiseDiskConstraints LeiosTestBlock

deriving via
  SelectViewDiffusionPipelining LeiosTestBlock
  instance
    BlockSupportsDiffusionPipelining LeiosTestBlock

{-------------------------------------------------------------------------------
  Mempool

  'RunNode' demands a mempool even of a node that never forges. This one
  rejects every transaction.
-------------------------------------------------------------------------------}

data instance GenTx LeiosTestBlock = LeiosTestGenTx
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialise, NoThunks)

newtype instance Validated (GenTx LeiosTestBlock) = ValidatedLeiosTestGenTx
  { forgetValidatedLeiosTestGenTx :: GenTx LeiosTestBlock
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass NoThunks

data LeiosTestTxRejected = LeiosTestTxRejected
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialise, NoThunks)

type instance ApplyTxErr LeiosTestBlock = LeiosTestTxRejected

newtype instance TxId (GenTx LeiosTestBlock) = LeiosTestTxId Word64
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Serialise, NoThunks)

instance ShowProxy (GenTx LeiosTestBlock)
instance ShowProxy (TxId (GenTx LeiosTestBlock))
instance ShowProxy LeiosTestTxRejected

instance HasTxId (GenTx LeiosTestBlock) where
  txId LeiosTestGenTx = LeiosTestTxId 0

instance ConvertRawTxId (GenTx LeiosTestBlock) where
  toRawTxIdHash = SBS.toShort . BL.toStrict . serialise

instance LedgerSupportsMempool LeiosTestBlock where
  applyTx _cfg _wti _slot _tx _st = throwError LeiosTestTxRejected
  reapplyTx _cfg _slot _tx _st = throwError LeiosTestTxRejected
  txForgetValidated = forgetValidatedLeiosTestGenTx
  getTransactionKeySets _tx = emptyLedgerTables
  mkMempoolApplyTxError = nothingMkMempoolApplyTxError

instance TxLimits LeiosTestBlock where
  type TxMeasurePhase1 LeiosTestBlock = IgnoringOverflow ByteSize32
  type TxMeasurePhase2 LeiosTestBlock = TrivialTxMeasurePhase2
  txWireSize = const 0
  blockCapacityTxMeasure _cfg _st =
    TxMeasure (IgnoringOverflow (ByteSize32 (100 * 1024))) TrivialTxMeasurePhase2
  txMeasurePhase1 _cfg _st _tx = pure $ IgnoringOverflow $ ByteSize32 0
  txMeasurePhase2 _cfg _st _tx = pure TrivialTxMeasurePhase2
  type TxEbMeasure LeiosTestBlock = TxMeasure LeiosTestBlock
  txEbMeasure _ = id
  ebCapacityTxMeasure _cfg _st = Measure.zero
  mempoolEbReservation _ = id

{-------------------------------------------------------------------------------
  Queries

  No test asks the node anything, so there are no queries.
-------------------------------------------------------------------------------}

data instance BlockQuery LeiosTestBlock fp result
  deriving stock Show

instance ShowProxy (BlockQuery LeiosTestBlock)

instance ShowQuery (BlockQuery LeiosTestBlock fp) where
  showResult = \case {}

instance BlockSupportsLedgerQuery LeiosTestBlock where
  answerPureBlockQuery _ = \case {}
  answerBlockQueryLookup _ = \case {}
  answerBlockQueryTraverse _ = \case {}
  blockQueryIsSupportedOnVersion = \case {}

instance SameDepIndex2 (BlockQuery LeiosTestBlock) where
  sameDepIndex2 = \case {}

{-------------------------------------------------------------------------------
  The rest of what a node insists on
-------------------------------------------------------------------------------}

instance CommonProtocolParams LeiosTestBlock where
  maxHeaderSize _ = maxBound
  maxTxSize _ = maxBound

instance LedgerSupportsPeerSelection LeiosTestBlock where
  getPeers = const []

instance NodeInitStorage LeiosTestBlock where
  nodeCheckIntegrity _ _ = True

  -- Only how the ImmutableDB splits its blocks into files depends on this, so
  -- any positive size works. Cardano's is 10k slots, Byron's epoch size, never
  -- changed since; this one is small, so that a short test chain still spans
  -- several chunks.
  nodeImmutableDbChunkInfo _ = simpleChunkInfo (EpochSize 10)

instance BlockSupportsMetrics LeiosTestBlock where
  isSelfIssued = isSelfIssuedConstUnknown

instance BlockSupportsSanityCheck LeiosTestBlock where
  configAllSecurityParams = pure . configSecurityParam

instance SupportedNetworkProtocolVersion LeiosTestBlock where
  supportedNodeToNodeVersions _ =
    Map.singleton maxBound ()
  supportedNodeToClientVersions _ =
    Map.singleton maxBound ()
  latestReleasedNodeVersion = latestReleasedNodeVersionDefault

{-------------------------------------------------------------------------------
  Node-to-node and node-to-client serialisation

  Everything rides on the same 'Serialise' instances the on-disk format uses.
-------------------------------------------------------------------------------}

instance SerialiseNodeToNode LeiosTestBlock LeiosTestBlock
instance SerialiseNodeToNode LeiosTestBlock (Header LeiosTestBlock)
instance SerialiseNodeToNode LeiosTestBlock (Serialised LeiosTestBlock)
instance SerialiseNodeToNode LeiosTestBlock (SerialisedHeader LeiosTestBlock) where
  encodeNodeToNode _ _ = encodeTrivialSerialisedHeader
  decodeNodeToNode _ _ = decodeTrivialSerialisedHeader
instance SerialiseNodeToNode LeiosTestBlock (GenTx LeiosTestBlock)
instance SerialiseNodeToNode LeiosTestBlock (GenTxId LeiosTestBlock)

instance SerialiseNodeToNodeConstraints LeiosTestBlock where
  -- A block is just its header.
  estimateBlockSize = fromIntegral . BL.length . serialise

instance SerialiseNodeToClient LeiosTestBlock LeiosTestBlock
instance SerialiseNodeToClient LeiosTestBlock (Serialised LeiosTestBlock)
instance SerialiseNodeToClient LeiosTestBlock (GenTx LeiosTestBlock)
instance SerialiseNodeToClient LeiosTestBlock (GenTxId LeiosTestBlock)
instance SerialiseNodeToClient LeiosTestBlock SlotNo
instance SerialiseNodeToClient LeiosTestBlock LeiosTestTxRejected
instance SerialiseNodeToClient LeiosTestBlock LeiosTestLedgerConfig

instance SerialiseNodeToClient LeiosTestBlock (SomeBlockQuery (BlockQuery LeiosTestBlock)) where
  encodeNodeToClient _ _ = \case {}
  decodeNodeToClient _ _ = fail "LeiosTestBlock: no queries"

instance SerialiseBlockQueryResult LeiosTestBlock BlockQuery where
  encodeBlockQueryResult _ _ = \case {}
  decodeBlockQueryResult _ _ = \case {}

instance SerialiseNodeToClientConstraints LeiosTestBlock

instance RunNode LeiosTestBlock

-- | The node under test has no credentials, so it never forges; these exist
-- only because 'RunNode' asks for them.
type instance CannotForge LeiosTestBlock = Void

type instance ForgeStateInfo LeiosTestBlock = ()
type instance ForgeStateUpdateError LeiosTestBlock = Void

instance HasRawTxId (TxId (GenTx LeiosTestBlock)) where
  type RawTxId (TxId (GenTx LeiosTestBlock)) = Word64
  getRawTxId (LeiosTestTxId w) = w
