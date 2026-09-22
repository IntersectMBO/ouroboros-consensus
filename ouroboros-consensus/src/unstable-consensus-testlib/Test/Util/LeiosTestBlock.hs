{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- | A test block that carries Leios announcements and certificates.
--
-- Its protocol enforces no leader schedule, so a test is free to mint any
-- interesting chain shape. On the other hand, the txs and certificates it
-- carries are real. Its ledger view seats a committee that varies by epoch
-- (according to the test's by fiat updates) and is forecastable only within a
-- configured range (just like the Cardano ledger/protocol), so a forecast taken
-- at the wrong slot yields the wrong committee instead of passing unnoticed.
--
-- No chain may exceed 'maxLeiosTestChainLength' blocks, since the block hash is
-- a path from genesis and must fit a fixed size.
--
-- A header can be made to disagree with its body, by setting its body hash or
-- its certificate bit by hand. 'blockMatchesHeader' then rejects the pairing,
-- exactly as the Shelley envelope check does.
module Test.Util.LeiosTestBlock
  ( -- * Block
    ContainsCert (..)
  , Header
    ( LeiosTestHeader
    , lthAnnouncement
    , lthBodyHash
    , lthContainsCert
    , lthHash
    , lthIssuer
    , lthSlot
    , lthValidity
    )
  , LeiosTestBlock (..)
  , LeiosTestBody (..)
  , LeiosTestTx (..)
  , LeiosTestTxError (..)
  , applyLeiosTestTx
  , containsCertOf
  , hashLeiosTestBody
  , maxLeiosTestChainLength

    -- * Building chains
  , announcing
  , certifying
  , firstLeiosBlock
  , forkLeiosBlock
  , invalidate
  , issuedBy
  , setBody
  , successorLeiosBlock
  , withTxs

    -- * Ledger
  , LedgerState (LeiosTestLedger, ltlsCommittee, ltlsState, ltlsTip)
  , LeiosTestBlockError (..)
  , LeiosTestLedgerConfig (..)
  , Ticked (TickedLeiosTestLedger)
  , leiosTestInitExtLedger
  , leiosTestInitLedger
  , leiosTestLedgerConfig

    -- * Protocol
  , LeiosTestProtocol
  , LeiosTestView (..)

    -- * Node configuration
  , singleNodeLeiosTestConfig
  ) where

import Cardano.Binary (DecoderError)
import Cardano.Ledger.Binary (decodeFull', serialize', shelleyProtVer)
import Cardano.Slotting.EpochInfo (epochInfoEpoch, fixedEpochInfo)
import Codec.Serialise (Serialise (..), serialise)
import Control.Monad (foldM, guard, replicateM, replicateM_)
import Control.Monad.Except (throwError)
import qualified Data.Binary.Get as Get
import qualified Data.Binary.Put as Put
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Short as SBS
import Data.Foldable (for_)
import Data.Functor.Identity (runIdentity)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Proxy
import Data.Ratio (denominator, numerator, (%))
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..))
import Data.Void (Void)
import Data.Word (Word64)
import GHC.Generics (Generic)
import LeiosDemoLogic.Announcements.ElBimap (ElId (MkElId))
import LeiosDemoTypes
  ( BytesSize
  , HasLeiosVoting (..)
  , LeiosCert
  , LeiosCommittee
  , LeiosPoint (..)
  , RbHash (MkRbHash)
  , Weight
  )
import NoThunks.Class (NoThunks, OnlyCheckWhnfNamed (..))
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.BlockchainTime
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.Config.SupportsNode
import Ouroboros.Consensus.Forecast
import Ouroboros.Consensus.HardFork.Abstract
import Ouroboros.Consensus.HardFork.Combinator.Abstract
  ( ImmutableEraParams (immutableEraParams)
  )
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import Ouroboros.Consensus.HeaderValidation
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Ledger.Inspect
import Ouroboros.Consensus.Ledger.SupportsPeras (LedgerSupportsPeras)
import Ouroboros.Consensus.Ledger.SupportsProtocol
import Ouroboros.Consensus.Ledger.Tables.Utils
import Ouroboros.Consensus.Node.NetworkProtocolVersion
import Ouroboros.Consensus.Protocol.Abstract
import Ouroboros.Consensus.Storage.ChainDB (SerialiseDiskConstraints)
import Ouroboros.Consensus.Storage.LedgerDB
import Ouroboros.Consensus.Storage.Serialisation
import Ouroboros.Consensus.Util (ShowProxy (..))
import Ouroboros.Consensus.Util.Condense
import Ouroboros.Consensus.Util.IndexedMemPack
import Ouroboros.Consensus.Util.Orphans ()
import Ouroboros.Network.Magic (NetworkMagic (..))
import Test.Util.Orphans.ToExpr ()
import Test.Util.TestBlock (TestHash (..), Validity (..), unTestHash)

{-------------------------------------------------------------------------------
  Block
-------------------------------------------------------------------------------}

-- | Whether the header claims the body carries a Leios certificate.
--
-- The builders keep this in step with the body; setting it by hand is how a
-- test produces a header that 'blockMatchesHeader' will reject.
data ContainsCert = ContainsCert | DoesNotContainCert
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialise, NoThunks)

-- | What a truthful header would say about this body.
containsCertOf :: LeiosTestBody -> ContainsCert
containsCertOf body = case ltbCert body of
  Nothing -> DoesNotContainCert
  Just _ -> ContainsCert

data LeiosTestBody = LeiosTestBody
  { ltbCert :: !(Maybe LeiosCert)
  , ltbTxs :: ![LeiosTestTx]
  -- ^ Applied in order. A CertRB's own body is empty; its transactions live
  -- in the endorser block its certificate attests to.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass NoThunks

-- | A transaction: assertions about the ledger state, then writes to it.
--
-- An assertion of 'Nothing' requires the key to be absent, and of @'Just' c@
-- requires it to hold @c@. The writes must be non-empty, and each must either
-- introduce a key or /raise/ the value it holds. So a transaction that applies
-- changes the state, and --- because the change is monotone --- no series of
-- them can restore an earlier state. That mirrors the Cardano ledger, where a
-- consumed UTxO can never reappear, since its key commits to the transaction
-- that created it.
data LeiosTestTx = LeiosTestTx
  { ltxAsserts :: !(IntMap (Maybe Char))
  , ltxWrites :: !(IntMap Char)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialise, NoThunks)

data LeiosTestTxError
  = -- | The key, what was asserted, and what was there.
    AssertionViolated !Int !(Maybe Char) !(Maybe Char)
  | WritesNothing
  | -- | The key, the value it holds, and the value written, which does not
    -- exceed it.
    NonAscendingWrite !Int !Char !Char
  deriving stock (Eq, Show, Generic)
  deriving anyclass NoThunks

applyLeiosTestTx ::
  LeiosTestTx -> IntMap Char -> Either LeiosTestTxError (IntMap Char)
applyLeiosTestTx LeiosTestTx{ltxAsserts, ltxWrites} st
  | (key, expected) : _ <- violations =
      Left $ AssertionViolated key expected (IntMap.lookup key st)
  | IntMap.null ltxWrites = Left WritesNothing
  | (key, old, new) : _ <- nonAscending = Left $ NonAscendingWrite key old new
  | otherwise = Right $ IntMap.union ltxWrites st
 where
  violations =
    [ (key, expected)
    | (key, expected) <- IntMap.toList ltxAsserts
    , IntMap.lookup key st /= expected
    ]
  nonAscending =
    [ (key, old, new)
    | (key, new) <- IntMap.toList ltxWrites
    , Just old <- [IntMap.lookup key st]
    , old >= new
    ]

-- | A test block with some interesting Leios shapes
--
-- See the LIMIT on 'Header' 'LeiosTestBlock' for the most surprising
-- limitation.
data LeiosTestBlock = LeiosTestBlock
  { ltbHeader :: !(Header LeiosTestBlock)
  , ltbBody :: !LeiosTestBody
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass NoThunks

-- | The hash is a path through the tree of forks, exactly as
-- 'Test.Util.TestBlock.TestHash': it is an identity assigned by the test rather
-- than a digest of the contents, which is what lets a header disagree with its
-- body.
--
-- LIMIT: no chain may exceed 'maxLeiosTestChainLength' blocks. The path is as
-- long as the chain, and 'ConvertRawHash' requires a fixed-size hash, so
-- 'toRawHash' throws beyond that.
data instance Header LeiosTestBlock = LeiosTestHeader
  { lthHash :: !TestHash
  , lthSlot :: !SlotNo
  , lthIssuer :: !Word64
  -- ^ Stands in for the block issuer, so that 'headerElId' is a real election
  -- identity rather than a stub.
  , lthAnnouncement :: !(Maybe (LeiosPoint, BytesSize))
  , lthBodyHash :: !Word64
  -- ^ What binds this header to a body; see 'blockMatchesHeader'.
  , lthContainsCert :: !ContainsCert
  , lthValidity :: !Validity
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass NoThunks

-- | Enough of a digest for a test: the header must not be free to accept any
-- body.
hashLeiosTestBody :: LeiosTestBody -> Word64
hashLeiosTestBody = BL.foldl' step 5381 . serialise
 where
  step acc w = acc * 33 + fromIntegral w

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

  -- The same envelope the Shelley header checks: the header commits to the
  -- body, its cheap cert bit agrees with what the body carries, and a CertRB
  -- carries a certificate instead of transactions.
  blockMatchesHeader hdr blk =
    lthBodyHash hdr == hashLeiosTestBody body
      && lthContainsCert hdr == containsCertOf body
      && not (bodyHasCert && bodyHasTxs)
   where
    body = ltbBody blk
    bodyHasCert = case containsCertOf body of
      ContainsCert -> True
      DoesNotContainCert -> False
    bodyHasTxs = not $ null $ ltbTxs body

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
      , case ltbCert (ltbBody blk) of
          Nothing -> ""
          Just _ -> ",cert"
      , ")"
      ]

instance Condense (Header LeiosTestBlock) where
  condense = condense . flip LeiosTestBlock (LeiosTestBody Nothing [])

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
-- hash. This is how a test equivocates.
forkLeiosBlock :: LeiosTestBlock -> LeiosTestBlock
forkLeiosBlock blk =
  blk
    { ltbHeader = hdr{lthHash = TestHash (succ f :| h)}
    }
 where
  hdr = ltbHeader blk
  TestHash (f :| h) = lthHash hdr

mkLeiosTestBlock :: TestHash -> SlotNo -> LeiosTestBlock
mkLeiosTestBlock hash slot =
  LeiosTestBlock
    { ltbHeader =
        LeiosTestHeader
          { lthHash = hash
          , lthSlot = slot
          , lthIssuer = 0
          , lthAnnouncement = Nothing
          , lthBodyHash = hashLeiosTestBody body
          , lthContainsCert = containsCertOf body
          , lthValidity = Valid
          }
    , ltbBody = body
    }
 where
  body = LeiosTestBody Nothing []

-- | Replace the body, keeping the header truthful about it. A test that wants
-- a header that lies sets the header's fields itself.
setBody :: LeiosTestBody -> LeiosTestBlock -> LeiosTestBlock
setBody body blk =
  blk
    { ltbHeader =
        (ltbHeader blk)
          { lthBodyHash = hashLeiosTestBody body
          , lthContainsCert = containsCertOf body
          }
    , ltbBody = body
    }

-- | Announce an endorser block on this block's header.
announcing :: LeiosPoint -> BytesSize -> LeiosTestBlock -> LeiosTestBlock
announcing point size blk =
  blk{ltbHeader = (ltbHeader blk){lthAnnouncement = Just (point, size)}}

-- | Carry this certificate, and say so in the header.
certifying :: LeiosCert -> LeiosTestBlock -> LeiosTestBlock
certifying cert blk = setBody (ltbBody blk){ltbCert = Just cert} blk

-- | Carry these transactions.
withTxs :: [LeiosTestTx] -> LeiosTestBlock -> LeiosTestBlock
withTxs txs blk = setBody (ltbBody blk){ltbTxs = txs} blk

issuedBy :: Word64 -> LeiosTestBlock -> LeiosTestBlock
issuedBy issuer blk = blk{ltbHeader = (ltbHeader blk){lthIssuer = issuer}}

-- | Make the ledger reject this block.
invalidate :: LeiosTestBlock -> LeiosTestBlock
invalidate blk = blk{ltbHeader = (ltbHeader blk){lthValidity = Invalid}}

{-------------------------------------------------------------------------------
  Leios
-------------------------------------------------------------------------------}

instance ResolveLeiosBlock LeiosTestBlock where
  blockLeiosCert = ltbCert . ltbBody

  announcingRbHash blk = case blockPrevHash blk of
    GenesisHash -> Nothing
    BlockHash h -> Just $ MkRbHash $ toRawHash (Proxy @LeiosTestBlock) h

  headerContainsLeiosCert hdr = case lthContainsCert hdr of
    ContainsCert -> True
    DoesNotContainCert -> False

  headerLeiosAnnouncement = lthAnnouncement

  headerElId hdr =
    MkElId (lthSlot hdr) (SBS.pack [fromIntegral (lthIssuer hdr)])

-- | The committee lives in the ledger view, which the forecast computes from
-- the ledger config; the ledger /state/ carries none, so the voting-path
-- methods have nothing to offer. The harness does not vote.
instance HasLeiosVoting LeiosTestBlock where
  getLeiosCommittee = fmap fst . ltlsCommittee
  getCurrentThreshold = fmap snd . ltlsCommittee
  getMinCertificationGap _ _ = Nothing
  getLeiosCommitteeFromView _ = ltvCommittee

{-------------------------------------------------------------------------------
  Protocol
-------------------------------------------------------------------------------}

-- | No leader schedule, no header crypto, no chain-dependent state: the only
-- thing this protocol contributes is the ledger view.
data LeiosTestProtocol

-- | What Leios needs of the ledger view, and nothing else.
newtype LeiosTestView = LeiosTestView
  { ltvCommittee :: Maybe (LeiosCommittee, Weight)
  }
  deriving stock Generic
  deriving anyclass NoThunks

instance Show LeiosTestView where
  show (LeiosTestView mbCommittee) = case mbCommittee of
    Nothing -> "LeiosTestView{no committee}"
    Just (_committee, threshold) ->
      -- The committee's own 'Show' would dwarf every counterexample it appears in.
      "LeiosTestView{committee = <elided>, threshold = " <> show threshold <> "}"

data instance ConsensusConfig LeiosTestProtocol = LeiosTestProtocolConfig
  { ltpcSecurityParam :: !SecurityParam
  }
  deriving stock Generic
  deriving anyclass NoThunks

instance ConsensusProtocol LeiosTestProtocol where
  type ChainDepState LeiosTestProtocol = ()
  type IsLeader LeiosTestProtocol = ()
  type CanBeLeader LeiosTestProtocol = ()
  type LedgerView LeiosTestProtocol = LeiosTestView
  type ValidationErr LeiosTestProtocol = Void
  type ValidateView LeiosTestProtocol = ()

  protocolSecurityParam = ltpcSecurityParam

  -- Every node leads in every slot; the test itself decides what actually gets
  -- minted.
  checkIsLeader _ _ _ _ = Just ()

  tickChainDepState _ _ _ _ = TickedTrivial
  updateChainDepState _ _ _ _ = pure ()
  reupdateChainDepState _ _ _ _ = ()

type instance BlockProtocol LeiosTestBlock = LeiosTestProtocol

instance BlockSupportsProtocol LeiosTestBlock where
  validateView _ _ = ()

{-------------------------------------------------------------------------------
  Ledger
-------------------------------------------------------------------------------}

data instance LedgerState LeiosTestBlock mk = LeiosTestLedger
  { ltlsTip :: !(Point LeiosTestBlock)
  , ltlsState :: !(IntMap Char)
  , ltlsCommittee :: !(Maybe (LeiosCommittee, Weight))
  -- ^ The committee seated for this state's slot.
  --
  -- Transactions never affect it; it is merely a cache of the oracle that
  -- 'ltlcCommittees' is. Keeping it here is what keeps 'getLeiosCommittee' in
  -- agreement with 'getLeiosCommitteeFromView': both are that oracle applied
  -- to the same slot.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass NoThunks

newtype instance Ticked (LedgerState LeiosTestBlock) mk = TickedLeiosTestLedger
  { getTickedLeiosTestLedger :: LedgerState LeiosTestBlock mk
  }
  deriving stock Generic
  deriving anyclass NoThunks

data LeiosTestBlockError
  = -- | The block does not fit onto the ledger's tip.
    LeiosTestInvalidHash
      (ChainHash LeiosTestBlock)
      (ChainHash LeiosTestBlock)
  | -- | The block is marked invalid; see 'invalidate'.
    LeiosTestInvalidBlock
  | -- | One of the block's transactions does not apply.
    LeiosTestInvalidTx !LeiosTestTxError
  deriving stock (Eq, Show, Generic)
  deriving anyclass NoThunks

-- | The committee schedule is a function of the epoch, so a test can put a
-- committee change wherever it likes, and the forecast range is finite, so a
-- test can reach 'OutsideForecastRange' on demand.
data LeiosTestLedgerConfig = LeiosTestLedgerConfig
  { ltlcHardForkParams :: !HardFork.EraParams
  , ltlcForecastRange :: !SlotNo
  , ltlcCommittees :: EpochNo -> Maybe (LeiosCommittee, Weight)
  }
  deriving NoThunks via OnlyCheckWhnfNamed "LeiosTestLedgerConfig" LeiosTestLedgerConfig

instance Show LeiosTestLedgerConfig where
  show LeiosTestLedgerConfig{ltlcHardForkParams, ltlcForecastRange} =
    "LeiosTestLedgerConfig "
      <> show ltlcHardForkParams
      <> " "
      <> show ltlcForecastRange
      <> " <committees>"

leiosTestLedgerConfig ::
  HardFork.EraParams ->
  SlotNo ->
  (EpochNo -> Maybe (LeiosCommittee, Weight)) ->
  LeiosTestLedgerConfig
leiosTestLedgerConfig = LeiosTestLedgerConfig

type instance LedgerCfg (LedgerState LeiosTestBlock) = LeiosTestLedgerConfig

-- | The genesis ledger, seeded with the given state so that a test's first tx
-- can have non-trivial assertions.
leiosTestInitLedger ::
  LeiosTestLedgerConfig -> IntMap Char -> LedgerState LeiosTestBlock ValuesMK
leiosTestInitLedger cfg st = LeiosTestLedger GenesisPoint st (committeeAt cfg 0)

leiosTestInitExtLedger ::
  LeiosTestLedgerConfig -> IntMap Char -> ExtLedgerState LeiosTestBlock ValuesMK
leiosTestInitExtLedger cfg st =
  ExtLedgerState
    { ledgerState = leiosTestInitLedger cfg st
    , headerState = genesisHeaderState ()
    }

-- | No UTxO HD for this block, at least not yet
type instance TxIn (LedgerState LeiosTestBlock) = Void

-- | No UTxO HD for this block, at least not yet
type instance TxOut (LedgerState LeiosTestBlock) = Void

instance LedgerTablesAreTrivial (LedgerState LeiosTestBlock) where
  convertMapKind (LeiosTestLedger tip st c) = LeiosTestLedger tip st c

instance LedgerTablesAreTrivial (Ticked (LedgerState LeiosTestBlock)) where
  convertMapKind (TickedLeiosTestLedger x) =
    TickedLeiosTestLedger $ convertMapKind x

deriving via
  TrivialLedgerTables (LedgerState LeiosTestBlock)
  instance
    HasLedgerTables (LedgerState LeiosTestBlock)
deriving via
  TrivialLedgerTables (LedgerState LeiosTestBlock)
  instance
    HasLedgerTables (Ticked (LedgerState LeiosTestBlock))
deriving via
  TrivialLedgerTables (LedgerState LeiosTestBlock)
  instance
    CanStowLedgerTables (LedgerState LeiosTestBlock)
deriving via
  TrivialLedgerTables (LedgerState LeiosTestBlock)
  instance
    CanUpgradeLedgerTables (LedgerState LeiosTestBlock)
deriving via
  TrivialLedgerTables (LedgerState LeiosTestBlock)
  instance
    SerializeTablesWithHint (LedgerState LeiosTestBlock)
deriving via
  Void
  instance
    IndexedMemPack (LedgerState LeiosTestBlock EmptyMK) Void

instance GetTip (LedgerState LeiosTestBlock) where
  getTip = castPoint . ltlsTip

instance GetTip (Ticked (LedgerState LeiosTestBlock)) where
  getTip = castPoint . ltlsTip . getTickedLeiosTestLedger

instance IsLedger (LedgerState LeiosTestBlock) where
  type LedgerErr (LedgerState LeiosTestBlock) = LeiosTestBlockError

  type
    AuxLedgerEvent (LedgerState LeiosTestBlock) =
      VoidLedgerEvent (LedgerState LeiosTestBlock)

  applyChainTickLedgerResult _events cfg slot st =
    pureLedgerResult $
      TickedLeiosTestLedger
        (noNewTickingDiffs st){ltlsCommittee = committeeAt cfg slot}

instance ApplyBlock (LedgerState LeiosTestBlock) LeiosTestBlock where
  applyBlockLedgerResultWithValidation
    _validation
    _events
    cfg
    blk
    (TickedLeiosTestLedger LeiosTestLedger{ltlsTip, ltlsState})
      | blockPrevHash blk /= pointHash ltlsTip =
          throwError $ LeiosTestInvalidHash (pointHash ltlsTip) (blockPrevHash blk)
      | Invalid <- lthValidity (ltbHeader blk) =
          throwError LeiosTestInvalidBlock
      | otherwise = case foldM (flip applyLeiosTestTx) ltlsState txs of
          Left err -> throwError $ LeiosTestInvalidTx err
          Right st ->
            pure
              $ pureLedgerResult
                . trackingToDiffs
              $ LeiosTestLedger (blockPoint blk) st (committeeAt cfg (blockSlot blk))
     where
      txs = ltbTxs (ltbBody blk)

  applyBlockLedgerResult = defaultApplyBlockLedgerResult
  reapplyBlockLedgerResult =
    defaultReapplyBlockLedgerResult
      (error . ("reapplying a block failed: " ++) . show)

  getBlockKeySets = const trivialLedgerTables

instance UpdateLedger LeiosTestBlock

instance InspectLedger LeiosTestBlock

instance HasAnnTip LeiosTestBlock

instance BasicEnvelopeValidation LeiosTestBlock

instance ValidateEnvelope LeiosTestBlock

instance LedgerSupportsPeras LeiosTestBlock

instance LedgerSupportsProtocol LeiosTestBlock where
  protocolLedgerView _cfg =
    LeiosTestView . ltlsCommittee . getTickedLeiosTestLedger

  ledgerViewForecastAt cfg st =
    Forecast
      { forecastAt = at
      , forecastFor = \for ->
          let maxFor = succWithOrigin at + ltlcForecastRange cfg
           in if for >= maxFor
                then
                  throwError
                    OutsideForecastRange
                      { outsideForecastAt = at
                      , outsideForecastMaxFor = maxFor
                      , outsideForecastFor = for
                      }
                else pure $ viewAt cfg for
      }
   where
    at = getTipSlot st

-- | The committee the schedule seats for the epoch that contains this slot.
viewAt :: LeiosTestLedgerConfig -> SlotNo -> LeiosTestView
viewAt cfg = LeiosTestView . committeeAt cfg

committeeAt :: LeiosTestLedgerConfig -> SlotNo -> Maybe (LeiosCommittee, Weight)
committeeAt cfg slot = ltlcCommittees cfg epoch
 where
  epoch =
    runIdentity $
      epochInfoEpoch
        ( fixedEpochInfo
            (HardFork.eraEpochSize (ltlcHardForkParams cfg))
            (HardFork.eraSlotLength (ltlcHardForkParams cfg))
        )
        slot

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
  getSystemStart = const (SystemStart dummyDate)
   where
    dummyDate = UTCTime (fromGregorian 2019 8 13) 0

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
    , topLevelConfigVotingKeys = []
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

-- | The header is encoded first and unwrapped, so that 'getBinaryBlockInfo' can
-- name its bytes exactly.
instance Serialise LeiosTestBlock where
  encode (LeiosTestBlock hdr body) = encode hdr <> encode body
  decode = LeiosTestBlock <$> decode <*> decode

instance Serialise (Header LeiosTestBlock) where
  encode LeiosTestHeader{..} =
    encode lthHash
      <> encode lthSlot
      <> encode lthIssuer
      <> encode (flatten <$> lthAnnouncement)
      <> encode lthBodyHash
      <> encode lthContainsCert
      <> encode lthValidity
   where
    flatten (MkLeiosPoint slot ebHash, size) = (slot, ebHash, size)
  decode = do
    lthHash <- decode
    lthSlot <- decode
    lthIssuer <- decode
    mbAnnouncement <- decode
    lthBodyHash <- decode
    lthContainsCert <- decode
    lthValidity <- decode
    pure
      LeiosTestHeader
        { lthHash
        , lthSlot
        , lthIssuer
        , lthAnnouncement = unflatten <$> mbAnnouncement
        , lthBodyHash
        , lthContainsCert
        , lthValidity
        }
   where
    unflatten (slot, ebHash, size) = (MkLeiosPoint slot ebHash, size)

-- | The certificate rides on the ledger's own encoding, which is the one real
-- nodes use.
instance Serialise LeiosTestBody where
  encode (LeiosTestBody mbCert txs) =
    encode (serialize' shelleyProtVer <$> mbCert) <> encode txs
  decode = do
    mbBytes <- decode
    LeiosTestBody <$> traverse dec mbBytes <*> decode
   where
    dec bytes = case decodeFull' shelleyProtVer bytes of
      Right cert -> pure cert
      Left err -> fail $ "LeiosTestBody: " <> show err

instance HasBinaryBlockInfo LeiosTestBlock where
  getBinaryBlockInfo blk =
    BinaryBlockInfo
      { headerOffset = 0
      , headerSize = fromIntegral . BL.length . serialise . ltbHeader $ blk
      }

-- | The cap this block's hash imposes on chain length; see the LIMIT on
-- 'Header' 'LeiosTestBlock'.
maxLeiosTestChainLength :: Int
maxLeiosTestChainLength = 100

instance ConvertRawHash LeiosTestBlock where
  -- The length, and one 'Word64' per path component.
  hashSize _ = 8 * fromIntegral (1 + maxLeiosTestChainLength)
  toRawHash _ (TestHash h)
    | len > maxLeiosTestChainLength =
        error $
          "LeiosTestBlock: chain longer than "
            <> show maxLeiosTestChainLength
            <> " blocks"
    | otherwise = BL.toStrict . Put.runPut $ do
        Put.putWord64le (fromIntegral len)
        for_ h Put.putWord64le
        replicateM_ (maxLeiosTestChainLength - len) $ Put.putWord64le 0
   where
    len = length h
  fromRawHash _ bs = flip Get.runGet (BL.fromStrict bs) $ do
    len <- fromIntegral <$> Get.getWord64le
    (NE.nonEmpty -> Just h, rs) <-
      splitAt len <$> replicateM maxLeiosTestChainLength Get.getWord64le
    guard $ all (0 ==) rs
    pure $ TestHash h

instance Serialise (AnnTip LeiosTestBlock) where
  encode = defaultEncodeAnnTip encode
  decode = defaultDecodeAnnTip decode

instance Serialise (ExtLedgerState LeiosTestBlock EmptyMK) where
  encode = encodeExtLedgerState encode encode encode
  decode = decodeExtLedgerState decode decode decode

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

-- | The committee rides on the ledger's own encoding, as the certificate does.
instance Serialise (LedgerState LeiosTestBlock EmptyMK) where
  encode (LeiosTestLedger tip st mbCommittee) =
    encode tip <> encode st <> encode (flatten <$> mbCommittee)
   where
    flatten (committee, weight) =
      ( serialize' shelleyProtVer committee
      , numerator weight
      , denominator weight
      )
  decode = do
    tip <- decode
    st <- decode
    mbFlat <- decode
    LeiosTestLedger tip st <$> traverse unflatten mbFlat
   where
    unflatten (bytes, num, den) = case decodeFull' shelleyProtVer bytes of
      Right committee -> pure (committee, num % den)
      Left err -> fail $ "LeiosTestLedger: " <> show err

instance EncodeDisk LeiosTestBlock (LedgerState LeiosTestBlock EmptyMK)
instance DecodeDisk LeiosTestBlock (LedgerState LeiosTestBlock EmptyMK)

instance EncodeDiskDep (NestedCtxt Header) LeiosTestBlock
instance DecodeDiskDep (NestedCtxt Header) LeiosTestBlock

-- at least because ChainDepState LeiosTestProtocol ~ ()
instance EncodeDisk LeiosTestBlock ()
instance DecodeDisk LeiosTestBlock ()

instance SerialiseDiskConstraints LeiosTestBlock

deriving via
  SelectViewDiffusionPipelining LeiosTestBlock
  instance
    BlockSupportsDiffusionPipelining LeiosTestBlock
