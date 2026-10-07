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
  , leiosTestEbPoint
  , leiosTestTxBytes
  , mkLeiosTestEb
  , mkLeiosTestEbClaiming
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
  , LeiosTestChainDepState (..)
  , LeiosTestProtocol
  , LeiosTestView (..)

    -- * Node configuration
  , singleNodeLeiosTestConfig
  ) where

import Cardano.Binary (DecoderError, decodeMaybe, encodeMaybe, enforceSize)
import Cardano.Ledger.Binary (decodeFull', serialize', shelleyProtVer)
import Cardano.Slotting.EpochInfo (epochInfoEpoch, fixedEpochInfo)
import Codec.CBOR.Decoding (Decoder, decodeBytes)
import Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Encoding as CBOR
import Codec.Serialise (Serialise (..), deserialiseOrFail, serialise)
import Control.Monad (foldM, guard, replicateM, replicateM_)
import Control.Monad.Except (throwError)
import Data.String (fromString)
import qualified Data.Binary.Get as Get
import qualified Data.Binary.Put as Put
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Short as SBS
import Data.Foldable (for_)
import Data.Functor ((<&>))
import Data.Functor.Identity (runIdentity)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Proxy
import Data.Ratio (denominator, numerator, (%))
import qualified Data.Text as Text
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..))
import qualified Data.Vector.Strict as V
import Data.Void (Void)
import Data.Word (Word64)
import GHC.Generics (Generic)
import LeiosDemoDb (lookupTrustedEbClosure)
import LeiosDemoLogic.Announcements.ElBimap (ElId (MkElId))
import LeiosDemoTypes
  ( AnnouncementFields (..)
  , BytesSize
  , HasLeiosVoting (..)
  , LeiosCert
  , LeiosClosureError (LeiosClosureMissing, LeiosClosureTxUndecodable)
  , LeiosCommittee
  , LeiosEb (..)
  , LeiosPoint (..)
  , LeiosTx (MkLeiosTx)
  , RbHash (MkRbHash)
  , TxHash
  , Weight
  , decodeEbHash
  , encodeEbHash
  , encodeLeiosEb
  , hashLeiosEb
  , hashLeiosTx
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
import Ouroboros.Consensus.Ledger.CommonProtocolParams
  ( CommonProtocolParams (..)
  )
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Ledger.Inspect
import Ouroboros.Consensus.Ledger.Query
import Ouroboros.Consensus.Ledger.SupportsMempool
import Ouroboros.Consensus.Ledger.SupportsPeerSelection
  ( LedgerSupportsPeerSelection (..)
  )
import Ouroboros.Consensus.Ledger.SupportsPeras (LedgerSupportsPeras)
import Ouroboros.Consensus.Ledger.SupportsProtocol
import Ouroboros.Consensus.Ledger.Tables.Utils
import Ouroboros.Consensus.Node.InitStorage (NodeInitStorage (..))
import Ouroboros.Consensus.Node.NetworkProtocolVersion
import Ouroboros.Consensus.Node.Run
  ( RunNode
  , SerialiseNodeToClientConstraints
  , SerialiseNodeToNodeConstraints (..)
  )
import Ouroboros.Consensus.Node.Serialisation
import Ouroboros.Consensus.Protocol.Abstract
import Ouroboros.Consensus.Storage.ChainDB (SerialiseDiskConstraints)
import Ouroboros.Consensus.Storage.ImmutableDB (simpleChunkInfo)
import Ouroboros.Consensus.Storage.LedgerDB
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
  deriving anyclass (Serialise, NoThunks)

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
hashLeiosTestBody = djb2 . serialise

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

  protocolStateLeiosAnnouncement = ltcdsAnnouncement

  -- The closure the LeiosDb holds for this endorser block, decoded back into
  -- transactions. A CertRB is not selectable until this succeeds.
  resolveLeiosClosure leiosDb ebHash =
    lookupTrustedEbClosure leiosDb ebHash >>= \case
      Nothing -> pure $ Left $ LeiosClosureMissing ebHash
      Just closure -> pure $ traverse decodeOne closure
   where
    decodeOne (txHash, bytes) =
      case deserialiseOrFail (BL.fromStrict bytes) of
        Left err ->
          Left $
            LeiosClosureTxUndecodable ebHash txHash $
              Text.pack (show err)
        Right tx -> Right (txHash, tx)

  applyLeiosClosure _cfg txs st =
    case foldM (flip applyLeiosTestTx) (ltlsState st) (unLeiosTestGenTx <$> txs) of
      Left err -> Left $ LeiosTestInvalidTx err
      Right st' -> Right st{ltlsState = st'}

  inlineLeiosClosure blk txs = setBody (ltbBody blk){ltbTxs = map unLeiosTestGenTx txs} blk

  leiosClosureTxKeySets = getTransactionKeySets

  assumeValidatedClosureTx = ValidatedLeiosTestGenTx

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

-- | The announcement the latest header carried, which is the whole of this
-- protocol's chain-dependent state: a CertRB certifies the endorser block its
-- predecessor announced, and this is where the apply path reads that from.
newtype LeiosTestChainDepState = LeiosTestChainDepState
  { ltcdsAnnouncement :: Maybe AnnouncementFields
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass NoThunks

instance Serialise LeiosTestChainDepState where
  encode = encodeMaybe encodeAnnouncementFields . ltcdsAnnouncement
  decode = LeiosTestChainDepState <$> decodeMaybe decodeAnnouncementFields

newtype instance Ticked LeiosTestChainDepState
  = TickedLeiosTestChainDepState LeiosTestChainDepState

-- | No leader schedule and no header crypto: what this protocol contributes is
-- the ledger view and the announcement above.
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
  type ChainDepState LeiosTestProtocol = LeiosTestChainDepState
  type IsLeader LeiosTestProtocol = ()
  type CanBeLeader LeiosTestProtocol = ()
  type LedgerView LeiosTestProtocol = LeiosTestView
  type ValidationErr LeiosTestProtocol = Void
  type ValidateView LeiosTestProtocol = Maybe AnnouncementFields

  protocolSecurityParam = ltpcSecurityParam

  -- Every node leads in every slot; the test itself decides what actually gets
  -- minted.
  checkIsLeader _ _ _ _ = Just ()

  tickChainDepState _ _ _ = TickedLeiosTestChainDepState
  updateChainDepState _ announcement _ _ = pure (LeiosTestChainDepState announcement)
  reupdateChainDepState _ announcement _ _ = LeiosTestChainDepState announcement

type instance BlockProtocol LeiosTestBlock = LeiosTestProtocol

instance BlockSupportsProtocol LeiosTestBlock where
  validateView _ hdr =
    lthAnnouncement hdr <&> \(point, size) ->
      MkAnnouncementFields (headerElId hdr) (pointEbHash point) size

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
    , headerState = genesisHeaderState (LeiosTestChainDepState Nothing)
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
      <> encodeMaybe encodeLeiosPointAndSize lthAnnouncement
      <> encode lthBodyHash
      <> encode lthContainsCert
      <> encode lthValidity
  decode = do
    lthHash <- decode
    lthSlot <- decode
    lthIssuer <- decode
    mbAnnouncement <- decodeMaybe decodeLeiosPointAndSize
    lthBodyHash <- decode
    lthContainsCert <- decode
    lthValidity <- decode
    pure
      LeiosTestHeader
        { lthHash
        , lthSlot
        , lthIssuer
        , lthAnnouncement = mbAnnouncement
        , lthBodyHash
        , lthContainsCert
        , lthValidity
        }

-- | What a header announces: the endorser block's slot and hash, and the size
-- it claims for the body, as one flat list.
--
-- 'EbHash' deliberately has no 'Serialise' instance --- the fixed-size codec is
-- a hash's only path --- so the hash goes through the codec 'LeiosDemoTypes'
-- exports.
encodeLeiosPointAndSize :: (LeiosPoint, BytesSize) -> Encoding
encodeLeiosPointAndSize (MkLeiosPoint ebSlot ebHash, size) =
  CBOR.encodeListLen 3
    <> encode ebSlot
    <> encodeEbHash ebHash
    <> encode size

decodeLeiosPointAndSize :: Decoder s (LeiosPoint, BytesSize)
decodeLeiosPointAndSize = do
  enforceSize (fromString "LeiosPointAndSize") 3
  ebSlot <- decode
  ebHash <- decodeEbHash
  size <- decode
  pure (MkLeiosPoint ebSlot ebHash, size)

-- | The announcement the chain-dependent state carries: the election that made
-- it, the endorser block it names and the size it claims for the body.
--
-- Neither 'ElId' nor the hash inside it has a 'Serialise' instance, so this
-- spells the fields out, taking the hash through the fixed-size codec
-- 'LeiosDemoTypes' exports.
encodeAnnouncementFields :: AnnouncementFields -> Encoding
encodeAnnouncementFields (MkAnnouncementFields (MkElId elSlot poolId) ebHash size) =
  CBOR.encodeListLen 4
    <> encode elSlot
    <> CBOR.encodeBytes (SBS.fromShort poolId)
    <> encodeEbHash ebHash
    <> encode size

decodeAnnouncementFields :: Decoder s AnnouncementFields
decodeAnnouncementFields = do
  enforceSize (fromString "AnnouncementFields") 4
  elSlot <- decode
  poolId <- SBS.toShort <$> decodeBytes
  ebHash <- decodeEbHash
  size <- decode
  pure $ MkAnnouncementFields (MkElId elSlot poolId) ebHash size

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
instance EncodeDisk LeiosTestBlock LeiosTestChainDepState
instance DecodeDisk LeiosTestBlock LeiosTestChainDepState

instance SerialiseDiskConstraints LeiosTestBlock

deriving via
  SelectViewDiffusionPipelining LeiosTestBlock
  instance
    BlockSupportsDiffusionPipelining LeiosTestBlock

{-------------------------------------------------------------------------------
  Mempool

  Transactions reach a real node through the mempool, and 'RunNode' demands it
  even of a node that never forges. Nothing here is interesting: the mempool's
  notion of applying a transaction is 'applyLeiosTestTx', and the sizes are
  arbitrary.
-------------------------------------------------------------------------------}

newtype instance GenTx LeiosTestBlock = LeiosTestGenTx
  { unLeiosTestGenTx :: LeiosTestTx
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialise, NoThunks)

newtype instance Validated (GenTx LeiosTestBlock) = ValidatedLeiosTestGenTx
  { forgetValidatedLeiosTestGenTx :: GenTx LeiosTestBlock
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass NoThunks

type instance ApplyTxErr LeiosTestBlock = LeiosTestTxError

newtype instance TxId (GenTx LeiosTestBlock) = LeiosTestTxId Word64
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Serialise, NoThunks)

instance ShowProxy (GenTx LeiosTestBlock)
instance ShowProxy (TxId (GenTx LeiosTestBlock))
instance ShowProxy LeiosTestTxError

instance HasTxId (GenTx LeiosTestBlock) where
  txId = LeiosTestTxId . djb2 . serialise

instance ConvertRawTxId (GenTx LeiosTestBlock) where
  toRawTxIdHash = SBS.toShort . BL.toStrict . serialise

instance LedgerSupportsMempool LeiosTestBlock where
  applyTx _cfg _wti _slot tx (TickedLeiosTestLedger st) =
    case applyLeiosTestTx (unLeiosTestGenTx tx) (ltlsState st) of
      Left err -> throwError err
      Right st' ->
        pure
          ( TickedLeiosTestLedger st{ltlsState = st'}
          , ValidatedLeiosTestGenTx tx
          )

  reapplyTx cfg slot tx st =
    applyDiffs st . fst
      <$> applyTx cfg DoNotIntervene slot (forgetValidatedLeiosTestGenTx tx) st

  txForgetValidated = forgetValidatedLeiosTestGenTx

  getTransactionKeySets _tx = trivialLedgerTables

  mkMempoolApplyTxError = nothingMkMempoolApplyTxError

instance TxLimits LeiosTestBlock where
  type TxMeasure LeiosTestBlock = IgnoringOverflow ByteSize32
  txWireSize = const 0
  blockCapacityTxMeasure _cfg _st = IgnoringOverflow $ ByteSize32 $ 100 * 1024
  txMeasure _cfg _st _tx = pure $ IgnoringOverflow $ ByteSize32 0

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

-- | A cheap deterministic digest; see 'hashLeiosTestBody'.
djb2 :: BL.ByteString -> Word64
djb2 = BL.foldl' step 5381
 where
  step acc w = acc * 33 + fromIntegral w

{-------------------------------------------------------------------------------
  Node-to-node and node-to-client serialisation

  These tests connect the node to its environment with the identity codecs, so
  none of this is exercised; 'RunNode' demands it regardless. Everything rides
  on the same 'Serialise' instances the on-disk format uses.
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
  estimateBlockSize = const 0

instance SerialiseNodeToClient LeiosTestBlock LeiosTestBlock
instance SerialiseNodeToClient LeiosTestBlock (Serialised LeiosTestBlock)
instance SerialiseNodeToClient LeiosTestBlock (GenTx LeiosTestBlock)
instance SerialiseNodeToClient LeiosTestBlock (GenTxId LeiosTestBlock)
instance SerialiseNodeToClient LeiosTestBlock SlotNo
instance SerialiseNodeToClient LeiosTestBlock LeiosTestTxError
instance SerialiseNodeToClient LeiosTestBlock LeiosTestLedgerConfig where
  encodeNodeToClient _ _ = error "LeiosTestBlock: no node-to-client config"
  decodeNodeToClient _ _ = error "LeiosTestBlock: no node-to-client config"

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

{-------------------------------------------------------------------------------
  Endorser blocks

  An endorser block is a list of transaction hashes and sizes; its closure is
  those transactions' bytes. A transaction's bytes here are its 'Serialise'
  encoding --- the same encoding it has on the wire, as for a real block.
-------------------------------------------------------------------------------}

-- | The wire bytes of a transaction, which are what the endorser block's
-- hashes are over and what the LeiosDb stores.
leiosTestTxBytes :: LeiosTestTx -> BS.ByteString
leiosTestTxBytes = BL.toStrict . serialise . LeiosTestGenTx

-- | The endorser block over these transactions, its closure, and the size of
-- the body on the wire --- which is what a header announces.
mkLeiosTestEb :: [LeiosTestTx] -> (LeiosEb, [(TxHash, BS.ByteString)], BytesSize)
mkLeiosTestEb txs =
  mkLeiosTestEbClaiming
    [ (tx, fromIntegral $ BS.length $ leiosTestTxBytes tx)
    | tx <- txs
    ]

-- | As 'mkLeiosTestEb', but each reference claims the given size rather than
-- the transaction's actual size.
--
-- Only an adversary builds one of these. An endorser block naming a
-- transaction is asserting how big it is, and a node that already holds that
-- transaction --- having fetched it for some other endorser block --- never
-- fetches it again, so it never compares the assertion against the bytes.
mkLeiosTestEbClaiming ::
  [(LeiosTestTx, BytesSize)] -> (LeiosEb, [(TxHash, BS.ByteString)], BytesSize)
mkLeiosTestEbClaiming claims =
  ( eb
  , [(txHash, bytes) | (txHash, _size, bytes) <- entries]
  , fromIntegral $ BS.length $ serialize' shelleyProtVer $ encodeLeiosEb eb
  )
 where
  entries =
    [ (hashLeiosTx leiosTx, claimed, bytes)
    | (tx, claimed) <- claims
    , let bytes = leiosTestTxBytes tx
    , let leiosTx = MkLeiosTx bytes
    ]

  eb = MkLeiosEb $ V.fromList [(txHash, size) | (txHash, size, _bytes) <- entries]

-- | Where a header announcing this endorser block points.
leiosTestEbPoint :: SlotNo -> LeiosEb -> LeiosPoint
leiosTestEbPoint slot eb = MkLeiosPoint slot (hashLeiosEb eb)
