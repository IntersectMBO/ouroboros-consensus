{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- | Minimal instantiation of the consensus layer to be able to run the ChainDB
module Test.Util.TestBlock
  ( -- * Blocks
    BlockConfig (..)
  , BlockQuery (..)
  , CodecConfig (..)
  , Header (..)
  , LedgerTables (..)
  , StorageConfig (..)
  , TestBlockError (..)
  , TestBlockWith (tbPayload, tbSlot, tbValid)
  , TestHash (TestHash)
  , Validity (..)
  , firstBlockWithPayload
  , forkBlock
  , modifyFork
  , successorBlockWithPayload
  , testHashFromList
  , unTestHash

    -- ** Test block without payload
  , TestBlock
  , firstBlock
  , successorBlock

    -- ** Payload semantics
  , PayloadDependentState (..)
  , PayloadSemantics (..)
  , applyDirectlyToPayloadDependentState

    -- * LedgerState
  , LedgerState (TestLedger, payloadDependentState, lastAppliedPoint)
  , Ticked (TickedTestLedger)
  , getTickedTestLedger

    -- * Chain
  , BlockChain (..)
  , blockChain
  , chainToBlocks

    -- * Tree
  , BlockTree (..)
  , blockTree
  , treePreferredChain
  , treeToBlocks
  , treeToChains

    -- * Ledger infrastructure
  , singleNodeTestConfig
  , singleNodeTestConfigWith
  , singleNodeTestConfigWithK
  , testInitExtLedger
  , testInitExtLedgerWithState
  , testInitLedger
  , testInitLedgerWithState

    -- * Support for tests
  , Permutation (..)
  , TestBlockLedgerConfig (..)
  , isAncestorOf
  , isDescendentOf
  , isStrictAncestorOf
  , isStrictDescendentOf
  , permute
  , testBlockLedgerConfigFrom
  , unsafeTestBlockWithPayload
  , updateToNextNumeral
  ) where

import Cardano.Crypto.DSIGN
import Cardano.Ledger.BaseTypes (knownNonZeroBounded, unNonZero)
import Codec.Serialise (Serialise (..), serialise)
import Control.DeepSeq (force)
import Control.Monad (guard, replicateM, replicateM_)
import Control.Monad.Except (throwError)
import qualified Data.Binary.Get as Get
import qualified Data.Binary.Put as Put
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (for_)
import Data.Int
import Data.Kind (Type)
import Data.List (isSuffixOf, transpose)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Maybe.Strict (StrictMaybe (..), strictMaybeToMaybe)
import Data.Proxy
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..))
import Data.Tree (Tree (..))
import qualified Data.Tree as Tree
import Data.TreeDiff (ToExpr)
import Data.Typeable (Typeable)
import Data.Void (Void)
import Data.Word
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.BlockchainTime
import Ouroboros.Consensus.Config
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
import Ouroboros.Consensus.Ledger.Query
import Ouroboros.Consensus.Ledger.SupportsProtocol
import Ouroboros.Consensus.Ledger.Tables.Utils
import Ouroboros.Consensus.Node.NetworkProtocolVersion
import Ouroboros.Consensus.Node.ProtocolInfo
import Ouroboros.Consensus.NodeId
import Ouroboros.Consensus.Protocol.Abstract
import Ouroboros.Consensus.Protocol.BFT
import Ouroboros.Consensus.Protocol.MockChainSel
import Ouroboros.Consensus.Protocol.Signed
import Ouroboros.Consensus.Storage.ChainDB (SerialiseDiskConstraints)
import Ouroboros.Consensus.Storage.LedgerDB
import Ouroboros.Consensus.Storage.Serialisation
import Ouroboros.Consensus.Util (ShowProxy (..))
import Ouroboros.Consensus.Util.Condense
import Ouroboros.Consensus.Util.IndexedMemPack
import Ouroboros.Consensus.Util.Orphans ()
import Ouroboros.Network.Magic (NetworkMagic (..))
import Ouroboros.Network.Mock.Chain (Chain (..))
import qualified Ouroboros.Network.Mock.Chain as Chain
import qualified System.Random as R
import Test.QuickCheck hiding (Result)
import Test.Util.Orphans.SignableRepresentation ()
import Test.Util.Orphans.ToExpr ()

{-------------------------------------------------------------------------------
  Test infrastructure: test block
-------------------------------------------------------------------------------}

-- The hash represents a path through a tree (of forks) as a list of
-- 'Word64's: @[0, 1, 0]@ means we forked off at the first block.
--
-- The following tree is just a small subset of the actual tree, which allows
-- multiple forks at /each/ node:
--
-- > G--A--B--C
-- > \  \--B"-C"
-- >  \-A'-B'-C'
--
-- Some examples:
--
-- > []      = G = Genesis
-- > [0]     = A
-- > [1]     = A'
-- > [0,0]   = B
-- > [0,0,0] = C
-- > [0,1]   = B"
-- > [0,1,0] = C"
-- > [1,0]   = B'
-- > [1,0,0] = C'
-- > ...
--
-- Since the empty list represents Genesis, which does not correspond to an
-- actual block, we use a NonEmpty list.
--
-- As it is easier to prepend to and access the front of a (NonEmpty) list, we
-- store the list in reverse order, e.g., @C'@ is stored as @[0,0,1]@, but we
-- print ('Show' and 'Condense') it as @[1,0,0]@.
--
-- The predecessor (parent in the tree) can be obtained by dropping the last
-- (in the printed representation) element in the list (or the head in the
-- in-memory representation).
--
-- The 'BlockNo' of the corresponding block is just the length of the list.
newtype TestHash = UnsafeTestHash
  { unTestHash :: NonEmpty Word64
  }
  deriving stock Generic
  deriving newtype (Eq, Ord, Serialise, ToExpr)
  deriving anyclass NoThunks

pattern TestHash :: NonEmpty Word64 -> TestHash
pattern TestHash path <- UnsafeTestHash path
  where
    TestHash path = UnsafeTestHash (force path)

{-# COMPLETE TestHash #-}

testHashFromList :: [Word64] -> TestHash
testHashFromList = TestHash . NE.fromList . reverse

instance Show TestHash where
  show (TestHash h) = "(testHashFromList " <> show (reverse (NE.toList h)) <> ")"

instance Condense TestHash where
  condense = condense . reverse . NE.toList . unTestHash

data Validity = Valid | Invalid
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass (Serialise, NoThunks, ToExpr)

-- | Test block parametrized on the payload type
--
-- For blocks without payload see the 'TestBlock' type alias.
--
-- By defining a 'PayloadSemantics' it is possible to obtain an 'ApplyBlock'
-- instance. See the former class for more details.
data TestBlockWith ptype = TestBlockWith
  { tbHash :: !TestHash
  , tbSlot :: !SlotNo
  -- ^ We store a separate 'Block.SlotNo', as slots can have gaps between
  -- them, unlike block numbers.
  --
  -- Note that when generating a 'TestBlock', you must make sure that
  -- blocks with the same 'TestHash' have the same slot number.
  , tbValid :: !Validity
  -- ^ Note that when generating a 'TestBlock', you must make sure that
  -- blocks with the same 'TestHash' have the same value for 'tbValid'.
  , tbPayload :: !ptype
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Serialise, NoThunks, ToExpr)

-- | Create a block directly with the given parameters. This allows creating
-- inconsistent blocks; prefer 'firstBlockWithPayload' or 'successorBlockWithPayload'.
unsafeTestBlockWithPayload :: TestHash -> SlotNo -> Validity -> ptype -> TestBlockWith ptype
unsafeTestBlockWithPayload tbHash tbSlot tbValid tbPayload =
  TestBlockWith{tbHash, tbSlot, tbValid, tbPayload}

-- | Create the first block in the given fork, @[fork]@, with the given payload.
-- The 'SlotNo' will be 1.
firstBlockWithPayload :: Word64 -> ptype -> TestBlockWith ptype
firstBlockWithPayload forkNo payload =
  TestBlockWith
    { tbHash = TestHash (forkNo NE.:| [])
    , tbSlot = 1
    , tbValid = Valid
    , tbPayload = payload
    }

-- | Create the successor of the given block without forking: @b -> b ++ [0]@ (in
-- the printed representation) The 'SlotNo' is increased by 1.
--
-- In Zipper parlance, this corresponds to going down in a tree.
successorBlockWithPayload ::
  TestHash -> SlotNo -> ptype -> TestBlockWith ptype
successorBlockWithPayload hash slot payload =
  TestBlockWith
    { tbHash = TestHash (NE.cons 0 (unTestHash hash))
    , tbSlot = succ slot
    , tbValid = Valid
    , tbPayload = payload
    }

-- | A block @b1@ is the ancestor of another block @b2@ if there exists a chain
-- of blocks from @b1@ to @b2@. For test blocks in particular, this can be seen
-- in the hash: the hash of @b1@ should be a prefix of the hash of @b2@.
--
-- Note that this is a partial comparison function. In particular, it does hold
-- that for all @b1@ and @b2@, @b1 `isDescendentOf` b2 === b2 `isAncestorOf` b1@
-- but it does not hold that for all @b1@ and @b2@, @b1 `isDescendentOf` b2 ===
-- not (b1 `isAncestorOf` b2) || b1 == b2@.
isAncestorOf :: TestBlock -> TestBlock -> Bool
isAncestorOf b1 b2 =
  -- NOTE: 'unTestHash' returns the list of hash components _in reverse
  -- order_ so we need to test that one hash is the _suffix_ of the other.
  NE.toList (unTestHash (blockHash b1))
    `isSuffixOf` NE.toList (unTestHash (blockHash b2))

-- | Variant of 'isAncestorOf' that returns @False@ when the two blocks are
-- equal.
isStrictAncestorOf :: TestBlock -> TestBlock -> Bool
isStrictAncestorOf b1 b2 = b1 `isAncestorOf` b2 && b1 /= b2

-- | A block @b1@ is the descendent of another block @b2@ if there exists a
-- chain of blocks from @b2@ to @b1@. For test blocks in particular, this can be
-- seen in the hash: the hash of @b2@ should be a prefix of the hash of @b1@.
--
-- Note that this is a partial comparison function. In particular, it does hold
-- that for all @b1@ and @b2@, @b1 `isDescendentOf` b2 === b2 `isAncestorOf` b1@
-- but it does not hold that for all @b1@ and @b2@, @b1 `isDescendentOf` b2 ===
-- not (b1 `isAncestorOf` b2) || b1 == b2@.
isDescendentOf :: TestBlock -> TestBlock -> Bool
isDescendentOf = flip isAncestorOf

-- | Variant of 'isDescendentOf' that returns @False@ when the two blocks are
-- equal.
isStrictDescendentOf :: TestBlock -> TestBlock -> Bool
isStrictDescendentOf b1 b2 = b1 `isDescendentOf` b2 && b1 /= b2

instance ShowProxy TestBlock

newtype instance Header (TestBlockWith ptype)
  = TestHeader {testHeader :: TestBlockWith ptype}
  deriving stock (Eq, Show)
  deriving newtype (NoThunks, Serialise)

instance Typeable ptype => ShowProxy (Header (TestBlockWith ptype))

instance Typeable ptype => HasHeader (Header (TestBlockWith ptype)) where
  getHeaderFields (TestHeader TestBlockWith{..}) =
    HeaderFields
      { headerFieldHash = tbHash
      , headerFieldSlot = tbSlot
      , headerFieldBlockNo = fromIntegral . NE.length . unTestHash $ tbHash
      }

instance (Typeable ptype, Eq ptype) => GetHeader (TestBlockWith ptype) where
  getHeader = TestHeader
  blockMatchesHeader (TestHeader blk') blk = blk == blk'
  headerIsEBB = const Nothing

type instance HeaderHash (TestBlockWith ptype) = TestHash

instance (Typeable ptype, Eq ptype) => HasHeader (TestBlockWith ptype) where
  getHeaderFields = getBlockHeaderFields

instance (Typeable ptype, Eq ptype) => GetPrevHash (TestBlockWith ptype) where
  headerPrevHash (TestHeader b) =
    case NE.nonEmpty . NE.tail . unTestHash . tbHash $ b of
      Nothing -> GenesisHash
      Just prevHash -> BlockHash (TestHash prevHash)

instance StandardHash (TestBlockWith ptype)

instance (Typeable ptype, Eq ptype) => Condense (TestBlockWith ptype) where
  condense b =
    mconcat
      [ "(H:"
      , condense (blockHash b)
      , ",S:"
      , condense (blockSlot b)
      , ",B:"
      , condense (unBlockNo (blockNo b))
      , ")"
      ]

instance (Typeable ptype, Eq ptype) => Condense (Header (TestBlockWith ptype)) where
  condense = condense . testHeader

instance Condense (ChainHash (TestBlockWith ptype)) where
  condense GenesisHash = "genesis"
  condense (BlockHash h) = show h

data instance BlockConfig (TestBlockWith ptype) = TestBlockConfig
  { testBlockNumCoreNodes :: !NumCoreNodes
  -- ^ Number of core nodes
  --
  -- We need this in order to compute the 'ValidateView', which must
  -- conjure up a validation key out of thin air
  }
  deriving (Show, Generic, NoThunks)

instance HasNetworkProtocolVersion (TestBlockWith ptype)

-- Use defaults

instance ConfigSupportsNode (TestBlockWith ptype) where
  getSystemStart = const (SystemStart dummyDate)
   where
    --  This doesn't matter much
    dummyDate = UTCTime (fromGregorian 2019 8 13) 0

  getNetworkMagic = const (NetworkMagic 42)

{-------------------------------------------------------------------------------
  Payload semantics
-------------------------------------------------------------------------------}

class
  ( Typeable ptype
  , Eq ptype
  , NoThunks ptype
  , forall mk. EqMK mk => Eq (PayloadDependentState ptype mk)
  , forall mk. NoThunksMK mk => NoThunks (PayloadDependentState ptype mk)
  , forall mk. ShowMK mk => Show (PayloadDependentState ptype mk)
  , forall mk. Generic (PayloadDependentState ptype mk)
  , Serialise (PayloadDependentState ptype EmptyMK)
  , HasLedgerTables (LedgerState (TestBlockWith ptype))
  , HasLedgerTables (Ticked (LedgerState (TestBlockWith ptype)))
  , CanStowLedgerTables (LedgerState (TestBlockWith ptype))
  , Eq (PayloadDependentError ptype)
  , Show (PayloadDependentError ptype)
  , Generic (PayloadDependentError ptype)
  , ToExpr (PayloadDependentError ptype)
  , Serialise (PayloadDependentError ptype)
  , NoThunks (PayloadDependentError ptype)
  , NoThunks (CodecConfig (TestBlockWith ptype))
  , NoThunks (StorageConfig (TestBlockWith ptype))
  ) =>
  PayloadSemantics ptype
  where
  data PayloadDependentState ptype (mk :: MapKind) :: Type

  type PayloadDependentError ptype :: Type

  applyPayload ::
    PayloadDependentState ptype ValuesMK ->
    ptype ->
    Either (PayloadDependentError ptype) (PayloadDependentState ptype TrackingMK)

  -- | This function is used to implement the 'getBlockKeySets' function of the
  -- 'ApplyBlock' class. Thus we assume that the payload contains all the
  -- information needed to determine which keys should be retrieved from the
  -- backing store to apply a 'TestBlockWith'.
  getPayloadKeySets :: ptype -> LedgerTables (LedgerState (TestBlockWith ptype)) KeysMK

instance PayloadSemantics () where
  data PayloadDependentState () mk = EmptyPLDS
    deriving stock (Eq, Show, Generic)
    deriving anyclass (Serialise, NoThunks)

  type PayloadDependentError () = ()

  applyPayload _ _ = Right EmptyPLDS

  getPayloadKeySets = const trivialLedgerTables

-- | Apply the payload directly to the payload dependent state portion of a
-- ticked state, leaving the rest of the input ticked state unaltered.
applyDirectlyToPayloadDependentState ::
  PayloadSemantics ptype =>
  Ticked (LedgerState (TestBlockWith ptype)) ValuesMK ->
  ptype ->
  Either
    (PayloadDependentError ptype)
    (Ticked (LedgerState (TestBlockWith ptype)) TrackingMK)
applyDirectlyToPayloadDependentState (TickedTestLedger st) tx = do
  payloadDepSt' <- applyPayload (payloadDependentState st) tx
  pure $ TickedTestLedger $ st{payloadDependentState = payloadDepSt'}

{-------------------------------------------------------------------------------
  NestedCtxt
-------------------------------------------------------------------------------}

data instance NestedCtxt_ (TestBlockWith ptype) f a where
  CtxtTestBlock :: NestedCtxt_ (TestBlockWith ptype) f (f (TestBlockWith ptype))

deriving instance Show (NestedCtxt_ (TestBlockWith ptype) f a)

instance TrivialDependency (NestedCtxt_ (TestBlockWith ptype) f) where
  type TrivialIndex (NestedCtxt_ (TestBlockWith ptype) f) = f (TestBlockWith ptype)
  hasSingleIndex CtxtTestBlock CtxtTestBlock = Refl
  indexIsTrivial = CtxtTestBlock

instance SameDepIndex (NestedCtxt_ (TestBlockWith ptype) f)
instance HasNestedContent f (TestBlockWith ptype)

{-------------------------------------------------------------------------------
  Test infrastructure: ledger state
-------------------------------------------------------------------------------}

type instance BlockProtocol (TestBlockWith ptype) = Bft BftMockCrypto

type instance Signed (Header (TestBlockWith ptype)) = ()
instance SignedHeader (Header (TestBlockWith ptype)) where
  headerSigned _ = ()

data TestBlockError ptype
  = -- | The hashes don't line up
    InvalidHash
      -- | Expected hash
      (ChainHash (TestBlockWith ptype))
      -- | Invalid hash
      (ChainHash (TestBlockWith ptype))
  | -- | The block itself is invalid
    InvalidBlock
  | InvalidPayload (PayloadDependentError ptype)

deriving stock instance Eq (PayloadDependentError ptype) => Eq (TestBlockError ptype)
deriving stock instance Show (PayloadDependentError ptype) => Show (TestBlockError ptype)
deriving stock instance Generic (TestBlockError ptype)

deriving anyclass instance
  ( Typeable ptype
  , Generic (PayloadDependentError ptype)
  , NoThunks (PayloadDependentError ptype)
  ) =>
  NoThunks (TestBlockError ptype)

instance
  ( Typeable ptype
  , Eq ptype
  , NoThunks ptype
  , NoThunks (CodecConfig (TestBlockWith ptype))
  , NoThunks (StorageConfig (TestBlockWith ptype))
  ) =>
  BlockSupportsProtocol (TestBlockWith ptype)
  where
  validateView TestBlockConfig{..} =
    bftValidateView bftFields
   where
    NumCoreNodes numCore = testBlockNumCoreNodes

    bftFields :: Header (TestBlockWith ptype) -> BftFields BftMockCrypto ()
    bftFields (TestHeader tb) =
      BftFields
        { bftSignature = SignedDSIGN $ mockSign () (signKey (tbSlot tb))
        }

    -- We don't want /our/ signing key, but rather the signing key of the
    -- node that produced the block
    signKey :: SlotNo -> SignKeyDSIGN MockDSIGN
    signKey (SlotNo n) = SignKeyMockDSIGN $ n `mod` numCore

type instance TxIn (LedgerState TestBlock) = Void
type instance TxOut (LedgerState TestBlock) = Void

instance LedgerTablesAreTrivial (LedgerState TestBlock) where
  convertMapKind (TestLedger x EmptyPLDS) = TestLedger x EmptyPLDS
instance LedgerTablesAreTrivial (Ticked (LedgerState TestBlock)) where
  convertMapKind (TickedTestLedger x) = TickedTestLedger $ convertMapKind x

deriving via
  TrivialLedgerTables (LedgerState TestBlock)
  instance
    HasLedgerTables (LedgerState TestBlock)
deriving via
  TrivialLedgerTables (LedgerState TestBlock)
  instance
    HasLedgerTables (Ticked (LedgerState TestBlock))
deriving via
  TrivialLedgerTables (LedgerState TestBlock)
  instance
    CanStowLedgerTables (LedgerState TestBlock)
deriving via
  TrivialLedgerTables (LedgerState TestBlock)
  instance
    CanUpgradeLedgerTables (LedgerState TestBlock)
deriving via
  TrivialLedgerTables (LedgerState TestBlock)
  instance
    SerializeTablesWithHint (LedgerState TestBlock)
deriving via
  Void
  instance
    IndexedMemPack (LedgerState TestBlock EmptyMK) Void

instance
  PayloadSemantics ptype =>
  ApplyBlock (LedgerState (TestBlockWith ptype)) (TestBlockWith ptype)
  where
  applyBlockLedgerResultWithValidation _validation _events _ tb@TestBlockWith{..} (TickedTestLedger TestLedger{..})
    | blockPrevHash tb /= pointHash lastAppliedPoint =
        throwError $ InvalidHash (pointHash lastAppliedPoint) (blockPrevHash tb)
    | tbValid == Invalid =
        throwError $ InvalidBlock
    | otherwise =
        case applyPayload payloadDependentState tbPayload of
          Left err -> throwError $ InvalidPayload err
          Right st' ->
            return $
              pureLedgerResult $
                trackingToDiffs $
                  TestLedger
                    { lastAppliedPoint = Chain.blockPoint tb
                    , payloadDependentState = st'
                    }

  applyBlockLedgerResult = defaultApplyBlockLedgerResult
  reapplyBlockLedgerResult =
    defaultReapplyBlockLedgerResult (error . ("Found an error when reapplying a block: " ++) . show)

  getBlockKeySets = getPayloadKeySets . tbPayload

data instance LedgerState (TestBlockWith ptype) mk
  = TestLedger
  { lastAppliedPoint :: Point (TestBlockWith ptype)
  -- ^ The ledger state simply consists of the last applied block
  , payloadDependentState :: PayloadDependentState ptype mk
  -- ^ State that depends on the application of the block payload to the
  -- state.
  }

deriving stock instance
  (ShowMK mk, PayloadSemantics ptype) =>
  Show (LedgerState (TestBlockWith ptype) mk)

deriving stock instance
  Eq (PayloadDependentState ptype mk) =>
  Eq (LedgerState (TestBlockWith ptype) mk)

deriving stock instance Generic (LedgerState (TestBlockWith ptype) mk)

deriving anyclass instance
  PayloadSemantics ptype =>
  Serialise (LedgerState (TestBlockWith ptype) EmptyMK)
deriving anyclass instance
  NoThunks (PayloadDependentState ptype mk) =>
  NoThunks (LedgerState (TestBlockWith ptype) mk)

testInitLedgerWithState ::
  PayloadDependentState ptype mk -> LedgerState (TestBlockWith ptype) mk
testInitLedgerWithState = TestLedger GenesisPoint

-- Ticking has no effect
newtype instance Ticked (LedgerState (TestBlockWith ptype)) mk = TickedTestLedger
  { getTickedTestLedger :: LedgerState (TestBlockWith ptype) mk
  }

deriving stock instance Generic (Ticked (LedgerState (TestBlockWith ptype)) mk)
deriving anyclass instance
  (NoThunksMK mk, NoThunks (PayloadDependentState ptype mk)) =>
  NoThunks (Ticked (LedgerState (TestBlockWith ptype)) mk)

testInitExtLedgerWithState ::
  PayloadDependentState ptype mk -> ExtLedgerState (TestBlockWith ptype) mk
testInitExtLedgerWithState st =
  ExtLedgerState
    { ledgerState = testInitLedgerWithState st
    , headerState = genesisHeaderState ()
    }

data TestBlockLedgerConfig = TestBlockLedgerConfig
  { tblcHardForkParams :: !HardFork.EraParams
  , tblcForecastRange :: !(StrictMaybe SlotNo)
  -- ^ `Nothing` means an infinite forecast range.
  -- Instead of SlotNo, it should be something like "SlotRange"
  }
  deriving (Show, Eq, Generic)
  deriving anyclass NoThunks

testBlockLedgerConfigFrom :: HardFork.EraParams -> TestBlockLedgerConfig
testBlockLedgerConfigFrom eraParams = TestBlockLedgerConfig eraParams SNothing

type instance LedgerCfg (LedgerState (TestBlockWith ptype)) = TestBlockLedgerConfig

instance GetTip (LedgerState (TestBlockWith ptype)) where
  getTip = castPoint . lastAppliedPoint

instance GetTip (Ticked (LedgerState (TestBlockWith ptype))) where
  getTip = castPoint . lastAppliedPoint . getTickedTestLedger

instance PayloadSemantics ptype => IsLedger (LedgerState (TestBlockWith ptype)) where
  type LedgerErr (LedgerState (TestBlockWith ptype)) = TestBlockError ptype

  type
    AuxLedgerEvent (LedgerState (TestBlockWith ptype)) =
      VoidLedgerEvent (LedgerState (TestBlockWith ptype))

  applyChainTickLedgerResult _ _ _ =
    pureLedgerResult
      . TickedTestLedger
      . noNewTickingDiffs

instance PayloadSemantics ptype => UpdateLedger (TestBlockWith ptype)

instance InspectLedger (TestBlockWith ptype)

-- Defaults are fine

instance PayloadSemantics ptype => HasAnnTip (TestBlockWith ptype)

-- Use defaults

instance PayloadSemantics ptype => BasicEnvelopeValidation (TestBlockWith ptype) where
  -- The block number of a test block is derived from the length of the hash
  expectedFirstBlockNo _ = BlockNo 1

instance PayloadSemantics ptype => ValidateEnvelope (TestBlockWith ptype)

-- Use defaults

instance PayloadSemantics ptype => LedgerSupportsProtocol (TestBlockWith ptype) where
  protocolLedgerView _ _ = ()
  ledgerViewForecastAt cfg state =
    constantForecastInRange (strictMaybeToMaybe (tblcForecastRange cfg)) () (getTipSlot state)

singleNodeTestConfigWith ::
  CodecConfig (TestBlockWith ptype) ->
  StorageConfig (TestBlockWith ptype) ->
  SecurityParam ->
  GenesisWindow ->
  TopLevelConfig (TestBlockWith ptype)
singleNodeTestConfigWith codecConfig storageConfig k genesisWindow =
  TopLevelConfig
    { topLevelConfigProtocol =
        BftConfig
          { bftParams =
              BftParams
                { bftSecurityParam = k
                , bftNumNodes = numCoreNodes
                }
          , bftSignKey = SignKeyMockDSIGN 0
          , bftVerKeys = Map.singleton (CoreId (CoreNodeId 0)) (VerKeyMockDSIGN 0)
          }
    , topLevelConfigLedger = ledgerCfgParams
    , topLevelConfigBlock = TestBlockConfig numCoreNodes
    , topLevelConfigCodec = codecConfig
    , topLevelConfigStorage = storageConfig
    , topLevelConfigCheckpoints = emptyCheckpointsMap
    }
 where
  slotLength :: SlotLength
  slotLength = slotLengthFromSec 20

  numCoreNodes :: NumCoreNodes
  numCoreNodes = NumCoreNodes 1

  ledgerCfgParams :: TestBlockLedgerConfig
  ledgerCfgParams =
    TestBlockLedgerConfig
      { tblcHardForkParams =
          (HardFork.defaultEraParams k slotLength)
            { HardFork.eraGenesisWin = genesisWindow
            }
      , tblcForecastRange = SNothing
      }

instance ImmutableEraParams (TestBlockWith ptype) where
  immutableEraParams = tblcHardForkParams . topLevelConfigLedger

{-------------------------------------------------------------------------------
  Test blocks without payload
-------------------------------------------------------------------------------}

-- | Block without payload
type TestBlock = TestBlockWith ()

-- | The 'TestBlock' does not need any codec config
data instance CodecConfig TestBlock = TestBlockCodecConfig
  deriving (Show, Generic, NoThunks)

-- | The 'TestBlock' does not need any storage config
data instance StorageConfig TestBlock = TestBlockStorageConfig
  deriving (Show, Generic, NoThunks)

instance HasHardForkHistory TestBlock where
  type HardForkIndices TestBlock = '[TestBlock]
  hardForkSummary = neverForksHardForkSummary tblcHardForkParams

data instance BlockQuery TestBlock fp result where
  QueryLedgerTip :: BlockQuery TestBlock QFNoTables (Point TestBlock)

instance BlockSupportsLedgerQuery TestBlock where
  answerPureBlockQuery _cfg QueryLedgerTip dlv =
    let
      TestLedger{lastAppliedPoint} = ledgerState dlv
     in
      lastAppliedPoint
  answerBlockQueryLookup _cfg q = case q of {}
  answerBlockQueryTraverse _cfg q = case q of {}
  blockQueryIsSupportedOnVersion QueryLedgerTip = const True

instance SameDepIndex2 (BlockQuery TestBlock) where
  sameDepIndex2 QueryLedgerTip QueryLedgerTip = Just Refl

deriving instance Eq (BlockQuery TestBlock fp result)
deriving instance Show (BlockQuery TestBlock fp result)

instance ShowQuery (BlockQuery TestBlock fp) where
  showResult QueryLedgerTip = show

testInitLedger :: LedgerState TestBlock ValuesMK
testInitLedger = testInitLedgerWithState EmptyPLDS

testInitExtLedger :: ExtLedgerState TestBlock ValuesMK
testInitExtLedger = testInitExtLedgerWithState EmptyPLDS

-- | Trivial test configuration with a single core node
singleNodeTestConfig :: TopLevelConfig TestBlock
singleNodeTestConfig = singleNodeTestConfigWithK (SecurityParam $ knownNonZeroBounded @4)

singleNodeTestConfigWithK :: SecurityParam -> TopLevelConfig TestBlock
singleNodeTestConfigWithK k =
  singleNodeTestConfigWith
    TestBlockCodecConfig
    TestBlockStorageConfig
    k
    (GenesisWindow (2 * unNonZero (maxRollbacks k)))

{-------------------------------------------------------------------------------
  Chain of blocks (without payload)
-------------------------------------------------------------------------------}

newtype BlockChain = BlockChain Word64
  deriving Show

blockChain :: BlockChain -> Chain TestBlock
blockChain = Chain.fromOldestFirst . chainToBlocks

chainToBlocks :: BlockChain -> [TestBlock]
chainToBlocks (BlockChain c) =
  take (fromIntegral c) $ iterate successorBlock (firstBlock 0)

instance Arbitrary BlockChain where
  arbitrary = BlockChain <$> choose (0, 30)
  shrink (BlockChain c) = BlockChain <$> shrink c

-- | See 'firstBlockWithPayload'.
firstBlock :: Word64 -> TestBlock
firstBlock forkNo = firstBlockWithPayload forkNo ()

-- | See 'successorBlockWithPayload'.
successorBlock :: TestBlock -> TestBlock
successorBlock TestBlockWith{tbHash, tbSlot} = successorBlockWithPayload tbHash tbSlot ()

-- Modify the (last) fork number of the given block:
-- @g@ -> @[.., f]@ -> @[.., g f]@
-- The 'SlotNo' is left unchanged.
modifyFork :: (Word64 -> Word64) -> TestBlock -> TestBlock
modifyFork g tb@TestBlockWith{tbHash = UnsafeTestHash (f NE.:| h)} =
  tb
    { tbHash = let !gf = g f in UnsafeTestHash (gf NE.:| h)
    }

-- Increase the fork number of the given block:
-- @[.., f]@ -> @[.., f+1]@
-- The 'SlotNo' is left unchanged.
--
-- In Zipper parlance, this corresponds to going right in a tree.
forkBlock :: TestBlock -> TestBlock
forkBlock = modifyFork succ

{-------------------------------------------------------------------------------
  Tree of blocks (without payload)
-------------------------------------------------------------------------------}

newtype BlockTree = BlockTree (Tree ())

blockTree :: BlockTree -> Tree TestBlock
blockTree (BlockTree t) = go (firstBlock 0) t
 where
  go :: TestBlock -> Tree () -> Tree TestBlock
  go b (Node () ts) = Node b (zipWith go bs ts)
   where
    -- The first child of a node is the sucessor of b ("go down"), each
    -- subsequent child is a "fork" ("go right")
    bs = iterate forkBlock (successorBlock b)

treeToBlocks :: BlockTree -> [TestBlock]
treeToBlocks = Tree.flatten . blockTree

treeToChains :: BlockTree -> [Chain TestBlock]
treeToChains = map Chain.fromOldestFirst . allPaths . blockTree

treePreferredChain :: BlockTree -> Chain TestBlock
treePreferredChain =
  fromMaybe Genesis
    . selectUnvalidatedChain
      (Proxy @(BlockProtocol TestBlock))
      (() :: ChainOrderConfig (SelectView (BlockProtocol TestBlock)))
      (\hdr -> SelectView (blockNo hdr) NoTiebreaker)
      Genesis
    . treeToChains

instance Show BlockTree where
  show (BlockTree t) = Tree.drawTree (fmap show t)

instance Arbitrary BlockTree where
  arbitrary = sized $ \n ->
    BlockTree <$> mkTree 0.2 (replicate (max 1 n) ())
  shrink (BlockTree t) =
    BlockTree <$> shrinkTree t

{-------------------------------------------------------------------------------
  Generic auxiliary
-------------------------------------------------------------------------------}

-- | Construct random binary tree from given set of elements
mkTree ::
  forall a.
  -- | Likelyhood of branching at any point
  Double ->
  [a] ->
  Gen (Tree a)
mkTree threshold = go
 where
  go :: [a] -> Gen (Tree a)
  go [] = error "go: no elements"
  go [a] = return $ Node a []
  go (a : as) = do
    n <- choose (0, 1)
    if n >= threshold || null right
      then (\t -> Node a [t]) <$> go as
      else (\l r -> Node a [l, r]) <$> go left <*> go right
   where
    (left, right) = split as

-- | Shrink tree (without shrinking any elements)
shrinkTree :: Tree a -> [Tree a]
shrinkTree (Node a ts) =
  map (Node a) (shrinkList shrinkTree ts)
    -- Also try shrinking all subtrees at once
    ++ map (Node a) (transpose (map shrinkTree ts))

-- | Split list into two
--
-- > split [1..6] == ([1,3,5],[2,4,6])
-- > take 5 (fst (split [1..])) == [1,3,5,7,9]
-- > take 5 (snd (split [1..])) == [2,4,6,8,10]
split :: [a] -> ([a], [a])
split [] = ([], [])
split (a : as) = let (xs, ys) = split as in (a : ys, xs)

-- | All paths through a tree
allPaths :: Tree a -> [[a]]
allPaths t = [] : nonEmptyPaths t

nonEmptyPaths :: Tree a -> [[a]]
nonEmptyPaths (Node a ts) = [a] : map (a :) (concatMap nonEmptyPaths ts)

{-------------------------------------------------------------------------------
  Test auxiliary
-------------------------------------------------------------------------------}

newtype Permutation = Permutation Int
  deriving Show

instance Arbitrary Permutation where
  arbitrary = Permutation . cast <$> arbitrary
   where
    -- Use the generator for 'Int64' (rather than 'Int') as it is not biased
    -- towards small values
    cast :: Int64 -> Int
    cast = fromIntegral

  -- Doesn't make sense to shrink PRNG seed
  shrink _ = []

permute :: Permutation -> [a] -> [a]
permute (Permutation n) = go (R.mkStdGen n)
 where
  go :: R.StdGen -> [a] -> [a]
  go _ [] = []
  go g as =
    let (i, g') = R.randomR (0, length as - 1) g
        (before, a : after) = splitAt i as
     in a : go g' (before ++ after)

{-------------------------------------------------------------------------------
  Additional serialisation instances
-------------------------------------------------------------------------------}

instance Serialise (AnnTip (TestBlockWith ptype)) where
  encode = defaultEncodeAnnTip encode
  decode = defaultDecodeAnnTip decode

instance PayloadSemantics ptype => Serialise (ExtLedgerState (TestBlockWith ptype) EmptyMK) where
  encode = encodeExtLedgerState encode encode encode
  decode = decodeExtLedgerState decode decode decode

instance Serialise (RealPoint (TestBlockWith ptype)) where
  encode = encodeRealPoint encode
  decode = decodeRealPoint decode

-- 'ConvertRawHash' expects a constant-size hash. As a compromise, we allow to
-- encode hashes with a block length of up to 100.
instance ConvertRawHash (TestBlockWith ptype) where
  -- 8 + 100 * 8: size of the list, and its elements, one Word64 each
  hashSize _ = 808
  toRawHash _ (TestHash h)
    | len > 100 = error "hash too long"
    | otherwise = BL.toStrict . Put.runPut $ do
        Put.putWord64le (fromIntegral len)
        for_ h Put.putWord64le
        replicateM_ (100 - len) $ Put.putWord64le 0
   where
    len = length h
  fromRawHash _ bs = flip Get.runGet (BL.fromStrict bs) $ do
    len <- fromIntegral <$> Get.getWord64le
    (NE.nonEmpty -> Just h, rs) <-
      splitAt len <$> replicateM 100 Get.getWord64le
    guard $ all (0 ==) rs
    pure $ TestHash h

instance Serialise ptype => EncodeDisk (TestBlockWith ptype) (TestBlockWith ptype)
instance Serialise ptype => DecodeDisk (TestBlockWith ptype) (BL.ByteString -> TestBlockWith ptype) where
  decodeDisk _ = const <$> decode

instance Serialise ptype => EncodeDisk (TestBlockWith ptype) (Header (TestBlockWith ptype))
instance Serialise ptype => DecodeDisk (TestBlockWith ptype) (BL.ByteString -> Header (TestBlockWith ptype)) where
  decodeDisk _ = const <$> decode

instance EncodeDisk (TestBlockWith ptype) (AnnTip (TestBlockWith ptype))
instance DecodeDisk (TestBlockWith ptype) (AnnTip (TestBlockWith ptype))

instance ReconstructNestedCtxt Header (TestBlockWith ptype)

instance
  PayloadSemantics ptype =>
  EncodeDisk (TestBlockWith ptype) (LedgerState (TestBlockWith ptype) EmptyMK)
instance
  PayloadSemantics ptype =>
  DecodeDisk (TestBlockWith ptype) (LedgerState (TestBlockWith ptype) EmptyMK)

instance Serialise ptype => EncodeDiskDep (NestedCtxt Header) (TestBlockWith ptype)
instance Serialise ptype => DecodeDiskDep (NestedCtxt Header) (TestBlockWith ptype)

-- ChainDepState (BlockProtocol (TestBlockWith ptype)) ~ ()
instance EncodeDisk (TestBlockWith ptype) ()
instance DecodeDisk (TestBlockWith ptype) ()

-- Header (TestBlockWith ptype) is a newtype around TestBlockWith ptype
instance Serialise ptype => HasBinaryBlockInfo (TestBlockWith ptype) where
  getBinaryBlockInfo blk =
    BinaryBlockInfo
      { headerOffset = 0
      , headerSize = fromIntegral . BL.length . serialise $ blk
      }

instance
  ( Serialise ptype
  , PayloadSemantics ptype
  , IndexedMemPack
      (LedgerState (TestBlockWith ptype) EmptyMK)
      (TxOut (LedgerState (TestBlockWith ptype)))
  , SerializeTablesWithHint (LedgerState (TestBlockWith ptype))
  ) =>
  SerialiseDiskConstraints (TestBlockWith ptype)

-----

deriving via
  SelectViewDiffusionPipelining (TestBlockWith ptype)
  instance
    BlockSupportsProtocol (TestBlockWith ptype) =>
    BlockSupportsDiffusionPipelining (TestBlockWith ptype)

-----

-- | Given a point to a chain of length L, generates a 'SwitchFork' that
-- switches to the "next" block of length L, where "next" is determined by
-- interpreting the "forks" in the 'TestHash' as binary digits (except the
-- deepest, which is a simple counter).
--
-- For example, the following are input and outputs for a chains of length 3,
-- where the 'TestHash'es and 'Point's are denoted by numerals (the 'SlotNo' is
-- merely the number of digits).
--
-- @
-- 000 :-> [RollBack 00, AddBlock 001]
-- 001 :-> [RollBack 0 , AddBlock 01 , AddBlock 010]
-- 010 :-> [RollBack 01, AddBlock 011]
-- 011 :-> [RollBack G , AddBlock 1  , AddBlock 10 , AddBlock 100]
--
-- 100 :-> [RollBack 10, AddBlock 101]
-- 101 :-> [RollBack 1 , AddBlock 11 , AddBlock 110]
-- 110 :-> [RollBack 11, AddBlock 111]
-- 111 :-> [RollBack G , AddBlock 2  , AddBlock 20 , AddBlock 200]
--
-- 200 :-> [RollBack 20, AddBlock 201]
-- 201 :-> [RollBack 2 , AddBlock 21 , AddBlock 210]
-- 210 :-> [RollBack 21, AddBlock 211]
-- 211 :-> [RollBack G , AddBlock 3  , AddBlock 30 , AddBlock 300]
--
-- etc
-- @
updateToNextNumeral :: RealPoint TestBlock -> (Point TestBlock, NonEmpty TestBlock)
updateToNextNumeral rp0 =
  let TestHash (fork :| forks) = hash0 in go (0 :: Int) fork forks
 where
  RealPoint slot0 hash0 = rp0

  go !depth fork = \case
    [] -> rebuild depth (fork + 1) []
    fork2 : forks ->
      if 0 == fork
        then rebuild depth 1 (fork2 : forks)
        else
          go (depth + 1) fork2 forks

  rebuild depth fork' forks =
    let islot = slot0 - fromIntegral depth - 1
        ipoint = case NE.nonEmpty forks of
          Nothing -> GenesisPoint
          Just ne -> BlockPoint islot (TestHash ne)

        next =
          TestBlockWith
            { tbHash = TestHash (fork' :| forks)
            , tbSlot = islot + 1
            , tbValid = Valid
            , tbPayload = ()
            }
     in (ipoint, go' (next :| []) depth)

  go' ne = \case
    0 -> NE.reverse ne
    depth ->
      go'
        (successorBlock (NE.head ne) `NE.cons` ne)
        (depth - 1)
