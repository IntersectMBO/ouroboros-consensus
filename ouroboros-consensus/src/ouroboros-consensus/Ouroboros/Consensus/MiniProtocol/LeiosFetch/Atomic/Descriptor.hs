{-# LANGUAGE NamedFieldPuns #-}

-- | Immutable work prepared before irreversible request admission. Preparation
-- allocates no wire identity or capacity. The planned checkpoint builder will
-- assemble these descriptors from wanted transactions.
--
-- Rationale (repository-root path):
-- @docs/fetch-policy/cancellation-contract.md#validation-descriptors-and-progress-events@.
-- That section links descriptor and size-accounting choices to the contract.
module Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Descriptor
  ( PayloadBytes (..)
  , EncodedBytes (..)
  , EntryCount (..)
  , ExpectedTx (..)
  , BodyDescriptor (..)
  , BatchDescriptor (..)
  , RequestDescriptor (..)
  , PreparedRequest
  , PreparationError (..)
  , prepareBody
  , prepareBatch
  , preparedDescriptor
  , preparedResponseBytes
  , preparedElements
  ) where

import qualified Data.IntMap.Strict as IntMap
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Word (Word32)
import Numeric.Natural (Natural)
import Ouroboros.Consensus.Leios.Types
  ( EndorserBlock
  , LeiosEbBounds (..)
  , LeiosPoint
  , TxHash
  , TxOffset
  , endorserBlockReferences
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Identity (AcquisitionGeneration)

-- These units intentionally have no implicit arithmetic/conversion instances.
-- Natural sums avoid overflow before a later checked wire-size conversion.
newtype PayloadBytes = PayloadBytes Natural deriving (Eq, Ord, Show)
newtype EncodedBytes = EncodedBytes Natural deriving (Eq, Ord, Show)
newtype EntryCount = EntryCount Natural deriving (Eq, Ord, Show)

data ExpectedTx = ExpectedTx
  { expectedOffset :: !TxOffset
  , expectedHash :: !TxHash
  , expectedSize :: !Word32
  }
  deriving (Eq, Show)

data BodyDescriptor = BodyDescriptor
  { bodyPoint :: !LeiosPoint
  , bodyGeneration :: !AcquisitionGeneration
  , bodyAnnouncedBytes :: !Word32
  -- ^ Untrusted timer input, distinct from the reservation ceiling.
  , bodyTrustedBounds :: !LeiosEbBounds
  }
  deriving (Eq, Show)

data BatchDescriptor = BatchDescriptor
  { batchPoint :: !LeiosPoint
  , batchGeneration :: !AcquisitionGeneration
  , batchExpected :: !(NonEmpty ExpectedTx)
  }
  deriving (Eq, Show)

data RequestDescriptor = BodyRequest !BodyDescriptor | BatchRequest !BatchDescriptor
  deriving (Eq, Show)

data PreparedRequest = PreparedRequest
  { preparedDescriptor :: !RequestDescriptor
  , preparedResponseBytes :: !EncodedBytes
  -- ^ Upper bound including response framing, supplied by the wire builder.
  , preparedElements :: !(NonEmpty PayloadBytes)
  -- ^ Body: one upper bound. TX batch: exact sizes in ascending offset order.
  }
  deriving (Eq, Show)

data PreparationError = InvalidSize | InvalidOffsets | OffsetOutsideBody | InsufficientFraming
  deriving (Eq, Show)

prepareBody :: BodyDescriptor -> EncodedBytes -> Either PreparationError PreparedRequest
prepareBody body@BodyDescriptor{bodyTrustedBounds = MkLeiosEbBounds{lebMaxBodySize}} encoded =
  prepare (BodyRequest body) encoded (PayloadBytes (fromIntegral lebMaxBodySize) NE.:| [])

-- | Resolve every offset against the trusted body now, rather than consulting
-- a mutable current-wanted set when a response arrives.
prepareBatch ::
  LeiosPoint ->
  AcquisitionGeneration ->
  EndorserBlock ->
  NonEmpty TxOffset ->
  EncodedBytes ->
  Either PreparationError PreparedRequest
prepareBatch point generation body offsets encoded
  | not (and (zipWith (<) os (drop 1 os))) = Left InvalidOffsets
  | otherwise = do
      expected <- traverse lookupTx offsets
      prepare
        (BatchRequest (BatchDescriptor point generation expected))
        encoded
        (fmap (PayloadBytes . fromIntegral . expectedSize) expected)
 where
  os = NE.toList offsets
  refs = IntMap.fromDistinctAscList (zip [0 ..] (endorserBlockReferences body))
  lookupTx offset = case IntMap.lookup offset refs of
    Nothing -> Left OffsetOutsideBody
    Just (hash, size) -> Right (ExpectedTx offset hash size)

prepare ::
  RequestDescriptor ->
  EncodedBytes ->
  NonEmpty PayloadBytes ->
  Either PreparationError PreparedRequest
prepare descriptor encoded@(EncodedBytes wire) elements
  | any (== PayloadBytes 0) elements = Left InvalidSize
  | wire < sum [n | PayloadBytes n <- NE.toList elements] = Left InsufficientFraming
  | otherwise = Right (PreparedRequest descriptor encoded elements)
