{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Ledger.Query (
    -- * Queries that can be answered by the Consensus layer
    Query (..)
  , answerQuery
    -- * How to answer specific queries
  , BlockQuery
  , BlockSupportsLedgerQuery (..)
  , ConfigSupportsNode (..)
  , ShowQuery (..)
    -- * Version
  , QueryVersion (..)
  , nodeToClientVersionToQueryVersion
    -- * Serialization
  , queryDecodeNodeToClient
  , queryEncodeNodeToClient
    -- * Footprints
  , QueryFootprint (..)
  , SQueryFootprint (..)
  , SomeBlockQuery (..)
  ) where

import           Cardano.Binary (FromCBOR (..), ToCBOR (..))
import           Cardano.Slotting.Block (BlockNo (..))
import           Cardano.Slotting.Slot (WithOrigin (..))
import           Codec.CBOR.Decoding
import           Codec.CBOR.Encoding
import           Codec.Serialise (Serialise)
import           Codec.Serialise.Class (decode, encode)
import           Control.Exception (throw)
import           Data.Kind (Type)
import           Data.Maybe (isJust)
import           Data.Singletons
import           Data.SOP.BasicFunctors
import           Ouroboros.Consensus.Block.Abstract (CodecConfig)
import           Ouroboros.Consensus.BlockchainTime (SystemStart)
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Config.SupportsNode
import           Ouroboros.Consensus.HeaderValidation (HasAnnTip (..),
                     headerStateBlockNo, headerStatePoint)
import           Ouroboros.Consensus.Ledger.Basics
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Query.Version
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
                     (BlockNodeToClientVersion)
import           Ouroboros.Consensus.Node.Serialisation
                     (SerialiseBlockQueryResult (..),
                     SerialiseNodeToClient (..), SerialiseResult (..))
import           Ouroboros.Consensus.Storage.LedgerDB
import qualified Ouroboros.Consensus.Storage.LedgerDB as LedgerDB
import           Ouroboros.Consensus.Util (ShowProxy (..), SomeSecond (..))
import           Ouroboros.Consensus.Util.DepPair
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Network.Block (HeaderHash, Point (..), StandardHash,
                     decodePoint, encodePoint)
import           Ouroboros.Network.Protocol.LocalStateQuery.Type

{-------------------------------------------------------------------------------
  Footprints
-------------------------------------------------------------------------------}

-- | Queries on the local state might require reading ledger tables from disk.
-- This datatype (which will sometimes be concretized via @sing@) allows
-- Consensus to categorize the queries.
data QueryFootprint =
    -- | The query doesn't need ledger tables, thus can be answered only with
    -- the ledger state.
    QFNoTables
    -- | The query needs some tables, but doesn't need to traverse the whole
    -- backing store.
  | QFLookupTables
    -- | The query needs to traverse the whole backing store.
  | QFTraverseTables

type instance Sing = SQueryFootprint

type SQueryFootprint :: QueryFootprint -> Type
data SQueryFootprint a where
    SQFNoTables       :: SQueryFootprint QFNoTables
    SQFLookupTables   :: SQueryFootprint QFLookupTables
    SQFTraverseTables :: SQueryFootprint QFTraverseTables

instance SingI QFNoTables where
  sing = SQFNoTables
instance SingI QFLookupTables where
  sing = SQFLookupTables
instance SingI QFTraverseTables where
  sing = SQFTraverseTables

type SomeBlockQuery :: (QueryFootprint -> Type -> Type) -> Type
data SomeBlockQuery q =
  forall footprint result. SingI footprint => SomeBlockQuery !(q footprint result)

{-------------------------------------------------------------------------------
  Block Queries
-------------------------------------------------------------------------------}

-- | Different queries supported by the ledger, indexed by the result type.
type BlockQuery :: Type -> QueryFootprint ->  Type -> Type
data family BlockQuery

-- | Query the ledger extended state.
--
-- Used by the LocalStateQuery protocol to allow clients to query the extended
-- ledger state.
class
     -- These instances are not needed for BlockSupportsLedgerQuery but we bundle them here
     -- so that we don't need to put them in 'SingleEraBlock' later on
     (
#if __GLASGOW_HASKELL__ <= 902
       forall fp result. Show          (BlockQuery blk fp result),
#endif
       forall fp.        ShowQuery     (BlockQuery blk fp)
     ,                   SameDepIndex2 (BlockQuery blk)
     )
  => BlockSupportsLedgerQuery blk where

  -- | Answer the given query about the extended ledger state, without reading
  -- ledger tables from the disk.
  answerPureBlockQuery ::
       ExtLedgerCfg blk
    -> BlockQuery blk QFNoTables result
    -> ExtLedgerState blk EmptyMK
    -> result

  -- | Answer a query that requires to perform a lookup on the ledger tables. As
  -- consensus always runs with a HardForkBlock, this might result in a
  -- different code path to answer a query compared to the one that a single
  -- block would take, one that is aware of the fact that the ledger tables
  -- might be HF ledger tables thus making use of some utilities to make these
  -- queries faster.
  --
  -- For the hard fork block this will be instantiated to
  -- 'Ouroboros.Consensus.HardFork.Combinator.Ledger.Query.answerBlockQueryHFLookup'.
  answerBlockQueryLookup ::
       MonadSTM m
    => ExtLedgerCfg blk
    -> BlockQuery blk QFLookupTables result
    -> ReadOnlyForker' m blk
    -> m result

  -- | Answer a query that requires to traverse the ledger tables. As consensus
  -- always runs with a HardForkBlock, this might result in a different code
  -- path to answer a query compared to the one that a single block would take,
  -- one that is aware of the fact that the ledger tables might be HF ledger
  -- tables thus making use of some utilities to make these queries faster.
  --
  -- For the hard fork block this will be instantiated to
  -- 'Ouroboros.Consensus.HardFork.Combinator.Ledger.Query.answerBlockQueryHFTraverse'.
  answerBlockQueryTraverse ::
       MonadSTM m
    => ExtLedgerCfg blk
    -> BlockQuery blk QFTraverseTables result
    -> ReadOnlyForker' m blk
    -> m result

{-------------------------------------------------------------------------------
  Queries
-------------------------------------------------------------------------------}

queryName :: Query blk result -> String
queryName query = case query of
  BlockQuery _    -> "BlockQuery"
  GetSystemStart  -> "GetSystemStart"
  GetChainBlockNo -> "GetChainBlockNo"
  GetChainPoint   -> "GetChainPoint"

-- | Different queries supported by the ledger for all block types, indexed
-- by the result type.
--
-- Additions to the set of queries is versioned by 'QueryVersion'
type Query :: Type -> Type -> Type
data Query blk result where
  -- | This constructor is supported by all @QueryVersion@s. The @BlockQuery@
  -- argument is versioned by the @BlockNodeToClientVersion blk@.
  BlockQuery ::
    SingI footprint => BlockQuery blk footprint result -> Query blk result

  -- | Get the 'SystemStart' time.
  --
  -- Supported by 'QueryVersion' >= 'QueryVersion1'.
  GetSystemStart :: Query blk SystemStart

  -- | Get the 'GetChainBlockNo' time.
  --
  -- Supported by 'QueryVersion' >= 'QueryVersion2'.
  GetChainBlockNo :: Query blk (WithOrigin BlockNo)

  -- | Get the 'GetChainPoint' time.
  --
  -- Supported by 'QueryVersion' >= 'QueryVersion2'.
  GetChainPoint :: Query blk (Point blk)

-- | Answer the given query about the extended ledger state.
answerQuery ::
     forall blk m result.
     (BlockSupportsLedgerQuery blk, ConfigSupportsNode blk, HasAnnTip blk, MonadSTM m)
  => ExtLedgerCfg blk
  -> ReadOnlyForker' m blk
  -> Query blk result
  -> m result
answerQuery config forker query = case query of
    BlockQuery (blockQuery :: BlockQuery blk footprint result) ->
      case sing :: Sing footprint of
        SQFNoTables ->
          answerPureBlockQuery config blockQuery <$>
            atomically (LedgerDB.roforkerGetLedgerState forker)
        SQFLookupTables ->
          answerBlockQueryLookup config blockQuery forker
        SQFTraverseTables ->
          answerBlockQueryTraverse config blockQuery forker
    GetSystemStart ->
      pure $ getSystemStart (topLevelConfigBlock (getExtLedgerCfg config))
    GetChainBlockNo ->
      headerStateBlockNo . headerState <$>
        atomically (LedgerDB.roforkerGetLedgerState forker)
    GetChainPoint ->
      headerStatePoint . headerState <$>
        atomically (LedgerDB.roforkerGetLedgerState forker)

{-------------------------------------------------------------------------------
  Query instances
-------------------------------------------------------------------------------}

------
-- Show
------

deriving instance
     (forall footprint result. Show (BlockQuery blk footprint result))
  => Show (SomeBlockQuery (BlockQuery blk))

deriving instance
     (forall footprint. Show (BlockQuery blk footprint result))
  => Show (Query blk result)

instance (ShowProxy (BlockQuery blk)) => ShowProxy (Query blk) where
  showProxy (Proxy :: Proxy (Query blk)) =
    "Query (" ++ showProxy (Proxy @(BlockQuery blk)) ++ ")"

instance
     (forall footprint. ShowQuery (BlockQuery blk footprint), StandardHash blk)
  => ShowQuery (Query blk) where
  showResult (BlockQuery blockQuery) = showResult blockQuery
  showResult GetSystemStart          = show
  showResult GetChainBlockNo         = show
  showResult GetChainPoint           = show

instance Show (SomeBlockQuery (BlockQuery blk)) => Show (SomeSecond Query blk) where
  show (SomeSecond (BlockQuery blockQueryA))  =
    "Query " ++ show (SomeBlockQuery blockQueryA)
  show (SomeSecond GetSystemStart)            = "Query GetSystemStart"
  show (SomeSecond GetChainBlockNo)           = "Query GetChainBlockNo"
  show (SomeSecond GetChainPoint)             = "Query GetChainPoint"

------
-- Eq
------

instance SameDepIndex (Query blk) => Eq (SomeSecond Query blk) where
  SomeSecond l == SomeSecond r = isJust $ sameDepIndex l r

instance SameDepIndex2 query => Eq (SomeBlockQuery query) where
  SomeBlockQuery l == SomeBlockQuery r = isJust $ sameDepIndex2 l r

instance SameDepIndex2 (BlockQuery blk) => SameDepIndex (Query blk) where
  sameDepIndex (BlockQuery blockQueryA) (BlockQuery blockQueryB)
    = (\Refl -> Refl) <$> sameDepIndex2 blockQueryA blockQueryB
  sameDepIndex (BlockQuery _) _
    = Nothing
  sameDepIndex GetSystemStart GetSystemStart
    = Just Refl
  sameDepIndex GetSystemStart _
    = Nothing
  sameDepIndex GetChainBlockNo GetChainBlockNo
    = Just Refl
  sameDepIndex GetChainBlockNo _
    = Nothing
  sameDepIndex GetChainPoint GetChainPoint
    = Just Refl
  sameDepIndex GetChainPoint _
    = Nothing

------
-- Serialization
------

deriving newtype instance
     SerialiseNodeToClient blk ( SomeBlockQuery     (query blk))
  => SerialiseNodeToClient blk ((SomeBlockQuery :.: query) blk)

-- | Exception thrown in the encoders: A query was submitted that is not
-- supported by the given 'QueryVersion'
data QueryEncoderException = forall blk. Show (SomeSecond Query blk) =>
    QueryEncoderUnsupportedQuery (SomeSecond Query blk) QueryVersion

deriving instance Show QueryEncoderException
instance Show QueryEncoderException => Exception QueryEncoderException

queryEncodeNodeToClient ::
     forall blk.
     SerialiseNodeToClient blk (SomeBlockQuery (BlockQuery blk))
  => Show (SomeSecond Query blk)
  => CodecConfig blk
  -> QueryVersion
  -> BlockNodeToClientVersion blk
  -> SomeSecond Query blk
  -> Encoding
queryEncodeNodeToClient codecConfig queryVersion blockVersion (SomeSecond query)
  = case query of
      BlockQuery blockQuery ->
        requireVersion QueryVersion1 $ mconcat
          [ encodeListLen 2
          , encodeWord8 0
          , encodeBlockQuery blockQuery
          ]

      GetSystemStart ->
        requireVersion QueryVersion1 $ mconcat
          [ encodeListLen 1
          , encodeWord8 1
          ]

      GetChainBlockNo ->
        requireVersion QueryVersion2 $ mconcat
          [ encodeListLen 1
          , encodeWord8 2
          ]

      GetChainPoint ->
        requireVersion QueryVersion2 $ mconcat
          [ encodeListLen 1
          , encodeWord8 3
          ]

  where
    requireVersion :: QueryVersion -> a -> a
    requireVersion expectedVersion a =
      if queryVersion >= expectedVersion
        then a
        else throw $ QueryEncoderUnsupportedQuery (SomeSecond query) queryVersion

    encodeBlockQuery ::
         SingI footprint
      => BlockQuery blk footprint result
      -> Encoding
    encodeBlockQuery blockQuery =
      encodeNodeToClient
        @blk
        @(SomeBlockQuery (BlockQuery blk))
        codecConfig
        blockVersion
        (SomeBlockQuery blockQuery)

queryDecodeNodeToClient ::
     forall blk.
     SerialiseNodeToClient blk (SomeBlockQuery (BlockQuery blk))
  => CodecConfig blk
  -> QueryVersion
  -> BlockNodeToClientVersion blk
  -> forall s. Decoder s (SomeSecond Query blk)
queryDecodeNodeToClient codecConfig queryVersion blockVersion
  = case queryVersion of
      QueryVersion1 -> handleTopLevelQuery
      QueryVersion2 -> handleTopLevelQuery
  where
    handleTopLevelQuery :: Decoder s (SomeSecond Query blk)
    handleTopLevelQuery = do
        size <- decodeListLen
        tag  <- decodeWord8
        case (size, tag) of
          (2, 0) -> requireVersion QueryVersion1 =<< decodeBlockQuery
          (1, 1) -> requireVersion QueryVersion1 $ SomeSecond GetSystemStart
          (1, 2) -> requireVersion QueryVersion2 $ SomeSecond GetChainBlockNo
          (1, 3) -> requireVersion QueryVersion2 $ SomeSecond GetChainPoint
          _      -> fail $ "Query: invalid size and tag" <> show (size, tag)

    requireVersion ::
         QueryVersion
      -> SomeSecond Query blk
      -> Decoder s (SomeSecond Query blk)
    requireVersion expectedVersion someSecondQuery =
      if queryVersion >= expectedVersion
        then return someSecondQuery
        else case someSecondQuery of
          SomeSecond query -> fail $ "Query: " <> queryName query <> " requires at least " <> show expectedVersion

    decodeBlockQuery :: Decoder s (SomeSecond Query blk)
    decodeBlockQuery = do
      SomeBlockQuery blockQuery <- decodeNodeToClient
        @blk
        @(SomeBlockQuery (BlockQuery blk))
        codecConfig
        blockVersion
      return (SomeSecond (BlockQuery blockQuery))

instance ( SerialiseBlockQueryResult blk BlockQuery
         , Serialise (HeaderHash blk)
         ) => SerialiseResult blk Query where
  encodeResult codecConfig blockVersion (BlockQuery blockQuery) result
    = encodeBlockQueryResult codecConfig blockVersion blockQuery result
  encodeResult _ _ GetSystemStart result
    = toCBOR result
  encodeResult _ _ GetChainBlockNo result
    = toCBOR result
  encodeResult _ _ GetChainPoint result
    = encodePoint encode result

  decodeResult codecConfig blockVersion (BlockQuery query)
    = decodeBlockQueryResult codecConfig blockVersion query
  decodeResult _ _ GetSystemStart
    = fromCBOR
  decodeResult _ _ GetChainBlockNo
    = fromCBOR
  decodeResult _ _ GetChainPoint
    = decodePoint decode
