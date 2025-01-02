{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Ledger.Query (
    BlockQuery
  , BlockSupportsLedgerQuery (..)
  , ConfigSupportsNode (..)
  , Query (..)
  , QueryVersion (..)
  , ShowQuery (..)
  , answerQueryM
  , nodeToClientVersionToQueryVersion
  , queryDecodeNodeToClient
  , queryEncodeNodeToClient
  ) where

import           Cardano.Binary (FromCBOR (..), ToCBOR (..))
import           Cardano.Slotting.Block (BlockNo (..))
import           Cardano.Slotting.Slot (WithOrigin (..))
import           Codec.CBOR.Decoding
import           Codec.CBOR.Encoding
import           Codec.Serialise (Serialise)
import           Codec.Serialise.Class (decode, encode)
import           Control.Exception (Exception, throw)
import           Data.Kind (Type)
import           Data.Maybe (isJust)
import           Data.Typeable (Typeable)
import           Ouroboros.Consensus.Block.Abstract (CodecConfig)
import           Ouroboros.Consensus.BlockchainTime (SystemStart)
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Config.SupportsNode
import           Ouroboros.Consensus.HeaderValidation (HasAnnTip (..),
                     headerStateBlockNo, headerStatePoint)
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Query.Version
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
                     (BlockNodeToClientVersion)
import           Ouroboros.Consensus.Node.Serialisation
                     (SerialiseNodeToClient (..), SerialiseResult (..))
import           Ouroboros.Consensus.Util (ShowProxy (..), SomeSecond (..), SomeThird (..))
import           Ouroboros.Consensus.Util.DepPair
import           Ouroboros.Network.Block (HeaderHash, Point (..), StandardHash,
                     decodePoint, encodePoint)
import           Ouroboros.Network.Protocol.LocalStateQuery.Type
                     (ShowQuery (..))
import qualified Ouroboros.Network.PublicState as Public

{-------------------------------------------------------------------------------
  Queries
-------------------------------------------------------------------------------}

queryName :: Query blk addr result -> String
queryName query = case query of
  BlockQuery _    -> "BlockQuery"
  GetSystemStart  -> "GetSystemStart"
  GetChainBlockNo -> "GetChainBlockNo"
  GetChainPoint   -> "GetChainPoint"
  GetNetworkState -> "GetNetworkState"

-- | Different queries supported by the ledger for all block types, indexed
-- by the result type.
--
-- Additions to the set of queries is versioned by 'QueryVersion'
data Query blk addr result where
  -- | This constructor is supported by all @QueryVersion@s. The @BlockQuery@
  -- argument is versioned by the @BlockNodeToClientVersion blk@.
  BlockQuery :: BlockQuery blk result -> Query blk addr result

  -- | Get the 'SystemStart' time.
  --
  -- Supported by 'QueryVersion' >= 'QueryVersion1'.
  GetSystemStart :: Query blk addr SystemStart

  -- | Get the 'GetChainBlockNo' time.
  --
  -- Supported by 'QueryVersion' >= 'QueryVersion2'.
  GetChainBlockNo :: Query blk addr (WithOrigin BlockNo)

  -- | Get the 'GetChainPoint' time.
  --
  -- Supported by 'QueryVersion' >= 'QueryVersion2'.
  GetChainPoint :: Query blk addr (Point blk)

  -- | Get the `Public.NetworkState`.
  --
  GetNetworkState :: Query blk addr (Public.NetworkState addr)

instance (ShowProxy (BlockQuery blk)) => ShowProxy (Query blk addr) where
  showProxy (Proxy :: Proxy (Query blk addr)) = "Query (" ++ showProxy (Proxy @(BlockQuery blk)) ++ ")"

instance (ShowQuery (BlockQuery blk), StandardHash blk, Show addr) => ShowQuery (Query blk addr) where
  showResult (BlockQuery blockQuery) = showResult blockQuery
  showResult GetSystemStart          = show
  showResult GetChainBlockNo         = show
  showResult GetChainPoint           = show
  showResult GetNetworkState         = show

instance Eq (SomeSecond BlockQuery blk) => Eq (SomeThird Query blk addr) where
  SomeThird (BlockQuery blockQueryA) == SomeThird (BlockQuery blockQueryB)
    = SomeSecond blockQueryA == SomeSecond blockQueryB
  SomeThird (BlockQuery _) == _ = False

  SomeThird GetSystemStart == SomeThird GetSystemStart = True
  SomeThird GetSystemStart == _                         = False

  SomeThird GetChainBlockNo == SomeThird GetChainBlockNo  = True
  SomeThird GetChainBlockNo == _                           = False

  SomeThird GetChainPoint == SomeThird GetChainPoint  = True
  SomeThird GetChainPoint == _                         = False

  SomeThird GetNetworkState == SomeThird GetNetworkState = True
  SomeThird GetNetworkState == _                          = True

instance Show (SomeSecond BlockQuery blk) => Show (SomeThird Query blk addr) where
  show (SomeThird (BlockQuery blockQueryA))  = "Query " ++ show (SomeSecond blockQueryA)
  show (SomeThird GetSystemStart)            = "Query GetSystemStart"
  show (SomeThird GetChainBlockNo)           = "Query GetChainBlockNo"
  show (SomeThird GetChainPoint)             = "Query GetChainPoint"
  show (SomeThird GetNetworkState)           = "Query GetNetworkState"


-- | Exception thrown in the encoders
data QueryEncoderException blk addr =
    -- | A query was submitted that is not supported by the given 'QueryVersion'
    QueryEncoderUnsupportedQuery
         (SomeThird Query blk addr)
         QueryVersion

deriving instance Show (SomeSecond BlockQuery blk)
    => Show (QueryEncoderException blk addr)
instance (Typeable blk, Show (SomeSecond BlockQuery blk), Typeable addr, Show addr)
    => Exception (QueryEncoderException blk addr)

queryEncodeNodeToClient ::
     forall blk addr.
     Typeable blk
  => Show (SomeSecond BlockQuery blk)
  => Typeable addr
  => Show addr
  => SerialiseNodeToClient blk (SomeSecond BlockQuery blk)
  => CodecConfig blk
  -> QueryVersion
  -> BlockNodeToClientVersion blk
  -> SomeThird Query blk addr
  -> Encoding
queryEncodeNodeToClient codecConfig queryVersion blockVersion (SomeThird query)
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

      GetNetworkState ->
        requireVersion QueryVersion3 $ mconcat
          [ encodeListLen 1
          , encodeWord8 4
          ]

  where
    requireVersion :: QueryVersion -> a -> a
    requireVersion expectedVersion a =
      if queryVersion >= expectedVersion
        then a
        else throw $ QueryEncoderUnsupportedQuery (SomeThird query) queryVersion

    encodeBlockQuery blockQuery =
      encodeNodeToClient
        @blk
        @(SomeSecond BlockQuery blk)
        codecConfig
        blockVersion
        (SomeSecond blockQuery)

queryDecodeNodeToClient ::
     forall blk addr.
     SerialiseNodeToClient blk (SomeSecond BlockQuery blk)
  => CodecConfig blk
  -> QueryVersion
  -> BlockNodeToClientVersion blk
  -> forall s. Decoder s (SomeThird Query blk addr)
queryDecodeNodeToClient codecConfig queryVersion blockVersion
  = case queryVersion of
      QueryVersion1 -> handleTopLevelQuery
      QueryVersion2 -> handleTopLevelQuery
      QueryVersion3 -> handleTopLevelQuery
  where
    handleTopLevelQuery :: Decoder s (SomeThird Query blk addr)
    handleTopLevelQuery = do
        size <- decodeListLen
        tag  <- decodeWord8
        case (size, tag) of
          (2, 0) -> requireVersion QueryVersion1 =<< decodeBlockQuery
          (1, 1) -> requireVersion QueryVersion1 $ SomeThird GetSystemStart
          (1, 2) -> requireVersion QueryVersion2 $ SomeThird GetChainBlockNo
          (1, 3) -> requireVersion QueryVersion2 $ SomeThird GetChainPoint
          (1, 4) -> requireVersion QueryVersion3 $ SomeThird GetNetworkState
          _      -> fail $ "Query: invalid size and tag" <> show (size, tag)

    requireVersion ::
         QueryVersion
      -> SomeThird Query blk addr
      -> Decoder s (SomeThird Query blk addr)
    requireVersion expectedVersion someQuery =
      if queryVersion >= expectedVersion
        then return someQuery
        else case someQuery of
          SomeThird query -> fail $ "Query: " <> queryName query <> " requires at least " <> show expectedVersion

    decodeBlockQuery :: Decoder s (SomeThird Query blk addr)
    decodeBlockQuery = do
      SomeSecond blockQuery <- decodeNodeToClient
        @blk
        @(SomeSecond BlockQuery blk)
        codecConfig
        blockVersion
      return (SomeThird (BlockQuery blockQuery))

instance ( SerialiseResult blk (BlockQuery blk)
         , Serialise (HeaderHash blk)
         , Serialise (Public.RemoteAddressEncoding addr)
         , Ord addr
         ) => SerialiseResult blk (Query blk addr) where
  encodeResult codecConfig blockVersion (BlockQuery blockQuery) result
    = encodeResult codecConfig blockVersion blockQuery result
  encodeResult _ _ GetSystemStart result
    = toCBOR result
  encodeResult _ _ GetChainBlockNo result
    = toCBOR result
  encodeResult _ _ GetChainPoint result
    = encodePoint encode result
  encodeResult _ _ GetNetworkState result
    = Public.encodeNetworkState result

  decodeResult codecConfig blockVersion (BlockQuery query)
    = decodeResult codecConfig blockVersion query
  decodeResult _ _ GetSystemStart
    = fromCBOR
  decodeResult _ _ GetChainBlockNo
    = fromCBOR
  decodeResult _ _ GetChainPoint
    = decodePoint decode
  decodeResult _ _ GetNetworkState
    = Public.decodeNetworkState


instance SameDepIndex (BlockQuery blk) => SameDepIndex (Query blk addr) where
  sameDepIndex (BlockQuery blockQueryA) (BlockQuery blockQueryB)
    = sameDepIndex blockQueryA blockQueryB
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
  sameDepIndex GetNetworkState GetNetworkState
    = Just Refl
  sameDepIndex GetNetworkState _
    = Nothing

deriving instance Show (BlockQuery blk result) => Show (Query blk addr result)

-- | Answer the given query about the extended ledger state.
answerQueryM ::
     (BlockSupportsLedgerQuery blk, ConfigSupportsNode blk, HasAnnTip blk,
      Monad m)
  => ExtLedgerCfg blk
  -> Query blk addr result
  -> ExtLedgerState blk
  -> m (Public.NetworkState addr)
  -> m result
answerQueryM cfg query st getNetworkState = case query of
  BlockQuery blockQuery -> return $ answerBlockQuery cfg blockQuery st
  GetSystemStart -> return $ getSystemStart (topLevelConfigBlock (getExtLedgerCfg cfg))
  GetChainBlockNo -> return $ headerStateBlockNo (headerState st)
  GetChainPoint -> return $ headerStatePoint (headerState st)
  GetNetworkState -> getNetworkState

-- | Different queries supported by the ledger, indexed by the result type.
data family BlockQuery blk :: Type -> Type

-- | Query the ledger extended state.
--
-- Used by the LocalStateQuery protocol to allow clients to query the extended
-- ledger state.
class (ShowQuery (BlockQuery blk), SameDepIndex (BlockQuery blk))
   => BlockSupportsLedgerQuery blk where

  -- | Answer the given query about the extended ledger state.
  answerBlockQuery ::
      ExtLedgerCfg blk
   -> BlockQuery blk result
   -> ExtLedgerState blk
   -> result

instance SameDepIndex (BlockQuery blk) => Eq (SomeSecond BlockQuery blk) where
  SomeSecond qry == SomeSecond qry' = isJust (sameDepIndex qry qry')

deriving instance (forall result. Show (BlockQuery blk result))
   => Show (SomeSecond BlockQuery blk)
