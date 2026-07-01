{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Block.RealPoint
  ( -- * Non-genesis points
    RealPoint (..)
  , decodeRealPoint
  , encodeRealPoint

    -- * Derived
  , blockRealPoint
  , castRealPoint
  , headerRealPoint
  , pointToWithOriginRealPoint
  , realPointHash
  , realPointSlot
  , realPointToPoint
  , withOriginRealPointToPoint

    -- * Bytes32RealPoint
  , Bytes32RealPoint
  , bytes32RealPointHash
  , bytes32RealPointSlot
  , decodeBytes32RealPoint
  , encodeBytes32RealPoint
  , fromBytes32RealPoint
  , toBytes32RealPoint
  ) where

import Cardano.Binary (enforceSize)
import Codec.CBOR.Decoding (Decoder)
import Codec.CBOR.Encoding (Encoding, encodeListLen)
import Codec.Serialise (decode, encode)
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as ByteString
import Data.Coerce
import Data.Proxy
import Data.Typeable (Typeable, typeRep)
import GHC.Generics
import NoThunks.Class (NoThunks (..))
import Ouroboros.Consensus.Block.Abstract
import Ouroboros.Consensus.Util.Condense
import Ouroboros.Consensus.Util.RedundantConstraints

{-------------------------------------------------------------------------------
  Non-genesis point
-------------------------------------------------------------------------------}

-- | Point of an actual block (i.e., not genesis)
data RealPoint blk = RealPoint !SlotNo !(HeaderHash blk)
  deriving Generic

-- TODO: The Ord instance should go
-- <https://github.com/IntersectMBO/ouroboros-network/issues/1693>
deriving instance StandardHash blk => Eq (RealPoint blk)
deriving instance StandardHash blk => Ord (RealPoint blk)
deriving instance StandardHash blk => Show (RealPoint blk)

instance
  (StandardHash blk, Typeable blk) =>
  NoThunks (RealPoint blk)
  where
  showTypeOf _ = show $ typeRep (Proxy @(RealPoint blk))

instance Condense (HeaderHash blk) => Condense (RealPoint blk) where
  condense (RealPoint s h) = "(Point " <> condense s <> ", " <> condense h <> ")"

encodeRealPoint ::
  (HeaderHash blk -> Encoding) ->
  (RealPoint blk -> Encoding)
encodeRealPoint encodeHash (RealPoint s h) =
  mconcat
    [ encodeListLen 2
    , encode s
    , encodeHash h
    ]

decodeRealPoint ::
  (forall s. Decoder s (HeaderHash blk)) ->
  (forall s. Decoder s (RealPoint blk))
decodeRealPoint decodeHash = do
  enforceSize "RealPoint" 2
  RealPoint <$> decode <*> decodeHash

{-------------------------------------------------------------------------------
  Derived
-------------------------------------------------------------------------------}

realPointSlot :: RealPoint blk -> SlotNo
realPointSlot (RealPoint s _) = s

realPointHash :: RealPoint blk -> HeaderHash blk
realPointHash (RealPoint _ h) = h

blockRealPoint :: HasHeader blk => blk -> RealPoint blk
blockRealPoint blk = RealPoint s h
 where
  HeaderFields{headerFieldSlot = s, headerFieldHash = h} = getHeaderFields blk

headerRealPoint ::
  forall blk.
  HasHeader (Header blk) =>
  Header blk ->
  RealPoint blk
headerRealPoint hdr = RealPoint s h
 where
  HeaderFields{headerFieldSlot = s, headerFieldHash = h} = hf

  hf :: HeaderFields (Header blk)
  hf = getHeaderFields hdr

realPointToPoint :: RealPoint blk -> Point blk
realPointToPoint (RealPoint s h) = BlockPoint s h

withOriginRealPointToPoint :: WithOrigin (RealPoint blk) -> Point blk
withOriginRealPointToPoint Origin = GenesisPoint
withOriginRealPointToPoint (NotOrigin p) = realPointToPoint p

pointToWithOriginRealPoint :: Point blk -> WithOrigin (RealPoint blk)
pointToWithOriginRealPoint GenesisPoint = Origin
pointToWithOriginRealPoint (BlockPoint s h) = NotOrigin $ RealPoint s h

castRealPoint ::
  forall blk blk'.
  Coercible (HeaderHash blk) (HeaderHash blk') =>
  RealPoint blk ->
  RealPoint blk'
castRealPoint (RealPoint s h) = RealPoint s (coerce h)

{-------------------------------------------------------------------------------
  Bytes32RealPoint
-------------------------------------------------------------------------------}

-- | A 'RealPoint' where the hash is always 32 bytes.
--
-- The length of the hash is enforced during decoding.
data Bytes32RealPoint = Bytes32RealPoint !SlotNo !ShortByteString
  deriving (Show, Eq, Generic, NoThunks)

bytes32RealPointSlot :: Bytes32RealPoint -> SlotNo
bytes32RealPointSlot (Bytes32RealPoint s _) = s

bytes32RealPointHash :: Bytes32RealPoint -> ShortByteString
bytes32RealPointHash (Bytes32RealPoint _ h) = h

encodeBytes32RealPoint :: Bytes32RealPoint -> Encoding
encodeBytes32RealPoint (Bytes32RealPoint s h) =
  mconcat
    [ encodeListLen 2
    , encode s
    , encode h
    ]

decodeBytes32RealPoint :: forall s. Decoder s Bytes32RealPoint
decodeBytes32RealPoint = do
  enforceSize "Bytes32RealPoint" 2
  s <- decode
  h <- decode
  case ByteString.length h of
    32 -> pure (Bytes32RealPoint s h)
    len -> fail $ "decodeBytes32RealPoint: expected 32 bytes, got " <> show len

fromBytes32RealPoint ::
  forall blk.
  (ConvertRawHash blk, HashSize blk ~ 32) =>
  Bytes32RealPoint ->
  RealPoint blk
fromBytes32RealPoint (Bytes32RealPoint s h) =
  RealPoint s (unsafeFromShortRawHash (Proxy @blk) h)
 where
  -- This constraint ensures we are allowed to call 'unsafeFromShortRawHash'.
  _ = keepRedundantConstraint (Proxy @(HashSize blk ~ 32))

toBytes32RealPoint ::
  forall blk.
  (ConvertRawHash blk, HashSize blk ~ 32) =>
  RealPoint blk ->
  Bytes32RealPoint
toBytes32RealPoint (RealPoint s h) =
  Bytes32RealPoint s (toShortRawHash (Proxy @blk) h)
 where
  -- This constraint ensures the hash we are wrapping in 'Bytes32RealPoint' is
  -- indeed 32 bytes long.
  _ = keepRedundantConstraint (Proxy @(HashSize blk ~ 32))
