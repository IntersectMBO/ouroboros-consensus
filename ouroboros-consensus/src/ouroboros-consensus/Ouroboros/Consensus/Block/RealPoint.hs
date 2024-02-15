{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Block.RealPoint (
    -- * Non-genesis points
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
  ) where

import           Cardano.Binary (enforceSize)
import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding, encodeListLen)
import           Codec.Serialise (decode, encode)
import           Data.Coerce
import           Data.Proxy
import           Data.Typeable (Typeable, typeRep)
import           GHC.Generics
import           NoThunks.Class (NoThunks (..))
import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.Util.Condense

{-------------------------------------------------------------------------------
  Non-genesis point
-------------------------------------------------------------------------------}

-- | Point of an actual block (i.e., not genesis)
data RealPoint blk = RealPoint !SlotNo !(HeaderHash blk)
  deriving (Generic)

-- TODO: The Ord instance should go
-- <https://github.com/IntersectMBO/ouroboros-network/issues/1693>
deriving instance StandardHash blk => Eq   (RealPoint blk)
deriving instance StandardHash blk => Ord  (RealPoint blk)
deriving instance StandardHash blk => Show (RealPoint blk)

instance (StandardHash blk, Typeable blk)
      => NoThunks (RealPoint blk) where
  showTypeOf _ = show $ typeRep (Proxy @(RealPoint blk))

instance Condense (HeaderHash blk) => Condense (RealPoint blk) where
  condense (RealPoint s h) = "(Point " <> condense s <> ", " <> condense h <> ")"

encodeRealPoint :: (HeaderHash blk -> Encoding)
                -> (RealPoint  blk -> Encoding)
encodeRealPoint encodeHash (RealPoint s h) = mconcat [
      encodeListLen 2
    , encode s
    , encodeHash h
    ]

decodeRealPoint :: (forall s. Decoder s (HeaderHash blk))
                -> (forall s. Decoder s (RealPoint  blk))
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
    HeaderFields { headerFieldSlot = s, headerFieldHash = h } = getHeaderFields blk

#if __GLASGOW_HASKELL__ >= 904
headerRealPoint :: (HasHeader blk, HasHeader (Header blk)) => Header blk -> RealPoint blk
#else
-- GHC 9.4 considers these constraints insufficient.
headerRealPoint :: HasHeader (Header blk) => Header blk -> RealPoint blk
#endif
headerRealPoint hdr = RealPoint s h
  where
    HeaderFields { headerFieldSlot = s, headerFieldHash = h } = getHeaderFields hdr

realPointToPoint :: RealPoint blk -> Point blk
realPointToPoint (RealPoint s h) = BlockPoint s h

withOriginRealPointToPoint :: WithOrigin (RealPoint blk) -> Point blk
withOriginRealPointToPoint Origin        = GenesisPoint
withOriginRealPointToPoint (NotOrigin p) = realPointToPoint p

pointToWithOriginRealPoint :: Point blk -> WithOrigin (RealPoint blk)
pointToWithOriginRealPoint GenesisPoint     = Origin
pointToWithOriginRealPoint (BlockPoint s h) = NotOrigin $ RealPoint s h

castRealPoint ::
     forall blk blk'. Coercible (HeaderHash blk) (HeaderHash blk')
  => RealPoint blk
  -> RealPoint blk'
castRealPoint (RealPoint s h) = RealPoint s (coerce h)
