{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | This module is a derivative of "Data.MemPack" but we provide something that
-- will be used to " index " the serialization.
--
-- The idea is that we can use this in the Cardano block to avoid serializing a
-- tag next to the TxOut, as the Ledger layer establishes the property that
-- TxOuts are forwards deserializable, meaning we can read them in any later
-- era.
module Ouroboros.Consensus.Util.IndexedMemPack
  ( IndexedMemPack (..)
  , MemPack (..)
  , indexedPackByteString
  , indexedPackByteArray
  , indexedUnpackError
  , indexedUnpackEither
  , unpackEither
  ) where

import qualified Control.Monad as Monad
import Control.Monad.ST
import Control.Monad.Trans.Fail
import Data.Array.Byte (ByteArray (..))
import Data.Bifunctor (first)
import Data.ByteString
import Data.MemPack
import Data.MemPack.Buffer
import Data.MemPack.Error
import Data.Proxy
import GHC.Stack
import Ouroboros.Consensus.Ledger.Tables.MapKind

-- | See 'MemPack'.
class IndexedMemPack l blk a where
  indexedPackedByteCount :: l blk EmptyMK -> a -> Int
  indexedPackM :: l blk EmptyMK -> a -> Pack s ()
  indexedUnpackM :: Buffer b => forall s. l blk EmptyMK -> Unpack s b a
  indexedTypeName :: Proxy a -> l blk EmptyMK -> String

indexedPackByteString ::
  forall a l blk. (IndexedMemPack l blk a, HasCallStack) => l blk EmptyMK -> a -> ByteString
indexedPackByteString idx = pinnedByteArrayToByteString . indexedPackByteArray True idx
{-# INLINE indexedPackByteString #-}

indexedPackByteArray ::
  forall a l blk.
  (IndexedMemPack l blk a, HasCallStack) =>
  Bool ->
  l blk EmptyMK ->
  a ->
  ByteArray
indexedPackByteArray isPinned idx a =
  packWithByteArray
    isPinned
    (indexedTypeName (Proxy @a) idx)
    (indexedPackedByteCount idx a)
    (indexedPackM idx a)
{-# INLINE indexedPackByteArray #-}

indexedUnpackError ::
  forall l blk a b. (Buffer b, IndexedMemPack l blk a, HasCallStack) => l blk EmptyMK -> b -> a
indexedUnpackError idx = errorFail . indexedUnpackFail idx
{-# INLINEABLE indexedUnpackError #-}

indexedUnpackFail ::
  forall l blk a b.
  (IndexedMemPack l blk a, Buffer b, HasCallStack) => l blk EmptyMK -> b -> Fail SomeError a
indexedUnpackFail idx b = do
  let len = bufferByteCount b
  (a, consumedBytes) <- indexedUnpackLeftOver idx b
  Monad.when (consumedBytes /= len) $
    unpackFailNotFullyConsumed (indexedTypeName (Proxy @a) idx) consumedBytes len
  pure a
{-# INLINEABLE indexedUnpackFail #-}

indexedUnpackLeftOver ::
  forall l blk a b.
  (IndexedMemPack l blk a, Buffer b, HasCallStack) => l blk EmptyMK -> b -> Fail SomeError (a, Int)
indexedUnpackLeftOver idx b = FailT $ pure $ runST $ runFailAggT $ indexedUnpackLeftOverST idx b
{-# INLINEABLE indexedUnpackLeftOver #-}

indexedUnpackLeftOverST ::
  forall l blk a b s.
  (IndexedMemPack l blk a, Buffer b, HasCallStack) =>
  l blk EmptyMK -> b -> FailT SomeError (ST s) (a, Int)
indexedUnpackLeftOverST idx b = do
  let len = bufferByteCount b
  res@(_, consumedBytes) <- runStateT (runUnpack (indexedUnpackM idx) b) 0
  Monad.when (consumedBytes > len) $ errorLeftOver (indexedTypeName (Proxy @a) idx) consumedBytes len
  pure res
{-# INLINEABLE indexedUnpackLeftOverST #-}

indexedUnpackEither ::
  forall l blk a b.
  (IndexedMemPack l blk a, Buffer b, HasCallStack) => l blk EmptyMK -> b -> Either SomeError a
indexedUnpackEither idx = first fromMultipleErrors . runFailAgg . indexedUnpackFail idx
{-# INLINEABLE indexedUnpackEither #-}

unpackEither ::
  forall a b.
  (MemPack a, Buffer b, HasCallStack) => b -> Either SomeError a
unpackEither = first fromMultipleErrors . runFailAgg . unpackFail
{-# INLINEABLE unpackEither #-}

errorLeftOver :: HasCallStack => String -> Int -> Int -> a
errorLeftOver name consumedBytes len =
  error $
    "Potential buffer overflow. Some bug in 'unpackM' was detected while unpacking " <> name
      ++ ". Consumed " <> showBytes (consumedBytes - len) <> " more than allowed from a buffer of length "
      ++ show len
{-# NOINLINE errorLeftOver #-}

unpackFailNotFullyConsumed :: Applicative m => String -> Int -> Int -> FailT SomeError m a
unpackFailNotFullyConsumed name consumedBytes len =
  failT $
    toSomeError $
      NotFullyConsumedError
        { notFullyConsumedRead = consumedBytes
        , notFullyConsumedAvailable = len
        , notFullyConsumedTypeName = name
        }
{-# NOINLINE unpackFailNotFullyConsumed #-}
