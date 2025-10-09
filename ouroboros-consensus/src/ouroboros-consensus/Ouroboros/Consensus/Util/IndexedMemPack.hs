{-# LANGUAGE AllowAmbiguousTypes #-}
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
import GHC.Stack

-- | See 'MemPack'.
class IndexedMemPack idx a where
  indexedPackedByteCount :: idx -> a -> Int
  indexedPackM :: idx -> a -> Pack s ()
  indexedUnpackM :: Buffer b => forall s. idx -> Unpack s b a
  indexedTypeName :: idx -> String

indexedPackByteString ::
  forall a idx. (IndexedMemPack idx a, HasCallStack) => idx -> a -> ByteString
indexedPackByteString idx = pinnedByteArrayToByteString . indexedPackByteArray True idx
{-# INLINE indexedPackByteString #-}

indexedPackByteArray ::
  forall a idx.
  (IndexedMemPack idx a, HasCallStack) =>
  Bool ->
  idx ->
  a ->
  ByteArray
indexedPackByteArray isPinned idx a =
  packWithByteArray
    isPinned
    (indexedTypeName @idx @a idx)
    (indexedPackedByteCount idx a)
    (indexedPackM idx a)
{-# INLINE indexedPackByteArray #-}

indexedUnpackError ::
  forall idx a b. (Buffer b, IndexedMemPack idx a, HasCallStack) => idx -> b -> a
indexedUnpackError idx = errorFail . indexedUnpackFail idx
{-# INLINEABLE indexedUnpackError #-}

indexedUnpackFail ::
  forall idx a b. (IndexedMemPack idx a, Buffer b, HasCallStack) => idx -> b -> Fail SomeError a
indexedUnpackFail idx b = do
  let len = bufferByteCount b
  (a, consumedBytes) <- indexedUnpackLeftOver idx b
  Monad.when (consumedBytes /= len) $
    unpackFailNotFullyConsumed (indexedTypeName @idx @a idx) consumedBytes len
  pure a
{-# INLINEABLE indexedUnpackFail #-}

indexedUnpackLeftOver ::
  forall idx a b.
  (IndexedMemPack idx a, Buffer b, HasCallStack) => idx -> b -> Fail SomeError (a, Int)
indexedUnpackLeftOver idx b = FailT $ pure $ runST $ runFailAggT $ indexedUnpackLeftOverST idx b
{-# INLINEABLE indexedUnpackLeftOver #-}

indexedUnpackLeftOverST ::
  forall idx a b s.
  (IndexedMemPack idx a, Buffer b, HasCallStack) => idx -> b -> FailT SomeError (ST s) (a, Int)
indexedUnpackLeftOverST idx b = do
  let len = bufferByteCount b
  res@(_, consumedBytes) <- runStateT (runUnpack (indexedUnpackM idx) b) 0
  Monad.when (consumedBytes > len) $ errorLeftOver (indexedTypeName @idx @a idx) consumedBytes len
  pure res
{-# INLINEABLE indexedUnpackLeftOverST #-}

indexedUnpackEither ::
  forall idx a b.
  (IndexedMemPack idx a, Buffer b, HasCallStack) => idx -> b -> Either SomeError a
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
