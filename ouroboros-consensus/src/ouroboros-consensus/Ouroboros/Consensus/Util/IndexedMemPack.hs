{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

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
import Data.Kind (Constraint, Type)
import Data.MemPack
import Data.MemPack.Buffer
import Data.MemPack.Error
import Data.Proxy
import GHC.Stack
import Ouroboros.Consensus.Ledger.LedgerStateType
import Ouroboros.Consensus.Ledger.Tables.MapKind (EmptyMK)

-- | See 'MemPack'.
type IndexedMemPack :: StateKind -> Type -> k -> Constraint
class IndexedMemPack l blk table where
  type IndexedValue l table blk
  indexedPackedByteCount ::
    Proxy l ->
    Proxy blk ->
    Proxy table ->
    l blk EmptyMK ->
    IndexedValue l table blk ->
    Int

  indexedPackM ::
    Proxy l ->
    Proxy blk ->
    Proxy table ->
    l blk EmptyMK ->
    IndexedValue l table blk ->
    Pack s ()

  indexedUnpackM ::
    Buffer b =>
    Proxy l ->
    Proxy blk ->
    Proxy table ->
    forall s. l blk EmptyMK -> Unpack s b (IndexedValue l table blk)

  indexedTypeName :: Proxy l -> Proxy blk -> Proxy table -> String

indexedPackByteString ::
  forall table l blk.
  (IndexedMemPack l blk table, HasCallStack) =>
  l blk EmptyMK -> IndexedValue l table blk -> ByteString
indexedPackByteString idx = pinnedByteArrayToByteString . indexedPackByteArray @table True idx
{-# INLINE indexedPackByteString #-}

indexedPackByteArray ::
  forall table (l :: StateKind) blk.
  (IndexedMemPack l blk table, HasCallStack) =>
  Bool ->
  l blk EmptyMK ->
  IndexedValue l table blk ->
  ByteArray
indexedPackByteArray isPinned idx a =
  packWithByteArray
    isPinned
    (indexedTypeName (Proxy @l) (Proxy @blk) (Proxy @table))
    (indexedPackedByteCount (Proxy @l) (Proxy @blk) (Proxy @table) idx a)
    (indexedPackM (Proxy @l) (Proxy @blk) (Proxy @table) idx a)
{-# INLINE indexedPackByteArray #-}

indexedUnpackError ::
  forall table l blk b.
  (Buffer b, IndexedMemPack l blk table, HasCallStack) =>
  l blk EmptyMK -> b -> IndexedValue l table blk
indexedUnpackError idx = errorFail . indexedUnpackFail @table idx
{-# INLINEABLE indexedUnpackError #-}

indexedUnpackFail ::
  forall table l blk b.
  (IndexedMemPack l blk table, Buffer b, HasCallStack) =>
  l blk EmptyMK -> b -> Fail SomeError (IndexedValue l table blk)
indexedUnpackFail idx b = do
  let len = bufferByteCount b
  (a, consumedBytes) <- indexedUnpackLeftOver @table idx b
  Monad.when (consumedBytes /= len) $
    unpackFailNotFullyConsumed
      (indexedTypeName (Proxy @l) (Proxy @blk) (Proxy @table))
      consumedBytes
      len
  pure a
{-# INLINEABLE indexedUnpackFail #-}

indexedUnpackLeftOver ::
  forall table l blk b.
  (IndexedMemPack l blk table, Buffer b, HasCallStack) =>
  l blk EmptyMK -> b -> Fail SomeError (IndexedValue l table blk, Int)
indexedUnpackLeftOver idx b = FailT $ pure $ runST $ runFailAggT $ indexedUnpackLeftOverST @table idx b
{-# INLINEABLE indexedUnpackLeftOver #-}

indexedUnpackLeftOverST ::
  forall table l blk b s.
  (IndexedMemPack l blk table, Buffer b, HasCallStack) =>
  l blk EmptyMK -> b -> FailT SomeError (ST s) (IndexedValue l table blk, Int)
indexedUnpackLeftOverST idx b = do
  let len = bufferByteCount b
  res@(_, consumedBytes) <-
    runStateT (runUnpack (indexedUnpackM (Proxy @l) (Proxy @blk) (Proxy @table) idx) b) 0
  Monad.when (consumedBytes > len) $
    errorLeftOver (indexedTypeName (Proxy @l) (Proxy @blk) (Proxy @table)) consumedBytes len
  pure res
{-# INLINEABLE indexedUnpackLeftOverST #-}

indexedUnpackEither ::
  forall table l blk b.
  (IndexedMemPack l blk table, Buffer b, HasCallStack) =>
  l blk EmptyMK -> b -> Either SomeError (IndexedValue l table blk)
indexedUnpackEither idx = first fromMultipleErrors . runFailAgg . indexedUnpackFail @table idx
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
