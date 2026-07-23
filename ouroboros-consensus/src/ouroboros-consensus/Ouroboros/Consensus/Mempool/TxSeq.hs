{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

-- | Intended for qualified import.
--
-- > import           Ouroboros.Consensus.Mempool.TxSeq (TxSeq (..))
-- > import qualified Ouroboros.Consensus.Mempool.TxSeq as TxSeq
module Ouroboros.Consensus.Mempool.TxSeq
  ( TicketNo (..)
  , TxSeq (Empty, (:>), (:<))
  , TxTicket (..)
  , fromList
  , lookupByTicketNo
  , splitAfterTicketNo
  , splitAfterTxSize
  , toList
  , toSize
  , toTuples
  , zeroTicketNo

    -- * Reference implementations for testing
  , splitAfterTxSizeSpec
  ) where

import Control.Arrow ((***))
import Data.FingerTree.Strict (StrictFingerTree)
import qualified Data.FingerTree.Strict as FingerTree
import qualified Data.Foldable as Foldable
import Data.Measure (Measure)
import qualified Data.Measure as Measure
import Data.Word (Word64)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)

{-------------------------------------------------------------------------------
  Mempool transaction sequence as a finger tree
-------------------------------------------------------------------------------}

-- | We allocate each transaction a (monotonically increasing) ticket number
-- as it enters the mempool.
newtype TicketNo = TicketNo Word64
  deriving stock (Eq, Ord, Show)
  deriving newtype (Enum, Bounded, NoThunks)

-- | The transaction ticket number from which our counter starts.
zeroTicketNo :: TicketNo
zeroTicketNo = TicketNo 0

-- | We associate transactions in the mempool with their ticket number and
-- size in bytes.
data TxTicket sz tx = TxTicket
  { txTicketTx :: !tx
  -- ^ The transaction associated with this ticket.
  , txTicketNo :: !TicketNo
  -- ^ The ticket number.
  , txTicketSize :: !sz
  -- ^ The size of 'txTicketTx'.
  }
  deriving (Eq, Show, Generic, NoThunks)

-- | The mempool is a sequence of transactions with their ticket numbers and
-- size in bytes.
--
-- Transactions are allocated monotonically increasing ticket numbers as they
-- are appended to the mempool sequence. Transactions can be removed from any
-- position, not just the front.
--
-- The sequence is thus ordered by the ticket numbers. We can use the ticket
-- numbers as a compact representation for a \"reader\" location in the
-- sequence. If a reader knows it has seen all txs with a lower ticket number
-- then it is only interested in transactions with higher ticket numbers.
--
-- The mempool sequence is represented by a fingertree. We use a fingertree
-- measure to allow not just normal sequence operations but also efficient
-- splitting and indexing by the ticket number.
newtype TxSeq sz tx
  = TxSeq (StrictFingerTree (TxSeqMeasure sz) (TxTicket sz tx))
  deriving stock Show
  deriving newtype NoThunks

instance Measure sz => Foldable (TxSeq sz) where
  foldMap f (TxSeq txs) = Foldable.foldMap (f . txTicketTx) txs
  null (TxSeq txs) = Foldable.null txs
  length (TxSeq txs) = mCount $ FingerTree.measure txs

-- | The 'StrictFingerTree' relies on a \"measure\" for subsequences in the
-- tree. A measure of the size of the subsequence allows for efficient
-- sequence operations. Also measuring the min and max ticket number allows
-- for efficient operations based on the ticket number (assuming the sequence
-- is ordered by ticket number).
--
-- To use a 'StrictFingerTree' with a 'TxSeqMeasure' we have to provide a way
-- to measure individual elements of the sequence (i.e. 'TxTicket's), via a
-- 'Measured' instance, and also a way to combine the measures, via a 'Monoid'
-- instance.
data TxSeqMeasure sz = TxSeqMeasure
  { mCount :: !Int
  , mMinTicket :: !TicketNo
  , mMaxTicket :: !TicketNo
  , mSize :: !sz
  }
  deriving Show

instance Measure sz => FingerTree.Measured (TxSeqMeasure sz) (TxTicket sz tx) where
  measure ticket =
    TxSeqMeasure
      { mCount = 1
      , mMinTicket = txTicketNo
      , mMaxTicket = txTicketNo
      , mSize = txTicketSize
      }
   where
    TxTicket{txTicketNo, txTicketSize} = ticket

instance Measure sz => Semigroup (TxSeqMeasure sz) where
  vl <> vr =
    TxSeqMeasure
      (mCount vl + mCount vr)
      (mMinTicket vl `min` mMinTicket vr)
      (mMaxTicket vl `max` mMaxTicket vr)
      (mSize vl `Measure.plus` mSize vr)

instance Measure sz => Monoid (TxSeqMeasure sz) where
  mempty =
    TxSeqMeasure
      { mCount = 0
      , mMinTicket = maxBound -- note the inversion!
      , mMaxTicket = minBound
      , mSize = Measure.zero
      }
  mappend = (<>)

-- | A helper function for the ':>' pattern.
viewBack :: Measure sz => TxSeq sz tx -> Maybe (TxSeq sz tx, TxTicket sz tx)
viewBack (TxSeq txs) = case FingerTree.viewr txs of
  FingerTree.EmptyR -> Nothing
  txs' FingerTree.:> tx -> Just (TxSeq txs', tx)

-- | A helper function for the ':<' pattern.
viewFront :: Measure sz => TxSeq sz tx -> Maybe (TxTicket sz tx, TxSeq sz tx)
viewFront (TxSeq txs) = case FingerTree.viewl txs of
  FingerTree.EmptyL -> Nothing
  tx FingerTree.:< txs' -> Just (tx, TxSeq txs')

-- | An empty mempool sequence.
pattern Empty :: Measure sz => TxSeq sz tx
pattern Empty <- (viewFront -> Nothing)
  where
    Empty = TxSeq FingerTree.empty

-- | \( O(1) \). Access or add a tx at the back of the mempool sequence.
--
-- New txs are always added at the back.
pattern (:>) :: Measure sz => TxSeq sz tx -> TxTicket sz tx -> TxSeq sz tx
pattern txs :> tx <- (viewBack -> Just (txs, tx))
  where
    TxSeq txs :> tx = TxSeq (txs FingerTree.|> tx) -- TODO: assert ordered by ticket no

-- | \( O(1) \). Access a tx at the front of the mempool sequence.
--
-- Note that we never add txs at the front. We access txs from front to back
-- when forwarding txs to other peers, or when adding txs to blocks.
pattern (:<) :: Measure sz => TxTicket sz tx -> TxSeq sz tx -> TxSeq sz tx
pattern tx :< txs <- (viewFront -> Just (tx, txs))

infixl 5 :>, :<

{-# COMPLETE Empty, (:>) #-}
{-# COMPLETE Empty, (:<) #-}

-- | \( O(\log(n)) \). Look up a transaction in the sequence by its 'TicketNo'.
lookupByTicketNo :: Measure sz => TxSeq sz tx -> TicketNo -> Maybe tx
lookupByTicketNo (TxSeq txs) n =
  case FingerTree.search
    ( \ml mr ->
        mMaxTicket ml >= n
          && mMinTicket mr > n
    )
    txs of
    FingerTree.Position _ (TxTicket tx n' _) _ | n' == n -> Just tx
    _ -> Nothing

-- | \( O(\log(n)) \). Split the sequence of transactions into two parts
-- based on the given 'TicketNo'. The first part has transactions with tickets
-- less than or equal to the given ticket, and the second part has transactions
-- with tickets strictly greater than the given ticket.
splitAfterTicketNo ::
  Measure sz =>
  TxSeq sz tx ->
  TicketNo ->
  (TxSeq sz tx, TxSeq sz tx)
splitAfterTicketNo (TxSeq txs) n =
  case FingerTree.split (\m -> mMaxTicket m > n) txs of
    (l, r) -> (TxSeq l, TxSeq r)

-- | \( O(\log(n)) \). Split the sequence of transactions into two parts based
-- on the given @sz@. The first part has transactions whose summed @sz@ is less
-- than or equal to the given @sz@, and the second part has the remaining
-- transactions in the sequence.
splitAfterTxSize ::
  Measure sz =>
  TxSeq sz tx ->
  sz ->
  (TxSeq sz tx, TxSeq sz tx)
splitAfterTxSize (TxSeq txs) n =
  case FingerTree.split (\m -> not $ mSize m Measure.<= n) txs of
    (l, r) -> (TxSeq l, TxSeq r)

-- | \( O(n) \). Specification of 'splitAfterTxSize'.
--
-- Use 'splitAfterTxSize' as it should be faster.
--
-- This function is used to verify whether 'splitAfterTxSize' behaves as
-- expected.
splitAfterTxSizeSpec ::
  forall sz tx.
  Measure sz =>
  TxSeq sz tx ->
  sz ->
  (TxSeq sz tx, TxSeq sz tx)
splitAfterTxSizeSpec txseq n =
  (fromList *** fromList) $
    go Measure.zero [] $
      toList txseq
 where
  go ::
    sz ->
    [TxTicket sz tx] ->
    [TxTicket sz tx] ->
    ([TxTicket sz tx], [TxTicket sz tx])
  go accSize accTickets = \case
    [] ->
      (reverse accTickets, [])
    t : ts
      | let accSize' = accSize `Measure.plus` txTicketSize t
      , accSize' Measure.<= n ->
          go accSize' (t : accTickets) ts
      | otherwise ->
          (reverse accTickets, t : ts)

-- | Given a list of 'TxTicket's, construct a 'TxSeq'.
fromList :: Measure sz => [TxTicket sz tx] -> TxSeq sz tx
fromList = Foldable.foldl' (:>) Empty

-- | Convert a 'TxSeq' to a list of 'TxTicket's.
toList :: TxSeq sz tx -> [TxTicket sz tx]
toList (TxSeq ftree) = Foldable.toList ftree

-- | Convert a 'TxSeq' to a list of pairs of transactions and their
-- associated 'TicketNo's and sizes.
toTuples :: TxSeq sz tx -> [(tx, TicketNo, sz)]
toTuples (TxSeq ftree) =
  fmap
    ( \ticket ->
        ( txTicketTx ticket
        , txTicketNo ticket
        , txTicketSize ticket
        )
    )
    (Foldable.toList ftree)

-- | \( O(1) \). Return the total size of the given 'TxSeq'.
toSize :: Measure sz => TxSeq sz tx -> sz
toSize (TxSeq ftree) = mSize
 where
  TxSeqMeasure{mSize} = FingerTree.measure ftree
