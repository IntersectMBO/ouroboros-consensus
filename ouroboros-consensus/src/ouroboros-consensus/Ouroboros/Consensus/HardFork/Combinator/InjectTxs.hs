{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Injecting a transaction from one block type to another
module Ouroboros.Consensus.HardFork.Combinator.InjectTxs (
    -- * Polymorphic
    InjectPolyTx (..)
  , cannotInjectPolyTx
  , matchPolyTx
  , matchPolyTxs
    -- * Unvalidated transactions
  , InjectTx
  , cannotInjectTx
  , matchTx
  , pattern InjectTx
    -- * Validated transactions
  , InjectValidatedTx
  , cannotInjectValidatedTx
  , matchValidatedTx
  , matchValidatedTxsNS
  , pattern InjectValidatedTx
  ) where

import           Data.Bifunctor
import           Data.Either (partitionEithers)
import           Data.Functor.Product
import           Data.SOP.BasicFunctors
import           Data.SOP.Constraint
import           Data.SOP.InPairs (InPairs (..))
import           Data.SOP.Match
import           Data.SOP.Strict
import           Data.SOP.Telescope (Telescope (..))
import qualified Data.SOP.Telescope as Telescope
import           Ouroboros.Consensus.HardFork.Combinator.State.Types
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Consensus.Util (pairFst)

{-------------------------------------------------------------------------------
  Polymorphic definitions
-------------------------------------------------------------------------------}

-- | @tx@ is either 'GenTx' or 'WrapValidatedGenTx'
--
-- See 'InjectTx' and 'InjectValidatedTx', respectively.
data InjectPolyTx tx blk blk' = InjectPolyTx {
      injectTxWith :: tx blk  -> Maybe (tx blk')
    }

-- | The injection that always fails
cannotInjectPolyTx :: InjectPolyTx tx blk blk'
cannotInjectPolyTx = InjectPolyTx $ const Nothing

-- | Match transaction with a telescope, attempting to inject where possible
matchPolyTx' ::
     InPairs (InjectPolyTx tx) xs
  -> NS tx xs
  -> Telescope g f xs
  -> Either (Mismatch tx f xs)
            (Telescope g (Product tx f) xs)
matchPolyTx' = go
  where
    go :: InPairs (InjectPolyTx tx) xs
       -> NS tx xs
       -> Telescope g f xs
       -> Either (Mismatch tx f xs)
                 (Telescope g (Product tx f) xs)
    go _            (Z x) (TZ f)   = Right $ TZ (Pair x f)
    go (PCons _ is) (S x) (TS g f) = bimap MS (TS g) $ go is x f
    go _            (S x) (TZ f)   = Left $ MR x f
    go (PCons i is) (Z x) (TS g f) =
        case injectTxWith i x of
          Nothing -> Left $ ML x (Telescope.tip f)
          Just x' -> bimap MS (TS g) $ go is (Z x') f

matchPolyTx ::
     SListI xs
  => InPairs (InjectPolyTx tx) xs
  -> NS tx xs
  -> HardForkState f xs
  -> Either (Mismatch tx (Current f) xs)
            (HardForkState (Product tx f) xs)
matchPolyTx is tx =
      fmap (HardForkState . hmap distrib)
    . matchPolyTx' is tx
    . getHardForkState
  where
    distrib :: Product tx (Current f) blk -> Current (Product tx f) blk
    distrib (Pair tx' Current{..}) = Current {
          currentStart = currentStart
        , currentState = Pair tx' currentState
        }

-- | A transaction coupled with its original version.
--
-- We use this to keep the original hard fork transaction around, as otherwise
-- we would lose the index at which the transaction was originally, before
-- translations.
data TxWithOriginal tx xs blk =
  TxWithOriginal { origTx :: !(NS tx xs)
                  , blkTx :: !(tx blk)
                  }

-- | A list of 'TxWithOriginal' that is ready to be partially applied by having
-- @blk@ as the final argument.
--
-- In the end it represents @[(orig :: NS tx xs, t :: tx blk), ...]@ for some
-- @blk@.
newtype ListOfTxs tx xs blk = ListOfTxs [TxWithOriginal tx xs blk]

-- | A special telescope. This type alias is used just for making this more
-- readable.
--
-- This in the end is basically:
--
-- > TS ... (
-- >    TZ (
-- >         [(orig, tx), ...]
-- >       , f
-- >    ) ...)
--
-- So at the tip of the telescope, we have both an @f@ and a list of tuples of
-- transactions.
type TelescopeWithTxList g f tx xs' xs  =
   Telescope g (Product (ListOfTxs tx xs') f) xs

matchPolyTxs' ::
     All Top xs
  => InPairs (InjectPolyTx tx) xs
  -> [NS tx xs]
  -> Telescope g f xs
  -> ( [(NS tx xs, Mismatch tx f xs)]
     , TelescopeWithTxList g f tx xs xs
     )
matchPolyTxs' ips txs = go ips [ hmap (TxWithOriginal x) x | x <- txs ]
  where
    tipFst :: All Top xs => NS (TxWithOriginal tx xs') xs -> NS tx xs'
    tipFst = hcollapse . hmap (K . origTx)

    go :: All Top xs
       => InPairs (InjectPolyTx tx) xs
       -> [NS (TxWithOriginal tx xs') xs]
       -> Telescope g f xs
       -> ( [(NS tx xs', Mismatch tx f xs)]
          , TelescopeWithTxList g f tx xs' xs
          )
    go _            txs' (TZ f)   =
      let (rejected, accepted) =
              partitionEithers
            $ map (\case
                      Z x -> Right x
                      -- The ones from later eras are invalid
                      S x -> Left (tipFst x, MR (hmap blkTx x) f)
                  ) txs'
      in (rejected, TZ (Pair (ListOfTxs accepted) f))

    go (PCons i is) txs' (TS g f) =
      let (rejected, translated) =
              partitionEithers
            $ map (\case
                      Z (TxWithOriginal origx x) ->
                        case injectTxWith i x of
                          -- The ones from this era that we cannot transport to
                          -- the next era are invalid
                          Nothing -> Left (origx, ML x (Telescope.tip f))
                          Just x' -> Right $ Z (TxWithOriginal origx x')
                      S x -> Right x
                  ) txs'
          (nextRejected, nextState) = go is translated f
      in (rejected ++ map (second MS) nextRejected, TS g nextState)

matchPolyTxs ::
     SListI xs
  => InPairs (InjectPolyTx tx) xs
  -> [NS tx xs]
  -> HardForkState f xs
  -> ( [(NS tx xs, Mismatch tx (Current f) xs)]
     , HardForkState (Product ([] :.: tx) f) xs
     )
matchPolyTxs is tx =
      fmap (HardForkState . hmap distrib)
    . matchPolyTxs' is tx
    . getHardForkState
  where
    distrib :: Product (ListOfTxs tx xs) (Current f) blk
            -> Current (Product ([] :.: tx) f) blk
    distrib (Pair (ListOfTxs txs) Current{..}) = Current {
          currentStart = currentStart
        , currentState = Pair (Comp [blkTx t | t <- txs])  currentState
        }

-- | Match transaction with an 'NS', attempting to inject where possible
matchPolyTxNS ::
     InPairs (InjectPolyTx tx) xs
  -> NS tx xs
  -> NS f xs
  -> Either (Mismatch tx f xs)
            (NS (Product tx f) xs)
matchPolyTxNS = go
  where
    go :: InPairs (InjectPolyTx tx) xs
       -> NS tx xs
       -> NS f xs
       -> Either (Mismatch tx f xs)
                 (NS (Product tx f) xs)
    go _            (Z x) (Z f) = Right $ Z (Pair x f)
    go (PCons _ is) (S x) (S f) = bimap MS S $ go is x f
    go _            (S x) (Z f) = Left $ MR x f
    go (PCons i is) (Z x) (S f) =
        case injectTxWith i x of
          Nothing -> Left $ ML x f
          Just x' -> bimap MS S $ go is (Z x') f

-- | Match a list of transactions with an 'NS', attempting to inject where
-- possible
matchPolyTxsNS ::
     forall tx f xs. SListI xs
  => InPairs (InjectPolyTx tx) xs
  -> NS f xs
  -> [NS tx xs]
  -> ([Mismatch tx f xs], NS (Product f ([] :.: tx)) xs)
matchPolyTxsNS is ns = go
  where
    go :: [NS tx xs]
       -> ([Mismatch tx f xs], NS (Product f ([] :.: tx)) xs)
    go []       = ([], hmap (`Pair` Comp []) ns)
    go (tx:txs) =
      let (mismatched, matched) = go txs
      in case matchPolyTxNS is tx matched of
           Left  err      -> (hmap pairFst err : mismatched, matched)
           Right matched' -> (mismatched, insert matched')

    insert :: NS (Product tx (Product f ([] :.: tx))) xs
           -> NS (Product f ([] :.: tx)) xs
    insert = hmap $ \(Pair tx (Pair f (Comp txs))) -> Pair f (Comp (tx:txs))

{-------------------------------------------------------------------------------
  Monomorphic aliases
-------------------------------------------------------------------------------}

type InjectTx = InjectPolyTx GenTx

-- | 'InjectPolyTx' at type 'InjectTx'
pattern InjectTx :: (GenTx blk -> Maybe (GenTx blk')) -> InjectTx blk blk'
pattern InjectTx f = InjectPolyTx f

-- | 'cannotInjectPolyTx' at type 'InjectTx'
cannotInjectTx :: InjectTx blk blk'
cannotInjectTx = cannotInjectPolyTx

-- | 'matchPolyTx' at type 'InjectTx'
matchTx ::
     SListI xs
  => InPairs InjectTx xs
  -> NS GenTx xs
  -> HardForkState f xs
  -> Either (Mismatch GenTx (Current f) xs)
            (HardForkState (Product GenTx f) xs)
matchTx = matchPolyTx

-----

type InjectValidatedTx = InjectPolyTx WrapValidatedGenTx

-- | 'InjectPolyTx' at type 'InjectValidatedTx'
pattern InjectValidatedTx ::
     (WrapValidatedGenTx blk -> Maybe (WrapValidatedGenTx blk'))
  -> InjectValidatedTx blk blk'
pattern InjectValidatedTx f = InjectPolyTx f

-- | 'cannotInjectPolyTx' at type 'InjectValidatedTx'
cannotInjectValidatedTx :: InjectValidatedTx blk blk'
cannotInjectValidatedTx = cannotInjectPolyTx

-- | 'matchPolyTx' at type 'InjectValidatedTx'
matchValidatedTx ::
     SListI xs
  => InPairs InjectValidatedTx xs
  -> NS WrapValidatedGenTx xs
  -> HardForkState f xs
  -> Either (Mismatch WrapValidatedGenTx (Current f) xs)
            (HardForkState (Product WrapValidatedGenTx f) xs)
matchValidatedTx = matchPolyTx

-- | 'matchPolyTxsNS' at type 'InjectValidatedTx'
matchValidatedTxsNS ::
     forall f xs. SListI xs
  => InPairs InjectValidatedTx xs
  -> NS f xs
  -> [NS WrapValidatedGenTx xs]
  -> ([Mismatch WrapValidatedGenTx f xs], NS (Product f ([] :.: WrapValidatedGenTx)) xs)
matchValidatedTxsNS = matchPolyTxsNS
