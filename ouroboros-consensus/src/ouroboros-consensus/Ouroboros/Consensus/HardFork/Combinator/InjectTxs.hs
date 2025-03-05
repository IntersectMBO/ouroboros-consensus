{-# LANGUAGE FlexibleContexts #-}
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
  , matchPolyTxsTele
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
import           Data.Functor.Product
import           Data.SOP.BasicFunctors
import           Data.SOP.InPairs (InPairs (..))
import           Data.SOP.Match
import           Data.SOP.Sing
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

-- | Match a list of transactions with an 'Telescope', attempting to inject
-- where possible
matchPolyTxsTele ::
     forall tx g f xs. SListI xs
  => InPairs (InjectPolyTx tx) xs
  -> Telescope g f xs
  -> [NS tx xs]
  -> ( [(NS tx xs, Mismatch tx f xs)]
     , Telescope g (Product f ([] :.: tx)) xs
     )
matchPolyTxsTele is ns = go
  where
    go :: [NS tx xs]
       -> ([(NS tx xs, Mismatch tx f xs)], Telescope g (Product f ([] :.: tx)) xs)
    go []       = ([], hmap (`Pair` Comp []) ns)
    go (tx:txs) =
      let (mismatched, matched) = go txs
      in case matchPolyTx' is tx matched of
           Left  err      -> ((tx, hmap pairFst err) : mismatched, matched)
           Right matched' -> (mismatched, insert matched')

    insert :: Telescope g (Product tx (Product f ([] :.: tx))) xs
           -> Telescope g (Product f ([] :.: tx)) xs
    insert = hmap (\(Pair tx (Pair f (Comp txs))) -> Pair f (Comp (tx:txs)))

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
matchValidatedTxsNS ips ns txs = bimap (map snd) Telescope.tip $ matchPolyTxsTele ips (Telescope.fromTip ns) txs
