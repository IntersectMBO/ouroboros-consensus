{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

-- | SOP-based utility functions for the HardFork combinator.
module Ouroboros.Consensus.HardFork.Combinator.Util
  ( -- * NS helpers
    ensureSameEraPair
  , ensureSameEraNonEmpty
  , ensureSameEraNonEmptyMap
  , alignNPWithNS
  , EitherF (..)
  , mkEitherF
  , hcollect
  )
where

import Data.Bifunctor (Bifunctor (..))
import Data.Functor.Product (Product (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.NonEmpty (NEMap)
import qualified Data.Map.NonEmpty as NEMap
import Data.SOP (All, K (..), Top, hmap, hzipWith, (:.:) (..))
import Data.SOP.Index (himap, injectNS)
import Data.SOP.Match (matchNS)
import Data.SOP.Strict (HCollapse (..), NP, NS)

-- * NS helpers

-- | Ensure that two 'NS' values are in the same era, pairing them together.
-- Returns 'Left ParamsEraMismatch' if they are from different eras.
--
-- NOTE: this is simply a wrapper around 'matchNS', but we provide it here
-- anyway for consistency with the other 'ensureSameEra*' functions.
ensureSameEraPair ::
  ( NS f xs
  , NS g xs
  ) ->
  Maybe (NS (Product f g) xs)
ensureSameEraPair (l, r) =
  case matchNS l r of
    Left _mismatch -> Nothing
    Right ns -> Just ns

-- | Ensure that all elements of a non-empty list of 'NS' values are in the same
-- era, collecting them into a single 'NS' containing a 'NonEmpty'.
ensureSameEraNonEmpty ::
  All Top xs =>
  NonEmpty (NS f xs) ->
  Maybe (NS (NonEmpty :.: f) xs)
ensureSameEraNonEmpty (x :| rest) =
  foldl go (Just (hmap (Comp . (:| [])) x)) rest
 where
  go Nothing _ =
    Nothing
  go (Just acc) ns =
    case matchNS acc ns of
      Left _mismatch ->
        Nothing
      Right nsPair ->
        Just
          $ hmap
            ( \(Pair (Comp fs) f) ->
                Comp $
                  fs <> (f :| [])
            )
          $ nsPair

-- | Ensure that all elements of a non-empty map of 'NS' values are in the same
-- era, collecting them into a single 'NS' containing a 'NEMap'.
ensureSameEraNonEmptyMap ::
  ( Ord k
  , All Top xs
  ) =>
  NEMap k (NS f xs) ->
  Maybe (NS (NEMap k :.: f) xs)
ensureSameEraNonEmptyMap neMap =
  case ensureSameEraNonEmpty keyValPairs of
    Nothing ->
      Nothing
    Just ns ->
      Just
        $ hmap
          ( \(Comp neKeyValPairs) ->
              Comp $
                NEMap.fromAscList $
                  fmap (\(Pair (K k) v) -> (k, v)) $
                    neKeyValPairs
          )
        $ ns
 where
  keyValPairs =
    fmap (\(k, v) -> hmap (\v' -> Pair (K k) v') v) $
      NEMap.toAscList neMap

-- | Align an 'NP' and an 'NS' of the same length, pairing them together.
alignNPWithNS ::
  All Top xs =>
  NP f xs ->
  NS g xs ->
  NS (Product f g) xs
alignNPWithNS =
  hzipWith Pair

-- | A wrapper for 'Either' that is a functor in its last argument.
newtype EitherF f g x = EitherF {unEitherF :: Either (f x) (g x)}

-- | Construct an 'EitherF' from two functions and an 'Either' value.
mkEitherF :: (a -> f x) -> (b -> g x) -> Either a b -> EitherF f g x
mkEitherF f g = EitherF . bimap f g

-- | Collect an 'NS' of 'EitherF' values into an 'Either' of 'NS' values.
hcollect ::
  All Top xs =>
  NS (EitherF f g) xs ->
  Either (NS f xs) (NS g xs)
hcollect ns =
  hcollapse (himap f ns)
 where
  f idx (EitherF (Left fx)) = K $ Left $ injectNS idx fx
  f idx (EitherF (Right gx)) = K $ Right $ injectNS idx gx
