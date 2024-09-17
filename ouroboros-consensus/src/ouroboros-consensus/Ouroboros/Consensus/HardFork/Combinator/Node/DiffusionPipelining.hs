{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.HardFork.Combinator.Node.DiffusionPipelining () where

import           Data.Functor.Product
import           Data.Proxy
import           Data.SOP.BasicFunctors
import qualified Data.SOP.Match as Match
import           Data.SOP.NonEmpty
import           Data.SOP.Strict
import           Ouroboros.Consensus.Block.SupportsDiffusionPipelining
import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras
import           Ouroboros.Consensus.HardFork.Combinator.Basics
import           Ouroboros.Consensus.HardFork.Combinator.Block
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Consensus.Util

-- | The 'BlockSupportsDiffusionPipelining' instance for the HFC is
-- compositional:
--
--  - @'TentativeHeaderState' ('HardForkBlock' xs)@ is
--    'OneEraTentativeHeaderState', the n-ary sum of per-era
--    'TentativeHeaderState's. The 'initialTentativeHeaderState' is the one for
--    the first era.
--
--  - Similarly, @'TentativeHeaderView' ('HardForkBlock' xs)@ is
--    'OneEraTentativeHeaderView', the n-ary sum of per-era
--    'TentativeHeaderView's, and 'tentativeHeaderView' delegates to the
--    era-specific 'tentativeHeaderView'.
--
--  - @'applyTentativeHeaderView' 'Proxy' thv st@ works like this:
--
--      - If @thv@ and @st@ are in the same era, we call the corresponding
--        era-specific 'applyTentativeHeaderView'.
--
--      - If @thv@ is in a later era than @st@, we replace @st@ with the
--        'initialTentativeHeaderState' for the era of @thv@, and then proceed
--        like in the previous step.
--
--      - If @thv@ is in an earlier era than @st@, we return 'Nothing' to
--        disallow pipelining.
--
-- This behavior guarantees the "Consistent validity under subsequences"
-- requirement if it is satisfied for every era.
--
-- Note that at an era boundary, the tip of the selection might switch multiple
-- times between two adjacent eras. Compared to the scenario where the
-- pipelining criteria in both eras are compatible and make sense even across
-- eras, this might lead to unnecessarily strict/relaxed diffusion pipelining.
-- However, the tip switching between different eras is rare and rather short,
-- so there is no direct need to address this, so we rather avoid the extra
-- complexity for now.
--
-- Still, a possible future refinement would be to allow custom logic for
-- "upgrading" the 'TentativeHeaderState' to a new era.
instance CanHardFork xs => BlockSupportsDiffusionPipelining (HardForkBlock xs) where
  type TentativeHeaderState (HardForkBlock xs) = OneEraTentativeHeaderState xs

  type TentativeHeaderView (HardForkBlock xs) = OneEraTentativeHeaderView xs

  initialTentativeHeaderState _
    | ProofNonEmpty proxyHead _ <- isNonEmpty (Proxy @xs)
    = OneEraTentativeHeaderState $ Z $ WrapTentativeHeaderState
    $ initialTentativeHeaderState proxyHead

  tentativeHeaderView
    (HardForkBlockConfig (PerEraBlockConfig bcfg))
    (HardForkHeader (OneEraHeader hdr)) =
        OneEraTentativeHeaderView
      . hcliftA2 proxySingle (WrapTentativeHeaderView .: tentativeHeaderView) bcfg
      $ hdr

  applyTentativeHeaderView
    Proxy
    (OneEraTentativeHeaderView thv)
    (OneEraTentativeHeaderState st) =
          fmap OneEraTentativeHeaderState
      .   sequence_NS'
      .   hcmap proxySingle updateSt
      =<< case Match.matchNS thv st of
            Right thvSt   -> Just thvSt
            Left mismatch -> fromMismatch mismatch
    where
      updateSt ::
           forall blk. BlockSupportsDiffusionPipelining blk
        => Product WrapTentativeHeaderView WrapTentativeHeaderState blk
        -> (Maybe :.: WrapTentativeHeaderState) blk
      updateSt (Pair (WrapTentativeHeaderView thv') (WrapTentativeHeaderState st')) =
            Comp $ fmap WrapTentativeHeaderState
          $ applyTentativeHeaderView (Proxy @blk) thv' st'

      -- If the mismatch indicates that the tentative header view is in a later
      -- era than the 'TentativeHeaderState', pair the view with the
      -- 'initialTentativeHeaderState' of its era.
      fromMismatch ::
           Match.Mismatch WrapTentativeHeaderView WrapTentativeHeaderState xs
        -> Maybe (NS (Product WrapTentativeHeaderView WrapTentativeHeaderState) xs)
      fromMismatch mismatch
        | ProofNonEmpty _ _ <- isNonEmpty (Proxy @xs)
        = case Match.mismatchNotFirst mismatch of
            -- The @thv@ is in an earlier era compared to the @st@, so it does
            -- not satisfy the HFC pipelining criterion.
            Right _   -> Nothing
            -- The @thv@ is in a later era compared to the @st@.
            Left thv' -> Just $ hcmap proxySingle withInitialSt (S thv')
        where
          withInitialSt ::
               forall blk. BlockSupportsDiffusionPipelining blk
            => WrapTentativeHeaderView blk
            -> Product WrapTentativeHeaderView WrapTentativeHeaderState blk
          withInitialSt v = Pair v (WrapTentativeHeaderState initialSt)
            where
              initialSt = initialTentativeHeaderState (Proxy @blk)
