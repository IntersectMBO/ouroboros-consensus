{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

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

-- | The 'BlockSupportsDiffusionPipelining' instance for the HFC is
-- compositional:
--
--  - @'TentativeHeaderState' ('HardForkBlock' xs)@ is
--    'OneEraTentativeHeaderState', the n-ary sum of per-era
--    'TentativeHeaderState's. The 'initialTentativeHeaderState' is the one for
--    the first era.
--
--  - @'updateTentativeHeaderState' bcfg hdr st@ works like this:
--
--      - If @hdr@ and @st@ are in the same era, we call the corresponding
--        era-specific 'updateTentativeHeaderState'.
--
--      - If @hdr@ is in a later era than @st@, we replace @st@ with the
--        'initialTentativeHeaderState' for the era of @hdr@, and then proceed
--        like in the previous step.
--
--      - If @hdr@ is in an earlier era than @st@, we return 'Nothing' to
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

  initialTentativeHeaderState _
    | ProofNonEmpty proxyHead _ <- isNonEmpty (Proxy @xs)
    = OneEraTentativeHeaderState $ Z $ WrapTentativeHeaderState
    $ initialTentativeHeaderState proxyHead

  updateTentativeHeaderState
    (HardForkBlockConfig (PerEraBlockConfig bcfgs))
    (HardForkHeader (OneEraHeader hdr))
    (OneEraTentativeHeaderState st) =
          fmap OneEraTentativeHeaderState
      .   sequence_NS'
      .   hcliftA2 proxySingle updateSt bcfgs
      =<< case Match.matchNS hdr st of
            Right hdrSt   -> Just hdrSt
            Left mismatch -> fromMismatch mismatch
    where
      updateSt ::
           BlockSupportsDiffusionPipelining blk
        => BlockConfig blk
        -> Product Header WrapTentativeHeaderState blk
        -> (Maybe :.: WrapTentativeHeaderState) blk
      updateSt bcfg (Pair hdr' (WrapTentativeHeaderState st')) =
            Comp $ fmap WrapTentativeHeaderState
          $ updateTentativeHeaderState bcfg hdr' st'

      -- If the mismatch indicates that the header is in a later era than the
      -- 'TentativeHeaderState', pair the header with the
      -- 'initialTentativeHeaderState' of its era.
      fromMismatch ::
           Match.Mismatch Header WrapTentativeHeaderState xs
        -> Maybe (NS (Product Header WrapTentativeHeaderState) xs)
      fromMismatch mismatch
        | ProofNonEmpty _ _ <- isNonEmpty (Proxy @xs)
        = case Match.mismatchNotFirst mismatch of
            -- The @hdr@ is in an earlier era compared to the @st@, so it does
            -- not satisfy the HFC pipelining criterion.
            Right _   -> Nothing
            -- The @hdr@ is in a later era compared to the @st@.
            Left hdr' -> Just $ hcmap proxySingle withInitialSt (S hdr')
        where
          withInitialSt ::
               forall blk. BlockSupportsDiffusionPipelining blk
            => Header blk -> Product Header WrapTentativeHeaderState blk
          withInitialSt h = Pair h (WrapTentativeHeaderState initialSt)
            where
              initialSt = initialTentativeHeaderState (Proxy @blk)
