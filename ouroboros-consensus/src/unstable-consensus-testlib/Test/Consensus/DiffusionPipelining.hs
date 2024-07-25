{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Test.Consensus.DiffusionPipelining (prop_diffusionPipeliningSubsequenceConsistency) where

import           Control.Exception (assert)
import           Data.Either (isRight)
import           Data.Proxy
import           Ouroboros.Consensus.Block
import           Test.QuickCheck

-- | See /Consistent validity under subsequences/ in
-- 'BlockSupportsDiffusionPipelining'.
prop_diffusionPipeliningSubsequenceConsistency ::
     forall blk.
     BlockSupportsDiffusionPipelining blk
  => Proxy blk
  -> [TentativeHeaderView blk]
     -- ^ Have to satisfy the pipelining criterion.
  -> Property
prop_diffusionPipeliningSubsequenceConsistency _ thvs =
    assert (isRight $ satisfyPipeliningCriterion thvs) $
    forAllShrink (sublistOf thvs) (shrinkList (const [])) $ \thvs' ->
      case satisfyPipeliningCriterion thvs' of
        Right () -> property ()
        Left (hdrs'', st) ->
            counterexample ("tentative header view subsequence: " <> show hdrs'')
          $ counterexample ("last state: " <> show st)
          $ counterexample "unexpected violation of pipelining criterion!"
            ()
  where
    satisfyPipeliningCriterion ::
         [TentativeHeaderView blk]
      -> Either ([TentativeHeaderView blk], TentativeHeaderState blk) ()
    satisfyPipeliningCriterion allThvs =
        go 1 (initialTentativeHeaderState (Proxy @blk)) allThvs
      where
        go ix st = \case
          [] -> Right ()
          thv : thvs' -> case applyTentativeHeaderView (Proxy @blk) thv st of
            Just st' -> go (ix + 1) st' thvs'
            Nothing  -> Left (take ix allThvs, st)
