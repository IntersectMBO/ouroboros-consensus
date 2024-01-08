{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE MultiWayIf           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Genesis.Governor (
    updateLoEFragStall
  , updateLoEFragUnconditional
  ) where

import           Control.Monad.Except ()
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Ouroboros.Consensus.Block.Abstract (GetHeader, Header)
import           Ouroboros.Consensus.Config.SecurityParam
                     (SecurityParam (SecurityParam))
import           Ouroboros.Consensus.Storage.ChainDB.API
                     (UpdateLoEFrag (UpdateLoEFrag))
import           Ouroboros.Consensus.Util.AnchoredFragment (stripCommonPrefix)
import           Ouroboros.Consensus.Util.MonadSTM.NormalForm
                     (MonadSTM (STM, atomically))
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF

-- | A dummy version of the LoE that sets the LoE fragment to the current
-- selection.
updateLoEFragUnconditional ::
  MonadSTM m =>
  UpdateLoEFrag m blk
updateLoEFragUnconditional =
  UpdateLoEFrag $ \ curChain _ setLoEFrag -> atomically (setLoEFrag curChain)

-- | Compute the fragment between the immutable tip, as given by the anchor
-- of @curChain@, and the earliest intersection of the @candidates@.
-- This excludes the selection from the set of intersected fragments since we
-- need to be able to select k+1 blocks on a new chain when a fork's peer is
-- killed on which we had selected k blocks, where the selection would
-- otherwise keep the LoE fragment at the killed peer's intersection.
sharedCandidatePrefix ::
  GetHeader blk =>
  SecurityParam ->
  AnchoredFragment (Header blk) ->
  Map peer (AnchoredFragment (Header blk)) ->
  AnchoredFragment (Header blk)
sharedCandidatePrefix (SecurityParam k) curChain candidates =
  trunc
  where
    trunc | excess > 0 = snd (AF.splitAt excess shared)
          | otherwise = shared

    excess = AF.length shared - fromIntegral k

    shared = fst (stripCommonPrefix (AF.anchor curChain) immutableTipSuffixes)

    immutableTip = AF.anchorPoint curChain

    splitAfterImmutableTip frag =
      snd <$> AF.splitAfterPoint frag immutableTip

    immutableTipSuffixes =
      -- If a ChainSync client's candidate forks off before the
      -- immutable tip, then this transaction is currently winning an
      -- innocuous race versus the thread that will fatally raise
      -- 'InvalidIntersection' within that ChainSync client, so it's
      -- sound to pre-emptively discard their candidate from this
      -- 'Map' via 'mapMaybe'.
      Map.mapMaybe splitAfterImmutableTip candidates

-- | This version of the LoE implements part of the intended Genesis approach.
-- The fragment is set to the prefix of all candidates, ranging from the
-- immutable tip to the earliest intersection of all peers.
--
-- Using this will cause ChainSel to stall indefinitely, or until a peer
-- disconnects for unrelated reasons.
-- In the future, the Genesis Density Disconnect Governor variant will extend
-- this with an analysis that will always result in disconnections from peers
-- to ensure the selection can advance.
updateLoEFragStall ::
  MonadSTM m =>
  GetHeader blk =>
  SecurityParam ->
  STM m (Map peer (AnchoredFragment (Header blk))) ->
  UpdateLoEFrag m blk
updateLoEFragStall k getCandidates =
  UpdateLoEFrag $ \ curChain _ setLoEFrag ->
    atomically $ do
      candidates <- getCandidates
      setLoEFrag (sharedCandidatePrefix k curChain candidates)
