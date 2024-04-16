{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Ouroboros.Consensus.Protocol.MockChainSel (
    selectChain
  , selectUnvalidatedChain
  ) where

import           Data.Function (on)
import           Data.List (sortBy)
import           Data.Maybe (listToMaybe, mapMaybe)
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Network.Mock.Chain (Chain)
import qualified Ouroboros.Network.Mock.Chain as Chain

{-------------------------------------------------------------------------------
  Chain selection
-------------------------------------------------------------------------------}

-- | Chain selection between our chain and list of candidates
--
-- This is only a /model/ of chain selection: in reality of course we will not
-- work with entire chains in memory. This function is intended as an
-- explanation of how chain selection should work conceptually.
--
-- The @l@ parameter here models the ledger state for each chain, and serves as
-- evidence that the chains we are selecting between have been validated. (It
-- would /not/ be  correct to run chain selection on unvalidated chains and then
-- somehow fail if the selected chain turns out to be invalid.)
--
-- Returns 'Nothing' if we stick with our current chain.
selectChain :: forall proxy p hdr l. ConsensusProtocol p
            => proxy p
            -> ChainOrderConfig (SelectView p)
            -> (hdr -> SelectView p)
            -> Chain hdr           -- ^ Our chain
            -> [(Chain hdr, l)]    -- ^ Upstream chains
            -> Maybe (Chain hdr, l)
selectChain p cfg view ours =
      listToMaybe
    . map snd
    . sortBy (flip (compareChains cfg) `on` fst)
    . mapMaybe selectPreferredCandidate
  where
    -- | Only retain a candidate if it is preferred over the current chain. As
    -- only a non-empty chain can be preferred over the current chain, we can
    -- extract the 'SelectView' of the tip of the candidate.
    selectPreferredCandidate ::
         (Chain hdr, l)
      -> Maybe (SelectView p, (Chain hdr, l))
    selectPreferredCandidate x@(cand, _) =
        case (Chain.head ours, Chain.head cand) of
          (Nothing, Just candTip)
            -> Just (view candTip, x)
          (Just ourTip, Just candTip)
            | let candView = view candTip
            , preferCandidate p cfg (view ourTip) candView
            -> Just (candView, x)
          _otherwise
            -> Nothing

-- | Chain selection on unvalidated chains
selectUnvalidatedChain :: ConsensusProtocol p
                       => proxy p
                       -> ChainOrderConfig (SelectView p)
                       -> (hdr -> SelectView p)
                       -> Chain hdr
                       -> [Chain hdr]
                       -> Maybe (Chain hdr)
selectUnvalidatedChain p cfg view ours =
      fmap fst
    . selectChain p cfg view ours
    . map (, ())
