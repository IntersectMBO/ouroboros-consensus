{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.Peras.State.Mock (ledgerStateHeaderStateMkMockPerasVotingCommitteeInput) where

import Cardano.Ledger.State (IndividualPoolStake (individualPoolStake), PoolDistr (..))
import Cardano.Prelude (maybeToEither)
import Data.Bifunctor (Bifunctor (..))
import Data.List.NonEmpty (nonEmpty)
import qualified Data.Map as Map
import Ouroboros.Consensus.Block.SupportsPeras (PerasParams (..))
import Ouroboros.Consensus.Committee.Class (CryptoSupportsVotingCommittee (..))
import Ouroboros.Consensus.Committee.Types (LedgerStake (..), PoolId (..))
import Ouroboros.Consensus.HardFork.Combinator.Basics (LedgerState)
import Ouroboros.Consensus.HeaderValidation (HeaderState (..))
import Ouroboros.Consensus.Ledger.SupportsPeras (LedgerSupportsPeras (..))
import Ouroboros.Consensus.Peras.Crypto.Mock (MockPerasCommittee, VotingCommitteeInput (..))
import Ouroboros.Consensus.Peras.Error.Mock (MockPerasError (..))

-- | NOTE: this function will return an error on an empty stake distr, which is what the default instance of 'LedgerSupportsPeras' returns.
ledgerStateHeaderStateMkMockPerasVotingCommitteeInput ::
  forall blk mk crypto.
  LedgerSupportsPeras blk =>
  PerasParams blk ->
  LedgerState blk mk ->
  HeaderState blk ->
  Either (MockPerasError blk) (VotingCommitteeInput crypto (MockPerasCommittee blk))
ledgerStateHeaderStateMkMockPerasVotingCommitteeInput _perasParams ledgerState _headerState = do
  let PoolDistr{unPoolDistr} = getPoolDistr ledgerState
      stakeDistr = nonEmpty $ fmap (bimap PoolId (LedgerStake . individualPoolStake)) . Map.toList $ unPoolDistr
  MockPerasVotingCommitteeInput <$> maybeToEither InputStakeDistrIsEmpty stakeDistr
