{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Ouroboros.Consensus.Peras.Voting.Mock
  ( mkMockPerasVotingCommitteeInput
  ) where

import Cardano.Ledger.State (IndividualPoolStake (..), PoolDistr (..))
import Cardano.Prelude (maybeToEither)
import Data.Bifunctor (Bifunctor (..))
import Data.List.NonEmpty (nonEmpty)
import qualified Data.Map as Map
import Ouroboros.Consensus.Block.SupportsPeras (BlockSupportsPeras (..))
import Ouroboros.Consensus.Committee.Class (CryptoSupportsVotingCommittee (..))
import Ouroboros.Consensus.Committee.Types (LedgerStake (..), PoolId (..))
import Ouroboros.Consensus.Ledger.Abstract (EmptyMK)
import Ouroboros.Consensus.Ledger.SupportsPeras (LedgerStateSupportsPeras (..))
import Ouroboros.Consensus.Peras.Crypto.Mock
  ( MockPerasCrypto
  , MockPerasVotingCommitteeScheme
  , VotingCommitteeInput (..)
  )
import Ouroboros.Consensus.Peras.Error.Mock (MockPerasError (..))

mkMockPerasVotingCommitteeInput ::
  forall blk ledgerState chainDepState.
  ( PerasCrypto blk ~ MockPerasCrypto blk
  , LedgerStateSupportsPeras ledgerState
  ) =>
  ledgerState EmptyMK ->
  chainDepState ->
  Either
    (MockPerasError blk)
    (VotingCommitteeInput (PerasCrypto blk) (MockPerasVotingCommitteeScheme blk))
mkMockPerasVotingCommitteeInput ledgerState _chainDepState = do
  MockPerasVotingCommitteeInput
    <$> maybeToEither InputStakeDistrIsEmpty stakeDistr
 where
  stakeDistr =
    nonEmpty
      . fmap (bimap PoolId (LedgerStake . individualPoolStake))
      . Map.toList
      . unPoolDistr
      . getPoolDistr
      $ ledgerState
