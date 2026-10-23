{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Ouroboros.Consensus.Peras.Voting.Mock
  ( mkMockPerasVotingCommitteeInput
  ) where

import Cardano.Ledger.BaseTypes (Nonce)
import Cardano.Ledger.State (IndividualPoolStake (..), PoolDistr (..))
import Cardano.Prelude (maybeToEither)
import Data.Bifunctor (Bifunctor (..))
import Data.List.NonEmpty (nonEmpty)
import qualified Data.Map as Map
import Ouroboros.Consensus.Block.SupportsPeras (BlockSupportsPeras (..))
import Ouroboros.Consensus.Committee.Class (CryptoSupportsVotingCommittee (..))
import Ouroboros.Consensus.Committee.Types
  ( LedgerStake (..)
  , PoolId (..)
  , TargetCommitteeSize
  )
import Ouroboros.Consensus.Peras.Crypto.Mock
  ( MockPerasCrypto
  , MockPerasVotingCommitteeScheme
  , VotingCommitteeInput (..)
  )
import Ouroboros.Consensus.Peras.Error.Mock (MockPerasError (..))

mkMockPerasVotingCommitteeInput ::
  forall blk.
  PerasCrypto blk ~ MockPerasCrypto blk =>
  PoolDistr ->
  Nonce ->
  TargetCommitteeSize ->
  Either
    (MockPerasError blk)
    (VotingCommitteeInput (PerasCrypto blk) (MockPerasVotingCommitteeScheme blk))
mkMockPerasVotingCommitteeInput poolDistr _epochNonce _targetCommitteeSize = do
  MockPerasVotingCommitteeInput
    <$> maybeToEither InputStakeDistrIsEmpty stakeDistr
 where
  stakeDistr =
    nonEmpty
      . fmap (bimap PoolId (LedgerStake . individualPoolStake))
      . Map.toList
      . unPoolDistr
      $ poolDistr
