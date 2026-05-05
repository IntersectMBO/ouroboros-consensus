{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Ouroboros.Consensus.Peras.State.Mock (mkMockPerasVotingCommitteeInput) where

import Cardano.Ledger.State (IndividualPoolStake (individualPoolStake), PoolDistr (..))
import Cardano.Prelude (maybeToEither)
import Data.Bifunctor (Bifunctor (..))
import Data.List.NonEmpty (nonEmpty)
import qualified Data.Map as Map
import Ouroboros.Consensus.Block.SupportsPeras (BlockSupportsPeras (..))
import Ouroboros.Consensus.Committee.Class (CryptoSupportsVotingCommittee (..))
import Ouroboros.Consensus.Committee.Types (LedgerStake (..), PoolId (..))
import Ouroboros.Consensus.Ledger.SupportsPeras (ALedgerStateSupportsPeras (..))
import Ouroboros.Consensus.Peras.Crypto.Mock
  ( MockPerasCrypto
  , MockPerasVotingCommitteeScheme
  , VotingCommitteeInput (..)
  )
import Ouroboros.Consensus.Peras.Error.Mock (MockPerasError (..))

mkMockPerasVotingCommitteeInput ::
  forall blk ledger chainDep.
  ( PerasCrypto blk ~ MockPerasCrypto blk
  , ALedgerStateSupportsPeras ledger
  ) =>
  ledger ->
  chainDep ->
  Either
    (MockPerasError blk)
    (VotingCommitteeInput (PerasCrypto blk) (MockPerasVotingCommitteeScheme blk))
mkMockPerasVotingCommitteeInput ledgerState _headerState = do
  let PoolDistr{unPoolDistr} = getPoolDistr ledgerState
      stakeDistr = nonEmpty $ fmap (bimap PoolId (LedgerStake . individualPoolStake)) . Map.toList $ unPoolDistr
  MockPerasVotingCommitteeInput <$> maybeToEither InputStakeDistrIsEmpty stakeDistr
