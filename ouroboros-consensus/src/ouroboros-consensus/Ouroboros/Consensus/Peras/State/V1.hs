{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Ouroboros.Consensus.Peras.State.V1 (mkPerasVotingCommitteeInput) where

import Data.Bifunctor (Bifunctor (..))
import Data.Data (Proxy (..))
import Ouroboros.Consensus.Block.SupportsPeras (PerasCrypto, PerasParams (..))
import Ouroboros.Consensus.Committee.Class (CryptoSupportsVotingCommittee (..))
import Ouroboros.Consensus.Committee.WFA
  ( mkExtWFAStakeDistr
  , wFATiebreakerWithEpochNonce
  )
import Ouroboros.Consensus.Committee.WFALS (VotingCommitteeInput (..), WFALS)
import Ouroboros.Consensus.Ledger.SupportsPeras
  ( LedgerStateSupportsPeras (..)
  )
import Ouroboros.Consensus.Peras.Crypto.BLS (PerasBLSCrypto)
import Ouroboros.Consensus.Peras.Crypto.BLS.Unsafe
  ( unsafeExtendPerasStakeDistrWithPublicKeysFromEnv
  )
import qualified Ouroboros.Consensus.Peras.Error.V1 as V1
import Ouroboros.Consensus.Protocol.Abstract
  ( ChainDepStateSupportsPeras (..)
  )

mkPerasVotingCommitteeInput ::
  forall blk ledgerState chainDepState.
  ( PerasCrypto blk ~ PerasBLSCrypto
  , LedgerStateSupportsPeras ledgerState
  , ChainDepStateSupportsPeras chainDepState
  ) =>
  ledgerState ->
  chainDepState ->
  Either (V1.PerasError blk) (VotingCommitteeInput (PerasCrypto blk) WFALS)
mkPerasVotingCommitteeInput ledgerState headerState = do
  let epochNonce = getEpochNonce headerState
      poolDistr = getPoolDistr ledgerState
  -- TODO: replace the following hack with proper on-chain key registration.
  stakeDistrWithPublicKeys <-
    bimap V1.PerasTemporaryPublicKeyHackError id $
      unsafeExtendPerasStakeDistrWithPublicKeysFromEnv poolDistr
  extWFAStakeDistr <-
    bimap V1.PerasVotingWFAError id $
      mkExtWFAStakeDistr
        (wFATiebreakerWithEpochNonce epochNonce)
        stakeDistrWithPublicKeys
  pure $
    WFALSVotingCommitteeInput
      epochNonce
      (perasTargetCommitteeSize (getPerasParams (Proxy @blk) ledgerState))
      extWFAStakeDistr
