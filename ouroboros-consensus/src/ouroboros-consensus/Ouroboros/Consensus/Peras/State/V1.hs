{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Ouroboros.Consensus.Peras.State.V1 (ledgerStateHeaderStateMkPerasVotingCommitteeInput) where

import Data.Bifunctor (Bifunctor (..))
import Data.Proxy (Proxy (..))
import Ouroboros.Consensus.Block.Abstract (BlockProtocol)
import Ouroboros.Consensus.Block.SupportsPeras (PerasParams (..))
import Ouroboros.Consensus.Committee.Class (CryptoSupportsVotingCommittee (..))
import Ouroboros.Consensus.Committee.Crypto (PublicKey)
import Ouroboros.Consensus.Committee.WFA (mkExtWFAStakeDistr, wFATiebreakerWithEpochNonce)
import Ouroboros.Consensus.Committee.WFALS (VotingCommitteeInput (..), WFALS)
import Ouroboros.Consensus.HardFork.Combinator.Basics (LedgerState)
import Ouroboros.Consensus.HeaderValidation (HeaderState (..))
import Ouroboros.Consensus.Ledger.SupportsPeras (LedgerSupportsPeras (..))
import qualified Ouroboros.Consensus.Peras.Crypto.BLS as BLS
import Ouroboros.Consensus.Peras.Crypto.BLS.Unsafe
  ( unsafeExtendPerasStakeDistrWithPublicKeysFromEnv
  )
import qualified Ouroboros.Consensus.Peras.Error.V1 as V1
import Ouroboros.Consensus.Protocol.Abstract (ChainDepStateSupportsPeras (..))

ledgerStateHeaderStateMkPerasVotingCommitteeInput ::
  forall blk mk crypto.
  ( PublicKey crypto ~ BLS.PerasPublicKey
  , LedgerSupportsPeras blk
  , ChainDepStateSupportsPeras (BlockProtocol blk)
  ) =>
  PerasParams blk ->
  LedgerState blk mk ->
  HeaderState blk ->
  Either (V1.PerasError blk) (VotingCommitteeInput crypto WFALS)
ledgerStateHeaderStateMkPerasVotingCommitteeInput perasParams ledgerState headerState = do
  let epochNonce = getEpochNonce (Proxy @(BlockProtocol blk)) . headerStateChainDep $ headerState
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
      (perasTargetCommitteeSize perasParams)
      extWFAStakeDistr
