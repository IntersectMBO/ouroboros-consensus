{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Voting interface for Peras derived from the Praos ledger view.
--
-- [TODO EPOCH CONTEXT PLUMBING]
module Ouroboros.Consensus.Protocol.Praos.Peras
  ( LedgerStateHeaderStateSupportsPerasVoting (..)
  , ledgerStateHeaderStateMkPerasVotingCommitteeInputV1
  ) where

import Data.Bifunctor (Bifunctor (..))
import Ouroboros.Consensus.Block.Abstract (StandardHash)
import Ouroboros.Consensus.Block.SupportsPeras
  ( BlockSupportsPeras (..)
  , IsPerasError (..)
  , PerasCrypto
  , PerasParams
  , PerasVotingCommittee
  , PerasVotingCommitteeInput
  , PerasVotingCommitteeScheme
  )
import Ouroboros.Consensus.Committee.Class (CryptoSupportsVotingCommittee)
import qualified Ouroboros.Consensus.Committee.Class as Committee
import Ouroboros.Consensus.Committee.Crypto (PublicKey)
import Ouroboros.Consensus.Committee.WFA
  ( mkExtWFAStakeDistr
  , wFATiebreakerWithEpochNonce
  )
import Ouroboros.Consensus.Committee.WFALS
  ( VotingCommitteeInput (..)
  , WFALS
  )
import Ouroboros.Consensus.HardFork.Combinator.Abstract (CanHardFork)
import Ouroboros.Consensus.HardFork.Combinator.Basics (HardForkBlock, LedgerState)
import qualified Ouroboros.Consensus.Peras.Crypto.BLS as BLS
import Ouroboros.Consensus.Peras.Crypto.BLS.Unsafe
  ( unsafeExtendPerasStakeDistrWithPublicKeysFromEnv
  )
import qualified Ouroboros.Consensus.Peras.Error.V1 as V1
import Ouroboros.Consensus.Peras.Params (PerasParams (..))
import Ouroboros.Consensus.Protocol.Praos
  ( PraosState (..)
  , Ticked (..), Praos
  )
import Ouroboros.Consensus.Protocol.Praos.Views (LedgerView (..))
import Ouroboros.Consensus.HeaderValidation (HeaderState, headerStateChainDep)
import Ouroboros.Consensus.Block.Abstract (BlockProtocol)
import Cardano.Ledger.State (PoolDistr)
import Cardano.Ledger.BaseTypes (Nonce)
import Ouroboros.Consensus.Protocol.Abstract (ChainDepState)
import Ouroboros.Consensus.HardFork.Combinator.Protocol (HardForkChainDepState)

class
  ( BlockSupportsPeras blk
  , CryptoSupportsVotingCommittee (PerasCrypto blk) (PerasVotingCommitteeScheme blk) -- TODO remove this constraint when it becomes a superclass constraint of 'BlockSupportsPeras'
  ) =>
  LedgerStateHeaderStateSupportsPerasVoting blk
  where
  ledgerStateHeaderStateMkPerasVotingCommitteeInput ::
    PerasParams blk ->
    LedgerState blk mk ->
    HeaderState blk ->
    Either
      (PerasError blk)
      (PerasVotingCommitteeInput blk)

  ledgerStateHeaderStateMkPerasVotingCommittee ::
    PerasParams blk ->
    LedgerState blk mk ->
    HeaderState blk ->
    Either
      (PerasError blk)
      (PerasVotingCommittee blk)
  ledgerStateHeaderStateMkPerasVotingCommittee perasParams ledgerState headerState = do
    committeeInput <-
      ledgerStateHeaderStateMkPerasVotingCommitteeInput perasParams ledgerState headerState
    bimap injectVotingCommitteeError id $
      Committee.mkVotingCommittee committeeInput

ledgerStateHeaderStateMkPerasVotingCommitteeInputV1 ::
  (PublicKey crypto ~ BLS.PerasPublicKey) =>
  (LedgerState blk mk -> PoolDistr) ->
  (ChainDepState (BlockProtocol blk) -> Nonce) ->
  PerasParams blk ->
  LedgerState blk mk ->
  HeaderState blk ->
  Either (V1.PerasError blk) (VotingCommitteeInput crypto WFALS)
ledgerStateHeaderStateMkPerasVotingCommitteeInputV1 extractPoolDistr extractEpochNonce perasParams ledgerState headerState = do
  let epochNonce = extractEpochNonce . headerStateChainDep $ headerState
      poolDistr = extractPoolDistr ledgerState
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

instance
  ( StandardHash (HardForkBlock xs)
  , CanHardFork xs
  ) =>
  LedgerStateHeaderStateSupportsPerasVoting (HardForkBlock xs)
  where
  ledgerStateHeaderStateMkPerasVotingCommitteeInput = ledgerStateHeaderStateMkPerasVotingCommitteeInputV1 extractPoolDistr extractEpochNonce where
    extractPoolDistr :: LedgerState (HardForkBlock xs) mk -> PoolDistr
    extractPoolDistr = undefined

    extractEpochNonce :: HardForkChainDepState xs -> Nonce
    extractEpochNonce = undefined
