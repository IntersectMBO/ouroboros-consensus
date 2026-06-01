{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Voting interface for Peras derived from the Praos ledger view.
module Ouroboros.Consensus.Protocol.Praos.Peras
  ( PraosStateSupportsPerasVoting (..)
  , praosStatePerasVotingCommitteeInputV1
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
import Ouroboros.Consensus.HardFork.Combinator.Basics (HardForkBlock)
import qualified Ouroboros.Consensus.Peras.Crypto.BLS as BLS
import Ouroboros.Consensus.Peras.Crypto.BLS.Unsafe
  ( unsafeExtendPerasStakeDistrWithPublicKeysFromEnv
  )
import qualified Ouroboros.Consensus.Peras.Error.V1 as V1
import Ouroboros.Consensus.Peras.Params (PerasParams (..))
import Ouroboros.Consensus.Protocol.Praos
  ( PraosState (..)
  , Ticked (..)
  )
import Ouroboros.Consensus.Protocol.Praos.Views (LedgerView (..))

class
  ( BlockSupportsPeras blk
  , CryptoSupportsVotingCommittee (PerasCrypto blk) (PerasVotingCommitteeScheme blk) -- TODO remove this constraint when it becomes a superclass constraint of 'BlockSupportsPeras'
  ) =>
  PraosStateSupportsPerasVoting blk
  where
  -- | How to extract a 'PerasVotingCommitteeInput' from a 'Ticked PraosState'.
  -- This is used to construct the 'PerasVotingCommittee' used for voting at a given ledger/praos state.
  praosStatePerasVotingCommitteeInput ::
    proxy blk ->
    PerasParams blk ->
    Ticked PraosState ->
    Either
      (PerasError blk)
      (PerasVotingCommitteeInput blk)

  -- | How to build a new 'PerasVotingCommittee' from a 'Ticked PraosState'. The implementation provided here relies on 'praosStatePerasVotingCommitteeInput'.
  praosStateGetPerasVotingCommittee ::
    proxy blk ->
    PerasParams blk ->
    Ticked PraosState ->
    Either
      (PerasError blk)
      (PerasVotingCommittee blk)
  praosStateGetPerasVotingCommittee p perasParams tickedPraosState = do
    committeeInput <-
      praosStatePerasVotingCommitteeInput p perasParams tickedPraosState
    bimap injectVotingCommitteeError id $
      Committee.mkVotingCommittee committeeInput

praosStatePerasVotingCommitteeInputV1 ::
  PublicKey crypto ~ BLS.PerasPublicKey =>
  proxy blk ->
  PerasParams blk ->
  Ticked PraosState ->
  Either (V1.PerasError blk) (VotingCommitteeInput crypto WFALS)
praosStatePerasVotingCommitteeInputV1 _ perasParams tickedPraosState = do
  let epochNonce =
        praosStateEpochNonce
          . tickedPraosStateChainDepState
          $ tickedPraosState
  -- TODO: replace the following hack with proper on-chain key registration.
  stakeDistrWithPublicKeys <-
    bimap V1.PerasTemporaryPublicKeyHackError id
      . unsafeExtendPerasStakeDistrWithPublicKeysFromEnv
      . lvPoolDistr
      . tickedPraosStateLedgerView
      $ tickedPraosState
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
  PraosStateSupportsPerasVoting (HardForkBlock xs)
  where
  praosStatePerasVotingCommitteeInput = praosStatePerasVotingCommitteeInputV1
