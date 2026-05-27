{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Voting interface for Peras derived from the Praos ledger view.
module Ouroboros.Consensus.Protocol.Praos.Peras
  ( PraosStateSupportsPerasVoting (..)
  ) where

import Data.Bifunctor (Bifunctor (..))
import Data.ByteString.Short (ShortByteString)
import Ouroboros.Consensus.Block.Abstract (HeaderHash, StandardHash)
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
import Ouroboros.Consensus.Committee.WFA
  ( mkExtWFAStakeDistr
  , wFATiebreakerWithEpochNonce
  )
import Ouroboros.Consensus.Committee.WFALS
  ( VotingCommitteeInput (..)
  , WFALS
  )
import qualified Ouroboros.Consensus.Peras.Cert.V1 as V1
import qualified Ouroboros.Consensus.Peras.Crypto.BLS as BLS
import Ouroboros.Consensus.Peras.Crypto.BLS.Unsafe
  ( unsafeExtendPerasStakeDistrWithPublicKeysFromEnv
  )
import qualified Ouroboros.Consensus.Peras.Error.V1 as V1
import Ouroboros.Consensus.Peras.Params (PerasParams (..))
import qualified Ouroboros.Consensus.Peras.Vote.V1 as V1
import Ouroboros.Consensus.Protocol.Praos
  ( PraosState (..)
  , Ticked (..)
  )
import Ouroboros.Consensus.Protocol.Praos.Views (LedgerView (..))

--------------------------------------------------------------------------------
-- This is a mocked up instance

data RealBlock

type instance PerasCrypto RealBlock = BLS.PerasBLSCrypto
type instance PerasVotingCommitteeScheme RealBlock = WFALS
type instance HeaderHash RealBlock = ShortByteString

instance StandardHash RealBlock

instance BlockSupportsPeras RealBlock where
  type PerasVote RealBlock = V1.PerasVote RealBlock
  type PerasCert RealBlock = V1.PerasCert RealBlock
  type PerasError RealBlock = V1.PerasError RealBlock

  -- TODO: uncomment as soon as we add this method to 'BlockSupportsPeras'
  -- forgePerasVoteIfEligible = implForgePerasVoteIfEligible
  forgePerasCert = undefined
  verifyPerasVote = undefined
  verifyPerasCert = undefined

instance PraosStateSupportsPerasVoting RealBlock where
  praosStatePerasVotingCommitteeInput _ perasParams tickedPraosState = do
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

--------------------------------------------------------------------------------

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
    PerasParams ->
    Ticked PraosState ->
    Either
      (PerasError blk)
      (PerasVotingCommitteeInput blk)

  -- | How to build a new 'PerasVotingCommittee' from a 'Ticked PraosState'. The implementation provided here relies on 'praosStatePerasVotingCommitteeInput'.
  praosStateGetPerasVotingCommittee ::
    proxy blk ->
    PerasParams ->
    Ticked PraosState ->
    Either
      (PerasError blk)
      (PerasVotingCommittee blk)
  praosStateGetPerasVotingCommittee p perasParams tickedPraosState = do
    committeeInput <-
      praosStatePerasVotingCommitteeInput p perasParams tickedPraosState
    bimap injectVotingCommitteeError id $
      Committee.mkVotingCommittee committeeInput
