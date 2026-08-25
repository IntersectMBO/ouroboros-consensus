{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Ouroboros.Consensus.Peras.Voting.V1
  ( PerasVotingCommitteeScheme
  , mkPerasVotingCommitteeInput

    -- * For testing purposes
  , extractPerasStakeDistrAndPublicKeys
  , ledgerKeyScope
  ) where

import Cardano.Ledger.State (IndividualPoolStake (..), PoolDistr (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Data (Proxy (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import GHC.Base (Any)
import Ouroboros.Consensus.Block.SupportsPeras (PerasCrypto, PerasParams (..))
import Ouroboros.Consensus.Committee.Class (CryptoSupportsVotingCommittee (..))
import qualified Ouroboros.Consensus.Committee.Crypto.BLS as BLS
import Ouroboros.Consensus.Committee.Types (LedgerStake (..), PoolId (..))
import Ouroboros.Consensus.Committee.WFA
  ( mkExtWFAStakeDistr
  , wFATiebreakerWithEpochNonce
  )
import Ouroboros.Consensus.Committee.WFALS (VotingCommitteeInput (..), WFALS)
import Ouroboros.Consensus.Ledger.Abstract (EmptyMK)
import Ouroboros.Consensus.Ledger.SupportsPeras
  ( LedgerStateSupportsPeras (..)
  )
import Ouroboros.Consensus.Peras.Crypto.BLS (PerasBLSCrypto, PerasPublicKey (..))
import qualified Ouroboros.Consensus.Peras.Error.V1 as V1
import Ouroboros.Consensus.Protocol.Abstract
  ( ChainDepStateSupportsPeras (..)
  )

type PerasVotingCommitteeScheme = WFALS

ledgerKeyScope :: BLS.KeyScope
ledgerKeyScope = "PERAS/LEDGER"

extractPerasStakeDistrAndPublicKeys ::
  PoolDistr ->
  Map PoolId (LedgerStake, PerasPublicKey)
extractPerasStakeDistrAndPublicKeys =
  Map.mapKeysMonotonic PoolId
    . Map.mapMaybeWithKey extractedEntry
    . unPoolDistr
 where
  extractedEntry _poolId IndividualPoolStake{individualPoolStake, individualPoolStakeBls} =
    case individualPoolStakeBls of
      SNothing -> Nothing
      SJust blsKey ->
        Just
          ( LedgerStake individualPoolStake
          , PerasPublicKey (BLS.coercePublicKey @Any (BLS.publicKeyFromLedgerBlsKey ledgerKeyScope blsKey))
          )

mkPerasVotingCommitteeInput ::
  forall blk ledgerState chainDepState.
  ( PerasCrypto blk ~ PerasBLSCrypto
  , LedgerStateSupportsPeras ledgerState
  , ChainDepStateSupportsPeras chainDepState
  ) =>
  ledgerState EmptyMK ->
  chainDepState ->
  Either (V1.PerasError blk) (VotingCommitteeInput (PerasCrypto blk) WFALS)
mkPerasVotingCommitteeInput ledgerState headerState = do
  let epochNonce = getEpochNonce headerState
      poolDistr = getPoolDistr ledgerState
      stakeDistrWithPublicKeys = extractPerasStakeDistrAndPublicKeys poolDistr
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
