{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Ouroboros.Consensus.Ledger.SupportsPeras
  ( LedgerSupportsPeras (..)
  , LedgerStateSupportsPeras (..)
  )
where

import qualified Cardano.Crypto.Hash as Hash
import Cardano.Ledger.Coin (Coin (..), compactCoinOrError, knownNonZeroCoin)
import Cardano.Ledger.Keys (KeyHash (..), toVRFVerKeyHash)
import Cardano.Ledger.State (IndividualPoolStake (..), PoolDistr (..))
import qualified Data.Map as Map
import Ouroboros.Consensus.Block.SupportsPeras
  ( PerasParams
  , PerasRoundNo
  , defaultPerasParams
  )
import Ouroboros.Consensus.Ledger.Abstract (EmptyMK, LedgerState)

-- | Extract Peras information stored in the ledger state (deprecated).
--
-- IMPORTANT: we are moving the cached latest Peras cert round from the
-- (non-extended) ledger state into the extended one, so we will remove this
-- type class during that refactor.
class LedgerSupportsPeras blk where
  -- | Extract the round number of the latest Peras certificate stored in the
  -- given ledger state (if any). This is needed to coordinate the end of a
  -- cooldown period.
  getLatestPerasCertRound :: LedgerState blk mk -> Maybe PerasRoundNo
  default getLatestPerasCertRound :: LedgerState blk mk -> Maybe PerasRoundNo
  getLatestPerasCertRound _ = Nothing

-- | Extract Peras information stored in the ledger state.
class LedgerStateSupportsPeras ledgerState where
  -- | Extract the stake distribution from the given ledger state.
  --
  -- PRECONDITION: this function will only return a meaningful result if the
  -- ledger state is from a block that supports Peras.
  getPoolDistr :: ledgerState EmptyMK -> PoolDistr
  default getPoolDistr :: ledgerState EmptyMK -> PoolDistr
  getPoolDistr _ = dummyPoolDistr

  -- | Extract the Peras parameters from the given ledger state.
  --
  -- TODO: when Peras params go on chain, update this.
  getPerasParams :: proxy blk -> ledgerState EmptyMK -> PerasParams blk
  default getPerasParams :: proxy blk -> ledgerState EmptyMK -> PerasParams blk
  getPerasParams _ _ = defaultPerasParams

-- NOTE: this is a bit of a hack for blocks that do not really support Peras.
-- We return a single dummy stake pool holding all of the active stake, so that
-- consumers relying on a non-empty stake distribution (e.g. the mock voting
-- committee) do not fail.
dummyPoolDistr :: PoolDistr
dummyPoolDistr =
  PoolDistr
    { unPoolDistr = Map.singleton dummyPoolId dummyPoolStake
    , pdTotalActiveStake = knownNonZeroCoin @1
    }
 where
  dummyPoolId =
    KeyHash
      . Hash.castHash
      . Hash.hashWith id
      $ "peras-mock-pool"
  dummyPoolVrf =
    toVRFVerKeyHash
      . Hash.castHash
      . Hash.hashWith id
      $ "peras-mock-pool-vrf"
  dummyPoolStake =
    IndividualPoolStake
      { individualPoolStake = 1
      , individualTotalPoolStake = compactCoinOrError (Coin 1)
      , individualPoolStakeVrf = dummyPoolVrf
      }
