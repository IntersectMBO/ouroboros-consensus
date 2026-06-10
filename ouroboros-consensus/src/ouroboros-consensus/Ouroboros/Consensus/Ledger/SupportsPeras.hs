{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}

module Ouroboros.Consensus.Ledger.SupportsPeras
  ( LedgerSupportsPeras
  , ALedgerSupportsPeras (..)
  )
where

import Cardano.Ledger.Coin (knownNonZeroCoin)
import Cardano.Ledger.State (PoolDistr (..))
import Data.Kind (Constraint, Type)
import qualified Data.Map as Map
import Ouroboros.Consensus.Block.SupportsPeras (PerasParams, PerasRoundNo, defaultPerasParams)
import Ouroboros.Consensus.Ledger.Abstract (LedgerState)

type LedgerSupportsPeras :: Type -> Constraint
type LedgerSupportsPeras blk = forall mk. ALedgerSupportsPeras (LedgerState blk mk)

-- | Extract Peras information stored in the ledger state
class ALedgerSupportsPeras ledger where
  -- | Extract the round number of the latest Peras certificate stored in the
  -- given ledger state (if any). This is needed to coordinate the end of a
  -- cooldown period.
  getLatestPerasCertRound :: ledger -> Maybe PerasRoundNo
  default getLatestPerasCertRound :: ledger -> Maybe PerasRoundNo
  getLatestPerasCertRound _ = Nothing

  -- | Extract the stake distribution from the given ledger state.
  -- PRECONDITION: this function will only return a meaningful result if the
  -- ledger state is from a block that supports Peras
  getPoolDistr :: ledger -> PoolDistr
  default getPoolDistr :: ledger -> PoolDistr
  -- NOTE: this is a bit of a hack for blocks that do not really support Peras.
  getPoolDistr _ =
    PoolDistr
      { unPoolDistr = Map.empty
      , pdTotalActiveStake = knownNonZeroCoin @1
      }

  -- | TODO: when Peras params go on chain, update this
  getPerasParams :: proxy blk -> ledger -> PerasParams blk
  default getPerasParams :: proxy blk -> ledger -> PerasParams blk
  getPerasParams _ _ = defaultPerasParams
