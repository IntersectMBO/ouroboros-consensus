{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module contains copies of older versions of types from Ledger in order
-- to retain backwards-compatibility. Eventually, types likes this should be
-- defined in Ledger instead of here, see
-- <https://github.com/IntersectMBO/cardano-ledger/issues/4415>.
module Ouroboros.Consensus.Shelley.Ledger.Query.Types (
    IndividualPoolStake (..)
  , PoolDistr (..)
  , fromLedgerIndividualPoolStake
  , fromLedgerPoolDistr
  ) where

import           Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..),
                     decodeRecordNamed, encodeListLen)
import           Cardano.Ledger.Crypto (Crypto)
import           Cardano.Ledger.Keys (Hash)
import qualified Cardano.Ledger.PoolDistr as SL
import qualified Cardano.Ledger.Shelley.API as SL
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           GHC.Generics (Generic)
import           NoThunks.Class

-- | Copy of 'SL.IndividualPoolStake' before
-- <https://github.com/IntersectMBO/cardano-ledger/pull/4324>.
data IndividualPoolStake c = IndividualPoolStake {
    individualPoolStake    :: !Rational
  , individualPoolStakeVrf :: !(Hash c (SL.VerKeyVRF c))
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NoThunks)

fromLedgerIndividualPoolStake :: SL.IndividualPoolStake c -> IndividualPoolStake c
fromLedgerIndividualPoolStake ips = IndividualPoolStake {
      individualPoolStake    = SL.individualPoolStake ips
    , individualPoolStakeVrf = SL.individualPoolStakeVrf ips
    }

instance Crypto c => EncCBOR (IndividualPoolStake c) where
  encCBOR (IndividualPoolStake stake vrf) =
    mconcat
      [ encodeListLen 2
      , encCBOR stake
      , encCBOR vrf
      ]

instance Crypto c => DecCBOR (IndividualPoolStake c) where
  decCBOR =
    decodeRecordNamed "IndividualPoolStake" (const 2) $
      IndividualPoolStake
        <$> decCBOR
        <*> decCBOR

-- | Copy of 'SL.PoolDistr' before
-- <https://github.com/IntersectMBO/cardano-ledger/pull/4324>.
newtype PoolDistr c = PoolDistr {
    unPoolDistr :: Map (SL.KeyHash SL.StakePool c) (IndividualPoolStake c)
  }
  deriving stock (Show, Eq, Generic)
  deriving newtype (EncCBOR, DecCBOR)

fromLedgerPoolDistr :: SL.PoolDistr c -> PoolDistr c
fromLedgerPoolDistr pd = PoolDistr {
      unPoolDistr = Map.map fromLedgerIndividualPoolStake $ SL.unPoolDistr pd
    }
