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

import qualified Cardano.Crypto.Hash as Hash
import qualified Cardano.Crypto.VRF as VRF
import           Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..),
                     decodeRecordNamed, encodeListLen)
import           Cardano.Ledger.Hashes (HASH)
import qualified Cardano.Ledger.Keys as SL
import qualified Cardano.Ledger.State as SL
import           Cardano.Protocol.Crypto (Crypto, VRF)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           GHC.Generics (Generic)
import           NoThunks.Class

-- | Copy of 'SL.IndividualPoolStake' before
-- <https://github.com/IntersectMBO/cardano-ledger/pull/4324>.
data IndividualPoolStake c = IndividualPoolStake {
    individualPoolStake    :: !Rational
  , individualPoolStakeVrf :: !(Hash.Hash HASH (VRF.VerKeyVRF (VRF c)))
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NoThunks)

fromLedgerIndividualPoolStake :: SL.IndividualPoolStake -> IndividualPoolStake c
fromLedgerIndividualPoolStake ips = IndividualPoolStake {
      individualPoolStake    = SL.individualPoolStake ips
    , individualPoolStakeVrf = SL.fromVRFVerKeyHash $ SL.individualPoolStakeVrf ips
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
    unPoolDistr :: Map (SL.KeyHash SL.StakePool) (IndividualPoolStake c)
  }
  deriving stock (Show, Eq, Generic)
  deriving newtype (EncCBOR, DecCBOR)

fromLedgerPoolDistr :: SL.PoolDistr -> PoolDistr c
fromLedgerPoolDistr pd = PoolDistr {
      unPoolDistr = Map.map fromLedgerIndividualPoolStake $ SL.unPoolDistr pd
    }
