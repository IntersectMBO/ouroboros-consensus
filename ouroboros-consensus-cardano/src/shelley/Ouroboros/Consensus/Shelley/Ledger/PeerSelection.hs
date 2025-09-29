{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Shelley.Ledger.PeerSelection () where

import Cardano.Ledger.BaseTypes
import qualified Cardano.Ledger.Keys as SL
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Ledger.State as SL
import Control.DeepSeq (force)
import Data.Bifunctor (second)
import Data.Foldable (toList)
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, mapMaybe)
import Data.Ord (Down (..))
import Data.Text.Encoding (encodeUtf8)
import Lens.Micro.Extras (view)
import Ouroboros.Consensus.Ledger.SupportsPeerSelection
import Ouroboros.Consensus.Shelley.Ledger.Block
import Ouroboros.Consensus.Shelley.Ledger.Ledger

instance SL.EraCertState era => LedgerSupportsPeerSelection (ShelleyBlock proto era) where
  getPeers ShelleyLedgerState{shelleyLedgerState} =
    catMaybes
      [ (poolStake,) <$> Map.lookup stakePool poolLedgerRelayAccessPoints
      | (stakePool, poolStake) <- orderByStake poolDistr
      ]
   where
    poolDistr :: SL.PoolDistr
    poolDistr = SL.nesPd shelleyLedgerState

    -- Sort stake pools by descending stake
    orderByStake ::
      SL.PoolDistr ->
      [(SL.KeyHash 'SL.StakePool, PoolStake)]
    orderByStake =
      sortOn (Down . snd)
        . map (second (PoolStake . SL.individualPoolStake))
        . Map.toList
        . SL.unPoolDistr

    relayToLedgerRelayAccessPoint :: SL.StakePoolRelay -> Maybe LedgerRelayAccessPoint
    relayToLedgerRelayAccessPoint (SL.SingleHostAddr (SJust (Port port)) (SJust ipv4) _) =
      Just $ LedgerRelayAccessAddress (IPv4 ipv4) (fromIntegral port)
    relayToLedgerRelayAccessPoint
      ( SL.SingleHostAddr
          (SJust (Port port))
          SNothing
          (SJust ipv6)
        ) =
        Just $ LedgerRelayAccessAddress (IPv6 ipv6) (fromIntegral port)
    -- no IP address or no port number
    relayToLedgerRelayAccessPoint (SL.SingleHostAddr SNothing _ _) = Nothing
    relayToLedgerRelayAccessPoint (SL.SingleHostAddr _ SNothing _) = Nothing
    relayToLedgerRelayAccessPoint (SL.SingleHostName (SJust (Port port)) dnsName) =
      Just $ LedgerRelayAccessDomain (encodeUtf8 $ dnsToText dnsName) (fromIntegral port)
    -- srv support: either `SingleHostName` without port number or
    -- `MultiHostName`
    relayToLedgerRelayAccessPoint (SL.SingleHostName SNothing dnsName) =
      Just $ LedgerRelayAccessSRVDomain (encodeUtf8 $ dnsToText dnsName)
    relayToLedgerRelayAccessPoint (SL.MultiHostName dnsName) =
      Just $ LedgerRelayAccessSRVDomain (encodeUtf8 $ dnsToText dnsName)

    -- Note that a stake pool can have multiple registered relays
    pparamsLedgerRelayAccessPoints ::
      (LedgerRelayAccessPoint -> StakePoolRelay) ->
      SL.StakePoolState ->
      Maybe (NonEmpty StakePoolRelay)
    pparamsLedgerRelayAccessPoints injStakePoolRelay =
      NE.nonEmpty
        . force
        . mapMaybe (fmap injStakePoolRelay . relayToLedgerRelayAccessPoint)
        . toList
        . SL.spsRelays

    -- Combine the stake pools registered in the future and the current pool
    -- parameters, and remove duplicates.
    poolLedgerRelayAccessPoints ::
      Map (SL.KeyHash 'SL.StakePool) (NonEmpty StakePoolRelay)
    poolLedgerRelayAccessPoints =
      Map.unionWith
        (\futureRelays currentRelays -> NE.nub (futureRelays <> currentRelays))
        (Map.mapMaybe (pparamsLedgerRelayAccessPoints FutureRelay) (SL.psStakePools pstate))
        (Map.mapMaybe (pparamsLedgerRelayAccessPoints CurrentRelay) (SL.psFutureStakePools pstate))

    pstate :: SL.PState era
    pstate =
      view SL.certPStateL
        . SL.lsCertState
        . SL.esLState
        . SL.nesEs
        $ shelleyLedgerState
