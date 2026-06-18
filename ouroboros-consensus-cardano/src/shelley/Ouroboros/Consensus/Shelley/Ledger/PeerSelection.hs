{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Shelley.Ledger.PeerSelection () where

import Cardano.Base.IP (unIPv4, unIPv6)
import qualified Cardano.Ledger.Api.State.Query as SL
import Cardano.Ledger.BaseTypes
import qualified Cardano.Ledger.Shelley.API as SL
import Control.DeepSeq (force)
import Data.Foldable (toList)
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, mapMaybe)
import Data.Ord (Down (..))
import Data.Sequence.Strict (StrictSeq)
import Data.Text.Encoding (encodeUtf8)
import Ouroboros.Consensus.Ledger.SupportsPeerSelection
import Ouroboros.Consensus.Shelley.Ledger.Block
import Ouroboros.Consensus.Shelley.Ledger.Ledger

instance SL.EraCertState era => LedgerSupportsPeerSelection (ShelleyBlock proto era) where
  getPeers st =
    catMaybes
      [ (PoolStake stake,) <$> ledgerRelayAccessPoints relays
      | (_stakePool, (stake, relays)) <- stakeOrdered
      ]
   where
    stakeOrdered =
      sortOn (Down . fst . snd) . Map.toList $
        SL.queryStakePoolRelays (shelleyLedgerState st)

    relayToLedgerRelayAccessPoint :: SL.StakePoolRelay -> Maybe LedgerRelayAccessPoint
    relayToLedgerRelayAccessPoint (SL.SingleHostAddr (SJust (Port port)) (SJust ipv4) _) =
      Just $ LedgerRelayAccessAddress (IPv4 (unIPv4 ipv4)) (fromIntegral port)
    relayToLedgerRelayAccessPoint
      ( SL.SingleHostAddr
          (SJust (Port port))
          SNothing
          (SJust ipv6)
        ) =
        Just $ LedgerRelayAccessAddress (IPv6 (unIPv6 ipv6)) (fromIntegral port)
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

    ledgerRelayAccessPoints ::
      StrictSeq SL.StakePoolRelay ->
      Maybe (NonEmpty StakePoolRelay)
    ledgerRelayAccessPoints =
      NE.nonEmpty
        . force
        . mapMaybe (fmap CurrentRelay . relayToLedgerRelayAccessPoint)
        . toList
