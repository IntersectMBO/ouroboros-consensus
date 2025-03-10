{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Shelley.Ledger.Query (
    BlockQuery (..)
  , NonMyopicMemberRewards (..)
  , StakeSnapshot (..)
  , StakeSnapshots (..)
  , querySupportedVersion
    -- * Serialisation
  , decodeShelleyQuery
  , decodeShelleyResult
  , encodeShelleyQuery
  , encodeShelleyResult
  ) where

import           Cardano.Binary (FromCBOR (..), ToCBOR (..), encodeListLen,
                     enforceSize)
import qualified Cardano.Ledger.Api.State.Query as SL
import           Cardano.Ledger.CertState (lookupDepositDState)
import qualified Cardano.Ledger.CertState as SL
import           Cardano.Ledger.Coin (Coin)
import           Cardano.Ledger.Compactible (Compactible (fromCompact))
import qualified Cardano.Ledger.Conway.Governance as CG
import           Cardano.Ledger.Credential (StakeCredential)
import           Cardano.Ledger.Crypto (Crypto)
import qualified Cardano.Ledger.EpochBoundary as SL
import           Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Ledger.Shelley.Core as LC
import           Cardano.Ledger.Shelley.LedgerState (AccountState)
import qualified Cardano.Ledger.Shelley.LedgerState as SL (RewardAccounts,
                     newEpochStateGovStateL)
import qualified Cardano.Ledger.Shelley.PParams as SL (emptyPPPUpdates)
import qualified Cardano.Ledger.Shelley.RewardProvenance as SL
                     (RewardProvenance)
import           Cardano.Ledger.UMap (UMap (..), rdReward, umElemDRep,
                     umElemRDPair, umElemSPool)
import           Codec.CBOR.Decoding (Decoder)
import qualified Codec.CBOR.Decoding as CBOR
import           Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Encoding as CBOR
import           Codec.Serialise (decode, encode)
import           Control.DeepSeq (NFData)
import           Data.Bifunctor (second)
import           Data.Kind (Type)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.Sequence (Seq (..))
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Type.Equality (apply)
import           Data.Typeable (Typeable)
import qualified Data.VMap as VMap
import           GHC.Generics (Generic)
import           Lens.Micro.Extras (view)
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Basics
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Query
import           Ouroboros.Consensus.Ledger.SupportsPeerSelection
import           Ouroboros.Consensus.Protocol.Abstract (ChainDepState)
import           Ouroboros.Consensus.Shelley.Eras (EraCrypto)
import qualified Ouroboros.Consensus.Shelley.Eras as SE
import           Ouroboros.Consensus.Shelley.Ledger.Block
import           Ouroboros.Consensus.Shelley.Ledger.Config
import           Ouroboros.Consensus.Shelley.Ledger.Ledger
import           Ouroboros.Consensus.Shelley.Ledger.NetworkProtocolVersion
                     (ShelleyNodeToClientVersion (..))
import           Ouroboros.Consensus.Shelley.Ledger.PeerSelection ()
import           Ouroboros.Consensus.Shelley.Ledger.Query.Types
import           Ouroboros.Consensus.Shelley.Protocol.Abstract (ProtoCrypto)
import           Ouroboros.Consensus.Util (ShowProxy (..))
import           Ouroboros.Network.Block (Serialised (..), decodePoint,
                     encodePoint, mkSerialised)
import           Ouroboros.Network.PeerSelection.LedgerPeers.Type
import           Ouroboros.Network.PeerSelection.LedgerPeers.Utils

{-------------------------------------------------------------------------------
  QueryLedger
-------------------------------------------------------------------------------}

newtype NonMyopicMemberRewards c = NonMyopicMemberRewards {
      unNonMyopicMemberRewards ::
        Map (Either SL.Coin (SL.Credential 'SL.Staking c))
            (Map (SL.KeyHash 'SL.StakePool c) SL.Coin)
    }
  deriving stock   (Show)
  deriving newtype (Eq, ToCBOR, FromCBOR)

type Delegations c =
  Map (SL.Credential 'SL.Staking c)
      (SL.KeyHash 'SL.StakePool c)

type VoteDelegatees c =
  Map (SL.Credential 'SL.Staking c)
      (SL.DRep c)

data instance BlockQuery (ShelleyBlock proto era) :: Type -> Type where
  GetLedgerTip :: BlockQuery (ShelleyBlock proto era) (Point (ShelleyBlock proto era))
  GetEpochNo :: BlockQuery (ShelleyBlock proto era) EpochNo
  -- | Calculate the Non-Myopic Pool Member Rewards for a set of
  -- credentials. See 'SL.getNonMyopicMemberRewards'
  GetNonMyopicMemberRewards
    :: Set (Either SL.Coin (SL.Credential 'SL.Staking (EraCrypto era)))
    -> BlockQuery (ShelleyBlock proto era) (NonMyopicMemberRewards (EraCrypto era))
  GetCurrentPParams
    :: BlockQuery (ShelleyBlock proto era) (LC.PParams era)
  GetProposedPParamsUpdates
    :: BlockQuery (ShelleyBlock proto era) (SL.ProposedPPUpdates era)
  -- | This gets the stake distribution, but not in terms of _active_ stake
  -- (which we need for the leader schedule), but rather in terms of _total_
  -- stake, which is relevant for rewards. It is used by the wallet to show
  -- saturation levels to the end user. We should consider refactoring this, to
  -- an endpoint that provides all the information that the wallet wants about
  -- pools, in an extensible fashion.
  GetStakeDistribution
    :: BlockQuery (ShelleyBlock proto era) (PoolDistr (EraCrypto era))

  -- | Get a subset of the UTxO, filtered by address. Although this will
  -- typically return a lot less data than 'GetUTxOWhole', it requires a linear
  -- search over the UTxO and so cost O(n) time.
  --
  -- Only 'GetUTxOByTxIn' is efficient in time and space.
  --
  GetUTxOByAddress
    :: Set (SL.Addr (EraCrypto era))
    -> BlockQuery (ShelleyBlock proto era) (SL.UTxO era)

  -- | Get the /entire/ UTxO. This is only suitable for debug/testing purposes
  -- because otherwise it is far too much data.
  --
  GetUTxOWhole
    :: BlockQuery (ShelleyBlock proto era) (SL.UTxO era)

  -- | Only for debugging purposes, we make no effort to ensure binary
  -- compatibility (cf the comment on 'GetCBOR'). Moreover, it is huge.
  DebugEpochState
    :: BlockQuery (ShelleyBlock proto era) (SL.EpochState era)

  -- | Wrap the result of the query using CBOR-in-CBOR.
  --
  -- For example, when a client is running a different version than the server
  -- and it sends a 'DebugEpochState' query, the client's decoder might fail to
  -- deserialise the epoch state as it might have changed between the two
  -- different versions. The client will then disconnect.
  --
  -- By using CBOR-in-CBOR, the client always successfully decodes the outer
  -- CBOR layer (so no disconnect) and can then manually try to decode the
  -- inner result. When the client's decoder is able to decode the inner
  -- result, it has access to the deserialised epoch state. When it fails to
  -- decode it, the client can fall back to pretty printing the actual CBOR,
  -- which is better than no output at all.
  GetCBOR
    :: BlockQuery (ShelleyBlock proto era) result
    -> BlockQuery (ShelleyBlock proto era) (Serialised result)

  GetFilteredDelegationsAndRewardAccounts
    :: Set (SL.Credential 'SL.Staking (EraCrypto era))
    -> BlockQuery (ShelleyBlock proto era)
             (Delegations (EraCrypto era), SL.RewardAccounts (EraCrypto era))

  GetGenesisConfig
    :: BlockQuery (ShelleyBlock proto era) (CompactGenesis (EraCrypto era))

  -- | Only for debugging purposes, we make no effort to ensure binary
  -- compatibility (cf the comment on 'GetCBOR'). Moreover, it is huge.
  DebugNewEpochState
    :: BlockQuery (ShelleyBlock proto era) (SL.NewEpochState era)

  -- | Only for debugging purposes, we make no effort to ensure binary
  -- compatibility (cf the comment on 'GetCBOR').
  DebugChainDepState
    :: BlockQuery (ShelleyBlock proto era) (ChainDepState proto)

  GetRewardProvenance
    :: BlockQuery (ShelleyBlock proto era) (SL.RewardProvenance (EraCrypto era))

  -- | Get a subset of the UTxO, filtered by transaction input. This is
  -- efficient and costs only O(m * log n) for m inputs and a UTxO of size n.
  --
  GetUTxOByTxIn
    :: Set (SL.TxIn (EraCrypto era))
    -> BlockQuery (ShelleyBlock proto era) (SL.UTxO era)

  GetStakePools
    :: BlockQuery (ShelleyBlock proto era)
                  (Set (SL.KeyHash 'SL.StakePool (EraCrypto era)))

  GetStakePoolParams
    :: Set (SL.KeyHash 'SL.StakePool (EraCrypto era))
    -> BlockQuery (ShelleyBlock proto era)
                  (Map (SL.KeyHash 'SL.StakePool (EraCrypto era))
                       (SL.PoolParams (EraCrypto era)))

  GetRewardInfoPools
    :: BlockQuery (ShelleyBlock proto era)
                  (SL.RewardParams,
                    Map (SL.KeyHash 'SL.StakePool (EraCrypto era))
                        (SL.RewardInfoPool))

  GetPoolState
    :: Maybe (Set (SL.KeyHash 'SL.StakePool (EraCrypto era)))
    -> BlockQuery (ShelleyBlock proto era)
                  (SL.PState era)

  GetStakeSnapshots
    :: Maybe (Set (SL.KeyHash 'SL.StakePool (EraCrypto era)))
    -> BlockQuery (ShelleyBlock proto era)
                  (StakeSnapshots (EraCrypto era))

  GetPoolDistr
    :: Maybe (Set (SL.KeyHash 'SL.StakePool (EraCrypto era)))
    -> BlockQuery (ShelleyBlock proto era)
                  (PoolDistr (EraCrypto era))

  GetStakeDelegDeposits
    :: Set (StakeCredential (EraCrypto era))
    -> BlockQuery (ShelleyBlock proto era)
                  (Map (StakeCredential (EraCrypto era)) Coin)

  -- | Not supported in eras before Conway
  GetConstitution
    :: CG.ConwayEraGov era
    => BlockQuery (ShelleyBlock proto era) (CG.Constitution era)

  -- | Although this query was introduced as part of Conway, it is general and
  --  so has non-degenerate semantics for eras before Conway.
  GetGovState
    :: BlockQuery (ShelleyBlock proto era) (LC.GovState era)

  -- | The argument specifies the credential of each 'DRep' whose state should
  -- be returned. When it's empty, the state of every 'DRep' is returned.
  --
  -- Not supported in eras before Conway.
  GetDRepState
    :: CG.ConwayEraGov era
    => Set (SL.Credential 'DRepRole (EraCrypto era))
    -> BlockQuery (ShelleyBlock proto era)
                  (Map
                       (SL.Credential 'DRepRole (EraCrypto era))
                       (SL.DRepState (EraCrypto era))
                  )

  -- | Query the 'DRep' stake distribution. Note that this can be an expensive
  -- query because there is a chance that the latest snapshot's distribution
  -- has not yet been fully computed.
  --
  -- The argument specifies whose stake should be returned. When it's empty,
  -- the stake of every 'DRep's is returned.
  --
  -- Not supported in eras before Conway.
  GetDRepStakeDistr
    :: CG.ConwayEraGov era
    => Set (SL.DRep (EraCrypto era))
    -> BlockQuery (ShelleyBlock proto era) (Map (SL.DRep (EraCrypto era)) Coin)

  -- | Query committee members
  --
  -- Not supported in eras before Conway.
  GetCommitteeMembersState
    :: CG.ConwayEraGov era
    => Set (SL.Credential 'ColdCommitteeRole (EraCrypto era) )
    -> Set (SL.Credential 'HotCommitteeRole (EraCrypto era))
    -> Set SL.MemberStatus
    -> BlockQuery (ShelleyBlock proto era) (SL.CommitteeMembersState (EraCrypto era))

  -- | Not supported in eras before Conway.
  GetFilteredVoteDelegatees
    :: CG.ConwayEraGov era
    => Set (SL.Credential 'SL.Staking (EraCrypto era))
    -> BlockQuery (ShelleyBlock proto era) (VoteDelegatees (EraCrypto era))

  GetAccountState
    :: BlockQuery (ShelleyBlock proto era) AccountState

  -- | Query the SPO voting stake distribution.
  -- This stake distribution is different from the one used in leader election.
  --
  -- See: https://github.com/IntersectMBO/cardano-ledger/issues/4342
  --
  -- Not supported in eras before Conway.
  GetSPOStakeDistr
    :: CG.ConwayEraGov era
    => Set (KeyHash 'StakePool (EraCrypto era))
    -> BlockQuery (ShelleyBlock proto era) (Map (KeyHash 'StakePool (EraCrypto era)) Coin)

  GetProposals
    :: CG.ConwayEraGov era
    => Set (CG.GovActionId (EraCrypto era))
    -> BlockQuery (ShelleyBlock proto era) (Seq (CG.GovActionState era))

  GetRatifyState
    :: CG.ConwayEraGov era
    => BlockQuery (ShelleyBlock proto era) (CG.RatifyState era)

  GetFuturePParams
    :: BlockQuery (ShelleyBlock proto era) (Maybe (LC.PParams era))

  -- | Obtain a snapshot of big ledger peers. CLI can serialize these,
  -- and if made available to the node by topology configuration,
  -- the diffusion layer can use these peers when syncing up from scratch
  -- or stale ledger state - especially useful for Genesis mode
  GetBigLedgerPeerSnapshot
    :: BlockQuery (ShelleyBlock proto era) LedgerPeerSnapshot

  -- WARNING: please add new queries to the end of the list and stick to this
  -- order in all other pattern matches on queries. This helps in particular
  -- with the en/decoders, as we want the CBOR tags to be ordered.
  --
  -- WARNING: when adding a new query, a new @ShelleyNodeToClientVersionX@ must
  -- be added. See #2830 for a template on how to do this.
  --
  -- WARNING: never modify an existing query that has been incorporated in a
  -- release of the node, as it will break compatibility with deployed nodes.
  -- Instead, add a new query. To remove the old query, first to stop supporting
  -- it by modifying 'querySupportedVersion' (@< X@) and when the version is no
  -- longer used (because mainnet has hard-forked to a newer version), it can be
  -- removed.

instance (Typeable era, Typeable proto)
  => ShowProxy (BlockQuery (ShelleyBlock proto era)) where

instance (ShelleyCompatible proto era, ProtoCrypto proto ~ crypto)
      => BlockSupportsLedgerQuery (ShelleyBlock proto era) where
  answerBlockQuery cfg query ext =
      case query of
        GetLedgerTip ->
          shelleyLedgerTipPoint lst
        GetEpochNo ->
          SL.nesEL st
        GetNonMyopicMemberRewards creds ->
          NonMyopicMemberRewards $
            SL.getNonMyopicMemberRewards globals st creds
        GetCurrentPParams ->
          getPParams st
        GetProposedPParamsUpdates ->
          getProposedPPUpdates st
        GetStakeDistribution ->
          fromLedgerPoolDistr $ SL.poolsByTotalStakeFraction globals st
        GetUTxOByAddress addrs ->
          SL.getFilteredUTxO st addrs
        GetUTxOWhole ->
          SL.getUTxO st
        DebugEpochState ->
          getEpochState st
        GetCBOR query' ->
          -- We encode using the latest (@maxBound@) ShelleyNodeToClientVersion,
          -- as the @GetCBOR@ query already is about opportunistically assuming
          -- both client and server are running the same version; cf. the
          -- @GetCBOR@ Haddocks.
          mkSerialised (encodeShelleyResult maxBound query') $
            answerBlockQuery cfg query' ext
        GetFilteredDelegationsAndRewardAccounts creds ->
          getFilteredDelegationsAndRewardAccounts st creds
        GetGenesisConfig ->
          shelleyLedgerCompactGenesis lcfg
        DebugNewEpochState ->
          st
        DebugChainDepState ->
          headerStateChainDep hst
        GetRewardProvenance ->
          snd $ SL.getRewardProvenance globals st
        GetUTxOByTxIn txins ->
          SL.getUTxOSubset st txins
        GetStakePools ->
          SL.getPools st
        GetStakePoolParams poolids ->
          SL.getPoolParameters st poolids
        GetRewardInfoPools ->
          SL.getRewardInfoPools globals st
        GetPoolState mPoolIds ->
          let certPState = SL.certPState . SL.lsCertState . SL.esLState . SL.nesEs $ st in
          case mPoolIds of
            Just poolIds ->
              SL.PState
                { SL.psStakePoolParams  =
                  Map.restrictKeys (SL.psStakePoolParams certPState) poolIds
                , SL.psFutureStakePoolParams =
                  Map.restrictKeys (SL.psFutureStakePoolParams certPState) poolIds
                , SL.psRetiring = Map.restrictKeys (SL.psRetiring certPState) poolIds
                , SL.psDeposits = Map.restrictKeys (SL.psDeposits certPState) poolIds
                }
            Nothing -> certPState
        GetStakeSnapshots mPoolIds ->
          let SL.SnapShots
                { SL.ssStakeMark
                , SL.ssStakeSet
                , SL.ssStakeGo
                } = SL.esSnapshots . SL.nesEs $ st

              totalMarkByPoolId :: Map (KeyHash 'StakePool crypto) Coin
              totalMarkByPoolId = SL.sumStakePerPool (SL.ssDelegations ssStakeMark) (SL.ssStake ssStakeMark)

              totalSetByPoolId :: Map (KeyHash 'StakePool crypto) Coin
              totalSetByPoolId = SL.sumStakePerPool (SL.ssDelegations ssStakeSet) (SL.ssStake ssStakeSet)

              totalGoByPoolId :: Map (KeyHash 'StakePool crypto) Coin
              totalGoByPoolId = SL.sumStakePerPool (SL.ssDelegations ssStakeGo) (SL.ssStake ssStakeGo)

              getPoolStakes :: Set (KeyHash 'StakePool crypto) -> Map (KeyHash 'StakePool crypto) (StakeSnapshot crypto)
              getPoolStakes poolIds = Map.fromSet mkStakeSnapshot poolIds
                where mkStakeSnapshot poolId = StakeSnapshot
                        { ssMarkPool = Map.findWithDefault mempty poolId totalMarkByPoolId
                        , ssSetPool  = Map.findWithDefault mempty poolId totalSetByPoolId
                        , ssGoPool   = Map.findWithDefault mempty poolId totalGoByPoolId
                        }

              getAllStake :: SL.SnapShot crypto -> SL.Coin
              getAllStake (SL.SnapShot stake _ _) = VMap.foldMap fromCompact (SL.unStake stake)
          in

          case mPoolIds of
            Nothing ->
              let poolIds = Set.fromList $ mconcat
                    [ VMap.elems (SL.ssDelegations ssStakeMark)
                    , VMap.elems (SL.ssDelegations ssStakeSet)
                    , VMap.elems (SL.ssDelegations ssStakeGo)
                    ]
              in
              StakeSnapshots
                { ssStakeSnapshots = getPoolStakes poolIds
                , ssMarkTotal      = getAllStake ssStakeMark
                , ssSetTotal       = getAllStake ssStakeSet
                , ssGoTotal        = getAllStake ssStakeGo
                }
            Just poolIds ->
              StakeSnapshots
                { ssStakeSnapshots = getPoolStakes poolIds
                , ssMarkTotal      = getAllStake ssStakeMark
                , ssSetTotal       = getAllStake ssStakeSet
                , ssGoTotal        = getAllStake ssStakeGo
                }

        GetPoolDistr mPoolIds ->
          let stakeSet = SL.ssStakeSet . SL.esSnapshots $ getEpochState st in
          fromLedgerPoolDistr $
            SL.calculatePoolDistr' (maybe (const True) (flip Set.member) mPoolIds) stakeSet
        GetStakeDelegDeposits stakeCreds ->
          let lookupDeposit =
                lookupDepositDState (SL.certDState $ SL.lsCertState $ SL.esLState $ SL.nesEs st)
              lookupInsert acc cred =
                case lookupDeposit cred of
                  Nothing      -> acc
                  Just deposit -> Map.insert cred deposit acc
          in Set.foldl' lookupInsert Map.empty stakeCreds
        GetConstitution ->
          SL.queryConstitution st
        GetGovState ->
          SL.queryGovState st
        GetDRepState drepCreds ->
          SL.queryDRepState st drepCreds
        GetDRepStakeDistr dreps ->
          SL.queryDRepStakeDistr st dreps
        GetCommitteeMembersState coldCreds hotCreds statuses ->
          SL.queryCommitteeMembersState coldCreds hotCreds statuses st
        GetFilteredVoteDelegatees stakeCreds ->
          getFilteredVoteDelegatees st stakeCreds
        GetAccountState ->
          SL.queryAccountState st
        GetSPOStakeDistr keys ->
          SL.querySPOStakeDistr st keys
        GetProposals gids ->
          SL.queryProposals st gids
        GetRatifyState ->
          SL.queryRatifyState st
        GetFuturePParams ->
          SL.queryFuturePParams st
        GetBigLedgerPeerSnapshot ->
          let slot = getTipSlot lst
              ledgerPeers = second (fmap stakePoolRelayAccessPoint) <$> getPeers lst
              bigLedgerPeers = accumulateBigLedgerStake ledgerPeers
          in LedgerPeerSnapshot (slot, bigLedgerPeers)
    where
      lcfg    = configLedger $ getExtLedgerCfg cfg
      globals = shelleyLedgerGlobals lcfg
      -- NOTE: we are not pattern matching on @ext@ but using the accessors
      -- here. The reason for that is that that pattern match blows up the
      -- compile time (in particular the time spent desugaring, which is when
      -- the compiler looks at pattern matches) to 2m30s! We don't really
      -- understand why, but our guess is that it has to do with the combination
      -- of the strictness of 'ExtLedgerState', the fact that @LedgerState@ is a
      -- data family, and the 'ShelleyBasedEra' constraint.
      lst = ledgerState ext
      hst = headerState ext
      st  = shelleyLedgerState lst

instance SameDepIndex (BlockQuery (ShelleyBlock proto era)) where
  sameDepIndex GetLedgerTip GetLedgerTip
    = Just Refl
  sameDepIndex GetLedgerTip _
    = Nothing
  sameDepIndex GetEpochNo GetEpochNo
    = Just Refl
  sameDepIndex GetEpochNo _
    = Nothing
  sameDepIndex (GetNonMyopicMemberRewards creds) (GetNonMyopicMemberRewards creds')
    | creds == creds'
    = Just Refl
    | otherwise
    = Nothing
  sameDepIndex (GetNonMyopicMemberRewards _) _
    = Nothing
  sameDepIndex GetCurrentPParams GetCurrentPParams
    = Just Refl
  sameDepIndex GetCurrentPParams _
    = Nothing
  sameDepIndex GetProposedPParamsUpdates GetProposedPParamsUpdates
    = Just Refl
  sameDepIndex GetProposedPParamsUpdates _
    = Nothing
  sameDepIndex GetStakeDistribution GetStakeDistribution
    = Just Refl
  sameDepIndex GetStakeDistribution _
    = Nothing
  sameDepIndex (GetUTxOByAddress addrs) (GetUTxOByAddress addrs')
    | addrs == addrs'
    = Just Refl
    | otherwise
    = Nothing
  sameDepIndex (GetUTxOByAddress _) _
    = Nothing
  sameDepIndex GetUTxOWhole GetUTxOWhole
    = Just Refl
  sameDepIndex GetUTxOWhole _
    = Nothing
  sameDepIndex DebugEpochState DebugEpochState
    = Just Refl
  sameDepIndex DebugEpochState _
    = Nothing
  sameDepIndex (GetCBOR q) (GetCBOR q')
    = apply Refl <$> sameDepIndex q q'
  sameDepIndex (GetCBOR _) _
    = Nothing
  sameDepIndex (GetFilteredDelegationsAndRewardAccounts creds)
               (GetFilteredDelegationsAndRewardAccounts creds')
    | creds == creds'
    = Just Refl
    | otherwise
    = Nothing
  sameDepIndex (GetFilteredDelegationsAndRewardAccounts _) _
    = Nothing
  sameDepIndex GetGenesisConfig GetGenesisConfig
    = Just Refl
  sameDepIndex GetGenesisConfig _
    = Nothing
  sameDepIndex DebugNewEpochState DebugNewEpochState
    = Just Refl
  sameDepIndex DebugNewEpochState _
    = Nothing
  sameDepIndex DebugChainDepState DebugChainDepState
    = Just Refl
  sameDepIndex DebugChainDepState _
    = Nothing
  sameDepIndex GetRewardProvenance GetRewardProvenance
    = Just Refl
  sameDepIndex GetRewardProvenance _
    = Nothing
  sameDepIndex (GetUTxOByTxIn addrs) (GetUTxOByTxIn addrs')
    | addrs == addrs'
    = Just Refl
    | otherwise
    = Nothing
  sameDepIndex (GetUTxOByTxIn _) _
    = Nothing
  sameDepIndex GetStakePools GetStakePools
    = Just Refl
  sameDepIndex GetStakePools _
    = Nothing
  sameDepIndex (GetStakePoolParams poolids) (GetStakePoolParams poolids')
    | poolids == poolids'
    = Just Refl
    | otherwise
    = Nothing
  sameDepIndex (GetStakePoolParams _) _
    = Nothing
  sameDepIndex GetRewardInfoPools GetRewardInfoPools
    = Just Refl
  sameDepIndex GetRewardInfoPools _
    = Nothing
  sameDepIndex (GetPoolState poolids) (GetPoolState poolids')
    | poolids == poolids'
    = Just Refl
    | otherwise
    = Nothing
  sameDepIndex (GetPoolState _) _
    = Nothing
  sameDepIndex (GetStakeSnapshots poolid) (GetStakeSnapshots poolid')
    | poolid == poolid'
    = Just Refl
    | otherwise
    = Nothing
  sameDepIndex (GetStakeSnapshots _) _
    = Nothing
  sameDepIndex (GetPoolDistr poolids) (GetPoolDistr poolids')
    | poolids == poolids'
    = Just Refl
    | otherwise
    = Nothing
  sameDepIndex (GetPoolDistr _) _
    = Nothing
  sameDepIndex (GetStakeDelegDeposits stakeCreds) (GetStakeDelegDeposits stakeCreds')
    | stakeCreds == stakeCreds'
    = Just Refl
    | otherwise
    = Nothing
  sameDepIndex (GetStakeDelegDeposits _) _
    = Nothing
  sameDepIndex GetConstitution GetConstitution = Just Refl
  sameDepIndex GetConstitution _ = Nothing
  sameDepIndex GetGovState GetGovState = Just Refl
  sameDepIndex GetGovState _ = Nothing
  sameDepIndex GetDRepState{} GetDRepState{} = Just Refl
  sameDepIndex GetDRepState{} _ = Nothing
  sameDepIndex GetDRepStakeDistr{} GetDRepStakeDistr{} = Just Refl
  sameDepIndex GetDRepStakeDistr{} _ = Nothing
  sameDepIndex GetCommitteeMembersState{} GetCommitteeMembersState{} = Just Refl
  sameDepIndex GetCommitteeMembersState{} _ = Nothing
  sameDepIndex (GetFilteredVoteDelegatees stakeCreds) (GetFilteredVoteDelegatees stakeCreds')
    | stakeCreds == stakeCreds'
    = Just Refl
    | otherwise
    = Nothing
  sameDepIndex GetFilteredVoteDelegatees {} _ = Nothing
  sameDepIndex GetAccountState {} GetAccountState {} = Just Refl
  sameDepIndex GetAccountState {} _ = Nothing
  sameDepIndex GetSPOStakeDistr{} GetSPOStakeDistr{} = Just Refl
  sameDepIndex GetSPOStakeDistr{} _ = Nothing
  sameDepIndex GetProposals{} GetProposals{} = Just Refl
  sameDepIndex GetProposals{} _ = Nothing
  sameDepIndex GetRatifyState{} GetRatifyState{} = Just Refl
  sameDepIndex GetRatifyState{} _ = Nothing
  sameDepIndex GetFuturePParams{} GetFuturePParams{} = Just Refl
  sameDepIndex GetFuturePParams{} _ = Nothing
  sameDepIndex GetBigLedgerPeerSnapshot GetBigLedgerPeerSnapshot = Just Refl
  sameDepIndex GetBigLedgerPeerSnapshot _ = Nothing

deriving instance Eq   (BlockQuery (ShelleyBlock proto era) result)
deriving instance Show (BlockQuery (ShelleyBlock proto era) result)

instance ShelleyCompatible proto era => ShowQuery (BlockQuery (ShelleyBlock proto era)) where
  showResult = \case
      GetLedgerTip                               -> show
      GetEpochNo                                 -> show
      GetNonMyopicMemberRewards {}               -> show
      GetCurrentPParams                          -> show
      GetProposedPParamsUpdates                  -> show
      GetStakeDistribution                       -> show
      GetUTxOByAddress {}                        -> show
      GetUTxOWhole                               -> show
      DebugEpochState                            -> show
      GetCBOR {}                                 -> show
      GetFilteredDelegationsAndRewardAccounts {} -> show
      GetGenesisConfig                           -> show
      DebugNewEpochState                         -> show
      DebugChainDepState                         -> show
      GetRewardProvenance                        -> show
      GetUTxOByTxIn {}                           -> show
      GetStakePools                              -> show
      GetStakePoolParams {}                      -> show
      GetRewardInfoPools                         -> show
      GetPoolState {}                            -> show
      GetStakeSnapshots {}                       -> show
      GetPoolDistr {}                            -> show
      GetStakeDelegDeposits {}                   -> show
      GetConstitution                            -> show
      GetGovState                                -> show
      GetDRepState {}                            -> show
      GetDRepStakeDistr {}                       -> show
      GetCommitteeMembersState {}                -> show
      GetFilteredVoteDelegatees {}               -> show
      GetAccountState {}                         -> show
      GetSPOStakeDistr {}                        -> show
      GetProposals {}                            -> show
      GetRatifyState {}                          -> show
      GetFuturePParams {}                        -> show
      GetBigLedgerPeerSnapshot                   -> show

-- | Is the given query supported by the given 'ShelleyNodeToClientVersion'?
querySupportedVersion :: BlockQuery (ShelleyBlock proto era) result -> ShelleyNodeToClientVersion -> Bool
querySupportedVersion = \case
    GetLedgerTip                               -> const True
    GetEpochNo                                 -> const True
    GetNonMyopicMemberRewards {}               -> const True
    GetCurrentPParams                          -> const True
    GetProposedPParamsUpdates                  -> const True
    GetStakeDistribution                       -> const True
    GetUTxOByAddress {}                        -> const True
    GetUTxOWhole                               -> const True
    DebugEpochState                            -> const True
    GetCBOR q                                  -> querySupportedVersion q
    GetFilteredDelegationsAndRewardAccounts {} -> const True
    GetGenesisConfig                           -> const True
    DebugNewEpochState                         -> const True
    DebugChainDepState                         -> const True
    GetRewardProvenance                        -> const True
    GetUTxOByTxIn {}                           -> const True
    GetStakePools                              -> const True
    GetStakePoolParams {}                      -> const True
    GetRewardInfoPools                         -> const True
    GetPoolState {}                            -> const True
    GetStakeSnapshots {}                       -> const True
    GetPoolDistr {}                            -> const True
    GetStakeDelegDeposits {}                   -> const True
    GetConstitution                            -> (>= v8)
    GetGovState                                -> (>= v8)
    GetDRepState {}                            -> (>= v8)
    GetDRepStakeDistr {}                       -> (>= v8)
    GetCommitteeMembersState {}                -> (>= v8)
    GetFilteredVoteDelegatees {}               -> (>= v8)
    GetAccountState {}                         -> (>= v8)
    GetSPOStakeDistr {}                        -> (>= v8)
    GetProposals {}                            -> (>= v9)
    GetRatifyState {}                          -> (>= v9)
    GetFuturePParams {}                        -> (>= v10)
    GetBigLedgerPeerSnapshot                   -> (>= v11)
    -- WARNING: when adding a new query, a new @ShelleyNodeToClientVersionX@
    -- must be added. See #2830 for a template on how to do this.
  where
    v8  = ShelleyNodeToClientVersion8
    v9  = ShelleyNodeToClientVersion9
    v10 = ShelleyNodeToClientVersion10
    v11 = ShelleyNodeToClientVersion11

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | /Note/ - This query will be deprecated starting with Conway era
getProposedPPUpdates ::
     ShelleyBasedEra era
  => SL.NewEpochState era -> SL.ProposedPPUpdates era
getProposedPPUpdates =
      fromMaybe SL.emptyPPPUpdates
    . LC.getProposedPPUpdates
    . view SL.newEpochStateGovStateL

-- Get the current 'EpochState.' This is mainly for debugging.
getEpochState :: SL.NewEpochState era -> SL.EpochState era
getEpochState = SL.nesEs

getDState :: SL.NewEpochState era -> SL.DState era
getDState = SL.certDState . SL.lsCertState . SL.esLState . SL.nesEs

getFilteredDelegationsAndRewardAccounts ::
     SL.NewEpochState era
  -> Set (SL.Credential 'SL.Staking (EraCrypto era))
  -> (Delegations (EraCrypto era), SL.RewardAccounts (EraCrypto era))
getFilteredDelegationsAndRewardAccounts ss creds =
    (filteredDelegations, filteredRwdAcnts)
  where
    UMap umElems _ = SL.dsUnified $ getDState ss
    umElemsRestricted = Map.restrictKeys umElems creds

    filteredDelegations = Map.mapMaybe umElemSPool umElemsRestricted
    filteredRwdAcnts =
      Map.mapMaybe (\e -> fromCompact . rdReward <$> umElemRDPair e) umElemsRestricted

getFilteredVoteDelegatees ::
     SL.NewEpochState era
  -> Set (SL.Credential 'SL.Staking (EraCrypto era))
  -> VoteDelegatees (EraCrypto era)
getFilteredVoteDelegatees ss creds = Map.mapMaybe umElemDRep umElemsRestricted
  where
    UMap umElems _ = SL.dsUnified $ getDState ss
    umElemsRestricted = Map.restrictKeys umElems creds

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

encodeShelleyQuery ::
     forall era proto result. ShelleyBasedEra era
  => BlockQuery (ShelleyBlock proto era) result -> Encoding
encodeShelleyQuery query = case query of
    GetLedgerTip ->
      CBOR.encodeListLen 1 <> CBOR.encodeWord8 0
    GetEpochNo ->
      CBOR.encodeListLen 1 <> CBOR.encodeWord8 1
    GetNonMyopicMemberRewards creds ->
      CBOR.encodeListLen 2 <> CBOR.encodeWord8 2 <> toCBOR creds
    GetCurrentPParams ->
      CBOR.encodeListLen 1 <> CBOR.encodeWord8 3
    GetProposedPParamsUpdates ->
      CBOR.encodeListLen 1 <> CBOR.encodeWord8 4
    GetStakeDistribution ->
      CBOR.encodeListLen 1 <> CBOR.encodeWord8 5
    GetUTxOByAddress addrs ->
      CBOR.encodeListLen 2 <> CBOR.encodeWord8 6 <> LC.toEraCBOR @era addrs
    GetUTxOWhole ->
      CBOR.encodeListLen 1 <> CBOR.encodeWord8 7
    DebugEpochState ->
      CBOR.encodeListLen 1 <> CBOR.encodeWord8 8
    GetCBOR query' ->
      CBOR.encodeListLen 2 <> CBOR.encodeWord8 9 <> encodeShelleyQuery query'
    GetFilteredDelegationsAndRewardAccounts creds ->
      CBOR.encodeListLen 2 <> CBOR.encodeWord8 10 <> LC.toEraCBOR @era creds
    GetGenesisConfig ->
      CBOR.encodeListLen 1 <> CBOR.encodeWord8 11
    DebugNewEpochState ->
      CBOR.encodeListLen 1 <> CBOR.encodeWord8 12
    DebugChainDepState ->
      CBOR.encodeListLen 1 <> CBOR.encodeWord8 13
    GetRewardProvenance ->
      CBOR.encodeListLen 1 <> CBOR.encodeWord8 14
    GetUTxOByTxIn txins ->
      CBOR.encodeListLen 2 <> CBOR.encodeWord8 15 <> LC.toEraCBOR @era txins
    GetStakePools ->
      CBOR.encodeListLen 1 <> CBOR.encodeWord8 16
    GetStakePoolParams poolids ->
      CBOR.encodeListLen 2 <> CBOR.encodeWord8 17 <> toCBOR poolids
    GetRewardInfoPools ->
      CBOR.encodeListLen 1 <> CBOR.encodeWord8 18
    GetPoolState poolids ->
      CBOR.encodeListLen 2 <> CBOR.encodeWord8 19 <> toCBOR poolids
    GetStakeSnapshots poolId ->
      CBOR.encodeListLen 2 <> CBOR.encodeWord8 20 <> toCBOR poolId
    GetPoolDistr poolids ->
      CBOR.encodeListLen 2 <> CBOR.encodeWord8 21 <> toCBOR poolids
    GetStakeDelegDeposits stakeCreds ->
      CBOR.encodeListLen 2 <> CBOR.encodeWord8 22 <> toCBOR stakeCreds
    GetConstitution ->
      CBOR.encodeListLen 1 <> CBOR.encodeWord8 23
    GetGovState ->
      CBOR.encodeListLen 1 <> CBOR.encodeWord8 24
    GetDRepState drepCreds ->
      CBOR.encodeListLen 2 <> CBOR.encodeWord8 25 <> toCBOR drepCreds
    GetDRepStakeDistr dreps ->
      CBOR.encodeListLen 2 <> CBOR.encodeWord8 26 <> LC.toEraCBOR @era dreps
    GetCommitteeMembersState coldCreds hotCreds statuses ->
      CBOR.encodeListLen 4 <> CBOR.encodeWord8 27 <> toCBOR coldCreds <> toCBOR hotCreds <> LC.toEraCBOR @era statuses
    GetFilteredVoteDelegatees stakeCreds ->
      CBOR.encodeListLen 2 <> CBOR.encodeWord8 28 <> LC.toEraCBOR @era stakeCreds
    GetAccountState ->
      CBOR.encodeListLen 1 <> CBOR.encodeWord8 29
    GetSPOStakeDistr keys ->
      CBOR.encodeListLen 2 <> CBOR.encodeWord8 30 <> LC.toEraCBOR @era keys
    GetProposals gids ->
      CBOR.encodeListLen 2 <> CBOR.encodeWord8 31 <> LC.toEraCBOR @era gids
    GetRatifyState ->
      CBOR.encodeListLen 1 <> CBOR.encodeWord8 32
    GetFuturePParams ->
      CBOR.encodeListLen 1 <> CBOR.encodeWord8 33
    GetBigLedgerPeerSnapshot ->
      CBOR.encodeListLen 1 <> CBOR.encodeWord8 34

decodeShelleyQuery ::
     forall era proto. ShelleyBasedEra era
  => forall s. Decoder s (SomeSecond BlockQuery (ShelleyBlock proto era))
decodeShelleyQuery = do
    len <- CBOR.decodeListLen
    tag <- CBOR.decodeWord8

    let failmsg :: forall s ans. String -> Decoder s ans
        failmsg msg = fail $
            "decodeShelleyQuery: " <> msg <> " (len, tag) = (" <>
            show len <> ", " <> show tag <> ")"

        requireCG ::
             forall s ans.
             (CG.ConwayEraGov era => Decoder s ans)
          -> Decoder s ans
        requireCG k = case SE.getConwayEraGovDict (Proxy @era) of
            Just SE.ConwayEraGovDict -> k
            Nothing                  -> failmsg "that query is not supported before Conway,"

    case (len, tag) of
      (1, 0)  ->             return $ SomeSecond GetLedgerTip
      (1, 1)  ->             return $ SomeSecond GetEpochNo
      (2, 2)  ->             SomeSecond . GetNonMyopicMemberRewards <$> fromCBOR
      (1, 3)  ->             return $ SomeSecond GetCurrentPParams
      (1, 4)  ->             return $ SomeSecond GetProposedPParamsUpdates
      (1, 5)  ->             return $ SomeSecond GetStakeDistribution
      (2, 6)  ->             SomeSecond . GetUTxOByAddress <$> LC.fromEraCBOR @era
      (1, 7)  ->             return $ SomeSecond GetUTxOWhole
      (1, 8)  ->             return $ SomeSecond DebugEpochState
      (2, 9)  ->             (\(SomeSecond q) -> SomeSecond (GetCBOR q)) <$> decodeShelleyQuery
      (2, 10) ->             SomeSecond . GetFilteredDelegationsAndRewardAccounts <$> LC.fromEraCBOR @era
      (1, 11) ->             return $ SomeSecond GetGenesisConfig
      (1, 12) ->             return $ SomeSecond DebugNewEpochState
      (1, 13) ->             return $ SomeSecond DebugChainDepState
      (1, 14) ->             return $ SomeSecond GetRewardProvenance
      (2, 15) ->             SomeSecond . GetUTxOByTxIn <$> LC.fromEraCBOR @era
      (1, 16) ->             return $ SomeSecond GetStakePools
      (2, 17) ->             SomeSecond . GetStakePoolParams <$> fromCBOR
      (1, 18) ->             return $ SomeSecond GetRewardInfoPools
      (2, 19) ->             SomeSecond . GetPoolState <$> fromCBOR
      (2, 20) ->             SomeSecond . GetStakeSnapshots <$> fromCBOR
      (2, 21) ->             SomeSecond . GetPoolDistr <$> fromCBOR
      (2, 22) ->             SomeSecond . GetStakeDelegDeposits <$> fromCBOR
      (1, 23) -> requireCG $ return $ SomeSecond GetConstitution
      (1, 24) ->             return $ SomeSecond GetGovState
      (2, 25) -> requireCG $ SomeSecond . GetDRepState <$> fromCBOR
      (2, 26) -> requireCG $ SomeSecond . GetDRepStakeDistr <$> LC.fromEraCBOR @era
      (4, 27) -> requireCG $ do
        coldCreds <- fromCBOR
        hotCreds <- fromCBOR
        statuses <- LC.fromEraCBOR @era
        return $ SomeSecond $ GetCommitteeMembersState coldCreds hotCreds statuses
      (2, 28) -> requireCG $ do
        SomeSecond . GetFilteredVoteDelegatees <$> LC.fromEraCBOR @era
      (1, 29) ->             return $ SomeSecond GetAccountState
      (2, 30) -> requireCG $ SomeSecond . GetSPOStakeDistr <$> LC.fromEraCBOR @era
      (2, 31) -> requireCG $ SomeSecond . GetProposals <$> LC.fromEraCBOR @era
      (1, 32) -> requireCG $ return $ SomeSecond GetRatifyState
      (1, 33) -> requireCG $ return $ SomeSecond GetFuturePParams
      (1, 34) ->             return $ SomeSecond GetBigLedgerPeerSnapshot
      _       -> failmsg "invalid"

encodeShelleyResult ::
     forall proto era result. ShelleyCompatible proto era
  => ShelleyNodeToClientVersion
  -> BlockQuery (ShelleyBlock proto era) result -> result -> Encoding
encodeShelleyResult _v query = case query of
    GetLedgerTip                               -> encodePoint encode
    GetEpochNo                                 -> toCBOR
    GetNonMyopicMemberRewards {}               -> toCBOR
    GetCurrentPParams                          -> toCBOR
    GetProposedPParamsUpdates                  -> toCBOR
    GetStakeDistribution                       -> LC.toEraCBOR @era
    GetUTxOByAddress {}                        -> toCBOR
    GetUTxOWhole                               -> toCBOR
    DebugEpochState                            -> toCBOR
    GetCBOR {}                                 -> encode
    GetFilteredDelegationsAndRewardAccounts {} -> LC.toEraCBOR @era
    GetGenesisConfig                           -> toCBOR
    DebugNewEpochState                         -> toCBOR
    DebugChainDepState                         -> encode
    GetRewardProvenance                        -> LC.toEraCBOR @era
    GetUTxOByTxIn {}                           -> toCBOR
    GetStakePools                              -> toCBOR
    GetStakePoolParams {}                      -> LC.toEraCBOR @era
    GetRewardInfoPools                         -> LC.toEraCBOR @era
    GetPoolState {}                            -> LC.toEraCBOR @era
    GetStakeSnapshots {}                       -> toCBOR
    GetPoolDistr {}                            -> LC.toEraCBOR @era
    GetStakeDelegDeposits {}                   -> LC.toEraCBOR @era
    GetConstitution                            -> toCBOR
    GetGovState                                -> toCBOR
    GetDRepState {}                            -> LC.toEraCBOR @era
    GetDRepStakeDistr {}                       -> LC.toEraCBOR @era
    GetCommitteeMembersState {}                -> LC.toEraCBOR @era
    GetFilteredVoteDelegatees {}               -> LC.toEraCBOR @era
    GetAccountState {}                         -> LC.toEraCBOR @era
    GetSPOStakeDistr {}                        -> LC.toEraCBOR @era
    GetProposals {}                            -> LC.toEraCBOR @era
    GetRatifyState {}                          -> LC.toEraCBOR @era
    GetFuturePParams {}                        -> LC.toEraCBOR @era
    GetBigLedgerPeerSnapshot                   -> toCBOR

decodeShelleyResult ::
     forall proto era result. ShelleyCompatible proto era
  => ShelleyNodeToClientVersion
  -> BlockQuery (ShelleyBlock proto era) result
  -> forall s. Decoder s result
decodeShelleyResult _v query = case query of
    GetLedgerTip                               -> decodePoint decode
    GetEpochNo                                 -> fromCBOR
    GetNonMyopicMemberRewards {}               -> fromCBOR
    GetCurrentPParams                          -> fromCBOR
    GetProposedPParamsUpdates                  -> fromCBOR
    GetStakeDistribution                       -> LC.fromEraCBOR @era
    GetUTxOByAddress {}                        -> fromCBOR
    GetUTxOWhole                               -> fromCBOR
    DebugEpochState                            -> fromCBOR
    GetCBOR {}                                 -> decode
    GetFilteredDelegationsAndRewardAccounts {} -> LC.fromEraCBOR @era
    GetGenesisConfig                           -> fromCBOR
    DebugNewEpochState                         -> fromCBOR
    DebugChainDepState                         -> decode
    GetRewardProvenance                        -> LC.fromEraCBOR @era
    GetUTxOByTxIn {}                           -> fromCBOR
    GetStakePools                              -> fromCBOR
    GetStakePoolParams {}                      -> LC.fromEraCBOR @era
    GetRewardInfoPools                         -> LC.fromEraCBOR @era
    GetPoolState {}                            -> LC.fromEraCBOR @era
    GetStakeSnapshots {}                       -> fromCBOR
    GetPoolDistr {}                            -> LC.fromEraCBOR @era
    GetStakeDelegDeposits {}                   -> LC.fromEraCBOR @era
    GetConstitution                            -> fromCBOR
    GetGovState                                -> fromCBOR
    GetDRepState {}                            -> LC.fromEraCBOR @era
    GetDRepStakeDistr {}                       -> LC.fromEraCBOR @era
    GetCommitteeMembersState {}                -> LC.fromEraCBOR @era
    GetFilteredVoteDelegatees {}               -> LC.fromEraCBOR @era
    GetAccountState {}                         -> LC.fromEraCBOR @era
    GetSPOStakeDistr {}                        -> LC.fromEraCBOR @era
    GetProposals {}                            -> LC.fromEraCBOR @era
    GetRatifyState {}                          -> LC.fromEraCBOR @era
    GetFuturePParams {}                        -> LC.fromEraCBOR @era
    GetBigLedgerPeerSnapshot                   -> fromCBOR

-- | The stake snapshot returns information about the mark, set, go ledger snapshots for a pool,
-- plus the total active stake for each snapshot that can be used in a 'sigma' calculation.
--
-- Each snapshot is taken at the end of a different era. The go snapshot is the current one and
-- was taken two epochs earlier, set was taken one epoch ago, and mark was taken immediately
-- before the start of the current epoch.
data StakeSnapshot crypto = StakeSnapshot
  { ssMarkPool :: !SL.Coin
  , ssSetPool  :: !SL.Coin
  , ssGoPool   :: !SL.Coin
  } deriving (Eq, Show, Generic)

instance NFData (StakeSnapshot crypto)

instance
  Crypto crypto =>
  ToCBOR (StakeSnapshot crypto)
  where
  toCBOR
    StakeSnapshot
    { ssMarkPool
    , ssSetPool
    , ssGoPool
    } = encodeListLen 3
      <> toCBOR ssMarkPool
      <> toCBOR ssSetPool
      <> toCBOR ssGoPool

instance
  Crypto crypto =>
  FromCBOR (StakeSnapshot crypto)
  where
  fromCBOR = do
    enforceSize "StakeSnapshot" 3
    StakeSnapshot
      <$> fromCBOR
      <*> fromCBOR
      <*> fromCBOR

data StakeSnapshots crypto = StakeSnapshots
  { ssStakeSnapshots  :: !(Map (SL.KeyHash 'SL.StakePool crypto) (StakeSnapshot crypto))
  , ssMarkTotal       :: !SL.Coin
  , ssSetTotal        :: !SL.Coin
  , ssGoTotal         :: !SL.Coin
  } deriving (Eq, Show, Generic)

instance NFData (StakeSnapshots crypto)

instance
  Crypto crypto =>
  ToCBOR (StakeSnapshots crypto)
  where
  toCBOR
    StakeSnapshots
    { ssStakeSnapshots
    , ssMarkTotal
    , ssSetTotal
    , ssGoTotal
    } = encodeListLen 4
      <> toCBOR ssStakeSnapshots
      <> toCBOR ssMarkTotal
      <> toCBOR ssSetTotal
      <> toCBOR ssGoTotal

instance
  Crypto crypto =>
  FromCBOR (StakeSnapshots crypto)
  where
  fromCBOR = do
    enforceSize "StakeSnapshots" 4
    StakeSnapshots
      <$> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
