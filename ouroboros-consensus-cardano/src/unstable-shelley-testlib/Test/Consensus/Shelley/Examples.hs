{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Consensus.Shelley.Examples
  ( -- * Setup
    codecConfig
  , Shelley.testShelleyGenesis

    -- * Examples
  , examplesAllegra
  , examplesAlonzo
  , examplesBabbage
  , examplesConway
  , examplesDijkstra
  , examplesMary
  , examplesShelley
  ) where

import qualified Cardano.Ledger.BaseTypes as SL
import qualified Cardano.Ledger.Block as SL
import qualified Cardano.Ledger.Conway.Governance as CG
import qualified Cardano.Ledger.Conway.State as CG
import Cardano.Ledger.Core
import qualified Cardano.Ledger.Shelley.API as SL
import Cardano.Ledger.State (EraGov, unPoolDistr)
import Cardano.Protocol.Crypto (StandardCrypto)
import Cardano.Protocol.Praos.BlockHeader
  ( HeaderBody (HeaderBody)
  )
import qualified Cardano.Protocol.Praos.BlockHeader as Praos
import qualified Cardano.Protocol.TPraos.BlockHeader as SL
import Cardano.Slotting.EpochInfo (fixedEpochInfo)
import Cardano.Slotting.Time (mkSlotLength)
import Data.Coerce (coerce)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.HeaderValidation
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Ledger.Peras (initPerasState)
import Ouroboros.Consensus.Ledger.Query
import Ouroboros.Consensus.Ledger.SupportsMempool
import Ouroboros.Consensus.Ledger.Tables hiding (TxIn)
import Ouroboros.Consensus.Ledger.Tables.Utils
import Ouroboros.Consensus.Protocol.Abstract (translateChainDepState)
import Ouroboros.Consensus.Protocol.Praos (Praos)
import Ouroboros.Consensus.Protocol.Praos.Common
import Ouroboros.Consensus.Protocol.TPraos
  ( TPraos
  , TPraosState (TPraosState)
  )
import Ouroboros.Consensus.Shelley.HFEras
import Ouroboros.Consensus.Shelley.Ledger
import Ouroboros.Consensus.Shelley.Protocol.TPraos ()
import Ouroboros.Consensus.Storage.Serialisation
import Ouroboros.Consensus.Util.Time (secondsToNominalDiffTime)
import Ouroboros.Network.Block (Serialised (..))
import Ouroboros.Network.Magic (NetworkMagic (..))
import Ouroboros.Network.PeerSelection.LedgerPeers.Type
import Ouroboros.Network.PeerSelection.RelayAccessPoint
import qualified Test.Cardano.Ledger.Babbage.Examples as Babbage
import qualified Test.Cardano.Ledger.Conway.Examples as Conway
import qualified Test.Cardano.Ledger.Dijkstra.Examples as Dijkstra
import qualified Test.Cardano.Ledger.Shelley.Examples as Shelley
import Test.Cardano.Protocol.TPraos.Examples
  ( ProtocolLedgerExamples (..)
  , ledgerExamplesAllegra
  , ledgerExamplesAlonzo
  , ledgerExamplesMary
  , ledgerExamplesShelley
  , ledgerExamplesTPraos
  )
import Test.Util.Orphans.Arbitrary ()
import Test.Util.Serialisation.Examples
  ( Examples (..)
  , Labelled
  , labelled
  , topLevelQueries
  , unlabelled
  )
import Test.Util.Serialisation.SomeResult (SomeResult (..))

{-------------------------------------------------------------------------------
  Examples
-------------------------------------------------------------------------------}

codecConfig :: CodecConfig StandardShelleyBlock
codecConfig = ShelleyCodecConfig

{-------------------------------------------------------------------------------
  Queries

  We list every constructor of 'BlockQuery', because the CBOR tag of each one is
  ours and a golden file is the only thing that catches a tag being reordered or
  reassigned. The arguments are mostly empty: the tag and the shape of the
  argument are what we want to pin, and the example ledger state does not give
  us interesting addresses or transaction inputs.
-------------------------------------------------------------------------------}

-- | The queries an era adds to 'eraIndependentQueries'.
--
-- The argument is the pool ids that the example ledger state knows about, for
-- the queries that take one.
type EraQueries proto era =
  [SL.KeyHash SL.StakePool] ->
  Labelled (SomeBlockQuery (BlockQuery (ShelleyBlock proto era)))

-- | For the eras before Conway, which add no queries of their own.
noEraQueries :: EraQueries proto era
noEraQueries _ = mempty

-- | The queries that every Shelley-based era supports.
eraIndependentQueries ::
  EraGov era =>
  -- | Credentials to ask the non-myopic member rewards for
  Set (Either SL.Coin (SL.Credential SL.Staking)) ->
  Labelled (SomeBlockQuery (BlockQuery (ShelleyBlock proto era)))
eraIndependentQueries rewardsCredentials =
  labelled
    [ ("GetLedgerTip", SomeBlockQuery GetLedgerTip)
    , ("GetEpochNo", SomeBlockQuery GetEpochNo)
    , ("GetNonMyopicMemberRewards", SomeBlockQuery $ GetNonMyopicMemberRewards rewardsCredentials)
    , ("GetCurrentPParams", SomeBlockQuery GetCurrentPParams)
    , ("GetUTxOByAddress", SomeBlockQuery $ GetUTxOByAddress Set.empty)
    , ("GetUTxOWhole", SomeBlockQuery GetUTxOWhole)
    , ("DebugEpochState", SomeBlockQuery DebugEpochState)
    , ("GetCBOR", SomeBlockQuery (GetCBOR GetLedgerTip))
    ,
      ( "GetFilteredDelegationsAndRewardAccounts"
      , SomeBlockQuery $ GetFilteredDelegationsAndRewardAccounts Set.empty
      )
    , ("GetGenesisConfig", SomeBlockQuery GetGenesisConfig)
    , ("DebugNewEpochState", SomeBlockQuery DebugNewEpochState)
    , ("DebugChainDepState", SomeBlockQuery DebugChainDepState)
    , ("GetRewardProvenance", SomeBlockQuery GetRewardProvenance)
    , ("GetUTxOByTxIn", SomeBlockQuery $ GetUTxOByTxIn Set.empty)
    , ("GetStakePools", SomeBlockQuery GetStakePools)
    , ("GetStakePoolParams", SomeBlockQuery $ GetStakePoolParams Set.empty)
    , ("GetRewardInfoPools", SomeBlockQuery GetRewardInfoPools)
    , ("GetPoolState", SomeBlockQuery $ GetPoolState Nothing)
    , ("GetStakeSnapshots", SomeBlockQuery $ GetStakeSnapshots Nothing)
    , ("GetStakeDelegDeposits", SomeBlockQuery $ GetStakeDelegDeposits Set.empty)
    , ("GetGovState", SomeBlockQuery GetGovState)
    , ("GetAccountState", SomeBlockQuery GetAccountState)
    , ("GetFuturePParams", SomeBlockQuery GetFuturePParams)
    , ("GetBigLedgerPeerSnapshot", SomeBlockQuery (GetLedgerPeerSnapshot SingBigLedgerPeers))
    , ("GetAllLedgerPeerSnapshot", SomeBlockQuery (GetLedgerPeerSnapshot SingAllLedgerPeers))
    , ("GetPoolDistr2", SomeBlockQuery $ GetPoolDistr2 Nothing)
    , ("GetStakeDistribution2", SomeBlockQuery GetStakeDistribution2)
    , ("GetMaxMajorProtocolVersion", SomeBlockQuery GetMaxMajorProtocolVersion)
    ]

-- | The queries that Conway introduced, which the later eras also support.
conwayQueries ::
  (CG.ConwayEraGov era, CG.ConwayEraCertState era) =>
  EraQueries proto era
conwayQueries poolIds =
  labelled $
    [ ("GetConstitution", SomeBlockQuery GetConstitution)
    , ("GetDRepState", SomeBlockQuery $ GetDRepState Set.empty)
    , ("GetDRepStakeDistr", SomeBlockQuery $ GetDRepStakeDistr Set.empty)
    ,
      ( "GetCommitteeMembersState"
      , SomeBlockQuery $ GetCommitteeMembersState Set.empty Set.empty Set.empty
      )
    , ("GetFilteredVoteDelegatees", SomeBlockQuery $ GetFilteredVoteDelegatees Set.empty)
    , ("GetSPOStakeDistr", SomeBlockQuery $ GetSPOStakeDistr Set.empty)
    , ("GetProposals", SomeBlockQuery $ GetProposals Set.empty)
    , ("GetRatifyState", SomeBlockQuery GetRatifyState)
    , ("GetDRepDelegations", SomeBlockQuery $ GetDRepDelegations Set.empty)
    ]
      ++ [ ("QueryStakePoolDefaultVote", SomeBlockQuery $ QueryStakePoolDefaultVote poolId)
         | poolId <- take 1 poolIds
         ]

fromShelleyLedgerExamples ::
  ShelleyCompatible (TPraos StandardCrypto) era =>
  EraQueries (TPraos StandardCrypto) era ->
  ProtocolLedgerExamples (SL.BHeader StandardCrypto) era ->
  Examples (ShelleyBlock (TPraos StandardCrypto) era)
fromShelleyLedgerExamples
  eraQueries
  ProtocolLedgerExamples
    { pleLedgerExamples = Shelley.LedgerExamples{..}
    , ..
    } =
    Examples
      { exampleBlock = unlabelled blk
      , exampleSerialisedBlock = unlabelled serialisedBlock
      , exampleHeader = unlabelled $ getHeader blk
      , exampleSerialisedHeader = unlabelled serialisedHeader
      , exampleHeaderHash = unlabelled hash
      , exampleGenTx = unlabelled tx
      , exampleGenTxId = unlabelled $ txId tx
      , exampleApplyTxErr = unlabelled leApplyTxError
      , exampleQuery = queries
      , exampleTopLevelQuery = topLevelQueries
      , exampleResult = results
      , exampleAnnTip = unlabelled annTip
      , exampleLedgerState = unlabelled ledgerState
      , exampleChainDepState = unlabelled chainDepState
      , exampleExtLedgerState = unlabelled extLedgerState
      , exampleSlotNo = unlabelled slotNo
      , exampleLedgerConfig = unlabelled ledgerConfig
      }
   where
    emptyTx = mkBasicTx mkBasicTxBody
    blk = mkShelleyBlock pleBlock
    hash = ShelleyHash $ SL.unHashHeader pleHashHeader
    serialisedBlock = Serialised "<BLOCK>"
    tx = mkShelleyTx emptyTx
    slotNo = SlotNo 42
    serialisedHeader =
      SerialisedHeaderFromDepPair $ GenDepPair (NestedCtxt CtxtShelley) (Serialised "<HEADER>")
    queries =
      eraIndependentQueries leRewardsCredentials
        <> eraQueries (Map.keys (unPoolDistr lePoolDistr))
    results =
      labelled
        [ ("LedgerTip", SomeResult GetLedgerTip (blockPoint blk))
        , ("GenesisConfig", SomeResult GetGenesisConfig (compactGenesis leShelleyGenesis))
        ,
          ( "GetBigLedgerPeerSnapshot"
          , SomeResult
              (GetLedgerPeerSnapshot SingBigLedgerPeers)
              ( LedgerBigPeerSnapshotV23
                  (BlockPoint slotNo (RawBlockHash "<BLOCK HASH, padded to 32 bytes>"))
                  (NetworkMagic 42)
                  [
                    ( AccPoolStake 0.9
                    ,
                      ( PoolStake 0.9
                      , LedgerRelayAccessAddress (IPv4 "1.1.1.1") 1234 :| []
                      )
                    )
                  ]
              )
          )
        ,
          ( "GetAllLedgerPeerSnapshot"
          , SomeResult
              (GetLedgerPeerSnapshot SingAllLedgerPeers)
              ( LedgerAllPeerSnapshotV23
                  (BlockPoint slotNo (RawBlockHash "<BLOCK HASH, padded to 32 bytes>"))
                  (NetworkMagic 42)
                  [
                    ( PoolStake 0.9
                    , LedgerRelayAccessAddress (IPv4 "1.1.1.1") 1234 :| []
                    )
                  ]
              )
          )
        ,
          ( "GetBigLedgerPeerSnapshotAtOrigin"
          , SomeResult
              (GetLedgerPeerSnapshot SingBigLedgerPeers)
              (LedgerBigPeerSnapshotV23 GenesisPoint (NetworkMagic 42) [])
          )
        ,
          ( "MaxMajorProtocolVersion"
          , SomeResult GetMaxMajorProtocolVersion $ MaxMajorProtVer (maxBound @SL.Version)
          )
        ]
    annTip =
      AnnTip
        { annTipSlotNo = SlotNo 14
        , annTipBlockNo = BlockNo 6
        , annTipInfo = hash
        }
    ledgerState =
      ShelleyLedgerState
        { shelleyLedgerTip =
            NotOrigin
              ShelleyTip
                { shelleyTipSlotNo = SlotNo 9
                , shelleyTipBlockNo = BlockNo 3
                , shelleyTipHash = hash
                }
        , shelleyLedgerState = leNewEpochState
        , shelleyLedgerTransition = ShelleyTransitionInfo{shelleyAfterVoting = 0}
        , shelleyLedgerTables = LedgerTables EmptyMK
        }
    chainDepState = TPraosState (NotOrigin 1) pleChainDepState
    extLedgerState =
      let headerState = genesisHeaderState chainDepState
          perasState = initPerasState ledgerConfig ledgerState headerState
       in ExtLedgerState
            { ledgerState
            , headerState
            , perasState
            }

    ledgerConfig = exampleShelleyLedgerConfig leTranslationContext

-- | TODO Factor this out into something nicer.
fromShelleyLedgerExamplesPraos ::
  forall era.
  ShelleyCompatible (Praos StandardCrypto) era =>
  EraQueries (Praos StandardCrypto) era ->
  ProtocolLedgerExamples (SL.BHeader StandardCrypto) era ->
  Examples (ShelleyBlock (Praos StandardCrypto) era)
fromShelleyLedgerExamplesPraos
  eraQueries
  ProtocolLedgerExamples
    { pleLedgerExamples = Shelley.LedgerExamples{..}
    , ..
    } =
    Examples
      { exampleBlock = unlabelled blk
      , exampleSerialisedBlock = unlabelled serialisedBlock
      , exampleHeader = unlabelled $ getHeader blk
      , exampleSerialisedHeader = unlabelled serialisedHeader
      , exampleHeaderHash = unlabelled hash
      , exampleGenTx = unlabelled tx
      , exampleGenTxId = unlabelled $ txId tx
      , exampleApplyTxErr = unlabelled leApplyTxError
      , exampleQuery = queries
      , exampleTopLevelQuery = topLevelQueries
      , exampleResult = results
      , exampleAnnTip = unlabelled annTip
      , exampleLedgerState = unlabelled ledgerState
      , exampleChainDepState = unlabelled chainDepState
      , exampleExtLedgerState = unlabelled extLedgerState
      , exampleSlotNo = unlabelled slotNo
      , exampleLedgerConfig = unlabelled ledgerConfig
      }
   where
    emptyTx = mkBasicTx mkBasicTxBody
    blk =
      mkShelleyBlock $
        let SL.Block hdr1 bdy = pleBlock
         in SL.Block (translateHeader hdr1) bdy

    translateHeader :: SL.BHeader StandardCrypto -> Praos.Header StandardCrypto
    translateHeader (SL.BHeader bhBody bhSig) =
      Praos.Header hBody hSig
     where
      hBody =
        HeaderBody
          { hbBlockNo = SL.bheaderBlockNo bhBody
          , hbSlotNo = SL.bheaderSlotNo bhBody
          , hbPrev = SL.bheaderPrev bhBody
          , hbVk = SL.bheaderVk bhBody
          , hbVrfVk = SL.bheaderVrfVk bhBody
          , hbVrfRes = coerce $ SL.bheaderEta bhBody
          , hbBodySize = SL.bsize bhBody
          , hbBodyHash = SL.bhash bhBody
          , hbOCert = SL.bheaderOCert bhBody
          , hbProtVer = SL.bprotver bhBody
          }
      hSig = coerce bhSig
    hash = ShelleyHash $ SL.unHashHeader pleHashHeader
    serialisedBlock = Serialised "<BLOCK>"
    tx = mkShelleyTx emptyTx
    slotNo = SlotNo 42
    serialisedHeader =
      SerialisedHeaderFromDepPair $ GenDepPair (NestedCtxt CtxtShelley) (Serialised "<HEADER>")
    queries =
      eraIndependentQueries leRewardsCredentials
        <> eraQueries (Map.keys (unPoolDistr lePoolDistr))
    results =
      labelled
        [ ("LedgerTip", SomeResult GetLedgerTip (blockPoint blk))
        , ("GenesisConfig", SomeResult GetGenesisConfig (compactGenesis leShelleyGenesis))
        ,
          ( "GetBigLedgerPeerSnapshot"
          , SomeResult
              (GetLedgerPeerSnapshot SingBigLedgerPeers)
              ( LedgerBigPeerSnapshotV23
                  (BlockPoint slotNo (RawBlockHash "<BLOCK HASH, padded to 32 bytes>"))
                  (NetworkMagic 42)
                  [
                    ( AccPoolStake 0.9
                    ,
                      ( PoolStake 0.9
                      , LedgerRelayAccessAddress (IPv4 "1.1.1.1") 1234 :| []
                      )
                    )
                  ]
              )
          )
        ,
          ( "GetAllLedgerPeerSnapshot"
          , SomeResult
              (GetLedgerPeerSnapshot SingAllLedgerPeers)
              ( LedgerAllPeerSnapshotV23
                  (BlockPoint slotNo (RawBlockHash "<BLOCK HASH, padded to 32 bytes>"))
                  (NetworkMagic 42)
                  [
                    ( PoolStake 0.9
                    , LedgerRelayAccessAddress (IPv4 "1.1.1.1") 1234 :| []
                    )
                  ]
              )
          )
        ,
          ( "GetBigLedgerPeerSnapshotAtOrigin"
          , SomeResult
              (GetLedgerPeerSnapshot SingBigLedgerPeers)
              (LedgerBigPeerSnapshotV23 GenesisPoint (NetworkMagic 42) [])
          )
        ,
          ( "MaxMajorProtocolVersion"
          , SomeResult GetMaxMajorProtocolVersion $ MaxMajorProtVer (maxBound @SL.Version)
          )
        ]
    annTip =
      AnnTip
        { annTipSlotNo = SlotNo 14
        , annTipBlockNo = BlockNo 6
        , annTipInfo = hash
        }
    ledgerState =
      ShelleyLedgerState
        { shelleyLedgerTip =
            NotOrigin
              ShelleyTip
                { shelleyTipSlotNo = SlotNo 9
                , shelleyTipBlockNo = BlockNo 3
                , shelleyTipHash = hash
                }
        , shelleyLedgerState = leNewEpochState
        , shelleyLedgerTransition = ShelleyTransitionInfo{shelleyAfterVoting = 0}
        , shelleyLedgerTables = emptyLedgerTables
        }
    chainDepState =
      translateChainDepState (Proxy @(TPraos StandardCrypto, Praos StandardCrypto)) $
        TPraosState (NotOrigin 1) pleChainDepState
    extLedgerState =
      let headerState = genesisHeaderState chainDepState
          perasState = initPerasState ledgerConfig ledgerState headerState
       in ExtLedgerState
            { ledgerState
            , headerState
            , perasState
            }

    ledgerConfig = exampleShelleyLedgerConfig leTranslationContext

examplesShelley :: Examples StandardShelleyBlock
examplesShelley = fromShelleyLedgerExamples noEraQueries ledgerExamplesShelley

examplesAllegra :: Examples StandardAllegraBlock
examplesAllegra = fromShelleyLedgerExamples noEraQueries ledgerExamplesAllegra

examplesMary :: Examples StandardMaryBlock
examplesMary = fromShelleyLedgerExamples noEraQueries ledgerExamplesMary

examplesAlonzo :: Examples StandardAlonzoBlock
examplesAlonzo = fromShelleyLedgerExamples noEraQueries ledgerExamplesAlonzo

examplesBabbage :: Examples StandardBabbageBlock
examplesBabbage = fromShelleyLedgerExamplesPraos noEraQueries (ledgerExamplesTPraos Babbage.ledgerExamples)

examplesConway :: Examples StandardConwayBlock
examplesConway = fromShelleyLedgerExamplesPraos conwayQueries (ledgerExamplesTPraos Conway.ledgerExamples)

examplesDijkstra :: Examples StandardDijkstraBlock
examplesDijkstra = fromShelleyLedgerExamplesPraos conwayQueries (ledgerExamplesTPraos Dijkstra.ledgerExamples)

exampleShelleyLedgerConfig :: TranslationContext era -> ShelleyLedgerConfig era
exampleShelleyLedgerConfig translationContext =
  ShelleyLedgerConfig
    { shelleyLedgerCompactGenesis = compactGenesis Shelley.testShelleyGenesis
    , shelleyLedgerGlobals =
        SL.mkShelleyGlobals
          Shelley.testShelleyGenesis
          epochInfo
    , shelleyLedgerTranslationContext = translationContext
    }
 where
  epochInfo = fixedEpochInfo (EpochSize 4) slotLength
  slotLength = mkSlotLength (secondsToNominalDiffTime 7)
