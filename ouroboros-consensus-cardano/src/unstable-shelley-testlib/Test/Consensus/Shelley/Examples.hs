{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

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
import Cardano.Ledger.Core
import qualified Cardano.Ledger.Core as LC
import qualified Cardano.Ledger.Shelley.API as SL
import Cardano.Protocol.Crypto (StandardCrypto)
import qualified Cardano.Protocol.TPraos.BHeader as SL
import Cardano.Slotting.EpochInfo (fixedEpochInfo)
import Cardano.Slotting.Time (mkSlotLength)
import Data.Coerce (coerce)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Lens.Micro
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.HeaderValidation
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Ledger.Query
import Ouroboros.Consensus.Ledger.SupportsMempool
import Ouroboros.Consensus.Ledger.Tables hiding (TxIn)
import Ouroboros.Consensus.Ledger.Tables.Utils
import Ouroboros.Consensus.Protocol.Abstract (translateChainDepState)
import Ouroboros.Consensus.Protocol.Praos (Praos)
import Ouroboros.Consensus.Protocol.Praos.Common
import Ouroboros.Consensus.Protocol.Praos.Header
  ( HeaderBody (HeaderBody)
  )
import qualified Ouroboros.Consensus.Protocol.Praos.Header as Praos
import Ouroboros.Consensus.Protocol.TPraos
  ( TPraos
  , TPraosState (TPraosState)
  )
import Ouroboros.Consensus.Shelley.HFEras
import Ouroboros.Consensus.Shelley.Ledger
import Ouroboros.Consensus.Shelley.Ledger.Query.Types
import Ouroboros.Consensus.Shelley.Protocol.TPraos ()
import Ouroboros.Consensus.Storage.Serialisation
import Ouroboros.Consensus.Util.Time (secondsToNominalDiffTime)
import Ouroboros.Network.Block (Serialised (..))
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
  , labelled
  , unlabelled
  )
import Test.Util.Serialisation.SomeResult (SomeResult (..))

{-------------------------------------------------------------------------------
  Examples
-------------------------------------------------------------------------------}

codecConfig :: CodecConfig StandardShelleyBlock
codecConfig = ShelleyCodecConfig

mkLedgerTables ::
  forall proto era.
  ShelleyCompatible proto era =>
  LC.Tx LC.TopTx era ->
  LedgerTables (LedgerState (ShelleyBlock proto era)) ValuesMK
mkLedgerTables tx =
  LedgerTables $
    ValuesMK $
      Map.fromList $
        zip exampleTxIns exampleTxOuts
 where
  exampleTxIns :: [BigEndianTxIn]
  exampleTxIns = case toList (tx ^. (LC.bodyTxL . LC.allInputsTxBodyF)) of
    [] -> error "No transaction inputs were provided to construct the ledger tables"
    -- We require at least one transaction input (and one
    -- transaction output) in the example provided by
    -- cardano-ledger to make sure that we test the serialization
    -- of ledger tables with at least one non-trivial example.
    --
    -- Also all transactions in Cardano have at least one input for
    -- automatic replay protection.
    xs -> map BigEndianTxIn xs

  exampleTxOuts :: [LC.TxOut era]
  exampleTxOuts = case toList (tx ^. (LC.bodyTxL . LC.outputsTxBodyL)) of
    [] -> error "No transaction outputs were provided to construct the ledger tables"
    xs -> xs

fromShelleyLedgerExamples ::
  ShelleyCompatible (TPraos StandardCrypto) era =>
  ProtocolLedgerExamples (SL.BHeader StandardCrypto) era ->
  Examples (ShelleyBlock (TPraos StandardCrypto) era)
fromShelleyLedgerExamples
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
      , exampleResult = results
      , exampleAnnTip = unlabelled annTip
      , exampleLedgerState = unlabelled ledgerState
      , exampleChainDepState = unlabelled chainDepState
      , exampleExtLedgerState = unlabelled extLedgerState
      , exampleSlotNo = unlabelled slotNo
      , exampleLedgerConfig = unlabelled ledgerConfig
      , exampleLedgerTables = unlabelled $ mkLedgerTables leTx
      }
   where
    blk = mkShelleyBlock pleBlock
    hash = ShelleyHash $ SL.unHashHeader pleHashHeader
    serialisedBlock = Serialised "<BLOCK>"
    tx = mkShelleyTx leTx
    slotNo = SlotNo 42
    serialisedHeader =
      SerialisedHeaderFromDepPair $ GenDepPair (NestedCtxt CtxtShelley) (Serialised "<HEADER>")
    queries =
      labelled
        [ ("GetLedgerTip", SomeBlockQuery GetLedgerTip)
        , ("GetEpochNo", SomeBlockQuery GetEpochNo)
        , ("GetCurrentPParams", SomeBlockQuery GetCurrentPParams)
        , ("GetStakeDistribution", SomeBlockQuery GetStakeDistribution)
        , ("GetNonMyopicMemberRewards", SomeBlockQuery $ GetNonMyopicMemberRewards leRewardsCredentials)
        , ("GetGenesisConfig", SomeBlockQuery GetGenesisConfig)
        , ("GetBigLedgerPeerSnapshot", SomeBlockQuery GetBigLedgerPeerSnapshot)
        , ("GetStakeDistribution2", SomeBlockQuery GetStakeDistribution2)
        , ("GetMaxMajorProtocolVersion", SomeBlockQuery GetMaxMajorProtocolVersion)
        ]
    results =
      labelled
        [ ("LedgerTip", SomeResult GetLedgerTip (blockPoint blk))
        , ("EpochNo", SomeResult GetEpochNo (EpochNo 10))
        , ("EmptyPParams", SomeResult GetCurrentPParams lePParams)
        , ("StakeDistribution", SomeResult GetStakeDistribution $ fromLedgerPoolDistr lePoolDistr)
        ,
          ( "NonMyopicMemberRewards"
          , SomeResult
              (GetNonMyopicMemberRewards Set.empty)
              (NonMyopicMemberRewards $ leNonMyopicRewards)
          )
        , ("GenesisConfig", SomeResult GetGenesisConfig (compactGenesis leShelleyGenesis))
        ,
          ( "GetBigLedgerPeerSnapshot"
          , SomeResult
              GetBigLedgerPeerSnapshot
              ( LedgerPeerSnapshot
                  ( NotOrigin slotNo
                  ,
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
          )
        , ("StakeDistribution2", SomeResult GetStakeDistribution2 lePoolDistr)
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
      ExtLedgerState
        ledgerState
        (genesisHeaderState chainDepState)

    ledgerConfig = exampleShelleyLedgerConfig leTranslationContext

-- | TODO Factor this out into something nicer.
fromShelleyLedgerExamplesPraos ::
  forall era.
  ShelleyCompatible (Praos StandardCrypto) era =>
  ProtocolLedgerExamples (SL.BHeader StandardCrypto) era ->
  Examples (ShelleyBlock (Praos StandardCrypto) era)
fromShelleyLedgerExamplesPraos
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
      , exampleResult = results
      , exampleAnnTip = unlabelled annTip
      , exampleLedgerState = unlabelled ledgerState
      , exampleLedgerTables = unlabelled $ mkLedgerTables leTx
      , exampleChainDepState = unlabelled chainDepState
      , exampleExtLedgerState = unlabelled extLedgerState
      , exampleSlotNo = unlabelled slotNo
      , exampleLedgerConfig = unlabelled ledgerConfig
      }
   where
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
    tx = mkShelleyTx leTx
    slotNo = SlotNo 42
    serialisedHeader =
      SerialisedHeaderFromDepPair $ GenDepPair (NestedCtxt CtxtShelley) (Serialised "<HEADER>")
    queries =
      labelled
        [ ("GetLedgerTip", SomeBlockQuery GetLedgerTip)
        , ("GetEpochNo", SomeBlockQuery GetEpochNo)
        , ("GetCurrentPParams", SomeBlockQuery GetCurrentPParams)
        , ("GetStakeDistribution", SomeBlockQuery GetStakeDistribution)
        , ("GetNonMyopicMemberRewards", SomeBlockQuery $ GetNonMyopicMemberRewards leRewardsCredentials)
        , ("GetGenesisConfig", SomeBlockQuery GetGenesisConfig)
        , ("GetBigLedgerPeerSnapshot", SomeBlockQuery GetBigLedgerPeerSnapshot)
        , ("GetStakeDistribution2", SomeBlockQuery GetStakeDistribution2)
        , ("GetMaxMajorProtocolVersion", SomeBlockQuery GetMaxMajorProtocolVersion)
        ]
    results =
      labelled
        [ ("LedgerTip", SomeResult GetLedgerTip (blockPoint blk))
        , ("EpochNo", SomeResult GetEpochNo (EpochNo 10))
        , ("EmptyPParams", SomeResult GetCurrentPParams lePParams)
        , ("StakeDistribution", SomeResult GetStakeDistribution $ fromLedgerPoolDistr lePoolDistr)
        ,
          ( "NonMyopicMemberRewards"
          , SomeResult
              (GetNonMyopicMemberRewards Set.empty)
              (NonMyopicMemberRewards $ leNonMyopicRewards)
          )
        , ("GenesisConfig", SomeResult GetGenesisConfig (compactGenesis leShelleyGenesis))
        ,
          ( "GetBigLedgerPeerSnapshot"
          , SomeResult
              GetBigLedgerPeerSnapshot
              ( LedgerPeerSnapshot
                  ( NotOrigin slotNo
                  ,
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
          )
        , ("StakeDistribution2", SomeResult GetStakeDistribution2 lePoolDistr)
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
      ExtLedgerState
        ledgerState
        (genesisHeaderState chainDepState)

    ledgerConfig = exampleShelleyLedgerConfig leTranslationContext

examplesShelley :: Examples StandardShelleyBlock
examplesShelley = fromShelleyLedgerExamples ledgerExamplesShelley

examplesAllegra :: Examples StandardAllegraBlock
examplesAllegra = fromShelleyLedgerExamples ledgerExamplesAllegra

examplesMary :: Examples StandardMaryBlock
examplesMary = fromShelleyLedgerExamples ledgerExamplesMary

examplesAlonzo :: Examples StandardAlonzoBlock
examplesAlonzo = fromShelleyLedgerExamples ledgerExamplesAlonzo

examplesBabbage :: Examples StandardBabbageBlock
examplesBabbage = fromShelleyLedgerExamplesPraos (ledgerExamplesTPraos Babbage.ledgerExamples)

examplesConway :: Examples StandardConwayBlock
examplesConway = fromShelleyLedgerExamplesPraos (ledgerExamplesTPraos Conway.ledgerExamples)

examplesDijkstra :: Examples StandardDijkstraBlock
examplesDijkstra = fromShelleyLedgerExamplesPraos (ledgerExamplesTPraos Dijkstra.ledgerExamples)

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
