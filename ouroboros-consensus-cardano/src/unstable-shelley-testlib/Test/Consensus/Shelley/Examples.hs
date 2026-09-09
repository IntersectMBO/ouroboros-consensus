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
import qualified Cardano.Protocol.Leios.BlockHeader as Leios
import qualified Cardano.Protocol.Praos.BlockHeader as Praos
import qualified Cardano.Protocol.TPraos.BlockHeader as SL
import Cardano.Slotting.EpochInfo (fixedEpochInfo)
import Cardano.Slotting.Time (mkSlotLength)
import Data.Coerce (coerce)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Map as Map
import Data.Maybe.Strict (StrictMaybe (..))
import qualified Data.Set as Set
import LeiosDemoTypes (EbAnnouncement (..), LeiosEb (..), hashLeiosEb)
import Lens.Micro
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.HeaderValidation
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Ledger.Query
import Ouroboros.Consensus.Ledger.SupportsMempool
import Ouroboros.Consensus.Ledger.Tables hiding (TxIn)
import Ouroboros.Consensus.Ledger.Tables.Utils
import Ouroboros.Consensus.Protocol.Abstract (translateChainDepState)
import Ouroboros.Consensus.Protocol.Praos (BasePraos, Praos, PraosWithLeios)
import Ouroboros.Consensus.Protocol.Praos.Common
import Ouroboros.Consensus.Protocol.TPraos
  ( TPraos
  , TPraosState (TPraosState)
  )
import Ouroboros.Consensus.Shelley.HFEras
import Ouroboros.Consensus.Shelley.Ledger
import Ouroboros.Consensus.Shelley.Ledger.Query.Types
import Ouroboros.Consensus.Shelley.Protocol.Abstract (ShelleyProtocolHeader)
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
import Test.Consensus.Protocol.Serialisation.Generators
  ( extendHeaderBodyWithLeios
  )
import Test.Consensus.Shelley.Generators (praosHeaderBodyFromTPraos)
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
        , ("GetBigLedgerPeerSnapshot", SomeBlockQuery (GetLedgerPeerSnapshot SingBigLedgerPeers))
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
              (GetLedgerPeerSnapshot SingBigLedgerPeers)
              ( LedgerPeerSnapshotV2
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
        , shelleyLedgerLatestPerasCertRound = SNothing
        , shelleyCumulativeTxBytes = 0
        }
    chainDepState = TPraosState (NotOrigin 1) pleChainDepState
    extLedgerState =
      ExtLedgerState
        ledgerState
        (genesisHeaderState chainDepState)

    ledgerConfig = exampleShelleyLedgerConfig leTranslationContext

-- | TODO Factor this out into something nicer.
fromShelleyLedgerExamplesBasePraos ::
  forall pext era.
  ( ShelleyCompatible (BasePraos pext StandardCrypto) era
  , KnownPraosExtension pext
  ) =>
  -- | Rebuild the example's TPraos header as this extension's header
  (SL.BHeader StandardCrypto -> ShelleyProtocolHeader (BasePraos pext StandardCrypto)) ->
  ProtocolLedgerExamples (SL.BHeader StandardCrypto) era ->
  Examples (ShelleyBlock (BasePraos pext StandardCrypto) era)
fromShelleyLedgerExamplesBasePraos
  translateHeader
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
        , ("GetBigLedgerPeerSnapshot", SomeBlockQuery (GetLedgerPeerSnapshot SingBigLedgerPeers))
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
              (GetLedgerPeerSnapshot SingBigLedgerPeers)
              ( LedgerPeerSnapshotV2
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
        , shelleyLedgerLatestPerasCertRound = SNothing
        , shelleyCumulativeTxBytes = 0
        }
    chainDepState =
      translateChainDepState
        (Proxy @(TPraos StandardCrypto, BasePraos pext StandardCrypto))
        $ TPraosState (NotOrigin 1) pleChainDepState
    extLedgerState =
      ExtLedgerState
        ledgerState
        (genesisHeaderState chainDepState)

    ledgerConfig = exampleShelleyLedgerConfig leTranslationContext

fromShelleyLedgerExamplesPraos ::
  ShelleyCompatible (Praos StandardCrypto) era =>
  ProtocolLedgerExamples (SL.BHeader StandardCrypto) era ->
  Examples (ShelleyBlock (Praos StandardCrypto) era)
fromShelleyLedgerExamplesPraos = fromShelleyLedgerExamplesBasePraos translatePraosHeader

fromShelleyLedgerExamplesPraosWithLeios ::
  ShelleyCompatible (PraosWithLeios StandardCrypto) era =>
  ProtocolLedgerExamples (SL.BHeader StandardCrypto) era ->
  Examples (ShelleyBlock (PraosWithLeios StandardCrypto) era)
fromShelleyLedgerExamplesPraosWithLeios =
  fromShelleyLedgerExamplesBasePraos translateLeiosHeader

-- | Rebuild a TPraos example header as a Praos one.
translatePraosHeader :: SL.BHeader StandardCrypto -> Praos.Header StandardCrypto
translatePraosHeader (SL.BHeader bhBody bhSig) =
  Praos.Header (praosHeaderBodyFromTPraos bhBody) (coerce bhSig)

-- | As 'translatePraosHeader', with the Leios fields of an example CertRB that
-- announces an endorser block of its own.
translateLeiosHeader :: SL.BHeader StandardCrypto -> Leios.Header StandardCrypto
translateLeiosHeader (SL.BHeader bhBody bhSig) =
  Leios.Header hBody (coerce bhSig)
 where
  hBody =
    extendHeaderBodyWithLeios
      (praosHeaderBodyFromTPraos bhBody)
      True
      ( SJust $
          toCodecEbAnnouncement
            EbAnnouncement
              { ebAnnouncementHash = hashLeiosEb $ MkLeiosEb mempty
              , ebAnnouncementSize = 123
              }
      )

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
examplesDijkstra =
  fromShelleyLedgerExamplesPraosWithLeios (ledgerExamplesTPraos Dijkstra.ledgerExamples)

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
