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

import qualified Cardano.Crypto.Hash as Hash
import qualified Cardano.Ledger.BaseTypes as SL
import qualified Cardano.Ledger.Block as SL
import Cardano.Ledger.Core
import Cardano.Ledger.Hashes (unsafeMakeSafeHash)
import qualified Cardano.Ledger.Shelley.API as SL
import Cardano.Protocol.Crypto (StandardCrypto)
import qualified Cardano.Protocol.Leios.BlockHeader as Leios
import Cardano.Protocol.Praos.BlockHeader
  ( HeaderBody (HeaderBody)
  )
import qualified Cardano.Protocol.Praos.BlockHeader as Praos
import qualified Cardano.Protocol.TPraos.BlockHeader as SL
import Cardano.Slotting.EpochInfo (fixedEpochInfo)
import Cardano.Slotting.Time (mkSlotLength)
import Data.Coerce (coerce)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.HeaderValidation
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Ledger.Peras (initPerasState)
import Ouroboros.Consensus.Ledger.Query
import Ouroboros.Consensus.Ledger.SupportsMempool
import Ouroboros.Consensus.Ledger.Tables hiding (TxIn)
import Ouroboros.Consensus.Ledger.Tables.Utils
import Ouroboros.Consensus.Protocol.Abstract (TranslateProto, translateChainDepState)
import Ouroboros.Consensus.Protocol.Praos (Praos)
import Ouroboros.Consensus.Protocol.Praos.Common
import Ouroboros.Consensus.Protocol.Praos2 (Praos2)
import Ouroboros.Consensus.Protocol.TPraos
  ( TPraos
  , TPraosState (TPraosState)
  )
import Ouroboros.Consensus.Shelley.Eras (DijkstraEra)
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
  , labelled
  , unlabelled
  )
import Test.Util.Serialisation.SomeResult (SomeResult (..))

{-------------------------------------------------------------------------------
  Examples
-------------------------------------------------------------------------------}

codecConfig :: CodecConfig StandardShelleyBlock
codecConfig = ShelleyCodecConfig

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
      labelled
        [ ("GetLedgerTip", SomeBlockQuery GetLedgerTip)
        , ("GetEpochNo", SomeBlockQuery GetEpochNo)
        , ("GetCurrentPParams", SomeBlockQuery GetCurrentPParams)
        , ("GetNonMyopicMemberRewards", SomeBlockQuery $ GetNonMyopicMemberRewards leRewardsCredentials)
        , ("GetGenesisConfig", SomeBlockQuery GetGenesisConfig)
        , ("GetBigLedgerPeerSnapshot", SomeBlockQuery (GetLedgerPeerSnapshot SingBigLedgerPeers))
        , ("GetAllLedgerPeerSnapshot", SomeBlockQuery (GetLedgerPeerSnapshot SingAllLedgerPeers))
        , ("GetStakeDistribution2", SomeBlockQuery GetStakeDistribution2)
        , ("GetMaxMajorProtocolVersion", SomeBlockQuery GetMaxMajorProtocolVersion)
        , ("GetCBOR", SomeBlockQuery (GetCBOR GetLedgerTip))
        ]
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

fromShelleyLedgerExamplesPraos ::
  ShelleyCompatible (Praos StandardCrypto) era =>
  ProtocolLedgerExamples (SL.BHeader StandardCrypto) era ->
  Examples (ShelleyBlock (Praos StandardCrypto) era)
fromShelleyLedgerExamplesPraos = fromShelleyLedgerExamplesPolyPraos translatePraosHeader

-- | TODO Factor this out into something nicer.
fromShelleyLedgerExamplesPolyPraos ::
  forall proto era.
  ( ShelleyCompatible proto era
  , TranslateProto (TPraos StandardCrypto) proto
  ) =>
  -- | Rebuild the example's TPraos header as this protocol's header
  (SL.BHeader StandardCrypto -> ShelleyProtocolHeader proto) ->
  ProtocolLedgerExamples (SL.BHeader StandardCrypto) era ->
  Examples (ShelleyBlock proto era)
fromShelleyLedgerExamplesPolyPraos
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

    hash = ShelleyHash $ SL.unHashHeader pleHashHeader
    serialisedBlock = Serialised "<BLOCK>"
    tx = mkShelleyTx emptyTx
    slotNo = SlotNo 42
    serialisedHeader =
      SerialisedHeaderFromDepPair $ GenDepPair (NestedCtxt CtxtShelley) (Serialised "<HEADER>")
    queries =
      labelled
        [ ("GetLedgerTip", SomeBlockQuery GetLedgerTip)
        , ("GetEpochNo", SomeBlockQuery GetEpochNo)
        , ("GetCurrentPParams", SomeBlockQuery GetCurrentPParams)
        , ("GetNonMyopicMemberRewards", SomeBlockQuery $ GetNonMyopicMemberRewards leRewardsCredentials)
        , ("GetGenesisConfig", SomeBlockQuery GetGenesisConfig)
        , ("GetBigLedgerPeerSnapshot", SomeBlockQuery (GetLedgerPeerSnapshot SingBigLedgerPeers))
        , ("GetAllLedgerPeerSnapshot", SomeBlockQuery (GetLedgerPeerSnapshot SingAllLedgerPeers))
        , ("GetStakeDistribution2", SomeBlockQuery GetStakeDistribution2)
        , ("GetMaxMajorProtocolVersion", SomeBlockQuery GetMaxMajorProtocolVersion)
        , ("GetCBOR", SomeBlockQuery (GetCBOR GetLedgerTip))
        ]
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
      translateChainDepState (Proxy @(TPraos StandardCrypto, proto)) $
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

-- | Rebuild a TPraos example header as a Praos one.
translatePraosHeader :: SL.BHeader StandardCrypto -> Praos.Header StandardCrypto
translatePraosHeader (SL.BHeader bhBody bhSig) =
  Praos.Header (praosHeaderBodyFromTPraos bhBody) (coerce bhSig)

praosHeaderBodyFromTPraos :: SL.BHBody StandardCrypto -> HeaderBody StandardCrypto
praosHeaderBodyFromTPraos bhBody =
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

fromShelleyLedgerExamplesPraos2 ::
  ShelleyCompatible (Praos2 StandardCrypto) era =>
  ProtocolLedgerExamples (SL.BHeader StandardCrypto) era ->
  Examples (ShelleyBlock (Praos2 StandardCrypto) era)
fromShelleyLedgerExamplesPraos2 =
  fromShelleyLedgerExamplesPolyPraos translateLeiosHeader

-- | As 'translatePraosHeader', with the Leios fields of an example block that
-- carries a certificate and announces an endorser block of its own.
translateLeiosHeader :: SL.BHeader StandardCrypto -> Leios.Header StandardCrypto
translateLeiosHeader (SL.BHeader bhBody bhSig) =
  Leios.mkHeader (Proxy @DijkstraEra) hBody (coerce bhSig)
 where
  pb = praosHeaderBodyFromTPraos bhBody
  SL.ProtVer major minor = Praos.hbProtVer pb
  hBody =
    Leios.mkHeaderBody (Proxy @DijkstraEra) $
      Leios.HeaderBodyRaw
        { Leios.hbrBlockNo = Praos.hbBlockNo pb
        , Leios.hbrSlotNo = Praos.hbSlotNo pb
        , Leios.hbrPrev = Praos.hbPrev pb
        , Leios.hbrVk = Praos.hbVk pb
        , Leios.hbrVrfVk = Praos.hbVrfVk pb
        , Leios.hbrVrfRes = Praos.hbVrfRes pb
        , Leios.hbrBodySize = Praos.hbBodySize pb
        , Leios.hbrBodyHash = Praos.hbBodyHash pb
        , Leios.hbrOCert = Praos.hbOCert pb
        , Leios.hbrVersionInfo = SL.BlockHeaderVersionInfo (SL.getVersion32 major) minor
        , Leios.hbrBlockBodyContainsLeiosCert = True
        , Leios.hbrEbReferencesAnnouncement =
            SL.SJust $
              SL.EbReferencesAnnouncement
                { SL.ebReferencesAnnouncementHash =
                    unsafeMakeSafeHash $ Hash.castHash $ Praos.hbBodyHash pb
                , SL.ebReferencesAnnouncementSize = 123
                }
        }

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
  fromShelleyLedgerExamplesPraos2 (ledgerExamplesTPraos Dijkstra.ledgerExamples)

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
