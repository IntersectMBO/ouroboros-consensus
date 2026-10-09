{-# LANGUAGE TypeApplications #-}

-- | The sizes that transactions take of an endorser block's capacity agree
-- with the bytes 'encodeLeiosEb' writes.
--
-- 'mkLeiosEb' references each Dijkstra transaction by the hash and the length
-- of its bytes, in the order of the transactions.
--
-- A Dijkstra forge puts the ranking-block part of the mempool snapshot in the
-- block. It traces the endorser block that it builds from the endorser-block
-- part.
module Test.Consensus.Shelley.EndorserBlock (tests) where

import qualified Cardano.Crypto.Hash as Hash
import qualified Cardano.Crypto.VRF as VRF
import qualified Cardano.Ledger.BaseTypes as SL
import Cardano.Ledger.Core (TopTx, Tx, eraProtVerLow, ppMaxBBSizeL, toEraCBOR)
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.PParams
  ( ppMaxEndorserBlockReferencesSizeL
  , ppMaxEndorserBlockTxsSizeL
  )
-- The 'DecCBOR' instance of the Dijkstra block body, which 'ShelleyCompatible'
-- needs, has a 'Data.Coerce.Coercible' constraint. Solving it needs this
-- constructor in scope.
import Cardano.Ledger.Dijkstra.Tx (Tx (MkDijkstraTx))
import Cardano.Ledger.Plutus.ExUnits (ExUnits (..))
import qualified Cardano.Ledger.Shelley.API as SL
import Cardano.Ledger.Shelley.LedgerState (curPParamsEpochStateL, nesEsL)
import Cardano.Protocol.Crypto (StandardCrypto)
import Cardano.Protocol.Praos.VRF (mkInputVRF)
import qualified Cardano.Protocol.TPraos.OCert as SL
import Cardano.Slotting.EpochInfo (EpochInfo, fixedEpochInfo)
import Cardano.Slotting.Time (mkSlotLength)
import qualified Codec.CBOR.Encoding as CBOR
import Codec.CBOR.Write (toStrictByteString)
import Control.Tracer (nullTracer)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Measure as Measure
import qualified Data.Vector.Strict as V
import Data.Word (Word32)
import Lens.Micro ((&), (.~))
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Config
  ( SecurityParam (..)
  , TopLevelConfig (..)
  , emptyCheckpointsMap
  )
import Ouroboros.Consensus.Ledger.SupportsMempool
  ( ByteSize32 (..)
  , GenTx
  , HasTxs (..)
  , IgnoringOverflow (..)
  , LedgerSupportsMempool (txForgetValidated)
  , TxLimits (..)
  , TxMeasure (..)
  , Validated
  )
import Ouroboros.Consensus.Ledger.Tables.Utils (emptyLedgerTables)
import Ouroboros.Consensus.Leios.Types
  ( BytesSize
  , LeiosEb (..)
  , TxHash (..)
  , encodeLeiosEb
  , encodeLeiosEbItemSize
  , encodeLeiosEbMaxFramingSize
  , leiosReferencesCapacity
  )
import Ouroboros.Consensus.Mempool.API (MempoolMeasure (..))
import Ouroboros.Consensus.Mempool.Impl.Common (snapshotFromValidTxs)
import Ouroboros.Consensus.Mempool.TxSeq (TicketNo (..), TxTicket (..))
import Ouroboros.Consensus.Protocol.Leios (ConsensusConfig (LeiosConfig))
import Ouroboros.Consensus.Protocol.Praos
  ( ConsensusConfig (PraosConfig)
  , PraosIsLeader (..)
  , PraosParams (..)
  )
import Ouroboros.Consensus.Protocol.Praos.Common
  ( MaxMajorProtVer (..)
  , PraosCanBeLeader (..)
  , instantiatePraosCredentials
  )
import Ouroboros.Consensus.Shelley.HFEras (StandardDijkstraBlock)
import Ouroboros.Consensus.Shelley.Ledger
  ( AlonzoMeasure (..)
  , CodecConfig (ShelleyCodecConfig)
  , RefScriptSize (..)
  , ShelleyTransition (..)
  , StorageConfig (ShelleyStorageConfig)
  , fixedBlockBodyOverhead
  , fromExUnits
  , mkShelleyBlockConfig
  , mkShelleyLedgerConfig
  , mkShelleyValidatedTx
  )
import Ouroboros.Consensus.Shelley.Ledger.Ledger (Ticked (..))
import Ouroboros.Consensus.Shelley.Node (ShelleyLeaderCredentials (..))
import Ouroboros.Consensus.Shelley.Node.Leios
  ( TraceLeiosForge (..)
  , leiosSharedBlockForging
  , mkLeiosEb
  )
import Test.Cardano.Ledger.Dijkstra.Arbitrary ()
import qualified Test.Cardano.Ledger.Dijkstra.Examples as Dijkstra
import Test.Cardano.Ledger.Shelley.Examples
  ( LedgerExamples (..)
  , testShelleyGenesis
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))
import Test.Tasty.QuickCheck
import Test.ThreadNet.Infra.Shelley
  ( CoreNode (..)
  , genCoreNode
  , mkLeaderCredentials
  )
import Test.ThreadNet.Util.Seed (Seed (..), runGen)
import Test.Util.Tracer (recordingTracerIORef)

tests :: TestTree
tests =
  testGroup
    "Endorser block"
    [ testProperty "encodeLeiosEbItemSize consistent with encodeLeiosEb" prop_referenceSizeConsistent
    , testProperty "encodeLeiosEb framing bounded by encodeLeiosEbMaxFramingSize" prop_ebFramingBounded
    , testProperty
        "leiosReferencesCapacity floors at zero instead of wrapping"
        prop_referencesCapacityFloorsAtZero
    , testGroup
        "mkLeiosEb"
        [ testCase "no transactions give no endorser block" test_mkLeiosEbEmpty
        , testProperty "references keep the order of the transactions" prop_mkLeiosEbKeepsOrder
        , testProperty "reference hash is the hash of the transaction bytes" prop_mkLeiosEbHash
        , testProperty "reference size is the length of the transaction bytes" prop_mkLeiosEbSize
        ]
    , testGroup
        "Dijkstra forge"
        [ testCase "traces the endorser block of the endorser-block part" test_forgeTracesEndorserBlockPart
        , testCase
            "traces nothing for a zero endorser-block byte capacity"
            test_forgeWithoutEndorserBlockCapacity
        ]
    ]

genTxHash :: Gen TxHash
genTxHash = MkTxHash . BS.pack <$> vectorOf 32 arbitrary

-- | A tx size with good coverage of CBOR encoding boundaries.
-- Values 0-23 encode in 1 byte, 24-255 in 2 bytes, 256-65535 in 3 bytes.
genTxBytesSize :: Gen BytesSize
genTxBytesSize =
  frequency
    [ (1, pure 55) -- smallest transaction
    , (1, pure 16384) -- largest transaction
    , (1, pure 255) -- boundary: last 2-byte CBOR uint
    , (1, pure 256) -- boundary: first 3-byte CBOR uint
    , (1, pure 65536) -- first 5-byte CBOR uint
    , (5, chooseEnum (55, 16384))
    ]

-- | The number of references with good coverage of CBOR encoding boundaries for
-- the map length: 0-23 in 1 byte, 24-255 in 2 bytes, 256 and more in 3 bytes.
genNumReferences :: Gen Int
genNumReferences =
  frequency
    [ (1, pure 0)
    , (1, pure 1)
    , (1, pure 23)
    , (1, pure 24)
    , (1, pure 255)
    , (1, pure 256)
    , (3, chooseInt (0, 1000))
    ]

genLeiosEb :: Int -> Gen LeiosEb
genLeiosEb numTxs =
  MkLeiosEb . V.fromList <$> vectorOf numTxs ((,) <$> genTxHash <*> genTxBytesSize)

-- | 'encodeLeiosEbItemSize', the bytes a transaction's reference takes of the
-- endorser-block references capacity, agrees with the bytes 'encodeLeiosEb'
-- writes for that reference.
prop_referenceSizeConsistent :: Property
prop_referenceSizeConsistent =
  forAll ((,) <$> genTxHash <*> genTxBytesSize) $ \(txHash@(MkTxHash bytes), txSize) ->
    let encoded = toStrictByteString $ CBOR.encodeBytes bytes <> CBOR.encodeWord32 txSize
        ByteSize32 estimatedSize = encodeLeiosEbItemSize (ByteSize32 txSize)
     in counterexample
          ("reference: " <> show (txHash, txSize))
          (estimatedSize === fromIntegral (BS.length encoded))

-- | Whatever 'encodeLeiosEb' writes around the references stays within
-- 'encodeLeiosEbMaxFramingSize', the one-off amount capacities subtract.
prop_ebFramingBounded :: Property
prop_ebFramingBounded =
  forAll (genNumReferences >>= genLeiosEb) $ \eb ->
    let actualSize = fromIntegral $ BS.length (toStrictByteString (encodeLeiosEb eb))
        itemsSize =
          sum
            [ unByteSize32 (encodeLeiosEbItemSize (ByteSize32 txSize))
            | (_txHash, txSize) <- V.toList (leiosEbTxs eb)
            ]
        framing = actualSize - itemsSize
     in counterexample
          ("references: " <> show (V.length (leiosEbTxs eb)) <> ", framing: " <> show framing)
          (property $ actualSize >= itemsSize && framing <= unByteSize32 encodeLeiosEbMaxFramingSize)

-- | A @maxEndorserBlockReferencesSize@ at or below the framing yields a zero
-- references capacity, so nothing fits. It never wraps around to \"no limit\",
-- which 'Data.Word.Word32' subtraction would do for a parameter of 0.
prop_referencesCapacityFloorsAtZero :: Property
prop_referencesCapacityFloorsAtZero =
  forAll genParamLimit $ \paramLimit ->
    let capacity = leiosReferencesCapacity paramLimit
     in counterexample ("capacity: " <> show capacity) $
          conjoin
            [ counterexample "capacity must never exceed the parameter (wrap-around)" $
                property $
                  capacity <= paramLimit
            , counterexample "a parameter within the framing must yield zero capacity" $
                paramLimit > framing .||. capacity === 0
            ]
 where
  framing = unByteSize32 encodeLeiosEbMaxFramingSize

  -- Weighted towards the boundary a plain subtraction wraps on.
  genParamLimit =
    frequency
      [ (4, chooseEnum (0, framing))
      , (1, chooseEnum (framing + 1, maxBound))
      ]

{-------------------------------------------------------------------------------
  Building an endorser block
-------------------------------------------------------------------------------}

type DijkstraTx = Tx TopTx DijkstraEra

-- | 'SL.unsafeMakeValidatedTx' skips the ledger rules, so the generated
-- transactions need not be valid. 'mkLeiosEb' and
-- 'Ouroboros.Consensus.Shelley.Ledger.Forge.forgeShelleyBlockWithTxs' read
-- only the transaction.
unsafeValidated :: DijkstraTx -> Validated (GenTx StandardDijkstraBlock)
unsafeValidated =
  mkShelleyValidatedTx
    . SL.unsafeMakeValidatedTx globals (SL.mkMempoolEnv nes 0) (SL.mkMempoolState nes)
 where
  nes = leNewEpochState Dijkstra.ledgerExamples
  globals = SL.mkShelleyGlobals testShelleyGenesis epochInfo

epochInfo :: Monad m => EpochInfo m
epochInfo = fixedEpochInfo (EpochSize 10) (mkSlotLength 1)

-- | The references in the endorser block that 'mkLeiosEb' builds.
references :: [DijkstraTx] -> [(TxHash, BytesSize)]
references = maybe [] (V.toList . leiosEbTxs) . mkLeiosEb . map unsafeValidated

-- | The full serialised bytes of a transaction, from the ledger encoder.
txBytes :: DijkstraTx -> ByteString
txBytes = toStrictByteString . toEraCBOR @DijkstraEra

-- | Arbitrary Dijkstra transactions are slow to generate at the default
-- size, so the generator caps the size.
genTx :: Gen DijkstraTx
genTx = resize 3 arbitrary

-- | At least two transactions, so a wrong order changes the references.
genTxs :: Gen [DijkstraTx]
genTxs = chooseInt (2, 5) >>= flip vectorOf genTx

test_mkLeiosEbEmpty :: Assertion
test_mkLeiosEbEmpty =
  mkLeiosEb ([] :: [Validated (GenTx StandardDijkstraBlock)]) @?= Nothing

-- | The endorser block of a list holds, in the order of the list, the
-- reference that each transaction gets on its own.
prop_mkLeiosEbKeepsOrder :: Property
prop_mkLeiosEbKeepsOrder =
  forAll genTxs $ \txs ->
    references txs === concatMap (references . pure) txs

-- | 'Hash.digest' computes the expected hash, so the property does not repeat
-- the code under test.
prop_mkLeiosEbHash :: Property
prop_mkLeiosEbHash =
  forAll genTx $ \tx ->
    map fst (references [tx])
      === [MkTxHash (Hash.digest (Proxy @Hash.Blake2b_256) (txBytes tx))]

prop_mkLeiosEbSize :: Property
prop_mkLeiosEbSize =
  forAll genTx $ \tx ->
    map snd (references [tx]) === [fromIntegral (BS.length (txBytes tx))]

{-------------------------------------------------------------------------------
  Forging an endorser block
-------------------------------------------------------------------------------}

-- | The mempool of 'forgeDijkstra' holds these transactions, in this order.
forgeTxs :: [Validated (GenTx StandardDijkstraBlock)]
forgeTxs = map unsafeValidated $ runGen (Seed 0) $ vectorOf 5 genTx

-- | The mempool measure of the transaction at position @i@ of 'forgeTxs'.
-- The transaction takes @100 + i@ bytes, so the ranking-block part and the
-- endorser-block part have different measures, and the tests can tell them
-- apart. The transaction takes no execution units and no reference-script
-- bytes, so only the byte capacities decide the parts.
forgeTxMeasure :: Int -> MempoolMeasure StandardDijkstraBlock
forgeTxMeasure i =
  MempoolMeasure
    { mmTxMeasure = txMeasure
    , mmTxEbMeasure = txEbMeasure (Proxy @StandardDijkstraBlock) txMeasure
    , mmDiffTime = Measure.zero
    }
 where
  txMeasure =
    TxMeasure
      AlonzoMeasure
        { byteSize = IgnoringOverflow $ ByteSize32 $ 100 + fromIntegral i
        , exUnits = fromExUnits $ ExUnits 0 0
        }
      (RefScriptSize $ IgnoringOverflow $ ByteSize32 0)

forgeSlot :: SlotNo
forgeSlot = 1

-- | Forge a Dijkstra block with 'leiosSharedBlockForging' and give the forge
-- result and the traced events.
--
-- The block capacity is 250 transaction bytes, so the block holds the first
-- two transactions of 'forgeTxs'. The given number sets both endorser-block
-- byte limits, 'ppMaxEndorserBlockTxsSizeL' and
-- 'ppMaxEndorserBlockReferencesSizeL'.
forgeDijkstra ::
  Word32 ->
  IO (ForgedBlock StandardDijkstraBlock, [TraceLeiosForge StandardDijkstraBlock])
forgeDijkstra ebBytes = do
  (tracer, getEvents) <- recordingTracerIORef
  hotKey <-
    instantiatePraosCredentials
      (SL.sgMaxKESEvolutions testShelleyGenesis)
      nullTracer
      (praosCanBeLeaderCredentialsSource (shelleyLeaderCredentialsCanBeLeader credentials))
  let forging = leiosSharedBlockForging tracer hotKey (const (SL.KESPeriod 0)) credentials
  forged <- forgeBlock forging args
  finalize forging
  (,) forged <$> getEvents
 where
  coreNode :: CoreNode StandardCrypto
  coreNode = runGen (Seed 0) $ genCoreNode (SL.KESPeriod 0)

  credentials = mkLeaderCredentials coreNode

  args =
    ForgeBlockArgs
      { fbConfig = config
      , fbCurrentBlockNo = 0
      , fbCurrentSlotNo = forgeSlot
      , fbPerasCert = Nothing
      , fbCurrentTickedLedgerState =
          TickedShelleyLedgerState
            { untickedShelleyLedgerTip = Origin
            , tickedShelleyLedgerTransition = ShelleyTransitionInfo{shelleyAfterVoting = 0}
            , tickedShelleyLedgerState =
                leNewEpochState Dijkstra.ledgerExamples
                  & nesEsL . curPParamsEpochStateL . ppMaxBBSizeL .~ fixedBlockBodyOverhead + 250
                  & nesEsL . curPParamsEpochStateL . ppMaxEndorserBlockTxsSizeL .~ ebBytes
                  & nesEsL . curPParamsEpochStateL . ppMaxEndorserBlockReferencesSizeL .~ ebBytes
            , tickedShelleyLedgerTables = emptyLedgerTables
            }
      , fbMempoolSnapshot =
          snapshotFromValidTxs
            [ TxTicket tx (TicketNo (fromIntegral i + 1)) (forgeTxMeasure i)
            | (i, tx) <- zip [0 ..] forgeTxs
            ]
            GenesisPoint
            forgeSlot
      , fbIsLeader =
          PraosIsLeader $
            VRF.evalCertified () (mkInputVRF forgeSlot SL.NeutralNonce) (cnVRF coreNode)
      }

  config =
    TopLevelConfig
      { topLevelConfigProtocol = LeiosConfig $ PraosConfig praosParams epochInfo
      , topLevelConfigLedger =
          mkShelleyLedgerConfig
            testShelleyGenesis
            (leTranslationContext Dijkstra.ledgerExamples)
            epochInfo
      , topLevelConfigBlock = mkShelleyBlockConfig protVer testShelleyGenesis []
      , topLevelConfigCodec = ShelleyCodecConfig
      , topLevelConfigStorage =
          ShelleyStorageConfig (SL.sgSlotsPerKESPeriod testShelleyGenesis) securityParam
      , topLevelConfigCheckpoints = emptyCheckpointsMap
      }

  praosParams =
    PraosParams
      { praosSlotsPerKESPeriod = SL.sgSlotsPerKESPeriod testShelleyGenesis
      , praosLeaderF = leaderF
      , praosSecurityParam = securityParam
      , praosMaxKESEvo = SL.sgMaxKESEvolutions testShelleyGenesis
      , praosMaxMajorPV = MaxMajorProtVer $ SL.pvMajor protVer
      , praosRandomnessStabilisationWindow =
          SL.computeRandomnessStabilisationWindow
            (SL.unNonZero $ SL.sgSecurityParam testShelleyGenesis)
            leaderF
      }

  leaderF = SL.mkActiveSlotCoeff $ SL.sgActiveSlotsCoeff testShelleyGenesis
  securityParam = SecurityParam $ SL.sgSecurityParam testShelleyGenesis
  protVer = SL.ProtVer (eraProtVerLow @DijkstraEra) 0

-- | The block holds the first two transactions. The endorser block holds the
-- next two, the longest prefix of the rest that fits 250 transaction bytes.
test_forgeTracesEndorserBlockPart :: Assertion
test_forgeTracesEndorserBlockPart = do
  (forged, events) <- forgeDijkstra 250
  forgedTxs forged @?= take 2 forgeTxs
  extractTxs (forgedBlock forged) @?= map txForgetValidated (take 2 forgeTxs)
  forgedTxsMeasure forged @?= foldMap forgeTxMeasure [0, 1]
  Just eb <- pure $ mkLeiosEb $ take 2 $ drop 2 forgeTxs
  events @?= [TraceForgedLeiosEb forgeSlot eb (foldMap forgeTxMeasure [2, 3])]

-- | With a zero endorser-block byte capacity, the block holds the same
-- transactions and there is no endorser block.
test_forgeWithoutEndorserBlockCapacity :: Assertion
test_forgeWithoutEndorserBlockCapacity = do
  (forged, events) <- forgeDijkstra 0
  forgedTxs forged @?= take 2 forgeTxs
  extractTxs (forgedBlock forged) @?= map txForgetValidated (take 2 forgeTxs)
  events @?= []
