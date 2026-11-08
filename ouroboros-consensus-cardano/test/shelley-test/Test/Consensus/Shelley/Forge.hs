{-# LANGUAGE TypeApplications #-}

-- | A forged Dijkstra block announces the endorser block that 'forgeBlock'
-- returns.
module Test.Consensus.Shelley.Forge (tests) where

import qualified Cardano.Crypto.Hash as Hash
import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import qualified Cardano.Ledger.BaseTypes as SL (Globals (..), ProtVer (..))
import Cardano.Ledger.Block (EbReferencesAnnouncement (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core (bodyTxL, feeTxBodyL, outputsTxBodyL, ppProtocolVersionL)
import Cardano.Ledger.Dijkstra.PParams (ppMaxEndorserBlockReferencesSizeL)
import Cardano.Ledger.Hashes (HASH, originalBytes, unsafeMakeSafeHash)
import Cardano.Ledger.MemoBytes (getMemoRawType)
import Cardano.Ledger.MempoolTx.Internal (MempoolTx (UnsafeMkMempoolTx), unwrapMempoolTx)
import qualified Cardano.Ledger.Shelley.API as SL
  ( mkMempoolEnv
  , mkMempoolState
  , unsafeMakeValidatedTx
  )
import Cardano.Ledger.Shelley.LedgerState
  ( curPParamsEpochStateL
  , esLStateL
  , lsLastEbAnnouncementL
  , nesEsL
  )
import Cardano.Ledger.State (EbAnnouncement (..))
import Cardano.Ledger.TxIn (TxIn (..))
import qualified Cardano.Protocol.Leios.BlockHeader as Leios
import qualified Cardano.Protocol.TPraos.OCert as SL (KESPeriod (..))
import Cardano.Slotting.EpochInfo (fixedEpochInfo)
import Control.Tracer (nullTracer)
import qualified Data.ByteString as BS
import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import qualified Data.Sequence.Strict as StrictSeq
import Lens.Micro ((&), (.~), (^.))
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.BlockchainTime (slotLengthFromSec)
import Ouroboros.Consensus.Config
  ( SecurityParam (..)
  , TopLevelConfig (..)
  , emptyCheckpointsMap
  )
import Ouroboros.Consensus.Ledger.Tables
  ( CanStowLedgerTables (..)
  , HasLedgerTables (..)
  , LedgerTables (..)
  )
import Ouroboros.Consensus.Ledger.Tables.Diff (Diff (..))
import Ouroboros.Consensus.Ledger.Tables.MapKind (DiffMK (..), EmptyMK, KeysMK, ValuesMK (..))
import Ouroboros.Consensus.Ledger.Tables.Utils (applyDiffs)
import Ouroboros.Consensus.Leios.Types
  ( BytesSize
  , LeiosClosure (..)
  , LeiosPoint (..)
  , LeiosTx (..)
  , TxHash
  , encodeEndorserBlockSize
  , endorserBlockHash
  , endorserBlockReferences
  , forgedEbBody
  , forgedEbTxs
  , serialiseEndorserBlock
  )
import Ouroboros.Consensus.Protocol.Praos
  ( ConsensusConfig (..)
  , PraosIsLeader (..)
  , PraosParams (..)
  )
import Ouroboros.Consensus.Protocol.Praos.Common
  ( MaxMajorProtVer (..)
  , PraosCanBeLeader (..)
  , instantiatePraosCredentials
  )
import Ouroboros.Consensus.Protocol.Praos2 (ConsensusConfig (..))
import Ouroboros.Consensus.Shelley.Eras (StandardCrypto)
import Ouroboros.Consensus.Shelley.HFEras (StandardDijkstraBlock)
import Ouroboros.Consensus.Shelley.Ledger
  ( BigEndianTxIn (..)
  , GenTx (..)
  , LedgerState (..)
  , ShelleyLedgerConfig (..)
  , Ticked (..)
  , Validated (..)
  , mkShelleyBlockConfig
  , mkShelleyTx
  , shelleyHeaderRaw
  )
import Ouroboros.Consensus.Shelley.Ledger.Config
  ( CodecConfig (..)
  , StorageConfig (..)
  )
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Consensus.Shelley.Node.Common (ShelleyLeaderCredentials (..))
import Ouroboros.Consensus.Shelley.Node.Praos (praos2SharedBlockForging)
import Test.Consensus.Shelley.Examples (examplesDijkstra, testShelleyGenesis)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck
import Test.ThreadNet.Infra.Shelley (CoreNode, genCoreNode, mkLeaderCredentials)
import Test.Util.Serialisation.Examples (Examples, Labelled)
import qualified Test.Util.Serialisation.Examples as Examples

tests :: TestTree
tests =
  testGroup
    "Forge"
    [ testProperty "a block announces the endorser block it comes with" prop_announcesEb
    , testProperty "a block without endorser-block transactions announces nothing" prop_noEb
    , testProperty
        "the closure of the announced endorser block applies its transactions"
        prop_appliesClosure
    , testProperty "a state that announced nothing applies no closure" prop_noClosure
    ]

prop_announcesEb :: Property
prop_announcesEb =
  forAllBlind (genCoreNode (SL.KESPeriod 0)) $ \coreNode ->
    forAll genNumEbTxs $ \numEbTxs -> ioProperty $ do
      let ebTxs = take numEbTxs distinctTxs
      (_, ForgeBlockResult blk forgedEb) <- forgeWith coreNode ebTxs
      pure $ case forgedEb of
        Nothing -> counterexample "no endorser block" False
        Just forged ->
          let eb = forgedEbBody forged
           in conjoin
                [ counterexample "the endorser block does not reference the transactions in order" $
                    endorserBlockReferences eb === map reference ebTxs
                , counterexample "forgedEbTxs does not hold the referenced transaction bytes" $
                    toList (forgedEbTxs forged) === map txBytes ebTxs
                , announcement blk
                    === SJust
                      EbReferencesAnnouncement
                        { ebReferencesAnnouncementHash = unsafeMakeSafeHash (endorserBlockHash eb)
                        , ebReferencesAnnouncementSize = encodeEndorserBlockSize eb
                        }
                , counterexample "the block does not match its header" $
                    blockMatchesHeader (getHeader blk) blk
                ]

prop_noEb :: Property
prop_noEb =
  forAllBlind (genCoreNode (SL.KESPeriod 0)) $ \coreNode -> ioProperty $ do
    (_, ForgeBlockResult blk forgedEb) <- forgeWith coreNode []
    pure $ (announcement blk, isJust forgedEb) === (SNothing, False)

prop_appliesClosure :: Property
prop_appliesClosure =
  forAllBlind (genCoreNode (SL.KESPeriod 0)) $ \coreNode ->
    forAll genNumEbTxs $ \numEbTxs -> ioProperty $ do
      let ebTxs = map withExampleOutput $ take numEbTxs distinctTxs
      (cfg, ForgeBlockResult blk forgedEb) <- forgeWith coreNode ebTxs
      forged <- maybe (fail "no endorser block") pure forgedEb
      ann <- case announcement blk of
        SJust ann -> pure ann
        SNothing -> fail "no announcement"
      let point = MkLeiosPoint (blockSlot blk) (endorserBlockHash (forgedEbBody forged))
          closure =
            MkLeiosClosure
              (serialiseEndorserBlock (forgedEbBody forged))
              (MkLeiosTx <$> toList (forgedEbTxs forged))
          resolve requested
            | requested == point = pure closure
            | otherwise = fail $ "resolved " <> show requested <> " instead of " <> show point
          announcing = announcedIn (blockSlot blk) ann exampleLedgerState
      after <- applyLeiosClosure (topLevelConfigLedger cfg) resolve readExampleTables announcing
      let utxo = getValuesMK . getLedgerTables . projectLedgerTables $ applyDiffs exampleValues after
      pure $
        conjoin
          [ counterexample ("output of " <> show txid <> " missing") $
              Map.member (BigEndianTxIn (TxIn txid minBound)) utxo
          | ShelleyTx txid _ <- ebTxs
          ]

prop_noClosure :: Property
prop_noClosure = ioProperty $ do
  after <-
    applyLeiosClosure
      (example Examples.exampleLedgerConfig)
      (\point -> fail $ "resolved " <> show point)
      readExampleTables
      exampleLedgerState
  let DiffMK (Diff diffs) = getLedgerTables (projectLedgerTables after)
  pure $ Map.null diffs

-- | The transaction, paying out a copy of the example UTxO's output, so that
-- applying it changes the UTxO.
withExampleOutput :: GenTx StandardDijkstraBlock -> GenTx StandardDijkstraBlock
withExampleOutput (ShelleyTx _ tx) =
  mkShelleyTx . UnsafeMkMempoolTx $
    unwrapMempoolTx tx & bodyTxL . outputsTxBodyL .~ StrictSeq.fromList exampleTxOuts
 where
  exampleTxOuts = Map.elems . getValuesMK . getLedgerTables $ projectLedgerTables exampleValues

-- | The example ledger state, recording that the block in the given slot
-- announced this endorser block.
announcedIn ::
  SlotNo ->
  EbReferencesAnnouncement ->
  LedgerState StandardDijkstraBlock EmptyMK ->
  LedgerState StandardDijkstraBlock EmptyMK
announcedIn slot ann st =
  st
    { shelleyLedgerState =
        shelleyLedgerState st
          & nesEsL . esLStateL . lsLastEbAnnouncementL
            .~ SJust
              EbAnnouncement
                { ebAnnouncement = ann
                , ebAnnouncementSlotNo = slot
                , ebAnnouncedInBlockHeader = unsafeMakeSafeHash (Hash.castHash (Hash.hashWith @HASH id BS.empty))
                }
    }

exampleLedgerState :: LedgerState StandardDijkstraBlock EmptyMK
exampleLedgerState = example Examples.exampleLedgerState

-- | The UTxO of the example ledger state.
exampleValues :: LedgerState StandardDijkstraBlock ValuesMK
exampleValues = unstowLedgerTables exampleLedgerState

readExampleTables ::
  LedgerTables StandardDijkstraBlock KeysMK -> IO (LedgerTables StandardDijkstraBlock ValuesMK)
readExampleTables _keys = pure $ projectLedgerTables exampleValues

-- | A number of endorser-block transactions.
-- 'Ouroboros.Consensus.Leios.Types.encodeEndorserBlock' writes a
-- longer map header from 24 and from 256 references.
genNumEbTxs :: Gen Int
genNumEbTxs =
  frequency
    [ (1, elements [1, 23, 24, 255, 256])
    , (3, chooseInt (1, 300))
    ]

-- | Forge the block after the example ledger state, with the given endorser
-- block transactions and no ranking block transactions. Also return the
-- config of the forge.
forgeWith ::
  CoreNode StandardCrypto ->
  [GenTx StandardDijkstraBlock] ->
  IO (TopLevelConfig StandardDijkstraBlock, ForgeBlockResult StandardDijkstraBlock)
forgeWith coreNode ebTxs = fmap ((,) topLevelConfig) $ do
  hotKey <-
    instantiatePraosCredentials
      (SL.maxKESEvo globals)
      nullTracer
      (praosCanBeLeaderCredentialsSource praosCanBeLeader)
  forgeBlock
    (praos2SharedBlockForging hotKey slotToPeriod credentials)
    ForgeBlockArgs
      { fbConfig = topLevelConfig
      , fbCurrentBlockNo = BlockNo 4
      , fbCurrentSlotNo = slot
      , fbPerasCert = Nothing
      , fbCurrentTickedLedgerState = tickedDijkstraLedgerState
      , fbTxs = []
      , fbEbTxs =
          [ ShelleyValidatedTx txid $
              SL.unsafeMakeValidatedTx
                globals
                (SL.mkMempoolEnv newEpochState slot)
                (SL.mkMempoolState newEpochState)
                (unwrapMempoolTx tx)
          | ShelleyTx txid tx <- ebTxs
          ]
      , fbIsLeader = PraosIsLeader $ Leios.hbrVrfRes (rawHeaderBody (getHeader exampleDijkstraBlock))
      }
 where
  slot = SlotNo 10
  credentials = mkLeaderCredentials coreNode
  praosCanBeLeader = shelleyLeaderCredentialsCanBeLeader credentials

  slotToPeriod (SlotNo slotNo) =
    SL.KESPeriod $ fromIntegral $ slotNo `div` SL.slotsPerKESPeriod globals

  topLevelConfig =
    TopLevelConfig
      { topLevelConfigProtocol =
          LeiosConfig $
            PraosConfig
              { praosParams =
                  PraosParams
                    { praosSlotsPerKESPeriod = SL.slotsPerKESPeriod globals
                    , praosLeaderF = SL.activeSlotCoeff globals
                    , praosSecurityParam = SecurityParam $ SL.securityParameter globals
                    , praosMaxKESEvo = SL.maxKESEvo globals
                    , praosMaxMajorPV = MaxMajorProtVer $ SL.pvMajor protVer
                    , praosRandomnessStabilisationWindow = SL.randomnessStabilisationWindow globals
                    }
              , praosEpochInfo = fixedEpochInfo (EpochSize 4) (slotLengthFromSec 7)
              }
      , topLevelConfigLedger = ledgerConfig
      , topLevelConfigBlock =
          mkShelleyBlockConfig
            protVer
            testShelleyGenesis
            [praosCanBeLeaderColdVerKey praosCanBeLeader]
      , topLevelConfigCodec = ShelleyCodecConfig
      , topLevelConfigStorage =
          ShelleyStorageConfig
            (SL.slotsPerKESPeriod globals)
            (SecurityParam $ SL.securityParameter globals)
      , topLevelConfigCheckpoints = emptyCheckpointsMap
      }

  protVer = newEpochState ^. nesEsL . curPParamsEpochStateL . ppProtocolVersionL
  ledgerConfig = example Examples.exampleLedgerConfig
  globals = shelleyLedgerGlobals ledgerConfig

  tickedDijkstraLedgerState =
    TickedShelleyLedgerState
      { untickedShelleyLedgerTip = shelleyLedgerTip ledgerState
      , tickedShelleyLedgerTransition = shelleyLedgerTransition ledgerState
      , tickedShelleyLedgerState = newEpochState
      , tickedShelleyLedgerTables = shelleyLedgerTables ledgerState
      }

  -- The 'maxEndorserBlockReferencesSize' of the example parameters is smaller
  -- than the endorser block of the example transaction.
  newEpochState =
    shelleyLedgerState ledgerState
      & nesEsL . curPParamsEpochStateL . ppMaxEndorserBlockReferencesSizeL .~ 512 * 1024

  ledgerState = example Examples.exampleLedgerState

announcement :: StandardDijkstraBlock -> StrictMaybe EbReferencesAnnouncement
announcement = Leios.hbrEbReferencesAnnouncement . rawHeaderBody . getHeader

rawHeaderBody :: Header StandardDijkstraBlock -> Leios.HeaderBodyRaw StandardCrypto
rawHeaderBody = getMemoRawType . Leios.headerBody . shelleyHeaderRaw

exampleTx :: GenTx StandardDijkstraBlock
exampleTx = example Examples.exampleGenTx

-- | Copies of 'exampleTx' with different fees, so that each copy has
-- different bytes and a reordered endorser block has a different hash.
distinctTxs :: [GenTx StandardDijkstraBlock]
distinctTxs =
  [ mkShelleyTx (UnsafeMkMempoolTx (unwrapMempoolTx tx & bodyTxL . feeTxBodyL .~ Coin fee))
  | let ShelleyTx _ tx = exampleTx
  , fee <- [1 ..]
  ]

-- | The reference that an endorser block holds for a transaction: the
-- Blake2b-256 hash and the length of the transaction bytes.
reference :: GenTx StandardDijkstraBlock -> (TxHash, BytesSize)
reference tx =
  ( Hash.castHash (Hash.hashWith @HASH id (txBytes tx))
  , fromIntegral (BS.length (txBytes tx))
  )

-- | The bytes of a transaction that an endorser block references: the
-- block-body encoding, which keeps the @is_valid@ flag.
txBytes :: GenTx StandardDijkstraBlock -> BS.ByteString
txBytes (ShelleyTx _ tx) = originalBytes (unwrapMempoolTx tx)

exampleDijkstraBlock :: StandardDijkstraBlock
exampleDijkstraBlock = example Examples.exampleBlock

example :: (Examples StandardDijkstraBlock -> Labelled b) -> b
example field = case field examplesDijkstra of
  (_, x) : _ -> x
  [] -> error "no example"
