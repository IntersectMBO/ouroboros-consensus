{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Tools.ThreadNet.Run
  ( Opts (..)
  , ThreadNetConfig (..)
  , CoreNodeConfig (..)
  , run
  ) where

import Cardano.Api.Any (HasTypeProxy (proxyToAsType))
import Cardano.Api.Key (Key (SigningKey))
import Cardano.Api.KeysShelley (PaymentKey, SigningKey (unPaymentSigningKey))
import Cardano.Api.SerialiseTextEnvelope (readFileTextEnvelope)
import qualified Cardano.Crypto.DSIGN as Crypto
import qualified Cardano.Ledger.Api as SL
import Cardano.Ledger.BaseTypes (TxIx (TxIx))
import Cardano.Ledger.Hashes (HashAnnotated (hashAnnotated))
import qualified Cardano.Ledger.Keys as Ledger
import qualified Cardano.Ledger.Shelley.API as SL
import Cardano.Ledger.Val (Val (coin, (<->)), inject)
import Cardano.Node.Protocol.Shelley (readLeaderCredentials)
import Cardano.Node.Types (ProtocolFilepaths (..))
import Cardano.Tools.DBAnalyser.Block.Cardano (mkCardanoProtocolParams)
import qualified Cardano.Tools.DBAnalyser.Block.Cardano as Cardano
import Control.Monad (forM, guard)
import Control.Monad.Except (runExceptT)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import Data.List (transpose)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Debug.Trace as Debug
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack, callStack)
import Lens.Micro ((&), (.~), (^.))
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.BlockchainTime
import Ouroboros.Consensus.Cardano.Block
import Ouroboros.Consensus.Cardano.Condense ()
import Ouroboros.Consensus.Cardano.Node (CardanoProtocolParams, protocolInfoCardano)
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.Ledger.Abstract (ValuesMK, getValuesMK)
import Ouroboros.Consensus.Node.ProtocolInfo
import Ouroboros.Consensus.NodeId (CoreNodeId (CoreNodeId))
import Ouroboros.Consensus.Shelley.HFEras ()
import Ouroboros.Consensus.Shelley.Ledger hiding (getPParams)
import qualified Ouroboros.Consensus.Shelley.Ledger.Ledger as SL
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Consensus.Shelley.Node
import System.Exit (die)
import Test.Cardano.Ledger.Core.KeyPair (KeyPair (..))
import qualified Test.Cardano.Ledger.Core.KeyPair as TL (mkWitnessVKey)
import Test.Cardano.Ledger.Shelley.Generator.ShelleyEraGen ()
import Test.QuickCheck (Gen)
import Test.ThreadNet.General
import Test.ThreadNet.Infra.Shelley ()
import Test.ThreadNet.Network
  ( LeiosState (..)
  , NodeOutput (..)
  , TestNodeInitialization (..)
  )
import Test.ThreadNet.TxGen (TxGen (..))
import Test.ThreadNet.Util.NodeJoinPlan (trivialNodeJoinPlan)
import Test.ThreadNet.Util.NodeRestarts (noRestarts)
import Test.ThreadNet.Util.NodeToNodeVersion (newestVersion)
import Test.ThreadNet.Util.NodeTopology (NodeTopology (NodeTopology))
import Test.ThreadNet.Util.Seed (Seed (Seed))
import Test.Util.HardFork.Future
import Test.Util.Orphans.Arbitrary ()
import Test.Util.Slots (NumSlots (NumSlots))

data Opts = Opts
  { threadNetConfigFile :: FilePath
  , slots :: Integer
  , txsPerSlot :: Integer
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- FIXME: get rid of these hard-coded paths (and provide key fixtures)
exampleThreadNetConfig :: ThreadNetConfig
exampleThreadNetConfig =
  ThreadNetConfig
    { tncCoreNodes =
        [ CoreNodeConfig
            { cncVrfSigningKey =
                "/home/bladyjoker/github/input-output-hk/ouroboros-leios/demo/proto-devnet/config/pools-keys/pool1/vrf.skey"
            , cncKesSigningKey =
                "/home/bladyjoker/github/input-output-hk/ouroboros-leios/demo/proto-devnet/config/pools-keys/pool1/kes.skey"
            , cncOCert =
                "/home/bladyjoker/github/input-output-hk/ouroboros-leios/demo/proto-devnet/config/pools-keys/pool1/opcert.cert"
            , cncCardanoConfig = "test-config/config.json"
            }
        , CoreNodeConfig
            { cncVrfSigningKey =
                "/home/bladyjoker/github/input-output-hk/ouroboros-leios/demo/proto-devnet/config/pools-keys/pool2/vrf.skey"
            , cncKesSigningKey =
                "/home/bladyjoker/github/input-output-hk/ouroboros-leios/demo/proto-devnet/config/pools-keys/pool2/kes.skey"
            , cncOCert =
                "/home/bladyjoker/github/input-output-hk/ouroboros-leios/demo/proto-devnet/config/pools-keys/pool2/opcert.cert"
            , cncCardanoConfig = "test-config/config.json"
            }
        , CoreNodeConfig
            { cncVrfSigningKey =
                "/home/bladyjoker/github/input-output-hk/ouroboros-leios/demo/proto-devnet/config/pools-keys/pool3/vrf.skey"
            , cncKesSigningKey =
                "/home/bladyjoker/github/input-output-hk/ouroboros-leios/demo/proto-devnet/config/pools-keys/pool3/kes.skey"
            , cncOCert =
                "/home/bladyjoker/github/input-output-hk/ouroboros-leios/demo/proto-devnet/config/pools-keys/pool3/opcert.cert"
            , cncCardanoConfig = "test-config/config.json"
            }
        ]
    , tncTopology =
        Map.fromList
          [ (0, Set.fromList [1, 2])
          , (1, Set.fromList [0, 2])
          , (2, Set.fromList [0, 1])
          ]
    , tncTxGenerators =
        [ TxGeneratorConfig
            { tgcSubmitToNodes = Set.fromList [0]
            , tgcTxGeneratorKind =
                TransferAllConfigKind $
                  TransferAllConfig
                    { tacPaymentSigningKey =
                        "/home/bladyjoker/github/input-output-hk/ouroboros-leios/demo/proto-devnet/config/stake-delegators/delegator2/payment.skey"
                    }
            }
        ]
    }

data ThreadNetConfig = ThreadNetConfig
  { tncCoreNodes :: [CoreNodeConfig]
  , tncTopology :: Map Int (Set Int)
  , tncTxGenerators :: [TxGeneratorConfig]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data CoreNodeConfig = CoreNodeConfig
  { cncVrfSigningKey :: FilePath
  , cncKesSigningKey :: FilePath
  , cncOCert :: FilePath
  , cncCardanoConfig :: FilePath
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data TxGeneratorConfig = TxGeneratorConfig
  { tgcSubmitToNodes :: Set Int
  , tgcTxGeneratorKind :: TxGeneratorKindConfig
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data TxGeneratorKindConfig = TransferAllConfigKind TransferAllConfig
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data TransferAllConfig = TransferAllConfig
  { tacPaymentSigningKey :: FilePath
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data ThreadNet = ThreadNet
  { tnCoreNodes :: [CoreNode]
  , tnTopology :: NodeTopology
  , tnTxGenerators :: [TxGenerator]
  }

newtype CoreNode = CoreNode
  { cnCardanoProtocolParams :: CardanoProtocolParams StandardCrypto
  }

data TxGenerator = TxGenerator
  { tgSubmitToNodes :: Set CoreNodeId
  , tgTxGeneratorKind :: TxGeneratorKind
  }

data TxGeneratorKind = TransferAllKind TransferAll

data TransferAll = TransferAll
  { taPaymentSigningKey :: Crypto.SignKeyDSIGN Ledger.DSIGN
  }

processThreadNetConfig ::
  HasCallStack => ThreadNetConfig -> IO ThreadNet
processThreadNetConfig ThreadNetConfig{..} = do
  tnCoreNodes <- forM tncCoreNodes processCoreNodeConfig
  let tnTopology =
        NodeTopology . Map.fromList $
          [ ( CoreNodeId . fromIntegral $ nodeIx
            , Set.fromList . fmap (CoreNodeId . fromIntegral) . Set.toList $ connectsTo
            )
          | (nodeIx, connectsTo) <- Map.toList tncTopology
          ]
  tnTxGenerators <- forM tncTxGenerators $ \TxGeneratorConfig{..} -> do
    tgSubmitToNodes <-
      Set.fromList
        <$> ( forM (Set.toList tgcSubmitToNodes) $ \dest ->
                if dest >= 0 && dest < length tnCoreNodes
                  then return $ CoreNodeId . fromIntegral $ dest
                  else
                    error $
                      show
                        ("Invalid node index", dest, callStack)
            )
    tgTxGeneratorKind <- case tgcTxGeneratorKind of
      TransferAllConfigKind tac -> TransferAllKind <$> processTransferAllConfig tac
    return TxGenerator{tgSubmitToNodes, tgTxGeneratorKind}

  return $
    ThreadNet
      { tnCoreNodes
      , tnTopology
      , tnTxGenerators
      }

processTransferAllConfig :: HasCallStack => TransferAllConfig -> IO TransferAll
processTransferAllConfig TransferAllConfig{..} = do
  errOrPsk <-
    fmap unPaymentSigningKey
      <$> readFileTextEnvelope
        (proxyToAsType (Proxy :: (Proxy (SigningKey PaymentKey))))
        tacPaymentSigningKey

  taPaymentSigningKey <-
    either (\err -> die . show $ ("failed reading psk: ", err, callStack)) return errOrPsk
  return $ TransferAll{taPaymentSigningKey}

processCoreNodeConfig :: HasCallStack => CoreNodeConfig -> IO CoreNode
processCoreNodeConfig CoreNodeConfig{..} = do
  credsOrError <-
    runExceptT $
      readLeaderCredentials $
        Just $
          ProtocolFilepaths
            { shelleyVRFFile = Just cncVrfSigningKey
            , shelleyKESFile = Just cncKesSigningKey
            , shelleyCertFile = Just cncOCert
            , shelleyBulkCredsFile = Nothing
            , byronKeyFile = Nothing
            , byronCertFile = Nothing
            }
  creds <- case credsOrError of
    Left err -> error . show $ ("Failed processing CoreNodeConfig", err, callStack)
    Right creds -> return creds

  cardanoProtocolParams <-
    mkCardanoProtocolParams creds (Cardano.CardanoBlockArgs cncCardanoConfig Nothing)

  return $ CoreNode cardanoProtocolParams

run :: HasCallStack => Opts -> IO ()
run Opts{..} = do
  writeJson "foo-example-threadnet.config" exampleThreadNetConfig -- TODO(bladyjoker): Remove this (currently useful)
  threadNetConfig <- readJson threadNetConfigFile
  threadNet <- processThreadNetConfig threadNetConfig

  let
    testOutput =
      runThreadNet $
        RunThreadNetArgs
          { rtnaSlots = slots
          , rtnaTxsPerSlot = txsPerSlot
          , rtnaThreadNet = threadNet
          }

  print $ Map.lookupMax . testOutputTipBlockNos $ testOutput
  print $
    [ ( nodeId
      , leiosKernelEvents . nodeLeiosState $ nodeOutput
      )
    | (nodeId, nodeOutput) <- Map.toList . testOutputNodes $ testOutput
    ]

  return ()

data RunThreadNetArgs = RunThreadNetArgs
  { rtnaSlots :: Integer
  , rtnaTxsPerSlot :: Integer
  , rtnaThreadNet :: ThreadNet
  }

runThreadNet :: HasCallStack => RunThreadNetArgs -> TestOutput (CardanoBlock StandardCrypto)
runThreadNet args@RunThreadNetArgs{..} =
  let
    numCoreNodes = NumCoreNodes . fromIntegral . length . tnCoreNodes $ rtnaThreadNet
    testConfig =
      TestConfig
        { numSlots = NumSlots $ fromIntegral rtnaSlots
        , numCoreNodes
        , nodeTopology = tnTopology rtnaThreadNet
        , initSeed = Seed 0
        }

    (pinfo, _) = protocolInfoCardano @_ @IO (cnCardanoProtocolParams . head . tnCoreNodes $ rtnaThreadNet) -- TODO(bladyjoker): Only to extract slot/epoch info
    shelleyGenesis :: ShelleyGenesis =
      shelleyLedgerGenesis
        . shelleyLedgerConfig
        . ( \(CardanoLedgerConfig _cfgByron cfgShelley _cfgAllegra _cfgMary _cfgAlonzo _cfgBabbage _cfgConway) -> cfgShelley
          )
        . topLevelConfigLedger
        . pInfoConfig
        $ pinfo
    slotLength = mkSlotLength $ SL.fromNominalDiffTimeMicro $ SL.sgSlotLength shelleyGenesis
    epochLength = sgEpochLength shelleyGenesis

    testConfigB =
      TestConfigB
        { forgeEbbEnv = Nothing
        , future =
            EraFinal slotLength epochLength
        , messageDelay = noCalcMessageDelay
        , nodeJoinPlan = trivialNodeJoinPlan numCoreNodes
        , nodeRestarts = noRestarts
        , txGenExtra = args
        , version = newestVersion (Proxy :: Proxy (CardanoBlock StandardCrypto))
        }
   in
    runTestNetwork
      testConfig
      testConfigB
      TestConfigMB
        { nodeInfo = \(CoreNodeId ix) ->
            let (protocolInfo, blockForging) =
                  protocolInfoCardano . cnCardanoProtocolParams . (!! fromIntegral ix) . tnCoreNodes $ rtnaThreadNet
             in TestNodeInitialization
                  { tniProtocolInfo = protocolInfo
                  , tniCrucialTxs = []
                  , tniBlockForging = blockForging
                  }
        , mkRekeyM = Nothing
        }

-- * Transaction generators

instance TxGen (CardanoBlock StandardCrypto) where
  type TxGenExtra (CardanoBlock StandardCrypto) = RunThreadNetArgs

  testGenTxs ::
    CoreNodeId ->
    NumCoreNodes ->
    SlotNo ->
    TopLevelConfig (CardanoBlock StandardCrypto) ->
    TxGenExtra (CardanoBlock StandardCrypto) ->
    LedgerState (CardanoBlock StandardCrypto) ValuesMK ->
    Gen [GenTx (CardanoBlock StandardCrypto)]
  testGenTxs coreNodeId _numCores slotNo _topCfg RunThreadNetArgs{..} (LedgerStateConway st) =
    let
      pparams = getPParams st
      utxos = getUTxOs st
     in
      return $
        Debug.trace (show ("current slot", slotNo)) $
          -- Interleave transactions produced by each generator and take `rtnaTxsPerSlot` of them
          take (fromIntegral rtnaTxsPerSlot) . interleave $
            do
              txg <- tnTxGenerators rtnaThreadNet
              let txs = GenTxConway . mkShelleyTx <$> handleTxGenerator coreNodeId pparams utxos txg
              return txs
  testGenTxs _coreNodeId _numCores _slotNo _topCfg _txGenExtra _st = error "not in conway"

handleTxGenerator ::
  SL.ShelleyBasedEra era =>
  CoreNodeId -> SL.PParams era -> Map SL.TxIn (SL.TxOut era) -> TxGenerator -> [SL.Tx era]
handleTxGenerator coreNodeId pparams utxos TxGenerator{..} = do
  guard (coreNodeId `Set.member` tgSubmitToNodes)
  tx <- case tgTxGeneratorKind of
    TransferAllKind ta -> handleTransferAllTx pparams utxos ta
  return tx

handleTransferAllTx ::
  SL.ShelleyBasedEra era => SL.PParams era -> Map SL.TxIn (SL.TxOut era) -> TransferAll -> [SL.Tx era]
handleTransferAllTx pparams utxos TransferAll{..} = do
  let
    paymentKeyPair = keyPairFromSigningKey taPaymentSigningKey
    paymentCred = getPaymentCredFromKeyPair paymentKeyPair
  (txOutRef, txOut) <- Map.toList $ getUTxOsByPaymentCred utxos paymentCred
  transferAllTxSequence (txOut, txOutRef, pparams, paymentKeyPair)

-- `transferAllTxSequence arg` makes a sequence of chained `transferAllTx` transactions.
transferAllTxSequence ::
  SL.ShelleyBasedEra era => (SL.TxOut era, SL.TxIn, SL.PParams era, KeyPair kr) -> [SL.Tx era]
transferAllTxSequence (txOut, txOutRef, pparams, paymentKeyPair) =
  let
    tx = buildTx $ transferAllTx (txOut, txOutRef, pparams, paymentKeyPair)
    txOut' =
      maybe (error "sequence lookup failed") id $
        StrictSeq.lookup 0 $
          tx ^. (SL.bodyTxL . SL.outputsTxBodyL)
    txOutRef' = SL.TxIn (SL.txIdTx tx) (TxIx 0)
   in
    tx : transferAllTxSequence (txOut', txOutRef', pparams, paymentKeyPair)

-- `transferAllTx (txOut, txOutRef, pparams, paymentKeyPair)` makes a transaction that
-- consumes the UTxO denoted by `txOutRef` and `txOut` that belogns to `paymentKeyPair`
-- and produces a single output with the value consumed.
transferAllTx ::
  SL.ShelleyBasedEra era =>
  (SL.TxOut era, SL.TxIn, SL.PParams era, KeyPair kr) -> SL.Tx era -> SL.Tx era
transferAllTx (txOut, txOutRef, pparams, paymentKeyPair) = \tx ->
  let
    inCoin = coin $ txOut ^. SL.valueTxOutL
    feeCoin = SL.getMinFeeTx pparams tx 0
    outCoin = inCoin <-> feeCoin

    txBody' =
      (tx ^. SL.bodyTxL)
        & SL.inputsTxBodyL
        .~ Set.singleton txOutRef
        & SL.outputsTxBodyL
        .~ StrictSeq.fromList
          [ SL.mkBasicTxOut (txOut ^. SL.addrTxOutL) (inject outCoin)
          ]
        & SL.feeTxBodyL
        .~ feeCoin
   in
    if outCoin > inCoin
      then error "spent all"
      else
        tx
          & SL.bodyTxL
          .~ txBody'
          & signTx paymentKeyPair

-- * Transaction building utils

keyPairFromSigningKey :: Crypto.SignKeyDSIGN Ledger.DSIGN -> KeyPair k
keyPairFromSigningKey signingKey =
  KeyPair
    { vKey = SL.VKey . Crypto.deriveVerKeyDSIGN $ signingKey
    , sKey = signingKey
    }

signTx :: SL.ShelleyBasedEra era => KeyPair k -> SL.Tx era -> SL.Tx era
signTx paymentKeyPair tx =
  let txBody = tx ^. SL.bodyTxL
   in tx
        & (SL.witsTxL . SL.addrTxWitsL)
        .~ ( Set.fromList
               [ TL.mkWitnessVKey (hashAnnotated txBody) paymentKeyPair
               ]
           )

getUTxOs ::
  LedgerState
    (ShelleyBlock c era)
    ValuesMK ->
  Map.Map SL.TxIn (SL.TxOut era)
getUTxOs = getValuesMK . getLedgerTables . SL.shelleyLedgerTables

getPParams ::
  forall era c mk. ShelleyBasedEra era => LedgerState (ShelleyBlock c era) mk -> SL.PParams era
getPParams = SL.getPParams . SL.shelleyLedgerState

getUTxOsByPaymentCred ::
  SL.ShelleyBasedEra era =>
  Map SL.TxIn (SL.TxOut era) -> SL.Credential SL.Payment -> Map SL.TxIn (SL.TxOut era)
getUTxOsByPaymentCred utxos payCred =
  let res =
        Map.filter
          ( \txOut -> case txOut ^. SL.addrTxOutL of
              SL.Addr _net payCred' _stakeRef -> payCred == payCred'
              _ -> False
          )
          utxos
   in res

getPaymentCredFromKeyPair :: KeyPair 'SL.Payment -> SL.Credential SL.Payment
getPaymentCredFromKeyPair keyPair =
  let
    pkh = SL.hashKey . vKey $ keyPair
   in
    SL.KeyHashObj pkh

buildTx ::
  ShelleyBasedEra era => (SL.Tx era -> SL.Tx era) -> SL.Tx era
buildTx mkTx =
  let
    initialTx = SL.mkBasicTx SL.mkBasicTxBody
   in
    convergeTx mkTx initialTx
 where
  convergeTx :: Eq a => (a -> a) -> a -> a
  convergeTx f start = until (\x -> f x == x) f start

-- * Aeson utils

readJson :: (HasCallStack, FromJSON a) => FilePath -> IO a
readJson path = do
  errOrVal <- Aeson.eitherDecodeFileStrict' path
  case errOrVal of
    Left err -> error $ show ("Failed reading JSON", err, callStack)
    Right val -> return val

writeJson :: ToJSON a => FilePath -> a -> IO ()
writeJson path val = Aeson.encodeFile path val

-- * Misc

interleave :: [[a]] -> [a]
interleave = concat . transpose
