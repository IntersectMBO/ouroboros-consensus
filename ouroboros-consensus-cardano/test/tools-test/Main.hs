{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main (main) where

import qualified Data.Set as Set
import Cardano.Ledger.Credential
import Cardano.Ledger.Keys
import Cardano.Api.Any
import Cardano.Api.Key
import Cardano.Ledger.Address
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Shelley.Genesis
import Cardano.Api.KeysShelley
import Cardano.Api.SerialiseTextEnvelope
import qualified Cardano.Tools.DBAnalyser.Block.Cardano as Cardano
import qualified Cardano.Tools.DBAnalyser.Run as DBAnalyser
import           Cardano.Tools.DBAnalyser.Types
import qualified Cardano.Tools.DBSynthesizer.Run as DBSynthesizer
import           Cardano.Tools.DBSynthesizer.Types
import           Cardano.Tools.DBSynthesizer.Tx
import           Ouroboros.Consensus.Cardano.Block
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Util.TestEnv


nodeConfig, chainDB :: FilePath
nodeConfig  = "test/tools-test/disk/config/config.json"
chainDB     = "test/tools-test/disk/chaindb"


testSynthOptionsCreate :: DBSynthesizerOptions
testSynthOptionsCreate =
    DBSynthesizerOptions {
        synthLimit          = ForgeLimitEpoch 1
      , synthOpenMode       = OpenCreateForce
    }

testSynthOptionsAppend :: DBSynthesizerOptions
testSynthOptionsAppend =
    DBSynthesizerOptions {
        synthLimit          = ForgeLimitSlot 8192
      , synthOpenMode       = OpenAppend
    }

testNodeFilePaths :: NodeFilePaths
testNodeFilePaths =
    NodeFilePaths {
        nfpConfig   = nodeConfig
      , nfpChainDB  = chainDB
    }

testNodeCredentials :: NodeCredentials
testNodeCredentials =
    NodeCredentials {
        credCertFile  = Nothing
      , credVRFFile   = Nothing
      , credKESFile   = Nothing
      , credBulkFile  = Just "test/tools-test/disk/config/bulk-creds-k2.json"
    }

testAnalyserConfig :: DBAnalyserConfig
testAnalyserConfig =
  DBAnalyserConfig {
      dbDir       = chainDB
    , verbose     = False
    , selectDB    = SelectChainDB
    , validation  = Just ValidateAllBlocks
    , analysis    = CountBlocks
    , confLimit   = Unlimited
    }

testBlockArgs :: Cardano.Args (CardanoBlock StandardCrypto)
testBlockArgs = Cardano.CardanoBlockArgs nodeConfig Nothing

-- | A multi-step test including synthesis and analaysis 'SomeConsensusProtocol' using the Cardano instance.
--
-- 1. step: synthesize a ChainDB from scratch and count the amount of blocks forged.
-- 2. step: append to the previous ChainDB and coutn the amount of blocks forged.
-- 3. step: analyze the ChainDB resulting from previous steps and confirm the total block count.

--
blockCountTest :: (String -> IO ()) -> Assertion
blockCountTest logStep = do
    logStep "running synthesis - create"
    (options, protocol) <- either assertFailure pure
        =<< DBSynthesizer.initialize
          testNodeFilePaths
          testNodeCredentials
          testSynthOptionsCreate
    (skey, cred) <- readFileTextEnvelope (AsSigningKey AsGenesisUTxOKey) "test/tools-test/disk/config/utxo-keys/utxo1.skey" >>= \case
      Left e -> throwErrorAsException e
      Right sk@(GenesisUTxOSigningKey k)
        | GenesisUTxOVerificationKey vk <- getVerificationKey sk -> pure (k, KeyHashObj (hashKey vk))
    let
      addr = Addr Testnet cred StakeRefNull
      utxo = OwnedTxIn
        { owned = initialFundsPseudoTxIn addr
        , skey
        }
      spec =
        [ TxOutsSpec
            { duplicates = 999
            , txOut = TxOutSpec
                { nativeAssets = []
                , datum = Nothing
                , delegation = DelegateHash
                }
            }
        , TxOutsSpec
            { duplicates = 4999
            , txOut = TxOutSpec
                { nativeAssets = [ NativeAssetMintsSpec { nameLengths = Set.fromList [ 0, 3, 6 ] }
                                 , NativeAssetMintsSpec { nameLengths = Set.singleton 5 }
                                 ]
                , datum = Just $ Inline 52
                , delegation = NoDelegate
                }
            }
        ]
    genTxs <- makeGenTxs utxo spec
    resultCreate <- DBSynthesizer.synthesize genTxs options protocol
    let blockCountCreate = resultForged resultCreate
    blockCountCreate > 0 @? "no blocks have been forged during create step"

    -- TODO check for txn gen

    logStep "running synthesis - append"
    resultAppend <- DBSynthesizer.synthesize genTxs options {confOptions = testSynthOptionsAppend} protocol
    let blockCountAppend = resultForged resultAppend
    blockCountAppend > 0 @? "no blocks have been forged during append step"

    logStep "running analysis"
    resultAnalysis <- DBAnalyser.analyse testAnalyserConfig testBlockArgs

    let blockCount = blockCountCreate + blockCountAppend
    resultAnalysis == Just (ResultCountBlock blockCount) @?
        "wrong number of blocks encountered during analysis \
        \ (counted: " ++ show resultAnalysis ++ "; expected: " ++ show blockCount ++ ")"

tests :: TestTree
tests =
    testGroup "cardano-tools"
      [ testCaseSteps "synthesize and analyse: blockCount\n" blockCountTest
      ]

main :: IO ()
main = defaultMainWithTestEnv defaultTestEnvConfig tests
