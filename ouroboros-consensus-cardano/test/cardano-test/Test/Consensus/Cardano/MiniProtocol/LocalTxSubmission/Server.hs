{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}

-- | Test that we can submit transactions to the mempool using the local
-- submission server, in different Cardano eras.
--
module Test.Consensus.Cardano.MiniProtocol.LocalTxSubmission.Server (tests) where

import           Control.Monad (void)
import           Control.Tracer (Tracer, nullTracer, stdoutTracer)
import           Data.Functor.Contravariant ((>$<))
import           Data.SOP.Strict (index_NS)
import qualified Data.SOP.Telescope as Telescope
import           Network.TypedProtocol.Proofs (connect)
import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.Config (topLevelConfigLedger)
import qualified Ouroboros.Consensus.Config as Consensus
import           Ouroboros.Consensus.HardFork.Combinator (getHardForkState,
                     hardForkLedgerStatePerEra)
import           Ouroboros.Consensus.Ledger.Extended (ledgerState)
import qualified Ouroboros.Consensus.Ledger.SupportsMempool as Ledger
import qualified Ouroboros.Consensus.Ledger.SupportsMempool as LedgerSupportsMempool
import qualified Ouroboros.Consensus.Mempool.Capacity as Mempool
import           Ouroboros.Consensus.MiniProtocol.LocalTxSubmission.Server
                     (TraceLocalTxSubmissionServerEvent,
                     localTxSubmissionServer)
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Network.Protocol.LocalTxSubmission.Client
                     (SubmitResult, localTxSubmissionClientPeer)
import           Ouroboros.Network.Protocol.LocalTxSubmission.Examples
                     (localTxSubmissionClient)
import           Ouroboros.Network.Protocol.LocalTxSubmission.Server
                     (localTxSubmissionServerPeer)
import           Ouroboros.Network.SizeInBytes
import           Test.Consensus.Cardano.MiniProtocol.LocalTxSubmission.ByteStringTxParser
                     (deserialiseTx)
import           Test.Consensus.Cardano.ProtocolInfo
                     (ByronSlotLengthInSeconds (..), Era (..),
                     ShelleySlotLengthInSeconds (..), hardForkInto,
                     mkSimpleTestProtocolInfo, protocolVersionZero)
import qualified Test.Consensus.Mempool.Mocked as Mocked
import           Test.Consensus.Mempool.Mocked (MockedMempool)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit (testCase, (@=?))
import qualified Test.ThreadNet.Infra.Shelley as Shelley

tests :: TestTree
tests =
    testGroup "LocalTxSubmissionServer"
      $ fmap localServerPassesRegressionTests [Byron ..]
  where
    localServerPassesRegressionTests era =
        testCase ("Passes the regression tests (" ++ show era ++ ")") $ do
          let
            pInfo :: ProtocolInfo (CardanoBlock StandardCrypto)
            pInfo = mkSimpleTestProtocolInfo
                        (Shelley.DecentralizationParam 1)
                        (Consensus.SecurityParam 10)
                        (ByronSlotLengthInSeconds 1)
                        (ShelleySlotLengthInSeconds 1)
                        protocolVersionZero
                        (hardForkInto era)

            eraIndex = index_NS
                     . Telescope.tip
                     . getHardForkState
                     . hardForkLedgerStatePerEra
                     . ledgerState
                     $ pInfoInitLedger pInfo

          eraIndex @=? fromEnum era

          let
            -- We don't want the mempool to fill up during these tests.
            capcityBytesOverride = Mempool.mkCapacityBytesOverride 100_000
            -- Use 'show >$< stdoutTracer' for debugging.
            tracer               = nullTracer
            mempoolParams        = Mocked.MempoolAndModelParams {
                Mocked.immpInitialState =
                  ledgerState $ pInfoInitLedger pInfo
              , Mocked.immpLedgerConfig =
                  topLevelConfigLedger $ pInfoConfig pInfo
              }

          mempool <- Mocked.openMockedMempool
                      capcityBytesOverride
                      tracer
                      mempoolParams

          mempool `should_process` [ _137 ]
      where
        -- Reported in https://github.com/IntersectMBO/ouroboros-consensus/issues/137
        _137 :: GenTx (CardanoBlock StandardCrypto)
        _137 = either (error . show) snd (deserialiseTx _137_bs)
          where
            _137_bs =  "8205d818590210" <> "84a400828258203a79a6a834e7779c67b0b3cbd3b7271883bbbeac15b1a89d78f057edc25e000b008258203a79a6a834e7779c67b0b3cbd3b7271883bbbeac15b1a89d78f057edc25e000b010182825839007d5a2560d23c3443b98d84c57b0c491311da4b3098de1945c7bcfc4c63ea8c5404f9ed9ae80d95b5544857b2011e3f26b63ddc3be1abd42d1a001e84808258390009ecea977429fa7a4993bc045ea618f3697e6b8eac9d5ea68bba7e4b63ea8c5404f9ed9ae80d95b5544857b2011e3f26b63ddc3be1abd42d821a560ea01ca4581c47be64fcc8a7fe5321b976282ce4e43e4d29015f6613cfabcea28eaba244546573741a3b97c0aa51576f52456d706972654c696368303037391a3443f4a0581c4cd2ea369880853541c5f446725f3e4ecaf141635f0c56c43104923ba14574464c41431b0de0b6b346d4b018581c85ef026c7da6a91f7acc1e662c50301bcce79eb401a3217690aa7044a14574464c41431b000000022eaca140581c92bd3be92d6a6eadd7c01ce9ff485809f3f2eb36845cd7a25c9177bfa14b546f20746865206d6f6f6e01021a0002b9b5031a05a18ef7a100818258202726733baa5c15d8d856c8d94e7d83bcfc7f5661ec7f952f052f311a2443feb258405f9d3d8a703baf700a3015994a3e8702fd7fe2e25d640487944b32ea999f36b314be9674be09b8b8f2c678976ecf994c83086180e854120d81243476c2b89e05f5f6"

-- | Check that the given transactions can be processed, irrespective of whether
-- they were sucessfully validated.
should_process :: MockedMempool IO blk -> [Ledger.GenTx blk] -> IO ()
should_process mockedMempool txs = do
    void $ processTxs nullTracer mockedMempool txs

processTxs ::
     Tracer IO (TraceLocalTxSubmissionServerEvent blk)
  -> MockedMempool IO blk
  -> [Ledger.GenTx blk]
  -> IO [(GenTx blk, SubmitResult (LedgerSupportsMempool.ApplyTxErr blk))]
processTxs tracer mockedMempool txs =
    (\(a, _, _) -> a) <$>
      connect (localTxSubmissionClientPeer client) (localTxSubmissionServerPeer mServer)
  where
    mServer = pure $ localTxSubmissionServer tracer
                                             (Mocked.getMempool mockedMempool)
    client  = localTxSubmissionClient txs

-- TODO: this function is unused at the moment. We will use it once we add tests
-- for Cardano transactions that are supposed to succeed.
_should_process_and_return ::
     ( Show (Ledger.GenTx blk)
     , Eq   (Ledger.ApplyTxErr blk)
     , Show (SubmitResult (LedgerSupportsMempool.ApplyTxErr blk))
     )
  => MockedMempool IO blk -> [(Ledger.GenTx blk, SubmitResult (LedgerSupportsMempool.ApplyTxErr blk))] -> IO ()
_should_process_and_return mockedMempool txs_ress = do
    processResult <- processTxs (show >$< stdoutTracer) mockedMempool (fmap fst txs_ress)
    let
      actualResults   = fmap snd processResult
      expectedResults = fmap snd txs_ress
    length actualResults  @=? length expectedResults
    mapM_ (uncurry (@=?)) $ zip expectedResults actualResults
    pure ()
