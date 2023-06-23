{-# LANGUAGE FlexibleContexts #-}
module Test.Consensus.MiniProtocol.LocalTxSubmission.Server (tests) where

import           Control.Monad.IOSim (runSimOrThrow)
import           Control.Tracer (nullTracer, stdoutTracer)
import           Data.Functor.Contravariant ((>$<))
import           Network.TypedProtocol.Proofs (connect)
import qualified Ouroboros.Consensus.Ledger.SupportsMempool as Ledger
import qualified Ouroboros.Consensus.Mempool.Capacity as Mempool
import           Ouroboros.Consensus.MiniProtocol.LocalTxSubmission.Server
                     (localTxSubmissionServer)
import           Ouroboros.Consensus.Util.IOLike (IOLike)
import           Ouroboros.Network.Protocol.LocalTxSubmission.Client
                     (localTxSubmissionClientPeer)
import           Ouroboros.Network.Protocol.LocalTxSubmission.Examples
                     (localTxSubmissionClient)
import           Ouroboros.Network.Protocol.LocalTxSubmission.Server
                     (localTxSubmissionServerPeer)
import           Ouroboros.Network.Protocol.LocalTxSubmission.Type
                     (SubmitResult (SubmitFail, SubmitSuccess))
import qualified Test.Consensus.Mempool.Mocked as Mocked
import           Test.Consensus.Mempool.Mocked (MockedMempool)
import           Test.Consensus.MiniProtocol.LocalTxSubmission.FaultyLedger as FaultyLedger
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit (testCase, (@=?))

{-------------------------------------------------------------------------------
  Top-level tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "LocalTxSubmissionServer"
    [ testCase  "Exceptions are properly handled" $ do
        mempool <- Mocked.openMockedMempool (Mempool.mkCapacityBytesOverride 100) -- TODO: see how to determine this
                                            (show >$< stdoutTracer)
                                            FaultyLedger.txSize
                                            FaultyLedger.initialLedgerState
                                            FaultyLedger.sampleLedgerConfig
        mempool `handles_the_exceptions_caused_by`
          [ (FaultyLedger.mkTxThatErrorsWith "Ledger error!", FaultyLedger.TxApplicationError) ]
    ]


{-------------------------------------------------------------------------------
  Test cases
-------------------------------------------------------------------------------}

-- TODO: We might want to abstract m away so that we can use IOSim. I don't know if it has added value though.
--
-- handles_the_exceptions_caused_by ::
--      (IOLike m, Show (Ledger.GenTx blk))
--   => MockedMempool m blk -> [(Ledger.GenTx blk, Ledger.ApplyTxErr blk)] -> m ()
handles_the_exceptions_caused_by ::
     ( Show (Ledger.GenTx blk)
     , Eq   (Ledger.ApplyTxErr blk)
     , Show (Ledger.ApplyTxErr blk)
     )
  => MockedMempool IO blk -> [(Ledger.GenTx blk, Ledger.ApplyTxErr blk)] -> IO ()
handles_the_exceptions_caused_by mockedMempool txs_errs = do
    res <- do
      let
        mServer = pure $ localTxSubmissionServer (show >$< stdoutTracer)
                                                 (Mocked.getMempool mockedMempool)
        client  = localTxSubmissionClient (fmap fst txs_errs)
      (\(a, _, _) -> a) <$>
        connect (localTxSubmissionClientPeer client) (localTxSubmissionServerPeer mServer)
    let expectedErrors = fmap snd txs_errs
        actualErrors   = fmap (unsafeGetError . snd) res
        unsafeGetError (SubmitFail reason) = reason
        unsafeGetError _                   = error "Expected error"
    length expectedErrors @=? length actualErrors
    mapM_ (uncurry (@=?)) $ zip expectedErrors actualErrors
