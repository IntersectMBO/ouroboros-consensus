{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Checks 'inlineCertifiedEbTxs': what node-to-client ChainSync sends for one
-- block.
module Test.Consensus.Cardano.MiniProtocol.ChainSync.Server (tests) where

import qualified Cardano.Crypto.Hash as Hash
import qualified Cardano.Ledger.BaseTypes as SL (ProtVer (..))
import qualified Cardano.Ledger.Core as Core
import qualified Codec.CBOR.Write as CBOR
import Control.Exception (throwIO, try)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as Lazy
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Cardano.Block
import Ouroboros.Consensus.Cardano.Node ()
import Ouroboros.Consensus.Leios.Types (LeiosPoint (..), LeiosTx (..), SerialisedEndorserBlock (..))
import Ouroboros.Consensus.MiniProtocol.ChainSync.Server
  ( CertRbWithoutEbTxs (..)
  , CertRbWithoutEbTxsReason (..)
  , inlineCertifiedEbTxs
  )
import Ouroboros.Consensus.Shelley.HFEras (StandardDijkstraBlock)
import Ouroboros.Consensus.Shelley.Ledger
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Consensus.Storage.LedgerDB.Forker (ResolveEndorserBlock)
import Ouroboros.Consensus.Storage.Serialisation (EncodeDisk (..))
import Ouroboros.Network.Block (Serialised (..))
import Test.Consensus.Cardano.SupportsLeios
  ( firstExample
  , genCertRb
  , genEbTxs
  , serialisedEb
  )
import qualified Test.Consensus.Shelley.Examples as Shelley
import Test.QuickCheck
import Test.QuickCheck.Gen (unGen)
import Test.QuickCheck.Random (mkQCGen)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "inlineCertifiedEbTxs"
    [ testCase
        "a block without a certificate is sent as stored, without decoding it"
        test_notCertRb
    , testCase
        "a CertRB is sent with the endorser block's transactions"
        test_certRb
    , testCase
        "a CertRB with no known announcement throws NoAnnouncement"
        test_noAnnouncement
    , testCase
        "a CertRB that does not decode throws Undecodable"
        test_undecodable
    , testCase
        "transactions that do not decode throw EbTxsNotInlined"
        test_ebTxsNotInlined
    ]

-- | The function only reads the header of a block without a certificate, so
-- its bytes need not even be a block, and the resolver is never called.
test_notCertRb :: IO ()
test_notCertRb = do
  let blk = firstExample Shelley.examplesDijkstra
  got <- send noResolver (Just announcement) blk notBytes
  got @?= notBytes

-- | What is sent is the CertRB with the transactions added by
-- 'inlineEndorserBlockTxs', which has its own property, so one CertRB is
-- enough.
test_certRb :: IO ()
test_certRb = do
  let certRb = fixed genDijkstraCertRb
      eb = serialisedEb (fixed genEbTxs)
  expected <- either (assertFailure . show) pure (inlineEndorserBlockTxs eb certRb)
  got <- send (resolveTo eb) (Just announcement) certRb (encode certRb)
  got @?= encode expected

test_noAnnouncement :: IO ()
test_noAnnouncement = do
  let certRb = fixed genDijkstraCertRb
  expectReason (\case NoAnnouncement -> True; _ -> False) $
    send noResolver Nothing certRb (encode certRb)

test_undecodable :: IO ()
test_undecodable = do
  let certRb = fixed genDijkstraCertRb
  expectReason (\case Undecodable{} -> True; _ -> False) $
    send noResolver (Just announcement) certRb notBytes

test_ebTxsNotInlined :: IO ()
test_ebTxsNotInlined = do
  let certRb = fixed genDijkstraCertRb
      eb = (serialisedEb []){serialisedTransactions = [MkLeiosTx (BS8.pack "not a tx")]}
  expectReason (\case EbTxsNotInlined{} -> True; _ -> False) $
    send (resolveTo eb) (Just announcement) certRb (encode certRb)

{-------------------------------------------------------------------------------
  Running the function
-------------------------------------------------------------------------------}

-- | What 'inlineCertifiedEbTxs' sends for this block, stored as these bytes.
send ::
  ResolveEndorserBlock IO ->
  Maybe LeiosPoint ->
  StandardDijkstraBlock ->
  Serialised StandardDijkstraBlock ->
  IO (Serialised StandardDijkstraBlock)
send resolveEb mAnnounced blk =
  inlineCertifiedEbTxs
    blockConfig
    codecConfig
    resolveEb
    mAnnounced
    (blockPoint blk)
    (getHeader blk)

blockConfig :: BlockConfig StandardDijkstraBlock
blockConfig =
  mkShelleyBlockConfig
    (SL.ProtVer (Core.eraProtVerLow @DijkstraEra) 0)
    Shelley.testShelleyGenesis
    []

codecConfig :: CodecConfig StandardDijkstraBlock
codecConfig = ShelleyCodecConfig

-- | The endorser block the block before the CertRB announced.
announcement :: LeiosPoint
announcement = MkLeiosPoint (SlotNo 0) (Hash.castHash (Hash.hashWith (const mempty) ()))

-- | A resolver that has this endorser block at 'announcement', and no other.
resolveTo :: SerialisedEndorserBlock -> ResolveEndorserBlock IO
resolveTo eb point
  | point == announcement = pure eb
  | otherwise = throwIO $ userError "resolved the wrong endorser block"

-- | A resolver the test does not expect to be called.
noResolver :: ResolveEndorserBlock IO
noResolver _ = throwIO $ userError "the resolver was called"

-- | Bytes that are not a block.
notBytes :: Serialised StandardDijkstraBlock
notBytes = Serialised (Lazy.fromStrict (BS8.pack "not a block"))

-- | That the action throws 'CertRbWithoutEbTxs' with a reason the predicate
-- accepts.
expectReason :: (CertRbWithoutEbTxsReason -> Bool) -> IO a -> IO ()
expectReason isExpected action =
  try @(CertRbWithoutEbTxs StandardDijkstraBlock) action >>= \case
    Left e
      | isExpected (certRbReason e) -> pure ()
      | otherwise -> assertFailure $ "unexpected reason: " <> show (certRbReason e)
    Right _ -> assertFailure "expected CertRbWithoutEbTxs"

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

-- | A value from the generator, the same on every run, so a failure can be
-- reproduced.
fixed :: Gen a -> a
fixed gen = unGen gen (mkQCGen 0) 30

genDijkstraCertRb :: Gen StandardDijkstraBlock
genDijkstraCertRb =
  genCertRb >>= \case
    BlockDijkstra blk -> pure blk
    _ -> error "genCertRb: not a Dijkstra block"

encode :: StandardDijkstraBlock -> Serialised StandardDijkstraBlock
encode = Serialised . CBOR.toLazyByteString . encodeDisk codecConfig
