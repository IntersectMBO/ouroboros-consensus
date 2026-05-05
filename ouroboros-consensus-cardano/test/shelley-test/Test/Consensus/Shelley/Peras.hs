{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Consensus.Shelley.Peras (tests) where

import Cardano.Binary (ToCBOR (toCBOR))
import qualified Cardano.Ledger.Dijkstra.BlockBody as SL
import qualified Codec.CBOR.Write as CBOR
import qualified Data.ByteString.Short as Short
import Data.MemPack.Buffer (byteArrayFromShortByteString)
import Ouroboros.Consensus.Block (Point (..))
import Ouroboros.Consensus.Block.SupportsPeras (PerasCert)
import Ouroboros.Consensus.Peras.Cert.Mock (MockPerasCert (..))
import Ouroboros.Consensus.Shelley.HFEras (StandardDijkstraBlock)
import Ouroboros.Consensus.Shelley.Ledger.Block
  ( ShelleyPerasCertCompatibleWithLedger (..)
  )
import Ouroboros.Consensus.Shelley.Node.Peras ()
import Test.Ouroboros.Storage.TestBlock (TestBlock)
import Test.QuickCheck
  ( Gen
  , Property
  , counterexample
  , forAll
  , property
  , (===)
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.Util.Peras (genPerasCert)
import Test.Util.Peras.Common (genRoundNo)
import Test.Util.Peras.Mock (genMockPerasVoterIndices)

tests :: TestTree
tests =
  testGroup
    "ShelleyBlockPerasCert"
    [ testProperty
        "Roundtrip through ShelleyBlockPerasCert for Dijkstra"
        prop_DijkstraPerasCertRoundtrip
    , testProperty
        "Deserializing an invalid Dijkstra Peras certificate fails"
        prop_DijkstraPerasCertRoundtripError
    ]

prop_DijkstraPerasCertRoundtrip :: Property
prop_DijkstraPerasCertRoundtrip =
  forAll (genPerasCert @StandardDijkstraBlock True) $ \cert -> do
    let ledgerCert = toLedgerPerasCert cert
    counterexample ("Ledger cert: " <> show ledgerCert) $
      fromLedgerPerasCert ledgerCert === Right cert

prop_DijkstraPerasCertRoundtripError :: Property
prop_DijkstraPerasCertRoundtripError =
  forAll genInvalidLedgerPerasCert $ \ledgerCert ->
    case fromLedgerPerasCert ledgerCert of
      Left _ ->
        property True
      Right (cert :: PerasCert StandardDijkstraBlock) ->
        counterexample
          ("Didn't fail to decode an invalid Dijkstra cert from: " <> show cert)
          $ False

-- | Generate an invalid ledger Peras cert by serializing a random mocked one.
genInvalidLedgerPerasCert :: Gen SL.PerasCert
genInvalidLedgerPerasCert = do
  mockCertRound <- genRoundNo
  mockCertVoters <- genMockPerasVoterIndices
  let mockCertBlock = GenesisPoint @TestBlock
  pure
    . SL.PerasCert
    . byteArrayFromShortByteString
    . Short.toShort
    . CBOR.toStrictByteString
    . toCBOR
    $ MockPerasCert
      { mockCertRound
      , mockCertBlock
      , mockCertVoters
      }
