{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

-- | Checks 'inlineEndorserBlockTxs', which node-to-client ChainSync uses to
-- send a CertRB with the transactions of the EB it certifies.
module Test.Consensus.Cardano.SupportsLeios (tests) where

import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import qualified Cardano.Ledger.Block as SL
import Cardano.Ledger.Core (Tx, TxLevel (TopTx))
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.TxIn as SL (TxId)
import Cardano.Protocol.Leios.BlockHeader
  ( HeaderBody (..)
  , HeaderBodyRaw (..)
  , headerBody
  , headerSig
  , mkHeader
  , mkHeaderBody
  )
import Data.Either (isLeft)
import Data.Foldable (toList)
import Lens.Micro ((^.))
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Cardano.Block
import Ouroboros.Consensus.Cardano.Node ()
import Ouroboros.Consensus.Leios.Types (LeiosTx (..), SerialisedEndorserBlock (..))
import Ouroboros.Consensus.Shelley.Ledger
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Network.Block (Serialised (..))
import Test.Cardano.Ledger.Dijkstra.Arbitrary
  ( genSmallDijkstraCertBlockBody
  , genSmallDijkstraTxsBlockBody
  )
import qualified Test.Consensus.Shelley.Examples as Shelley
import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)
import Test.Tasty.QuickCheck (testProperty)
import Test.Util.Serialisation.Examples (Examples (..))

tests :: TestTree
tests =
  testGroup
    "inlineEndorserBlockTxs"
    [ testProperty
        "a Dijkstra CertRB gets the EB's transactions, under the same hash"
        prop_dijkstraCertRb
    , testCase
        "a block in an era without Leios is refused"
        test_eraWithoutLeios
    ]

-- | The result keeps the CertRB's hash and certificate, and has the EB's
-- transactions in order. Its header's body hash no longer matches the body:
-- deliberately, as that is what node-to-client ChainSync sends.
--
-- 20 cases: the transactions don't change what the function does, and Dijkstra
-- transactions are slow to generate.
prop_dijkstraCertRb :: Property
prop_dijkstraCertRb =
  withNumTests 20 . forAllShow genCertRb (show . blockPoint) $ \certRb ->
    forAllShow genEbTxs (show . map Core.txIdTx) $ \ebTxs ->
      case inlineEndorserBlockTxs (serialisedEb ebTxs) certRb of
        Left err -> counterexample (show err) False
        Right got ->
          conjoin
            [ counterexample "hash" $ blockHash got === blockHash certRb
            , counterexample "certificate" $ blockLeiosCert got === blockLeiosCert certRb
            , counterexample "transactions" $ txIdsOf got === map Core.txIdTx ebTxs
            , counterexample "body hash of the stored CertRB" $ bodyHashMatches certRb
            , counterexample "body hash of the result" $ not (bodyHashMatches got)
            ]

test_eraWithoutLeios :: IO ()
test_eraWithoutLeios =
  assertBool "expected Left" . isLeft $
    inlineEndorserBlockTxs (serialisedEb []) (BlockConway (firstExample Shelley.examplesConway))

{-------------------------------------------------------------------------------
  Generators
-------------------------------------------------------------------------------}

-- | A CertRB in the form the node stores it, with a certificate in its body and
-- no transactions. Its header is the example Dijkstra block's header, changed
-- to claim a certificate and to hold the hash of this body.
genCertRb :: Gen (CardanoBlock StandardCrypto)
genCertRb = do
  body <- genSmallDijkstraCertBlockBody @DijkstraEra
  pure . BlockDijkstra . mkShelleyBlock $
    SL.Block
      (mkHeader (Proxy @DijkstraEra) (claimCert (headerBody hdr) body) (headerSig hdr))
      body
 where
  SL.Block hdr _ = shelleyBlockRaw (firstExample Shelley.examplesDijkstra)

-- | The header body for a CertRB with this body.
claimCert :: HeaderBody StandardCrypto -> Core.BlockBody DijkstraEra -> HeaderBody StandardCrypto
claimCert hb body =
  mkHeaderBody (Proxy @DijkstraEra) $
    HeaderBodyRaw
      { hbrBlockNo = hbBlockNo hb
      , hbrSlotNo = hbSlotNo hb
      , hbrPrev = hbPrev hb
      , hbrVk = hbVk hb
      , hbrVrfVk = hbVrfVk hb
      , hbrVrfRes = hbVrfRes hb
      , hbrBodySize = hbBodySize hb
      , hbrBodyHash = Core.hashBlockBody body
      , hbrOCert = hbOCert hb
      , hbrVersionInfo = hbVersionInfo hb
      , hbrBlockBodyContainsLeiosCert = True
      , hbrEbReferencesAnnouncement = SNothing
      }

-- | 1 to 5 small transactions, in order.
genEbTxs :: Gen [Tx TopTx DijkstraEra]
genEbTxs = do
  n <- choose (1, 5)
  (concat <$> vectorOf n (resize 5 genTxs)) `suchThat` (not . null)
 where
  genTxs = toList . (^. Core.txSeqBlockBodyL) <$> genSmallDijkstraTxsBlockBody @DijkstraEra

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

-- | An EB with these transactions, as the LeiosDB stores it.
-- 'inlineEndorserBlockTxs' reads only the transactions, so the endorsement is
-- left empty.
serialisedEb :: [Tx TopTx DijkstraEra] -> SerialisedEndorserBlock
serialisedEb txs =
  MkSerialisedEndorserBlock
    { serialisedEndorsement = Serialised mempty
    , serialisedTransactions = map (MkLeiosTx . eraTxLeiosBytes) txs
    }

-- | The transaction IDs in a Dijkstra block's body, in order.
txIdsOf :: CardanoBlock StandardCrypto -> [SL.TxId]
txIdsOf = \case
  BlockDijkstra blk ->
    let SL.Block _ body = shelleyBlockRaw blk
     in map Core.txIdTx (toList (body ^. Core.txSeqBlockBodyL))
  _ -> []

-- | Whether the body hash in the header matches the body.
bodyHashMatches :: CardanoBlock StandardCrypto -> Bool
bodyHashMatches = \case
  BlockDijkstra blk ->
    let SL.Block hdr body = shelleyBlockRaw blk
     in Core.hashBlockBody body == hbBodyHash (headerBody hdr)
  _ -> True

firstExample :: Examples blk -> blk
firstExample examples = case exampleBlock examples of
  (_, blk) : _ -> blk
  [] -> error "firstExample: no example block"
