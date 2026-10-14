{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | The Dijkstra predicate failures render to JSON.
--
-- These instances used to be @error@ stubs, so the first Dijkstra block that
-- failed validation crashed whichever thread forced its trace message. Each
-- case here goes through the full chain of instances for 'DijkstraEra', from
-- 'BlockTransitionError' or 'DijkstraLedgerPredFailure' down to the leaf
-- failure, and forces the encoded JSON.
module Test.Consensus.Tracing.Dijkstra (tests) where

import qualified Cardano.Crypto.Hash.Class as Crypto
import Cardano.Ledger.Address (AccountAddress (..), AccountId (..))
import Cardano.Ledger.BaseTypes (Network (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Governance (Voter (..))
import qualified Cardano.Ledger.Conway.Rules as Conway
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Dijkstra (DijkstraEra)
import qualified Cardano.Ledger.Dijkstra.Rules as Dijkstra
import Cardano.Ledger.Hashes (KeyHash (..))
import Cardano.Ledger.Shelley.API (BlockTransitionError (..), Withdrawals (..))
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Cardano.Logging (DetailLevel (..), LogFormatting (..))
import Control.Exception (evaluate)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import Ouroboros.Consensus.Tracing ()
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Dijkstra"
    [ testCase "BlockTransitionError" $
        rendersKind "DijkstraBbodyPredFail" $
          BlockTransitionError @DijkstraEra $
            NonEmpty.singleton $
              Dijkstra.LedgersFailure $
                Shelley.LedgerFailure $
                  Dijkstra.DijkstraUtxowFailure (Dijkstra.UtxoFailure Dijkstra.InputSetEmptyUTxO)
    , testCase "LEDGER > UTXOW > UTXO" $
        rendersKind "InputSetEmptyUTxO" $
          ledger (Dijkstra.DijkstraUtxowFailure (Dijkstra.UtxoFailure Dijkstra.InputSetEmptyUTxO))
    , testCase "LEDGER > ENTITIES" $
        rendersKind "WithdrawalAccountsMissing" $
          ledger . Dijkstra.DijkstraEntitiesFailure $
            Dijkstra.WithdrawalAccountsMissing (Withdrawals (Map.singleton account (Coin 1)))
    , testCase "LEDGER > ENTITIES > CERTS > CERT > POOL" $
        rendersKind "PoolFailure" $
          ledger . Dijkstra.DijkstraEntitiesFailure . Dijkstra.CertsFailure . Conway.CertFailure $
            Conway.PoolFailure (Dijkstra.StakePoolNotRegisteredOnKeyPOOL (keyHash 'p'))
    , testCase "LEDGER > ENTITIES > CERTS > CERT > GOVCERT" $
        rendersKind "GovCertFailure" $
          ledger . Dijkstra.DijkstraEntitiesFailure . Dijkstra.CertsFailure . Conway.CertFailure $
            Conway.GovCertFailure (Dijkstra.DijkstraDRepNotRegistered (KeyHashObj (keyHash 'd')))
    , testCase "LEDGER > GOV" $
        rendersKind "VotersDoNotExist" $
          ledger . Dijkstra.DijkstraGovFailure $
            Dijkstra.VotersDoNotExist (NonEmpty.singleton (DRepVoter (KeyHashObj (keyHash 'v'))))
    ]
 where
  ledger ::
    Dijkstra.DijkstraLedgerPredFailure DijkstraEra -> Dijkstra.DijkstraLedgerPredFailure DijkstraEra
  ledger = id

-- | Force the whole encoded message, as a tracer backend would, and check the
-- top-level @kind@.
rendersKind :: LogFormatting a => String -> a -> Assertion
rendersKind kind x = do
  rendered <- evaluate . BL.toStrict . Aeson.encode $ forMachine DDetailed x
  assertBool
    ("expected kind " <> show kind <> " in " <> BS8.unpack rendered)
    (BS8.pack ("\"kind\":\"" <> kind <> "\"") `BS8.isInfixOf` rendered)

keyHash :: Char -> KeyHash r
keyHash c = KeyHash (Crypto.castHash (Crypto.hashWith id (BS8.singleton c)))

account :: AccountAddress
account = AccountAddress Testnet (AccountId (KeyHashObj (keyHash 'a')))
