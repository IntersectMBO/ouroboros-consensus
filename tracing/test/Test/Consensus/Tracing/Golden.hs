{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- | Golden output for the tracing rendering helpers.
--
-- These functions are what the era tracing instances put into the log, and
-- they were reimplemented off @cardano-api@ when the instances moved here.
-- Two of them silently changed shape in the process, which nothing caught --
-- hence these files. They pin the output as bytes so that a change to it has
-- to be an explicit, reviewed change to a golden file.
--
-- A golden file is only ever as good as the review of the diff that created it:
-- these were checked by hand against @cardano-api-11.5.0.0@, the version
-- @cardano-node@ used before the move.
module Test.Consensus.Tracing.Golden (tests) where

import qualified Cardano.Crypto.Hash.Class as Crypto
import Cardano.Ledger.Address (AccountAddress (..), AccountId (..))
import Cardano.Ledger.Alonzo.Scripts (AsItem (..), AsIx (..))
import Cardano.Ledger.BaseTypes (Mismatch (..), Network (..), Relation (..), TxIx (..))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Scripts (ConwayPlutusPurpose (..))
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Hashes
  ( KeyHash (..)
  , KeyRole (..)
  , ScriptHash (..)
  , unsafeMakeSafeHash
  )
import Cardano.Ledger.Mary.Value (PolicyID (..))
import Cardano.Ledger.TxIn (TxId (..), TxIn (..))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.NonEmpty (NonEmptyMap)
import qualified Data.Map.NonEmpty as NonEmptyMap
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Ouroboros.Consensus.Tracing.Era.Shelley.Render
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.Golden (goldenVsString)
import Test.Util.Paths (getRelPath)

tests :: TestTree
tests =
  testGroup
    "Golden"
    [ goldenVsString
        "Era.Shelley.Render"
        ($(getRelPath "tracing/golden") </> "era-shelley-render.golden")
        (pure (report shelleyRender))
    ]

--

-- * The rendered values

--

shelleyRender :: [(String, Text)]
shelleyRender =
  concat
    [
      [ ("renderScriptHash", renderScriptHash (scriptHash '1'))
      ,
        ( "renderScriptIntegrityHash Nothing"
        , json (renderScriptIntegrityHash Nothing)
        )
      ,
        ( "renderScriptIntegrityHash (Just _)"
        , json (renderScriptIntegrityHash (Just (unsafeMakeSafeHash (hash '2'))))
        )
      ,
        ( "renderRewardAccount mainnet/key"
        , renderRewardAccount (accountAddress Mainnet (KeyHashObj (keyHash '3')))
        )
      ,
        ( "renderRewardAccount testnet/key"
        , renderRewardAccount (accountAddress Testnet (KeyHashObj (keyHash '3')))
        )
      ,
        ( "renderRewardAccount mainnet/script"
        , renderRewardAccount (accountAddress Mainnet (ScriptHashObj (scriptHash '4')))
        )
      ]
    , -- Every purpose, by index. This is the ExtraRedeemers field, and it has to
      -- keep matching cardano-api's toScriptIndex + ToJSON ScriptWitnessIndex:
      -- a "kind" naming the witness index constructor, and "value".
      [ ("renderScriptIndex " <> label, json (renderScriptIndex purpose))
      | (label, purpose) <- purposesByIndex
      ]
    , -- Note the asymmetry: spending and rewarding render their item directly,
      -- minting wraps it in {"item": ...} via ToJSON (AsItem ix it). That is what
      -- cardano-api did.
      [ ("renderScriptPurpose " <> label, json (renderScriptPurpose purpose))
      | (label, purpose) <- purposesByItem
      ]
    ,
      [ ("renderMissingRedeemers", json (renderMissingRedeemers missingRedeemers))
      , ("renderIncompleteWithdrawals", json (renderIncompleteWithdrawals withdrawals))
      ]
    ]

purposesByIndex :: [(String, ConwayPlutusPurpose AsIx ConwayEra)]
purposesByIndex =
  [ ("spending", ConwaySpending (AsIx 0))
  , ("minting", ConwayMinting (AsIx 1))
  , ("certifying", ConwayCertifying (AsIx 2))
  , ("withdrawing", ConwayWithdrawing (AsIx 3))
  , ("voting", ConwayVoting (AsIx 4))
  , ("proposing", ConwayProposing (AsIx 5))
  ]

purposesByItem :: [(String, ConwayPlutusPurpose AsItem ConwayEra)]
purposesByItem =
  [ ("spending", ConwaySpending (AsItem txIn))
  , ("minting", ConwayMinting (AsItem (PolicyID (scriptHash '5'))))
  , ("withdrawing", ConwayWithdrawing (AsItem (accountAddress Mainnet (KeyHashObj (keyHash '6')))))
  ]

missingRedeemers :: NonEmpty.NonEmpty (ConwayPlutusPurpose AsItem ConwayEra, ScriptHash)
missingRedeemers =
  (ConwaySpending (AsItem txIn), scriptHash '7')
    NonEmpty.:| [(ConwayMinting (AsItem (PolicyID (scriptHash '8'))), scriptHash '9')]

withdrawals :: NonEmptyMap AccountAddress (Mismatch RelEQ Int)
withdrawals =
  NonEmptyMap.singleton
    (accountAddress Mainnet (KeyHashObj (keyHash '0')))
    Mismatch{mismatchSupplied = 1, mismatchExpected = 2}

--

-- * Fixtures

--
-- Deliberately built from a single byte so that the golden file stays readable
-- and a diff points at the rendering rather than at the input.
--

hash :: Crypto.HashAlgorithm h => Char -> Crypto.Hash h a
hash c = Crypto.castHash (Crypto.hashWith id (BS8.singleton c))

scriptHash :: Char -> ScriptHash
scriptHash = ScriptHash . hash

keyHash :: Char -> KeyHash r
keyHash = KeyHash . hash

accountAddress :: Network -> Credential Staking -> AccountAddress
accountAddress n c = AccountAddress n (AccountId c)

txIn :: TxIn
txIn = TxIn (TxId (unsafeMakeSafeHash (hash 'a'))) (TxIx 0)

--

-- * Report rendering

--

json :: Aeson.Value -> Text
json = Text.decodeUtf8 . BL.toStrict . Aeson.encode

report :: [(String, Text)] -> BL.ByteString
report items =
  BL.fromStrict . Text.encodeUtf8 . Text.unlines $
    [Text.pack (pad label) <> " = " <> value | (label, value) <- items]
 where
  width = maximum (0 : map (length . fst) items)
  pad l = l <> replicate (width - length l) ' '
