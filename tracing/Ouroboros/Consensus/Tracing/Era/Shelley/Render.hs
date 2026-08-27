{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | JSON rendering helpers for Shelley-era ledger types used by the tracing
-- instances in "Ouroboros.Consensus.Tracing.Era.Shelley".
--
-- These were originally in @cardano-node@'s @Cardano.Node.Tracing.Render@ and
-- went through @cardano-api@. They are reimplemented here directly against
-- @cardano-ledger@ (so Consensus need not depend on @cardano-api@, which sits
-- downstream), reproducing @cardano-api@'s output: bech32 stake/reward
-- addresses (CIP-19), hex script hashes, and era-generic plutus purposes.
module Ouroboros.Consensus.Tracing.Era.Shelley.Render
  ( renderScriptHash
  , renderScriptIntegrityHash
  , renderScriptPurpose
  , renderScriptIndex
  , renderMissingRedeemers
  , renderIncompleteWithdrawals
  , renderRewardAccount
  , renderTxIn
  ) where

import qualified Cardano.Crypto.Hash.Class as Crypto
import           Cardano.Ledger.Address (AccountAddress (..), serialiseAccountAddress)
import           Cardano.Ledger.Alonzo.Scripts (AsItem (..), AsIx (..))
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import           Cardano.Ledger.Api.Scripts (AnyEraScript, PlutusPurpose,
                   pattern AnyEraCertifyingPurpose, pattern AnyEraMintingPurpose,
                   pattern AnyEraProposingPurpose, pattern AnyEraWithdrawingPurpose,
                   pattern AnyEraSpendingPurpose, pattern AnyEraVotingPurpose)
import           Cardano.Ledger.BaseTypes (Mismatch (..), Network (..), Relation (..),
                   TxIx (..))
import           Cardano.Ledger.Conway.Governance (ProposalProcedure)
import qualified Cardano.Ledger.Core as Ledger
import           Cardano.Ledger.TxIn (TxId (..), TxIn (..))
import qualified Cardano.Ledger.Hashes as Hashes
import           Cardano.Ledger.Hashes (ScriptHash (..))
import qualified Codec.Binary.Bech32 as Bech32
import           Data.Aeson (ToJSON, Value, toJSON, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson
import qualified Data.ByteString.Base16 as B16
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Map.NonEmpty (NonEmptyMap)
import qualified Data.Map.NonEmpty as NonEmptyMap
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import           Data.Word (Word32)

-- | Hex-encode a script hash, matching @cardano-api@'s
-- @serialiseToRawBytesHexText . fromShelleyScriptHash@.
renderScriptHash :: ScriptHash -> Text
renderScriptHash (ScriptHash h) = Crypto.hashToTextAsHex h

renderScriptIntegrityHash :: Maybe Alonzo.ScriptIntegrityHash -> Value
renderScriptIntegrityHash (Just witPPDataHash) =
  Aeson.String . Crypto.hashToTextAsHex $ Hashes.extractHash witPPDataHash
renderScriptIntegrityHash Nothing = Aeson.Null

-- | Bech32-encode a reward/stake account address (CIP-19), matching
-- @cardano-api@'s @serialiseAddress . fromShelleyStakeAddr@: human-readable
-- part @stake@ on mainnet, @stake_test@ on testnets.
renderRewardAccount :: AccountAddress -> Text
renderRewardAccount acct =
    case Bech32.encode hrp (Bech32.dataPartFromBytes bytes) of
      Right t  -> t
      -- 'encode' only fails if the payload exceeds bech32 length limits, which
      -- a 29-byte stake address never does; fall back to hex just in case.
      Left _   -> Text.Encoding.decodeLatin1 (B16.encode bytes)
  where
    bytes = serialiseAccountAddress acct
    hrp = case aaNetworkId acct of
      Mainnet -> hrpStake
      Testnet -> hrpStakeTest

hrpStake, hrpStakeTest :: Bech32.HumanReadablePart
hrpStake     = unsafeHrp "stake"
hrpStakeTest = unsafeHrp "stake_test"

-- | These literals are valid human-readable parts, so this never fails.
unsafeHrp :: Text -> Bech32.HumanReadablePart
unsafeHrp t = case Bech32.humanReadablePartFromText t of
  Right hrp -> hrp
  Left err  -> error ("renderRewardAccount: invalid HRP " <> show t <> ": " <> show err)

-- | Rendered in place of a plutus purpose that none of the @AnyEraScript@
-- projections below matched.
--
-- Those projections cover every purpose the ledger currently defines, but they
-- are pattern synonyms without a @COMPLETE@ pragma, so GHC cannot check that
-- for us and a purpose added by a future ledger era would fall through here.
-- Render a marker rather than @null@: a @null@ is indistinguishable from a
-- purpose that legitimately rendered as one, whereas this is greppable in the
-- logs and shows up as a distinct shape in the trace schemas.
unknownPurpose :: Value
unknownPurpose = Aeson.object ["kind" .= Aeson.String "UnknownPlutusPurpose"]

-- | Render a transaction input as @\<txid hex\>#\<index\>@.
--
-- Deliberately not @cardano-ledger@'s @ToJSON TxIn@: that one shows the index
-- newtype, giving @...#TxIx {unTxIx = 0}@. @cardano-api@ rendered the bare
-- number, and that is what the log has always carried.
renderTxIn :: TxIn -> Text
renderTxIn (TxIn (TxId h) (TxIx ix)) =
  Crypto.hashToTextAsHex (Hashes.extractHash h) <> "#" <> Text.pack (show ix)

-- | Render a plutus script purpose (as an item), era-generically via
-- @cardano-ledger-api@'s @AnyEraScript@ projections. Replaces @cardano-api@'s
-- per-era @renderAlonzoPlutusPurpose@/@renderConwayPlutusPurpose@.
renderScriptPurpose ::
     ( AnyEraScript era
     , ToJSON (Ledger.TxCert era)
     , ToJSON (ProposalProcedure era)
     )
  => PlutusPurpose AsItem era
  -> Value
-- Note the asymmetry in whether the 'AsItem' wrapper is unwrapped: spending and
-- rewarding render their item directly, the other four go through
-- @ToJSON (AsItem ix it)@ and so come out wrapped in an @{"item": ...}@ object.
-- That is what @cardano-api@'s renderer did, so it is what consumers parse;
-- changing it is a deliberate format change, not a cleanup to make here.
renderScriptPurpose = \case
  AnyEraSpendingPurpose (AsItem txin) ->
    Aeson.object ["spending" .= Aeson.String (renderTxIn txin)]
  AnyEraMintingPurpose pid ->
    Aeson.object ["minting" .= toJSON pid]
  AnyEraWithdrawingPurpose (AsItem rwdAcct) ->
    Aeson.object ["rewarding" .= Aeson.String (renderRewardAccount rwdAcct)]
  AnyEraCertifyingPurpose cert ->
    Aeson.object ["certifying" .= toJSON cert]
  AnyEraVotingPurpose voter ->
    Aeson.object ["voting" .= toJSON voter]
  AnyEraProposingPurpose proposal ->
    Aeson.object ["proposing" .= toJSON proposal]
  _ -> unknownPurpose

-- | Render a plutus script purpose given by its index (redeemer pointer),
-- era-generically.
--
-- Reproduces what @cardano-api@'s @toScriptIndex@ followed by
-- @ToJSON ScriptWitnessIndex@ emitted: a @kind@ naming the witness index
-- constructor and the index itself under @value@. The constructor names are
-- @cardano-api@'s and do not all match the purpose names used by
-- 'renderScriptPurpose' above.
renderScriptIndex :: AnyEraScript era => PlutusPurpose AsIx era -> Value
renderScriptIndex = \case
  AnyEraSpendingPurpose (AsIx ix)    -> witnessIndex "ScriptWitnessIndexTxIn" ix
  AnyEraMintingPurpose (AsIx ix)     -> witnessIndex "ScriptWitnessIndexMint" ix
  AnyEraWithdrawingPurpose (AsIx ix) -> witnessIndex "ScriptWitnessIndexWithdrawal" ix
  AnyEraCertifyingPurpose (AsIx ix)  -> witnessIndex "ScriptWitnessIndexCertificate" ix
  AnyEraVotingPurpose (AsIx ix)      -> witnessIndex "ScriptWitnessIndexVoting" ix
  AnyEraProposingPurpose (AsIx ix)   -> witnessIndex "ScriptWitnessIndexProposing" ix
  _ -> unknownPurpose
  where
    witnessIndex :: Text -> Word32 -> Value
    witnessIndex kind ix = Aeson.object ["kind" .= kind, "value" .= ix]

renderMissingRedeemers ::
     ( AnyEraScript era
     , Ledger.EraPParams era
     , ToJSON (Ledger.TxCert era)
     )
  => NonEmpty (PlutusPurpose AsItem era, ScriptHash)
  -> Value
renderMissingRedeemers scripts =
    Aeson.object $ NonEmpty.toList $ NonEmpty.map renderTuple scripts
  where
    renderTuple (scriptPurpose, sHash) =
      Aeson.fromText (renderScriptHash sHash) .= renderScriptPurpose scriptPurpose

renderIncompleteWithdrawals ::
     Show payload
  => NonEmptyMap AccountAddress (Mismatch RelEQ payload)
  -> Value
renderIncompleteWithdrawals payload =
    Aeson.object $ map renderTuple $ NonEmptyMap.toList payload
  where
    renderTuple (address, mismatch) =
      Aeson.fromText (renderRewardAccount address) .= show mismatch
