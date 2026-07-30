{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A minimal executable that probes the transaction-id fidelity of the
-- Babbage->Conway era /upgrade/, to explain the following report:
--
--   * The node's ledger is in Conway.
--   * A transaction submitted over LocalTxSubmission tagged as __Babbage__ is
--     rejected.
--   * The /same/ transaction submitted tagged as __Conway__ is accepted.
--
-- A Babbage-tagged transaction is upgraded to Conway inside the mempool by
-- @hardForkInjectTxs@ (the Babbage->Conway 'InjectTx', reproduced verbatim as
-- 'babbageToConwayInjection'); a Conway-tagged transaction skips that step and
-- is validated directly. So any difference between the two paths comes entirely
-- from the upgrade.
--
-- The mempool and consensus assume the upgrade is /transaction-id preserving/
-- (see the invariant on @OneEraGenTxId@ in
-- 'Ouroboros.Consensus.HardFork.Combinator.AcrossEras'): a client signs the
-- transaction id of the era it built for, so if the upgrade changed the id, the
-- witnesses a client signed for the Babbage id would no longer match the id the
-- Conway ledger derives — which is exactly the shape of the report.
--
-- This program builds one transaction body (with a non-empty input set, so that
-- any set/encoding differences between the eras are exercised) and prints three
-- transaction ids:
--
--   1. the Babbage-tagged tx id           (what a client signs on the Babbage path)
--   2. the id after Babbage->Conway upgrade (what the Conway ledger checks against)
--   3. the native Conway-tagged tx id      (what a client signs on the Conway path)
--
-- If (1) and (2) differ, the upgrade is not id-preserving and witnesses signed
-- on the Babbage path cannot verify after the upgrade — a concrete root cause
-- for the report. If all three agree, the id is stable and the cause lies
-- elsewhere.
module Main (main) where

import Cardano.Ledger.BaseTypes (TxIx (..))
import qualified Cardano.Ledger.Core as SL
import Cardano.Ledger.TxIn (TxIn (..))
import Control.Monad (when)
import Control.Monad.Except (runExcept)
import Data.SOP.BasicFunctors ((:.:) (Comp), unComp)
import qualified Data.Set as Set
import Lens.Micro ((&), (.~))
import Ouroboros.Consensus.HardFork.Combinator.InjectTxs
  ( InjectTx
  , injectTxWith
  , pattern InjectTx
  )
import Ouroboros.Consensus.Shelley.Eras (BabbageEra, ConwayEra)
import Ouroboros.Consensus.Shelley.HFEras
  ( StandardBabbageBlock
  , StandardConwayBlock
  )
import Ouroboros.Consensus.Shelley.Ledger.Mempool
  ( GenTx (ShelleyTx)
  , mkShelleyTx
  )
import Ouroboros.Consensus.Shelley.ShelleyHFC ()
import System.Exit (exitFailure)

-- | The Babbage->Conway transaction injection.
--
-- This is a copy of @translateTxBabbageToConwayWrapper@ from
-- 'Ouroboros.Consensus.Cardano.CanHardFork' (which is not exported). It is one
-- link of the @InPairs InjectTx@ chain returned by @hardForkInjectTxs@; the
-- mempool walks that chain to step a transaction forward, one era at a time,
-- until it reaches the era the ledger currently sits in.
--
-- The @'SL.TranslationContext' ConwayEra@ (a @ConwayGenesis@) is threaded
-- through only to match the shape of the real node code: the transaction-level
-- Conway translation is a pure CBOR round-trip and never forces it. We
-- therefore leave it 'undefined' — see 'main'.
babbageToConwayInjection ::
  SL.TranslationContext ConwayEra ->
  InjectTx StandardBabbageBlock StandardConwayBlock
babbageToConwayInjection ctxt =
  InjectTx $ fmap unComp . eitherToMaybe . runExcept . SL.translateEra ctxt . Comp
 where
  eitherToMaybe = either (const Nothing) Just

-- | Build a basic (invalid) transaction carrying a single input. The input's tx
-- id is stable and era-independent, so the Babbage and Conway transactions below
-- have identical /content/.
mkTxWithInput ::
  forall era.
  SL.EraTx era =>
  TxIn ->
  SL.Tx SL.TopTx era
mkTxWithInput input =
  SL.mkBasicTx (SL.mkBasicTxBody & SL.inputsTxBodyL .~ Set.singleton input)

main :: IO ()
main = do
  let -- A single synthetic input, reusing the id of a basic empty tx.
      dummyInput :: TxIn
      dummyInput =
        TxIn
          (SL.txIdTx (SL.mkBasicTx SL.mkBasicTxBody :: SL.Tx SL.TopTx BabbageEra))
          (TxIx 0)

      babbageGenTx :: GenTx StandardBabbageBlock
      babbageGenTx = mkShelleyTx (mkTxWithInput dummyInput)

      conwayNativeGenTx :: GenTx StandardConwayBlock
      conwayNativeGenTx = mkShelleyTx (mkTxWithInput dummyInput)

      -- A minimal (empty) body, for comparison. Note this still contains the
      -- always-present (here empty) inputs set, so it does not isolate set
      -- encoding; it shows the Babbage and Conway body encoders differ even at
      -- their smallest.
      emptyBabbageId = case mkShelleyTx (SL.mkBasicTx SL.mkBasicTxBody) :: GenTx StandardBabbageBlock of
        ShelleyTx i _ -> i
      emptyConwayId = case mkShelleyTx (SL.mkBasicTx SL.mkBasicTxBody) :: GenTx StandardConwayBlock of
        ShelleyTx i _ -> i

      -- Never forced by the transaction-level translation (see the haddock on
      -- 'babbageToConwayInjection').
      conwayCtxt :: SL.TranslationContext ConwayEra
      conwayCtxt = undefined

  case injectTxWith (babbageToConwayInjection conwayCtxt) babbageGenTx of
    Nothing -> do
      putStrLn "NOT UPGRADEABLE: the Babbage tx cannot be upgraded to Conway."
      exitFailure
    Just upgradedGenTx -> do
      let ShelleyTx babbageId _ = babbageGenTx
          ShelleyTx upgradedId _ = upgradedGenTx
          ShelleyTx nativeId _ = conwayNativeGenTx

      putStrLn "Transaction ids (identical body content, one input, differing only in era tag / path):"
      putStrLn $ "  1. Babbage-tagged           : " ++ show babbageId
      putStrLn $ "  2. after Babbage->Conway    : " ++ show upgradedId
      putStrLn $ "  3. native Conway-tagged     : " ++ show nativeId
      putStrLn ""
      putStrLn "Minimal empty body, for comparison:"
      putStrLn $ "  Babbage-tagged              : " ++ show emptyBabbageId
      putStrLn $ "  native Conway-tagged        : " ++ show emptyConwayId
      putStrLn ""
      putStrLn $ "  upgrade preserves tx id (1 == 2)          : " ++ show (babbageId == upgradedId)
      putStrLn $ "  upgraded == native Conway (2 == 3)        : " ++ show (upgradedId == nativeId)
      putStrLn $ "  empty body id agrees across eras          : " ++ show (emptyBabbageId == emptyConwayId)
      putStrLn ""
      if babbageId /= upgradedId
        then
          putStrLn $
            "ID-CHANGING UPGRADE: the upgrade changes the tx id; witnesses signed over the "
              ++ "Babbage id (1) cannot verify against the Conway-derived id (2)."
        else
          if upgradedId == nativeId
            then
              putStrLn $
                "FULLY STABLE: the upgrade preserves the id AND matches a native Conway tx. "
                  ++ "The report's cause lies elsewhere."
            else
              putStrLn $
                "SPLIT ENCODING: the upgrade preserves the Babbage bytes/id (1 == 2), but a "
                  ++ "native Conway tx of identical content has a different id (2 /= 3): the "
                  ++ "Babbage and Conway CBOR encodings of a transaction differ. So the "
                  ++ "Babbage-tagged and Conway-tagged submissions are byte-different "
                  ++ "transactions, and the upgraded tx carries legacy Babbage-format bytes. Any "
                  ++ "Conway validation or downstream check sensitive to that encoding will treat "
                  ++ "them differently -- the likely root of the report."
      when (upgradedId == nativeId && babbageId == upgradedId) $
        putStrLn "(No path divergence observed for this transaction.)"
