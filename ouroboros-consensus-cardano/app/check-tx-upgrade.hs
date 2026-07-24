{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A minimal executable that answers a single question:
--
-- /Can a transaction that arrived tagged with the Babbage era be upgraded to a
-- Conway transaction?/
--
-- This is exactly the check the hard-fork combinator performs in the mempool
-- when a transaction created in an older era is applied against a ledger that
-- has already moved to a newer era. See
-- 'Ouroboros.Consensus.HardFork.Combinator.Mempool' and the per-era-boundary
-- 'InjectTx's assembled by @hardForkInjectTxs@ in
-- 'Ouroboros.Consensus.Cardano.CanHardFork'. The Babbage->Conway entry of that
-- chain is reproduced verbatim below as 'babbageToConwayInjection'.
--
-- The transaction we build is deliberately empty and would /not/ pass ledger
-- validation. That is fine: this program only checks whether the era
-- /upgrade/ (a CBOR re-interpretation, 'SL.translateEra') succeeds, not whether
-- the resulting Conway transaction is valid.
module Main (main) where

import qualified Cardano.Ledger.Core as SL
import Control.Monad.Except (runExcept)
import Data.SOP.BasicFunctors ((:.:) (Comp), unComp)
import Ouroboros.Consensus.HardFork.Combinator.InjectTxs
  ( InjectTx
  , injectTxWith
  , pattern InjectTx
  )
import Ouroboros.Consensus.Ledger.SupportsMempool (GenTx)
import Ouroboros.Consensus.Shelley.Eras (BabbageEra, ConwayEra)
import Ouroboros.Consensus.Shelley.HFEras
  ( StandardBabbageBlock
  , StandardConwayBlock
  )
import Ouroboros.Consensus.Shelley.Ledger.Mempool (mkShelleyTx)
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

main :: IO ()
main = do
  -- An empty (and hence invalid) Babbage transaction, tagged as Babbage.
  let babbageTx :: SL.Tx SL.TopTx BabbageEra
      babbageTx = SL.mkBasicTx SL.mkBasicTxBody

      babbageGenTx :: GenTx StandardBabbageBlock
      babbageGenTx = mkShelleyTx babbageTx

      -- Never forced by the transaction-level translation (see the haddock on
      -- 'babbageToConwayInjection'). In a running node this comes from the
      -- Conway ledger config via @getConwayTranslationContext@.
      conwayCtxt :: SL.TranslationContext ConwayEra
      conwayCtxt = undefined

  case injectTxWith (babbageToConwayInjection conwayCtxt) babbageGenTx of
    Just (_ :: GenTx StandardConwayBlock) ->
      putStrLn "UPGRADEABLE: a Babbage-tagged transaction can be upgraded to Conway."
    Nothing -> do
      putStrLn "NOT UPGRADEABLE: a Babbage-tagged transaction cannot be upgraded to Conway."
      exitFailure
