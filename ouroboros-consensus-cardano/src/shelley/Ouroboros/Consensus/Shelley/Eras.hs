{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Shelley.Eras
  ( -- * Eras based on the Shelley ledger
    ShelleyEra
  , AllegraEra
  , MaryEra
  , AlonzoEra
  , BabbageEra
  , ConwayEra
  , DijkstraEra

    -- * Shelley-based era
  , ConwayEraGovDict (..)
  , ShelleyBasedEra (..)

    -- * Convenience functions
  , isBeforeConway

    -- * Re-exports
  , StandardCrypto
  ) where

import Cardano.Binary
import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.Allegra.Translation ()
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Core as Core
import qualified Cardano.Ledger.Alonzo.Rules as Alonzo
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import qualified Cardano.Ledger.Api.Era as L
import Cardano.Ledger.Babbage (BabbageEra)
import qualified Cardano.Ledger.Babbage.Rules as Babbage
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Binary (DecCBOR, EncCBOR)
import Cardano.Ledger.Conway (ConwayEra)
import qualified Cardano.Ledger.Conway.Governance as CG
import qualified Cardano.Ledger.Conway.Rules as Conway
import qualified Cardano.Ledger.Conway.Rules as SL
  ( ConwayLedgerPredFailure (..)
  )
import qualified Cardano.Ledger.Conway.State as CG
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Shelley (ShelleyEra)
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Ledger.Shelley.LedgerState as SL
import qualified Cardano.Ledger.Shelley.Rules as SL
import qualified Cardano.Ledger.Shelley.Transition as SL
import qualified Cardano.Protocol.TPraos.API as SL
import Control.Monad.Except
import Control.State.Transition (PredicateFailure)
import Data.Data (Proxy (Proxy))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text)
import Lens.Micro
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Ledger.SupportsMempool
  ( WhetherToIntervene (..)
  )
import Ouroboros.Consensus.Protocol.TPraos (StandardCrypto)

{-------------------------------------------------------------------------------
  Era polymorphism
-------------------------------------------------------------------------------}

-- | Consensus often needs some more functionality than the ledger currently
-- provides.
--
-- Either the functionality shouldn't or can't live in the ledger, in which case
-- it can be part and remain part of 'ShelleyBasedEra'. Or, the functionality
-- /should/ live in the ledger, but hasn't yet been added to the ledger, or it
-- hasn't yet been propagated to this repository, in which case it can be added
-- to this class until that is the case.
--
-- If this class becomes redundant, We can move it to ledger and re-export it
-- from here.
--
-- TODO Currently we include some constraints on the update state which are
-- needed to determine the hard fork point. In the future this should be
-- replaced with an appropriate API - see
-- https://github.com/IntersectMBO/ouroboros-network/issues/2890
class
  ( Core.EraBlockBody era
  , Core.EraGov era
  , SL.ApplyTx era
  , SL.ApplyBlock era
  , SL.EraTransition era
  , -- TODO This constraint is quite tight, since it fixes things to the
    -- original TPraos ledger view. We would like to ultimately remove it.
    SL.GetLedgerView era
  , NoThunks (SL.StashedAVVMAddresses era)
  , EncCBOR (SL.StashedAVVMAddresses era)
  , DecCBOR (SL.StashedAVVMAddresses era)
  , Show (SL.StashedAVVMAddresses era)
  , Eq (SL.StashedAVVMAddresses era)
  , DecCBOR (PredicateFailure (EraRule "LEDGER" era))
  , EncCBOR (PredicateFailure (EraRule "LEDGER" era))
  , DecCBOR (PredicateFailure (EraRule "UTXOW" era))
  , EncCBOR (PredicateFailure (EraRule "UTXOW" era))
  , Eq (PredicateFailure (EraRule "BBODY" era))
  , Show (PredicateFailure (EraRule "BBODY" era))
  , NoThunks (PredicateFailure (EraRule "BBODY" era))
  , NoThunks (Core.TranslationContext era)
  , ToCBOR (Core.TranslationContext era)
  , FromCBOR (Core.TranslationContext era)
  ) =>
  ShelleyBasedEra era
  where
  applyShelleyBasedTx ::
    SL.Globals ->
    SL.LedgerEnv era ->
    SL.LedgerState era ->
    WhetherToIntervene ->
    Core.Tx era ->
    Except
      (SL.ApplyTxError era)
      ( SL.LedgerState era
      , SL.Validated (Core.Tx era)
      )

  -- | Whether the era has an instance of 'CG.ConwayEraGov'
  getConwayEraGovDict :: proxy era -> Maybe (ConwayEraGovDict era)

  mkMkMempoolShelleyPredicateFailure :: proxy era -> Maybe (Text -> PredicateFailure (EraRule "LEDGER" era))

data ConwayEraGovDict era where
  ConwayEraGovDict :: (CG.ConwayEraGov era, CG.ConwayEraCertState era) => ConwayEraGovDict era

isBeforeConway :: forall era. L.Era era => Proxy era -> Bool
isBeforeConway _ =
  L.eraProtVerLow @era < L.eraProtVerLow @L.ConwayEra

-- | The default implementation of 'applyShelleyBasedTx', a thin wrapper around
-- 'SL.applyTx'
defaultApplyShelleyBasedTx ::
  ShelleyBasedEra era =>
  SL.Globals ->
  SL.LedgerEnv era ->
  SL.LedgerState era ->
  WhetherToIntervene ->
  Core.Tx era ->
  Except
    (SL.ApplyTxError era)
    ( SL.LedgerState era
    , SL.Validated (Core.Tx era)
    )
defaultApplyShelleyBasedTx globals ledgerEnv mempoolState _wti tx =
  liftEither $
    SL.applyTx
      globals
      ledgerEnv
      mempoolState
      tx

defaultGetConwayEraGovDict :: proxy era -> Maybe (ConwayEraGovDict era)
defaultGetConwayEraGovDict _ = Nothing

instance ShelleyBasedEra ShelleyEra where
  applyShelleyBasedTx = defaultApplyShelleyBasedTx

  getConwayEraGovDict = defaultGetConwayEraGovDict

  mkMkMempoolShelleyPredicateFailure _prx = Nothing

instance ShelleyBasedEra AllegraEra where
  applyShelleyBasedTx = defaultApplyShelleyBasedTx

  getConwayEraGovDict = defaultGetConwayEraGovDict

  mkMkMempoolShelleyPredicateFailure _prx = Nothing

instance ShelleyBasedEra MaryEra where
  applyShelleyBasedTx = defaultApplyShelleyBasedTx

  getConwayEraGovDict = defaultGetConwayEraGovDict

  mkMkMempoolShelleyPredicateFailure _prx = Nothing

instance ShelleyBasedEra AlonzoEra where
  applyShelleyBasedTx = applyAlonzoBasedTx

  getConwayEraGovDict = defaultGetConwayEraGovDict

  mkMkMempoolShelleyPredicateFailure _prx = Nothing

instance ShelleyBasedEra BabbageEra where
  applyShelleyBasedTx = applyAlonzoBasedTx

  getConwayEraGovDict = defaultGetConwayEraGovDict

  mkMkMempoolShelleyPredicateFailure _prx = Nothing

instance ShelleyBasedEra ConwayEra where
  applyShelleyBasedTx = applyAlonzoBasedTx

  getConwayEraGovDict _ = Just ConwayEraGovDict

  mkMkMempoolShelleyPredicateFailure _prx = Just Conway.ConwayMempoolFailure

instance ShelleyBasedEra DijkstraEra where
  applyShelleyBasedTx = applyAlonzoBasedTx

  getConwayEraGovDict _ = Just ConwayEraGovDict

  -- TODO we'll need to change the mini protocol (backwards-incompatibly?) to
  -- use MempoolFailure type family instead of just PredicateFailure type
  -- family
  mkMkMempoolShelleyPredicateFailure _prx = Nothing

applyAlonzoBasedTx ::
  forall era.
  ( AlonzoEraTx era
  , ShelleyBasedEra era
  , SupportsTwoPhaseValidation era
  ) =>
  Globals ->
  SL.LedgerEnv era ->
  SL.LedgerState era ->
  WhetherToIntervene ->
  Core.Tx era ->
  Except
    (SL.ApplyTxError era)
    ( SL.LedgerState era
    , SL.Validated (Core.Tx era)
    )
applyAlonzoBasedTx globals ledgerEnv mempoolState wti tx = do
  (mempoolState', vtx) <-
    (`catchError` handler) $
      defaultApplyShelleyBasedTx
        globals
        ledgerEnv
        mempoolState
        wti
        intervenedTx
  pure (mempoolState', vtx)
 where
  intervenedTx = case wti of
    DoNotIntervene -> tx & Core.isValidTxL .~ Alonzo.IsValid True
    Intervene -> tx

  handler e = case (wti, e) of
    (DoNotIntervene, SL.ApplyTxError (err :| []))
      | isIncorrectClaimedFlag (Proxy @era) err ->
          -- rectify the flag and include the transaction
          --
          -- This either lets the ledger punish the script author for sending
          -- a bad script or else prevents our peer's buggy script validator
          -- from preventing inclusion of a valid script.
          --
          -- TODO 'applyTx' et al needs to include a return value indicating
          -- whether we took this branch; it's a reason to disconnect from
          -- the peer who sent us the incorrect flag (ie Issue #3276)
          defaultApplyShelleyBasedTx
            globals
            ledgerEnv
            mempoolState
            wti
            (tx & Core.isValidTxL .~ Alonzo.IsValid False)
    _ -> throwError e

-- reject the transaction, protecting the local wallet

class SupportsTwoPhaseValidation era where
  -- NOTE: this class won't be needed once https://github.com/IntersectMBO/cardano-ledger/issues/4167 is implemented.
  isIncorrectClaimedFlag :: proxy era -> SL.PredicateFailure (Core.EraRule "LEDGER" era) -> Bool

instance SupportsTwoPhaseValidation AlonzoEra where
  isIncorrectClaimedFlag _ = \case
    SL.UtxowFailure
      ( Alonzo.ShelleyInAlonzoUtxowPredFailure
          ( SL.UtxoFailure
              ( Alonzo.UtxosFailure
                  ( Alonzo.ValidationTagMismatch
                      (Alonzo.IsValid _claimedFlag)
                      _validationErrs
                    )
                )
            )
        ) ->
        True
    _ -> False

instance SupportsTwoPhaseValidation BabbageEra where
  isIncorrectClaimedFlag _ = \case
    SL.UtxowFailure
      ( Babbage.AlonzoInBabbageUtxowPredFailure
          ( Alonzo.ShelleyInAlonzoUtxowPredFailure
              ( SL.UtxoFailure
                  ( Babbage.AlonzoInBabbageUtxoPredFailure
                      ( Alonzo.UtxosFailure
                          ( Alonzo.ValidationTagMismatch
                              (Alonzo.IsValid _claimedFlag)
                              _validationErrs
                            )
                        )
                    )
                )
            )
        ) -> True
    SL.UtxowFailure
      ( Babbage.UtxoFailure
          ( Babbage.AlonzoInBabbageUtxoPredFailure
              ( Alonzo.UtxosFailure
                  ( Alonzo.ValidationTagMismatch
                      (Alonzo.IsValid _claimedFlag)
                      _validationErrs
                    )
                )
            )
        ) -> True
    _ -> False

instance SupportsTwoPhaseValidation ConwayEra where
  isIncorrectClaimedFlag _ = \case
    SL.ConwayUtxowFailure
      ( Conway.UtxoFailure
          ( Conway.UtxosFailure
              ( Conway.ValidationTagMismatch
                  (Alonzo.IsValid _claimedFlag)
                  _validationErrs
                )
            )
        ) -> True
    _ -> False

instance SupportsTwoPhaseValidation DijkstraEra where
  isIncorrectClaimedFlag _ = \case
    SL.ConwayUtxowFailure
      ( Conway.UtxoFailure
          ( Conway.UtxosFailure
              ( Conway.ValidationTagMismatch
                  (Alonzo.IsValid _claimedFlag)
                  _validationErrs
                )
            )
        ) -> True
    _ -> False
