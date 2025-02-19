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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Shelley.Eras (
    -- * Eras based on the Shelley ledger
    AllegraEra
  , AlonzoEra
  , BabbageEra
  , ConwayEra
  , MaryEra
  , ShelleyEra
    -- * Eras instantiated with standard crypto
  , StandardAllegra
  , StandardAlonzo
  , StandardBabbage
  , StandardConway
  , StandardMary
  , StandardShelley
    -- * Shelley-based era
  , ConwayEraGovDict (..)
  , ShelleyBasedEra (..)
  , WrapTx (..)
    -- * Convenience functions
  , isBeforeConway
    -- * Re-exports
  , StandardCrypto
  ) where

import           Cardano.Binary
import           Cardano.Ledger.Allegra (AllegraEra)
import           Cardano.Ledger.Allegra.Translation ()
import           Cardano.Ledger.Alonzo (AlonzoEra)
import qualified Cardano.Ledger.Alonzo.Rules as Alonzo
import qualified Cardano.Ledger.Alonzo.Translation as Alonzo
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import qualified Cardano.Ledger.Api.Era as L
import           Cardano.Ledger.Babbage (BabbageEra)
import qualified Cardano.Ledger.Babbage.Rules as Babbage
import qualified Cardano.Ledger.Babbage.Translation as Babbage
import           Cardano.Ledger.BaseTypes
import           Cardano.Ledger.Binary (DecCBOR, EncCBOR)
import           Cardano.Ledger.Conway (ConwayEra)
import           Cardano.Ledger.Conway.Genesis
import qualified Cardano.Ledger.Conway.Governance as CG
import qualified Cardano.Ledger.Conway.Rules as Conway
import qualified Cardano.Ledger.Conway.Rules as SL
                     (ConwayLedgerPredFailure (..))
import qualified Cardano.Ledger.Conway.Translation as Conway
import           Cardano.Ledger.Core as Core
import           Cardano.Ledger.Crypto (StandardCrypto)
import           Cardano.Ledger.Keys (DSignable, Hash)
import           Cardano.Ledger.Core as Core
import           Cardano.Ledger.Crypto (StandardCrypto)
import           Cardano.Ledger.Genesis
import           Cardano.Ledger.Keys (DSignable, Hash)
import           Cardano.Ledger.Mary (MaryEra)
import           Cardano.Ledger.Mary.Translation ()
import           Cardano.Ledger.Shelley (ShelleyEra)
import qualified Cardano.Ledger.Shelley.API as SL
import           Cardano.Ledger.Shelley.Core as Core
import qualified Cardano.Ledger.Shelley.LedgerState as SL
import qualified Cardano.Ledger.Shelley.Rules as SL
import qualified Cardano.Ledger.Shelley.Transition as SL
import           Cardano.Protocol.Crypto (StandardCrypto)
import           Cardano.Ledger.Shelley.Translation
import qualified Cardano.Protocol.TPraos.API as SL
import           Control.Monad.Except
import           Control.State.Transition (PredicateFailure)
import           Data.Data (Proxy (Proxy))
import           Data.List.NonEmpty (NonEmpty ((:|)))
import           Data.Typeable (Typeable)
import           NoThunks.Class (NoThunks)
import           Ouroboros.Consensus.Ledger.SupportsMempool
                     (WhetherToIntervene (..))

{-------------------------------------------------------------------------------
  Eras instantiated with standard crypto
-------------------------------------------------------------------------------}

-- | The Shelley era with standard crypto
type StandardShelley = ShelleyEra
{-# DEPRECATED StandardShelley "In favor of `ShelleyEra`" #-}

-- | The Allegra era with standard crypto
type StandardAllegra = AllegraEra
{-# DEPRECATED StandardAllegra "In favor of `AllegraEra`" #-}

-- | The Mary era with standard crypto
type StandardMary = MaryEra
{-# DEPRECATED StandardMary "In favor of `MaryEra`" #-}

-- | The Alonzo era with standard crypto
type StandardAlonzo = AlonzoEra
{-# DEPRECATED StandardAlonzo "In favor of `AlonzoEra`" #-}

-- | The Babbage era with standard crypto
type StandardBabbage = BabbageEra
{-# DEPRECATED StandardBabbage "In favor of `BabbageEra`" #-}

-- | The Conway era with standard crypto
type StandardConway = ConwayEra
{-# DEPRECATED StandardConway "In favor of `ConwayEra`" #-}

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
class ( Core.EraSegWits era
      , Core.EraGov era
      , SL.ApplyTx era
      , SL.ApplyBlock era
      , SL.EraTransition era

        -- TODO This constraint is quite tight, since it fixes things to the
        -- original TPraos ledger view. We would like to ultimately remove it.
      , SL.GetLedgerView era

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

      ) => ShelleyBasedEra era where

  applyShelleyBasedTx ::
       SL.Globals
    -> SL.LedgerEnv era
    -> SL.LedgerState era
    -> WhetherToIntervene
    -> Core.Tx era
    -> Except
         (SL.ApplyTxError era)
         ( SL.LedgerState era
         , SL.Validated (Core.Tx era)
         )

  -- | Whether the era has an instance of 'CG.ConwayEraGov'
  getConwayEraGovDict :: proxy era -> Maybe (ConwayEraGovDict era)

data ConwayEraGovDict era where
    ConwayEraGovDict :: CG.ConwayEraGov era => ConwayEraGovDict era

isBeforeConway :: forall era. L.Era era => Proxy era -> Bool
isBeforeConway _ =
    L.eraProtVerLow @era < L.eraProtVerLow @L.ConwayEra

-- | The default implementation of 'applyShelleyBasedTx', a thin wrapper around
-- 'SL.applyTx'
defaultApplyShelleyBasedTx ::
     ShelleyBasedEra era
  => SL.Globals
  -> SL.LedgerEnv era
  -> SL.LedgerState era
  -> WhetherToIntervene
  -> Core.Tx era
  -> Except
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

instance ShelleyBasedEra AllegraEra where
  applyShelleyBasedTx = defaultApplyShelleyBasedTx

  getConwayEraGovDict = defaultGetConwayEraGovDict

instance ShelleyBasedEra MaryEra where
  applyShelleyBasedTx = defaultApplyShelleyBasedTx

  getConwayEraGovDict = defaultGetConwayEraGovDict

instance ShelleyBasedEra AlonzoEra where
  applyShelleyBasedTx = applyAlonzoBasedTx

  getConwayEraGovDict = defaultGetConwayEraGovDict

instance ShelleyBasedEra BabbageEra where
  applyShelleyBasedTx = applyAlonzoBasedTx

  getConwayEraGovDict = defaultGetConwayEraGovDict

instance ShelleyBasedEra ConwayEra where
  applyShelleyBasedTx = applyAlonzoBasedTx

  getConwayEraGovDict _ = Just ConwayEraGovDict

applyAlonzoBasedTx :: forall era.
  ( ShelleyBasedEra era,
    SupportsTwoPhaseValidation era,
    Core.Tx era ~ Alonzo.AlonzoTx era
  ) =>
  Globals ->
  SL.LedgerEnv era ->
  SL.LedgerState era ->
  WhetherToIntervene ->
  Alonzo.AlonzoTx era ->
  Except
    (SL.ApplyTxError era)
    ( SL.LedgerState era,
      SL.Validated (Alonzo.AlonzoTx era)
    )
applyAlonzoBasedTx globals ledgerEnv mempoolState wti tx = do
      (mempoolState', vtx) <-
          (`catchError` handler)
        $ defaultApplyShelleyBasedTx
            globals
            ledgerEnv
            mempoolState
            wti
            intervenedTx
      pure (mempoolState', vtx)
    where
      intervenedTx = case wti of
        DoNotIntervene -> tx { Alonzo.isValid = Alonzo.IsValid True }
        Intervene      -> tx

      handler e = case (wti, e) of
        (DoNotIntervene, SL.ApplyTxError (err :| []))
          | isIncorrectClaimedFlag (Proxy @era) err
          ->
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
              tx{Alonzo.isValid = Alonzo.IsValid False}
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

{-------------------------------------------------------------------------------
  Tx family wrapper
-------------------------------------------------------------------------------}

-- | Wrapper for partially applying the 'Tx' type family
--
-- For generality, Consensus uses that type family as eg the index of
-- 'Core.TranslateEra'. We thus need to partially apply it.
--
-- @cardano-ledger-specs@ also declares such a newtype, but currently it's only
-- defined in the Alonzo translation module, which seems somewhat inappropriate
-- to use for previous eras. Also, we use a @Wrap@ prefix in Consensus. Hence
-- this minor mediating definition. TODO I'm not even fully persuading myself
-- with this justification.
newtype WrapTx era = WrapTx {unwrapTx :: Core.Tx era}

instance ShelleyBasedEra AllegraEra => Core.TranslateEra AllegraEra WrapTx where
  type TranslationError AllegraEra WrapTx = Core.TranslationError AllegraEra SL.ShelleyTx
  translateEra ctxt = fmap WrapTx . Core.translateEra ctxt . unwrapTx

instance Core.TranslateEra MaryEra WrapTx where
  type TranslationError MaryEra WrapTx = Core.TranslationError MaryEra SL.ShelleyTx
  translateEra ctxt = fmap WrapTx . Core.translateEra ctxt . unwrapTx

instance Core.TranslateEra AlonzoEra WrapTx where
  type TranslationError AlonzoEra WrapTx = Core.TranslationError AlonzoEra Alonzo.Tx
  translateEra ctxt =
        fmap (WrapTx . Alonzo.unTx)
      . Core.translateEra @AlonzoEra ctxt
      . Alonzo.Tx . unwrapTx

instance Core.TranslateEra BabbageEra WrapTx where
  type TranslationError BabbageEra WrapTx = Core.TranslationError BabbageEra Babbage.Tx
  translateEra ctxt =
        fmap (WrapTx . Babbage.unTx)
      . Core.translateEra @BabbageEra ctxt
      . Babbage.Tx . unwrapTx

instance Core.TranslateEra ConwayEra WrapTx where
  type TranslationError ConwayEra WrapTx = Core.TranslationError ConwayEra Conway.Tx
  translateEra ctxt =
        fmap (WrapTx . Conway.unTx)
      . Core.translateEra @ConwayEra ctxt
      . Conway.Tx . unwrapTx
