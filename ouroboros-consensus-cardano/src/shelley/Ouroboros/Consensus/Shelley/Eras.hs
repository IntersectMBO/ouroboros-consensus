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
    -- * Type synonyms for convenience
  , EraCrypto
    -- * Convenience functions
  , isBeforeConway
    -- * Re-exports
  , StandardCrypto
  ) where

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
import qualified Cardano.Ledger.Conway.Governance as CG
import qualified Cardano.Ledger.Conway.Rules as Conway
import qualified Cardano.Ledger.Conway.Rules as SL
                     (ConwayLedgerPredFailure (..))
import qualified Cardano.Ledger.Conway.Translation as Conway
import           Cardano.Ledger.Core as Core
import           Cardano.Ledger.Crypto (StandardCrypto)
import           Cardano.Ledger.Keys (DSignable, Hash)
import           Cardano.Ledger.Mary (MaryEra)
import           Cardano.Ledger.Mary.Translation ()
import           Cardano.Ledger.Shelley (ShelleyEra)
import qualified Cardano.Ledger.Shelley.API as SL
import           Cardano.Ledger.Shelley.Core as Core
import qualified Cardano.Ledger.Shelley.LedgerState as SL
import qualified Cardano.Ledger.Shelley.Rules as SL
import qualified Cardano.Ledger.Shelley.Transition as SL
import qualified Cardano.Protocol.TPraos.API as SL
import           Control.Monad.Except
import           Control.State.Transition (PredicateFailure)
import           Data.Data (Proxy (Proxy))
import           Data.List.NonEmpty (NonEmpty ((:|)))
import           NoThunks.Class (NoThunks)
import           Ouroboros.Consensus.Ledger.SupportsMempool
                     (WhetherToIntervene (..))
import qualified Ouroboros.Consensus.Protocol.Praos as Praos

{-------------------------------------------------------------------------------
  Eras instantiated with standard crypto
-------------------------------------------------------------------------------}

-- | The Shelley era with standard crypto
type StandardShelley = ShelleyEra StandardCrypto

-- | The Allegra era with standard crypto
type StandardAllegra = AllegraEra StandardCrypto

-- | The Mary era with standard crypto
type StandardMary = MaryEra StandardCrypto

-- | The Alonzo era with standard crypto
type StandardAlonzo = AlonzoEra StandardCrypto

-- | The Babbage era with standard crypto
type StandardBabbage = BabbageEra StandardCrypto

-- | The Conway era with standard crypto
type StandardConway = ConwayEra StandardCrypto

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

      , DSignable (EraCrypto era) (Hash (EraCrypto era) EraIndependentTxBody)
      , NoThunks (PredicateFailure (Core.EraRule "BBODY" era))
      , NoThunks (Core.TranslationContext era)

      , DecCBOR (SL.TxIn (EraCrypto era))
      , EncCBOR (SL.TxIn (EraCrypto era))

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
    L.eraProtVerLow @era < L.eraProtVerLow @(L.ConwayEra (L.EraCrypto era))

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
defaultApplyShelleyBasedTx globals ledgerEnv mempoolState _wti tx = do
    SL.applyTx
      globals
      ledgerEnv
      mempoolState
      tx

defaultGetConwayEraGovDict :: proxy era -> Maybe (ConwayEraGovDict era)
defaultGetConwayEraGovDict _ = Nothing

instance (SL.PraosCrypto c, DSignable c (Hash c EraIndependentTxBody))
  => ShelleyBasedEra (ShelleyEra c) where
  applyShelleyBasedTx = defaultApplyShelleyBasedTx

  getConwayEraGovDict = defaultGetConwayEraGovDict

instance (SL.PraosCrypto c, DSignable c (Hash c EraIndependentTxBody))
  => ShelleyBasedEra (AllegraEra c) where
  applyShelleyBasedTx = defaultApplyShelleyBasedTx

  getConwayEraGovDict = defaultGetConwayEraGovDict

instance (SL.PraosCrypto c, DSignable c (Hash c EraIndependentTxBody))
  => ShelleyBasedEra (MaryEra c) where
  applyShelleyBasedTx = defaultApplyShelleyBasedTx

  getConwayEraGovDict = defaultGetConwayEraGovDict

instance (SL.PraosCrypto c, DSignable c (Hash c EraIndependentTxBody))
  => ShelleyBasedEra (AlonzoEra c) where
  applyShelleyBasedTx = applyAlonzoBasedTx

  getConwayEraGovDict = defaultGetConwayEraGovDict

instance (Praos.PraosCrypto c) => ShelleyBasedEra (BabbageEra c) where
  applyShelleyBasedTx = applyAlonzoBasedTx

  getConwayEraGovDict = defaultGetConwayEraGovDict

instance (Praos.PraosCrypto c) => ShelleyBasedEra (ConwayEra c) where
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

instance SupportsTwoPhaseValidation (AlonzoEra c) where
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

instance SupportsTwoPhaseValidation (BabbageEra c) where
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

instance SupportsTwoPhaseValidation (ConwayEra c) where
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

instance ShelleyBasedEra (AllegraEra c) => Core.TranslateEra (AllegraEra c) WrapTx where
  type TranslationError (AllegraEra c) WrapTx = Core.TranslationError (AllegraEra c) SL.ShelleyTx
  translateEra ctxt = fmap WrapTx . Core.translateEra ctxt . unwrapTx

instance ShelleyBasedEra (MaryEra c) => Core.TranslateEra (MaryEra c) WrapTx where
  type TranslationError (MaryEra c) WrapTx = Core.TranslationError (MaryEra c) SL.ShelleyTx
  translateEra ctxt = fmap WrapTx . Core.translateEra ctxt . unwrapTx

instance ShelleyBasedEra (AlonzoEra c) => Core.TranslateEra (AlonzoEra c) WrapTx where
  type TranslationError (AlonzoEra c) WrapTx = Core.TranslationError (AlonzoEra c) Alonzo.Tx
  translateEra ctxt =
        fmap (WrapTx . Alonzo.unTx)
      . Core.translateEra @(AlonzoEra c) ctxt
      . Alonzo.Tx . unwrapTx

instance ShelleyBasedEra (BabbageEra c) => Core.TranslateEra (BabbageEra c) WrapTx where
  type TranslationError (BabbageEra c) WrapTx = Core.TranslationError (BabbageEra c) Babbage.Tx
  translateEra ctxt =
        fmap (WrapTx . Babbage.unTx)
      . Core.translateEra @(BabbageEra c) ctxt
      . Babbage.Tx . unwrapTx

instance ShelleyBasedEra (ConwayEra c) => Core.TranslateEra (ConwayEra c) WrapTx where
  type TranslationError (ConwayEra c) WrapTx = Core.TranslationError (ConwayEra c) Conway.Tx
  translateEra ctxt =
        fmap (WrapTx . Conway.unTx)
      . Core.translateEra @(ConwayEra c) ctxt
      . Conway.Tx . unwrapTx
