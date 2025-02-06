{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | Shelley mempool integration
--
-- TODO nearly all of the logic in this module belongs in cardano-ledger, not
-- ouroboros-consensus; ouroboros-consensus-cardano should just be "glue code".
module Ouroboros.Consensus.Shelley.Ledger.Mempool (
    GenTx (..)
  , SL.ApplyTxError (..)
  , TxId (..)
  , Validated (..)
  , fixedBlockBodyOverhead
  , mkShelleyTx
  , mkShelleyValidatedTx
  , perTxOverhead
    -- * Exported for tests
  , AlonzoMeasure (..)
  , ConwayMeasure (..)
  , fromExUnits
  ) where

import qualified Cardano.Crypto.Hash as Hash
import qualified Cardano.Ledger.Allegra.Rules as AllegraEra
import           Cardano.Ledger.Alonzo.Core (Tx, TxSeq, bodyTxL, eraDecoder,
                     fromTxSeq, ppMaxBBSizeL, ppMaxBlockExUnitsL, sizeTxF)
import qualified Cardano.Ledger.Alonzo.Rules as AlonzoEra
import           Cardano.Ledger.Alonzo.Scripts (ExUnits, ExUnits',
                     pointWiseExUnits, unWrapExUnits)
import           Cardano.Ledger.Alonzo.Tx (totExUnits)
import qualified Cardano.Ledger.Api as L
import qualified Cardano.Ledger.Babbage.Rules as BabbageEra
import qualified Cardano.Ledger.BaseTypes as L
import           Cardano.Ledger.Binary (Annotator (..), DecCBOR (..),
                     EncCBOR (..), FromCBOR (..), FullByteString (..),
                     ToCBOR (..))
import qualified Cardano.Ledger.Conway.Rules as ConwayEra
import qualified Cardano.Ledger.Conway.Rules as SL
import qualified Cardano.Ledger.Conway.UTxO as SL
import qualified Cardano.Ledger.Core as SL (allInputsTxBodyF, txIdTxBody)
import           Cardano.Ledger.Crypto (Crypto)
import qualified Cardano.Ledger.SafeHash as SL
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Ledger.Shelley.Rules as ShelleyEra
import           Control.Arrow ((+++))
import           Control.Monad (guard)
import           Control.Monad.Except (Except, liftEither)
import           Control.Monad.Identity (Identity (..))
import           Data.DerivingVia (InstantiatedAt (..))
import           Data.Foldable (toList)
import           Data.Measure (Measure)
import           Data.Typeable (Typeable)
import qualified Data.Validation as V
import           GHC.Generics (Generic)
import           GHC.Natural (Natural)
import           Lens.Micro ((^.))
import           NoThunks.Class (NoThunks (..))
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Ledger.Tables.Utils
import           Ouroboros.Consensus.Shelley.Eras
import           Ouroboros.Consensus.Shelley.Ledger.Block
import           Ouroboros.Consensus.Shelley.Ledger.Ledger
                     (ShelleyLedgerConfig (shelleyLedgerGlobals),
                     Ticked (TickedShelleyLedgerState, tickedShelleyLedgerState),
                     getPParams)
import           Ouroboros.Consensus.Util (ShowProxy (..))
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Network.Block (unwrapCBORinCBOR, wrapCBORinCBOR)

data instance GenTx (ShelleyBlock proto era) = ShelleyTx !(SL.TxId (EraCrypto era)) !(Tx era)
  deriving stock    (Generic)

deriving instance ShelleyBasedEra era => NoThunks (GenTx (ShelleyBlock proto era))

deriving instance ShelleyBasedEra era => Eq (GenTx (ShelleyBlock proto era))

instance (Typeable era, Typeable proto)
  => ShowProxy (GenTx (ShelleyBlock proto era)) where

data instance Validated (GenTx (ShelleyBlock proto era)) =
    ShelleyValidatedTx
      !(SL.TxId (EraCrypto era))
      !(SL.Validated (Tx era))
  deriving stock (Generic)

deriving instance ShelleyBasedEra era => NoThunks (Validated (GenTx (ShelleyBlock proto era)))

deriving instance ShelleyBasedEra era => Eq (Validated (GenTx (ShelleyBlock proto era)))

deriving instance ShelleyBasedEra era => Show (Validated (GenTx (ShelleyBlock proto era)))

instance (Typeable era, Typeable proto)
  => ShowProxy (Validated (GenTx (ShelleyBlock proto era))) where

type instance ApplyTxErr (ShelleyBlock proto era) = SL.ApplyTxError era

-- orphaned instance
instance Typeable era => ShowProxy (SL.ApplyTxError era) where


-- |'txInBlockSize' is used to estimate how many transactions we can grab from
-- the Mempool to put into the block we are going to forge without exceeding
-- the maximum block body size according to the ledger. If we exceed that
-- limit, we will have forged a block that is invalid according to the ledger.
-- We ourselves won't even adopt it, causing us to lose our slot, something we
-- must try to avoid.
--
-- For this reason it is better to overestimate the size of a transaction than
-- to underestimate. The only downside is that we maybe could have put one (or
-- more?) transactions extra in that block.
--
-- As the sum of the serialised transaction sizes is not equal to the size of
-- the serialised block body ('TxSeq') consisting of those transactions
-- (see cardano-node#1545 for an example), we account for some extra overhead
-- per transaction as a safety margin.
--
-- Also see 'perTxOverhead'.
fixedBlockBodyOverhead :: Num a => a
fixedBlockBodyOverhead = 1024

-- | See 'fixedBlockBodyOverhead'.
perTxOverhead :: Num a => a
perTxOverhead = 4

instance (ShelleyCompatible proto era, TxLimits (ShelleyBlock proto era))
      => LedgerSupportsMempool (ShelleyBlock proto era) where
  txInvariant = const True

  applyTx = applyShelleyTx

  reapplyTx = reapplyShelleyTx

  txForgetValidated (ShelleyValidatedTx txid vtx) = ShelleyTx txid (SL.extractTx vtx)

  getTransactionKeySets (ShelleyTx _ tx) =
        LedgerTables
      $ KeysMK
        (tx ^. (bodyTxL . SL.allInputsTxBodyF))

mkShelleyTx :: forall era proto. ShelleyBasedEra era => Tx era -> GenTx (ShelleyBlock proto era)
mkShelleyTx tx = ShelleyTx (SL.txIdTxBody @era (tx ^. bodyTxL)) tx

mkShelleyValidatedTx :: forall era proto.
     ShelleyBasedEra era
  => SL.Validated (Tx era)
  -> Validated (GenTx (ShelleyBlock proto era))
mkShelleyValidatedTx vtx = ShelleyValidatedTx txid vtx
  where
    txid = SL.txIdTxBody @era (SL.extractTx vtx ^. bodyTxL)

newtype instance TxId (GenTx (ShelleyBlock proto era)) = ShelleyTxId (SL.TxId (EraCrypto era))
  deriving newtype (Eq, Ord, NoThunks)

deriving newtype instance (Crypto (EraCrypto era), Typeable era, Typeable proto)
                       => EncCBOR (TxId (GenTx (ShelleyBlock proto era)))
deriving newtype instance (Crypto (EraCrypto era), Typeable era, Typeable proto)
                       => DecCBOR (TxId (GenTx (ShelleyBlock proto era)))

instance (Typeable era, Typeable proto)
  => ShowProxy (TxId (GenTx (ShelleyBlock proto era))) where

instance ShelleyBasedEra era => HasTxId (GenTx (ShelleyBlock proto era)) where
  txId (ShelleyTx i _) = ShelleyTxId i

instance ShelleyBasedEra era => ConvertRawTxId (GenTx (ShelleyBlock proto era)) where
  toRawTxIdHash (ShelleyTxId i) =
      Hash.hashToBytesShort . SL.extractHash . SL.unTxId $ i

instance ShelleyBasedEra era => HasTxs (ShelleyBlock proto era) where
  extractTxs =
        map mkShelleyTx
      . txSeqToList
      . SL.bbody
      . shelleyBlockRaw
    where
      txSeqToList :: TxSeq era -> [Tx era]
      txSeqToList = toList . fromTxSeq @era

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

instance ShelleyCompatible proto era => ToCBOR (GenTx (ShelleyBlock proto era)) where
  -- No need to encode the 'TxId', it's just a hash of the 'SL.TxBody' inside
  -- 'SL.Tx', so it can be recomputed.
  toCBOR (ShelleyTx _txid tx) = wrapCBORinCBOR toCBOR tx

instance ShelleyCompatible proto era => FromCBOR (GenTx (ShelleyBlock proto era)) where
  fromCBOR = fmap mkShelleyTx $ unwrapCBORinCBOR
    $ eraDecoder @era $ (. Full) . runAnnotator <$> decCBOR

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance ShelleyBasedEra era => Condense (GenTx (ShelleyBlock proto era)) where
  condense (ShelleyTx _ tx ) = show tx

instance Condense (GenTxId (ShelleyBlock proto era)) where
  condense (ShelleyTxId i) = "txid: " <> show i

instance ShelleyBasedEra era => Show (GenTx (ShelleyBlock proto era)) where
  show = condense

instance Show (GenTxId (ShelleyBlock proto era)) where
  show = condense

{-------------------------------------------------------------------------------
  Applying transactions
-------------------------------------------------------------------------------}

applyShelleyTx :: forall era proto.
     ShelleyBasedEra era
  => LedgerConfig (ShelleyBlock proto era)
  -> WhetherToIntervene
  -> SlotNo
  -> GenTx (ShelleyBlock proto era)
  -> TickedLedgerState (ShelleyBlock proto era) ValuesMK
  -> Except (ApplyTxErr (ShelleyBlock proto era))
       ( TickedLedgerState (ShelleyBlock proto era) DiffMK
       , Validated (GenTx (ShelleyBlock proto era))
       )
applyShelleyTx cfg wti slot (ShelleyTx _ tx) st0 = do
    let st1 :: TickedLedgerState (ShelleyBlock proto era) EmptyMK
        st1 = stowLedgerTables st0

        innerSt :: SL.NewEpochState era
        innerSt = tickedShelleyLedgerState st1

    (mempoolState', vtx) <-
       applyShelleyBasedTx
         (shelleyLedgerGlobals cfg)
         (SL.mkMempoolEnv   innerSt slot)
         (SL.mkMempoolState innerSt)
         wti
         tx

    let st' :: TickedLedgerState (ShelleyBlock proto era) DiffMK
        st' = trackingToDiffs
            $ calculateDifference st0
            $ unstowLedgerTables
            $ set theLedgerLens mempoolState' st1

    pure (st', mkShelleyValidatedTx vtx)

reapplyShelleyTx ::
     ShelleyBasedEra era
  => ComputeDiffs
  -> LedgerConfig (ShelleyBlock proto era)
  -> SlotNo
  -> Validated (GenTx (ShelleyBlock proto era))
  -> TickedLedgerState (ShelleyBlock proto era) ValuesMK
  -> Except (ApplyTxErr (ShelleyBlock proto era)) (TickedLedgerState (ShelleyBlock proto era) TrackingMK)
reapplyShelleyTx doDiffs cfg slot vgtx st0 = do
    let st1     = stowLedgerTables st0
        innerSt = tickedShelleyLedgerState st1

    mempoolState' <-
        SL.reapplyTx
          (shelleyLedgerGlobals cfg)
          (SL.mkMempoolEnv   innerSt slot)
          (SL.mkMempoolState innerSt)
          vtx

    pure $ (case doDiffs of
              ComputeDiffs -> calculateDifference st0
              IgnoreDiffs -> attachEmptyDiffs
           )
         $ unstowLedgerTables
         $ set theLedgerLens mempoolState' st1

  where
    ShelleyValidatedTx _txid vtx = vgtx

-- | The lens combinator
set ::
     (forall f. Applicative f => (a -> f b) -> s -> f t)
  -> b -> s -> t
set lens inner outer =
    runIdentity $ lens (\_ -> Identity inner) outer

theLedgerLens ::
     Functor f
  => (SL.LedgerState era -> f (SL.LedgerState era))
  -> TickedLedgerState (ShelleyBlock proto era) mk
  -> f (TickedLedgerState (ShelleyBlock proto era) mk)
theLedgerLens f x =
        (\y -> x{tickedShelleyLedgerState = y})
    <$> SL.overNewEpochState f (tickedShelleyLedgerState x)

{-------------------------------------------------------------------------------
  Tx Limits
-------------------------------------------------------------------------------}

-- | A non-exported newtype wrapper just to give a 'Semigroup' instance
newtype TxErrorSG era = TxErrorSG { unTxErrorSG :: SL.ApplyTxError era }

instance Semigroup (TxErrorSG era) where
  TxErrorSG (SL.ApplyTxError x) <> TxErrorSG (SL.ApplyTxError y) =
    TxErrorSG (SL.ApplyTxError (x <> y))

validateMaybe ::
     SL.ApplyTxError era
  -> Maybe a
  -> V.Validation (TxErrorSG era) a
validateMaybe err mb = V.validate (TxErrorSG err) id mb

runValidation ::
     V.Validation (TxErrorSG era) a
  -> Except (SL.ApplyTxError era) a
runValidation = liftEither . (unTxErrorSG +++ id) . V.toEither

-----

txsMaxBytes ::
     ShelleyCompatible proto era
  => TickedLedgerState (ShelleyBlock proto era) mk
  -> IgnoringOverflow ByteSize32
txsMaxBytes TickedShelleyLedgerState { tickedShelleyLedgerState } =
    -- `maxBlockBodySize` is expected to be bigger than `fixedBlockBodyOverhead`
      IgnoringOverflow
    $ ByteSize32
    $ maxBlockBodySize - fixedBlockBodyOverhead
  where
    maxBlockBodySize = getPParams tickedShelleyLedgerState ^. ppMaxBBSizeL

txInBlockSize ::
     (ShelleyCompatible proto era, MaxTxSizeUTxO era)
  => TickedLedgerState (ShelleyBlock proto era) mk
  -> GenTx (ShelleyBlock proto era)
  -> V.Validation (TxErrorSG era) (IgnoringOverflow ByteSize32)
txInBlockSize st (ShelleyTx _txid tx') =
    validateMaybe (maxTxSizeUTxO txsz limit) $ do
      guard $ txsz <= limit
      Just $ IgnoringOverflow $ ByteSize32 $ fromIntegral txsz + perTxOverhead
  where
    txsz = tx' ^. sizeTxF

    pparams = getPParams $ tickedShelleyLedgerState st
    limit   = fromIntegral (pparams ^. L.ppMaxTxSizeL) :: Integer

class MaxTxSizeUTxO era where
  maxTxSizeUTxO ::
       Integer
       -- ^ Actual transaction size
    -> Integer
       -- ^ Maximum transaction size
    -> SL.ApplyTxError era

instance MaxTxSizeUTxO (ShelleyEra c) where
  maxTxSizeUTxO txSize txSizeLimit =
      SL.ApplyTxError . pure
    $ ShelleyEra.UtxowFailure
    $ ShelleyEra.UtxoFailure
    $ ShelleyEra.MaxTxSizeUTxO
    $ L.Mismatch { mismatchSupplied = txSize
                 , mismatchExpected = txSizeLimit }

instance MaxTxSizeUTxO (AllegraEra c) where
  maxTxSizeUTxO txSize txSizeLimit =
      SL.ApplyTxError . pure
    $ ShelleyEra.UtxowFailure
    $ ShelleyEra.UtxoFailure
    $ AllegraEra.MaxTxSizeUTxO
    $ L.Mismatch { mismatchSupplied = txSize
                 , mismatchExpected = txSizeLimit }

instance MaxTxSizeUTxO (MaryEra c) where
  maxTxSizeUTxO txSize txSizeLimit =
      SL.ApplyTxError . pure
    $ ShelleyEra.UtxowFailure
    $ ShelleyEra.UtxoFailure
    $ AllegraEra.MaxTxSizeUTxO
    $ L.Mismatch { mismatchSupplied = txSize
                 , mismatchExpected = txSizeLimit }

instance MaxTxSizeUTxO (AlonzoEra c) where
  maxTxSizeUTxO txSize txSizeLimit =
      SL.ApplyTxError . pure
    $ ShelleyEra.UtxowFailure
    $ AlonzoEra.ShelleyInAlonzoUtxowPredFailure
    $ ShelleyEra.UtxoFailure
    $ AlonzoEra.MaxTxSizeUTxO
    $ L.Mismatch { mismatchSupplied = txSize
                 , mismatchExpected = txSizeLimit }

instance MaxTxSizeUTxO (BabbageEra c) where
  maxTxSizeUTxO txSize txSizeLimit =
      SL.ApplyTxError . pure
    $ ShelleyEra.UtxowFailure
    $ BabbageEra.UtxoFailure
    $ BabbageEra.AlonzoInBabbageUtxoPredFailure
    $ AlonzoEra.MaxTxSizeUTxO
    $ L.Mismatch { mismatchSupplied = txSize
                 , mismatchExpected = txSizeLimit }

instance MaxTxSizeUTxO (ConwayEra c) where
  maxTxSizeUTxO txSize txSizeLimit =
      SL.ApplyTxError . pure
    $ ConwayEra.ConwayUtxowFailure
    $ ConwayEra.UtxoFailure
    $ ConwayEra.MaxTxSizeUTxO
    $ L.Mismatch { mismatchSupplied = txSize
                 , mismatchExpected = txSizeLimit }

-----

instance ShelleyCompatible p (ShelleyEra c) => TxLimits (ShelleyBlock p (ShelleyEra c)) where
  type TxMeasure (ShelleyBlock p (ShelleyEra c)) = IgnoringOverflow ByteSize32
  txMeasure              _cfg st tx = runValidation $ txInBlockSize st tx
  blockCapacityTxMeasure _cfg       = txsMaxBytes

instance ShelleyCompatible p (AllegraEra c) => TxLimits (ShelleyBlock p (AllegraEra c)) where
  type TxMeasure (ShelleyBlock p (AllegraEra c)) = IgnoringOverflow ByteSize32
  txMeasure              _cfg st tx = runValidation $ txInBlockSize st tx
  blockCapacityTxMeasure _cfg       = txsMaxBytes

instance ShelleyCompatible p (MaryEra c) => TxLimits (ShelleyBlock p (MaryEra c)) where
  type TxMeasure (ShelleyBlock p (MaryEra c)) = IgnoringOverflow ByteSize32
  txMeasure              _cfg st tx = runValidation $ txInBlockSize st tx
  blockCapacityTxMeasure _cfg       = txsMaxBytes

-----

data AlonzoMeasure = AlonzoMeasure {
    byteSize :: !(IgnoringOverflow ByteSize32)
  , exUnits  :: !(ExUnits' Natural)
  } deriving stock (Eq, Generic, Show)
    deriving anyclass (NoThunks)
    deriving (Measure)
         via (InstantiatedAt Generic AlonzoMeasure)

instance HasByteSize AlonzoMeasure where
  txMeasureByteSize = unIgnoringOverflow . byteSize

fromExUnits :: ExUnits -> ExUnits' Natural
fromExUnits = unWrapExUnits

blockCapacityAlonzoMeasure ::
     forall proto era mk.
     (ShelleyCompatible proto era, L.AlonzoEraPParams era)
  => TickedLedgerState (ShelleyBlock proto era) mk
  -> AlonzoMeasure
blockCapacityAlonzoMeasure ledgerState =
    AlonzoMeasure {
        byteSize = txsMaxBytes ledgerState
      , exUnits  = fromExUnits $ pparams ^. ppMaxBlockExUnitsL
      }
  where
    pparams = getPParams $ tickedShelleyLedgerState ledgerState

txMeasureAlonzo ::
     forall proto era.
     ( ShelleyCompatible proto era
     , L.AlonzoEraPParams era
     , L.AlonzoEraTxWits era
     , ExUnitsTooBigUTxO era
     , MaxTxSizeUTxO era
     )
  => TickedLedgerState (ShelleyBlock proto era) ValuesMK
  -> GenTx (ShelleyBlock proto era)
  -> V.Validation (TxErrorSG era) AlonzoMeasure
txMeasureAlonzo st tx@(ShelleyTx _txid tx') =
    AlonzoMeasure <$> txInBlockSize st tx <*> exunits
  where
    txsz = totExUnits tx'

    pparams = getPParams $ tickedShelleyLedgerState st
    limit   = pparams ^. L.ppMaxTxExUnitsL

    exunits =
      validateMaybe (exUnitsTooBigUTxO txsz limit) $ do
        guard $ pointWiseExUnits (<=) txsz limit
        Just $ fromExUnits txsz

class ExUnitsTooBigUTxO era where
  exUnitsTooBigUTxO :: ExUnits -> ExUnits -> SL.ApplyTxError era

instance Crypto c => ExUnitsTooBigUTxO (AlonzoEra c) where
  exUnitsTooBigUTxO txsz limit =
      SL.ApplyTxError . pure
    $ ShelleyEra.UtxowFailure
    $ AlonzoEra.ShelleyInAlonzoUtxowPredFailure
    $ ShelleyEra.UtxoFailure
    $ AlonzoEra.ExUnitsTooBigUTxO
    $ L.Mismatch { mismatchSupplied = txsz
                 , mismatchExpected = limit }

instance Crypto c => ExUnitsTooBigUTxO (BabbageEra c) where
  exUnitsTooBigUTxO txsz limit =
      SL.ApplyTxError . pure
    $ ShelleyEra.UtxowFailure
    $ BabbageEra.AlonzoInBabbageUtxowPredFailure
    $ AlonzoEra.ShelleyInAlonzoUtxowPredFailure
    $ ShelleyEra.UtxoFailure
    $ BabbageEra.AlonzoInBabbageUtxoPredFailure
    $ AlonzoEra.ExUnitsTooBigUTxO
    $ L.Mismatch { mismatchSupplied = txsz
                 , mismatchExpected = limit }

instance Crypto c => ExUnitsTooBigUTxO (ConwayEra c) where
  exUnitsTooBigUTxO txsz limit =
      SL.ApplyTxError . pure
    $ ConwayEra.ConwayUtxowFailure
    $ ConwayEra.UtxoFailure
    $ ConwayEra.ExUnitsTooBigUTxO
    $ L.Mismatch { mismatchSupplied = txsz
                 , mismatchExpected = limit }

-----

instance ( ShelleyCompatible p (AlonzoEra c)
         ) => TxLimits (ShelleyBlock p (AlonzoEra c)) where

  type TxMeasure (ShelleyBlock p (AlonzoEra c)) = AlonzoMeasure
  txMeasure              _cfg st tx = runValidation $ txMeasureAlonzo st tx
  blockCapacityTxMeasure _cfg       = blockCapacityAlonzoMeasure

-----

data ConwayMeasure = ConwayMeasure {
    alonzoMeasure  :: !AlonzoMeasure
  , refScriptsSize :: !(IgnoringOverflow ByteSize32)
  } deriving stock (Eq, Generic, Show)
    deriving anyclass (NoThunks)
    deriving (Measure)
         via (InstantiatedAt Generic ConwayMeasure)

instance HasByteSize ConwayMeasure where
  txMeasureByteSize = txMeasureByteSize . alonzoMeasure

blockCapacityConwayMeasure ::
     forall proto era mk.
     ( ShelleyCompatible proto era
     , L.AlonzoEraPParams era
     )
  => TickedLedgerState (ShelleyBlock proto era) mk
  -> ConwayMeasure
blockCapacityConwayMeasure st =
    ConwayMeasure {
        alonzoMeasure  = blockCapacityAlonzoMeasure st
      , refScriptsSize = IgnoringOverflow $ ByteSize32 $ fromIntegral $
          -- For post-Conway eras, this will become a protocol parameter.
          SL.maxRefScriptSizePerBlock
      }

txMeasureConway ::
     forall proto era.
     ( ShelleyCompatible proto era
     , L.AlonzoEraTxWits era
     , L.BabbageEraTxBody era
     , ExUnitsTooBigUTxO era
     , MaxTxSizeUTxO era
     , TxRefScriptsSizeTooBig era
     )
  => TickedLedgerState (ShelleyBlock proto era) ValuesMK
  -> GenTx (ShelleyBlock proto era)
  -> V.Validation (TxErrorSG era) ConwayMeasure
txMeasureConway st tx@(ShelleyTx _txid tx') =
    ConwayMeasure <$> txMeasureAlonzo st tx <*> refScriptBytes
  where
    utxo = SL.getUTxO . tickedShelleyLedgerState $ st
    txsz = SL.txNonDistinctRefScriptsSize utxo tx' :: Int

    -- For post-Conway eras, this will become a protocol parameter.
    limit = SL.maxRefScriptSizePerTx

    refScriptBytes =
      validateMaybe (txRefScriptsSizeTooBig txsz limit) $ do
        guard $ txsz <= limit
        Just $ IgnoringOverflow $ ByteSize32 $ fromIntegral txsz

class TxRefScriptsSizeTooBig era where
  txRefScriptsSizeTooBig :: Int -> Int -> SL.ApplyTxError era

instance Crypto c => TxRefScriptsSizeTooBig (ConwayEra c) where
  txRefScriptsSizeTooBig txsz limit =
      SL.ApplyTxError . pure
    $ ConwayEra.ConwayTxRefScriptsSizeTooBig
    $ L.Mismatch { mismatchSupplied = txsz
                 , mismatchExpected = limit }

-----

txMeasureBabbage ::
     forall proto era.
     ( ShelleyCompatible proto era
     , L.AlonzoEraTxWits era
     , L.BabbageEraTxBody era
     , ExUnitsTooBigUTxO era
     , MaxTxSizeUTxO era
     )
  => TickedLedgerState (ShelleyBlock proto era) ValuesMK
  -> GenTx (ShelleyBlock proto era)
  -> V.Validation (TxErrorSG era) ConwayMeasure
txMeasureBabbage st tx@(ShelleyTx _txid tx') =
    (\x -> ConwayMeasure x refScriptBytes) <$> txMeasureAlonzo st tx
  where
    utxo = SL.getUTxO $ tickedShelleyLedgerState st

    -- The Babbage rules should have checked this ref script size against a
    -- limit, but they did not. Now that Cardano @mainnet@ is in Conway, that
    -- omission is no longer an attack vector. Any other chain intending to
    -- ever use Babbage as its current era ought to patch this.
    refScriptBytes =
        IgnoringOverflow
      $ ByteSize32
      $ fromIntegral (SL.txNonDistinctRefScriptsSize utxo tx' :: Int)

-- | We anachronistically use 'ConwayMeasure' in Babbage.
instance ( ShelleyCompatible p (BabbageEra c)
         ) => TxLimits (ShelleyBlock p (BabbageEra c)) where

  type TxMeasure (ShelleyBlock p (BabbageEra c)) = ConwayMeasure
  txMeasure              _cfg st tx = runValidation $ txMeasureBabbage st tx
  blockCapacityTxMeasure _cfg       = blockCapacityConwayMeasure

instance ( ShelleyCompatible p (ConwayEra c)
         ) => TxLimits (ShelleyBlock p (ConwayEra c)) where

  type TxMeasure (ShelleyBlock p (ConwayEra c)) = ConwayMeasure
  txMeasure              _cfg st tx = runValidation $ txMeasureConway st tx
  blockCapacityTxMeasure _cfg       = blockCapacityConwayMeasure
