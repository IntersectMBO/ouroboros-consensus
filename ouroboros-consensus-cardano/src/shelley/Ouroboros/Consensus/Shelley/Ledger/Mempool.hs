{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE DisambiguateRecordFields   #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | Shelley mempool integration
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
import           Cardano.Ledger.Alonzo.Core (Tx, TxSeq, bodyTxL, eraProtVerLow,
                     fromTxSeq, ppMaxBBSizeL, ppMaxBlockExUnitsL, sizeTxF)
import           Cardano.Ledger.Alonzo.Scripts (ExUnits, ExUnits',
                     unWrapExUnits)
import           Cardano.Ledger.Alonzo.Tx (totExUnits)
import qualified Cardano.Ledger.Api as L
import           Cardano.Ledger.Binary (Annotator (..), DecCBOR (..),
                     EncCBOR (..), FromCBOR (..), FullByteString (..),
                     ToCBOR (..), toPlainDecoder)
import qualified Cardano.Ledger.Conway.Rules as SL
import qualified Cardano.Ledger.Conway.UTxO as SL
import qualified Cardano.Ledger.Core as SL (txIdTxBody)
import           Cardano.Ledger.Crypto (Crypto)
import qualified Cardano.Ledger.SafeHash as SL
import qualified Cardano.Ledger.Shelley.API as SL
import           Control.Monad.Except (Except)
import           Control.Monad.Identity (Identity (..))
import           Data.DerivingVia (InstantiatedAt (..))
import           Data.Foldable (toList)
import           Data.Measure (Measure)
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)
import           GHC.Natural (Natural)
import           Lens.Micro ((^.))
import           NoThunks.Class (NoThunks (..))
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsMempool
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
    $ toPlainDecoder (eraProtVerLow @era) $ (. Full) . runAnnotator <$> decCBOR

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
  -> TickedLedgerState (ShelleyBlock proto era)
  -> Except (ApplyTxErr (ShelleyBlock proto era))
       ( TickedLedgerState (ShelleyBlock proto era)
       , Validated (GenTx (ShelleyBlock proto era))
       )
applyShelleyTx cfg wti slot (ShelleyTx _ tx) st = do
    (mempoolState', vtx) <-
       applyShelleyBasedTx
         (shelleyLedgerGlobals cfg)
         (SL.mkMempoolEnv   innerSt slot)
         (SL.mkMempoolState innerSt)
         wti
         tx

    let st' = set theLedgerLens mempoolState' st

    pure (st', mkShelleyValidatedTx vtx)
  where
    innerSt = tickedShelleyLedgerState st

reapplyShelleyTx ::
     ShelleyBasedEra era
  => LedgerConfig (ShelleyBlock proto era)
  -> SlotNo
  -> Validated (GenTx (ShelleyBlock proto era))
  -> TickedLedgerState (ShelleyBlock proto era)
  -> Except (ApplyTxErr (ShelleyBlock proto era)) (TickedLedgerState (ShelleyBlock proto era))
reapplyShelleyTx cfg slot vgtx st = do
    mempoolState' <-
        SL.reapplyTx
          (shelleyLedgerGlobals cfg)
          (SL.mkMempoolEnv   innerSt slot)
          (SL.mkMempoolState innerSt)
          vtx

    pure $ set theLedgerLens mempoolState' st
  where
    ShelleyValidatedTx _txid vtx = vgtx

    innerSt = tickedShelleyLedgerState st

-- | The lens combinator
set ::
     (forall f. Applicative f => (a -> f b) -> s -> f t)
  -> b -> s -> t
set lens inner outer =
    runIdentity $ lens (\_ -> Identity inner) outer

theLedgerLens ::
     Functor f
  => (SL.LedgerState era -> f (SL.LedgerState era))
  -> TickedLedgerState (ShelleyBlock proto era)
  -> f (TickedLedgerState (ShelleyBlock proto era))
theLedgerLens f x =
        (\y -> x{tickedShelleyLedgerState = y})
    <$> SL.overNewEpochState f (tickedShelleyLedgerState x)

{-------------------------------------------------------------------------------
  Tx Limits
-------------------------------------------------------------------------------}

txsMaxBytes ::
     ShelleyCompatible proto era
  => TickedLedgerState (ShelleyBlock proto era)
  -> ByteSize
txsMaxBytes TickedShelleyLedgerState { tickedShelleyLedgerState } =
    -- `maxBlockBodySize` is expected to be bigger than `fixedBlockBodyOverhead`
    ByteSize $ maxBlockBodySize - fixedBlockBodyOverhead
  where
    maxBlockBodySize = getPParams tickedShelleyLedgerState ^. ppMaxBBSizeL

txInBlockSize ::
     ShelleyCompatible proto era
  => GenTx (ShelleyBlock proto era)
  -> ByteSize
txInBlockSize (ShelleyTx _ tx) =
    ByteSize $ txSize + perTxOverhead
  where
    txSize = fromIntegral $ tx ^. sizeTxF

instance ShelleyCompatible p (ShelleyEra c) => TxLimits (ShelleyBlock p (ShelleyEra c)) where
  type TxMeasure (ShelleyBlock p (ShelleyEra c)) = ByteSize
  txMeasure              _cfg _st = txInBlockSize
  blockCapacityTxMeasure _cfg     = txsMaxBytes

instance ShelleyCompatible p (AllegraEra c) => TxLimits (ShelleyBlock p (AllegraEra c)) where
  type TxMeasure (ShelleyBlock p (AllegraEra c)) = ByteSize
  txMeasure              _cfg _st = txInBlockSize
  blockCapacityTxMeasure _cfg     = txsMaxBytes

instance ShelleyCompatible p (MaryEra c) => TxLimits (ShelleyBlock p (MaryEra c)) where
  type TxMeasure (ShelleyBlock p (MaryEra c)) = ByteSize
  txMeasure              _cfg _st = txInBlockSize
  blockCapacityTxMeasure _cfg     = txsMaxBytes

instance ( ShelleyCompatible p (AlonzoEra c)
         ) => TxLimits (ShelleyBlock p (AlonzoEra c)) where

  type TxMeasure (ShelleyBlock p (AlonzoEra c)) = AlonzoMeasure
  txMeasure              _cfg _st = txMeasureAlonzo
  blockCapacityTxMeasure _cfg     = blockCapacityAlonzoMeasure

data AlonzoMeasure = AlonzoMeasure {
    byteSize :: !ByteSize
  , exUnits  :: !(ExUnits' Natural)
  } deriving stock (Eq, Generic, Show)
    deriving anyclass (NoThunks)
    deriving (Measure)
         via (InstantiatedAt Generic AlonzoMeasure)

instance HasByteSize AlonzoMeasure where
  txMeasureByteSize = byteSize

fromExUnits :: ExUnits -> ExUnits' Natural
fromExUnits = unWrapExUnits

txMeasureAlonzo ::
     forall proto era.
     (ShelleyCompatible proto era, L.AlonzoEraTxWits era)
  => GenTx (ShelleyBlock proto era) -> AlonzoMeasure
txMeasureAlonzo (ShelleyTx _txid tx) =
    AlonzoMeasure {
        byteSize = txInBlockSize (mkShelleyTx @era @proto tx)
      , exUnits  = fromExUnits $ totExUnits tx
      }

blockCapacityAlonzoMeasure ::
     forall proto era.
     (ShelleyCompatible proto era, L.AlonzoEraPParams era)
  => TickedLedgerState (ShelleyBlock proto era)
  -> AlonzoMeasure
blockCapacityAlonzoMeasure ledgerState =
    AlonzoMeasure {
        byteSize = txsMaxBytes ledgerState
      , exUnits  = fromExUnits $ pparams ^. ppMaxBlockExUnitsL
      }
  where
    pparams = getPParams $ tickedShelleyLedgerState ledgerState

instance ( ShelleyCompatible p (BabbageEra c)
         ) => TxLimits (ShelleyBlock p (BabbageEra c)) where

  type TxMeasure (ShelleyBlock p (BabbageEra c)) = AlonzoMeasure
  txMeasure              _cfg _st = txMeasureAlonzo
  blockCapacityTxMeasure _cfg     = blockCapacityAlonzoMeasure

data ConwayMeasure = ConwayMeasure {
    alonzoMeasure  :: !AlonzoMeasure
  , refScriptsSize :: !ByteSize
  } deriving stock (Eq, Generic, Show)
    deriving anyclass (NoThunks)
    deriving (Measure)
         via (InstantiatedAt Generic ConwayMeasure)

instance HasByteSize ConwayMeasure where
  txMeasureByteSize = txMeasureByteSize . alonzoMeasure

instance ( ShelleyCompatible p (ConwayEra c)
         ) => TxLimits (ShelleyBlock p (ConwayEra c)) where

  type TxMeasure (ShelleyBlock p (ConwayEra c)) = ConwayMeasure

  txMeasure _cfg st tx@(ShelleyTx _txid tx') =
      ConwayMeasure {
          alonzoMeasure  = txMeasureAlonzo tx
        , refScriptsSize = ByteSize $ fromIntegral $
            SL.txNonDistinctRefScriptsSize utxo tx'
        }
    where
      utxo = SL.getUTxO . tickedShelleyLedgerState $ st

  blockCapacityTxMeasure _cfg st =
      ConwayMeasure {
          alonzoMeasure  = blockCapacityAlonzoMeasure st
        , refScriptsSize = ByteSize $ fromIntegral $
            -- For post-Conway eras, this will become a protocol parameter.
            SL.maxRefScriptSizePerBlock
        }
