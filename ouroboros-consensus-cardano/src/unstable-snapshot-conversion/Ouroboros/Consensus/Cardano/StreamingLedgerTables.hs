{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Ouroboros.Consensus.Cardano.StreamingLedgerTables
  ( mkInMemYieldArgs
  , mkInMemSinkArgs
  ) where

import Cardano.Ledger.Binary
import Cardano.Ledger.Core (ByronEra, Era, eraDecoder, toEraCBOR)
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Ledger.Shelley.LedgerState as SL
import qualified Cardano.Ledger.State as SL
import qualified Codec.CBOR.Encoding
import Data.Proxy
import Data.SOP.BasicFunctors
import Data.SOP.Functors
import Data.SOP.Strict
import qualified Data.SOP.Telescope as Telescope
import Lens.Micro
import Ouroboros.Consensus.Byron.Ledger
import Ouroboros.Consensus.Cardano.Block
import Ouroboros.Consensus.Cardano.Ledger
import Ouroboros.Consensus.HardFork.Combinator
import Ouroboros.Consensus.HardFork.Combinator.State
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Shelley.Ledger
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Consensus.Storage.LedgerDB.API
import Ouroboros.Consensus.Storage.LedgerDB.Snapshots
import Ouroboros.Consensus.Storage.LedgerDB.V2.InMemory as V2
import System.FS.API

type L = LedgerState (CardanoBlock StandardCrypto)

mkInMemYieldArgs :: SomeHasFS IO -> DiskSnapshot -> L EmptyMK -> YieldArgs IO V2.Mem L
mkInMemYieldArgs fs ds (HardForkLedgerState (HardForkState idx)) =
  let
    np ::
      NP
        (Current (Flip LedgerState EmptyMK) -.-> K (Decoders L))
        (CardanoEras StandardCrypto)
    np =
      (Fn $ const $ K $ error "Byron")
        :* (Fn $ K . fromEra ShelleyTxOut . unFlip . currentState)
        :* (Fn $ K . fromEra AllegraTxOut . unFlip . currentState)
        :* (Fn $ K . fromEra MaryTxOut . unFlip . currentState)
        :* (Fn $ K . fromEra AlonzoTxOut . unFlip . currentState)
        :* (Fn $ K . fromEra BabbageTxOut . unFlip . currentState)
        :* (Fn $ K . fromEra ConwayTxOut . unFlip . currentState)
        :* (Fn $ K . fromEra DijkstraTxOut . unFlip . currentState)
        :* Nil
   in
    YieldInMemory
      fs
      ds
      (hcollapse $ hap np $ Telescope.tip idx)
 where
  fromEra ::
    forall proto era.
    ShelleyCompatible proto era =>
    (TxOut (LedgerState (ShelleyBlock proto era)) -> CardanoTxOut StandardCrypto) ->
    LedgerState (ShelleyBlock proto era) EmptyMK ->
    Decoders L
  fromEra toCardanoTxOut st =
    let certInterns =
          internsFromMap $
            shelleyLedgerState st
              ^. SL.nesEsL
                . SL.esLStateL
                . SL.lsCertStateL
                . SL.certDStateL
                . SL.accountsL
                . SL.accountsMapL
     in Decoders
          (eraDecoder @era decodeMemPack)
          (eraDecoder @era $ toCardanoTxOut <$> decShareCBOR certInterns)

mkInMemSinkArgs ::
  SomeHasFS IO ->
  DiskSnapshot ->
  L EmptyMK ->
  SinkArgs IO V2.Mem L
mkInMemSinkArgs fs ds (HardForkLedgerState (HardForkState idx)) = do
  let
    np =
      (Fn $ const $ K $ encOne (Proxy @ByronEra))
        :* (Fn $ const $ K $ encOne (Proxy @ShelleyEra))
        :* (Fn $ const $ K $ encOne (Proxy @AllegraEra))
        :* (Fn $ const $ K $ encOne (Proxy @MaryEra))
        :* (Fn $ const $ K $ encOne (Proxy @AlonzoEra))
        :* (Fn $ const $ K $ encOne (Proxy @BabbageEra))
        :* (Fn $ const $ K $ encOne (Proxy @ConwayEra))
        :* (Fn $ const $ K $ encOne (Proxy @DijkstraEra))
        :* Nil
    (encTxIn, encTxOut) = hcollapse $ hap np $ Telescope.tip idx
   in
    SinkInMemory
      1000
      encTxIn
      encTxOut
      fs
      ds
 where
  encOne ::
    forall era.
    Era era =>
    Proxy era ->
    (TxIn L -> Codec.CBOR.Encoding.Encoding, TxOut L -> Codec.CBOR.Encoding.Encoding)
  encOne _ =
    (toEraCBOR @era . encodeMemPack, toEraCBOR @era . eliminateCardanoTxOut (const encodeMemPack))
