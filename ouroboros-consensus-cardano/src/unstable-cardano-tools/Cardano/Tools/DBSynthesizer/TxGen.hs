{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

-- | What the transaction generators of 'Cardano.Tools.DBSynthesizer.Run.synthesize'
-- share.
--
-- The generators themselves are 'Cardano.Tools.DBSynthesizer.TxGen.Respend',
-- which builds a chain of transactions in the forge loop, and
-- 'Cardano.Tools.DBSynthesizer.TxGen.File', which replays transactions written
-- ahead of time.
module Cardano.Tools.DBSynthesizer.TxGen
  ( measureTx
  , ownsAddr
  , readPaymentSigningKey
  ) where

import Cardano.Crypto.DSIGN (SignKeyDSIGN)
import qualified Cardano.Keys as Keys
import Cardano.Ledger.Api (Addr (Addr))
import qualified Cardano.Ledger.Keys as LK
import Cardano.Tools.DBSynthesizer.BlsKey (renderKeyFileError)
import Control.Monad.Except (Except)
import Data.Bifunctor (first)
import Ouroboros.Consensus.Ledger.Abstract (LedgerConfig, LedgerState, TickedLedgerState)
import Ouroboros.Consensus.Ledger.SupportsMempool
  ( ApplyTxErr
  , GenTx
  , TxLimits (..)
  , TxMeasure (..)
  )
import Ouroboros.Consensus.Ledger.Tables (HasLedgerTables, ValuesMK)
import Ouroboros.Consensus.Ledger.Tables.Utils (forgetLedgerTables)
import Ouroboros.Consensus.Ticked (Ticked)
import Test.ThreadNet.Infra.Shelley (mkCredential)

-- | Read a payment signing key from the JSON key file that
-- @cardano-cli address key-gen@ writes.
readPaymentSigningKey :: FilePath -> IO (Either String (Keys.SigningKey Keys.PaymentKey))
readPaymentSigningKey path =
  first (renderKeyFileError Keys.renderTextEnvelopeError) <$> Keys.readFileTextEnvelope path

-- | Both phases of a transaction's block measure, against a state holding the
-- values the transaction reads.
measureTx ::
  (TxLimits blk, HasLedgerTables (Ticked LedgerState) blk) =>
  LedgerConfig blk ->
  TickedLedgerState blk ValuesMK ->
  GenTx blk ->
  Except (ApplyTxErr blk) (TxMeasure blk)
measureTx cfg st tx =
  TxMeasure
    <$> txMeasurePhase1 cfg (forgetLedgerTables st) tx
    <*> txMeasurePhase2 cfg st tx

-- | Whether the payment credential of an address is this key's.
--
-- The staking part is not looked at, so one key owns every base address that
-- shares its payment credential. That is what lets a genesis set up many
-- outputs under one key, which is what a queue of them is built from.
ownsAddr :: SignKeyDSIGN LK.DSIGN -> Addr -> Bool
ownsAddr signKey = \case
  Addr _ credential _ -> credential == mkCredential signKey
  _ -> False
