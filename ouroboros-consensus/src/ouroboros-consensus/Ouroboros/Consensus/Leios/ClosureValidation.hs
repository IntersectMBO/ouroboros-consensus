{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | 'validateLeiosClosure' through the mempool's 'applyTx' and 'reapplyTx'.
module Ouroboros.Consensus.Leios.ClosureValidation (validateClosureInMempool) where

import Cardano.Ledger.Binary (DecoderError, Version)
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Slotting.Slot (WithOrigin (..))
import Control.Monad (when)
import Control.Monad.Except (liftEither, runExcept, runExceptT, throwError)
import Control.Monad.Trans.Class (lift)
import Data.Bifunctor (first)
import Data.List (zip4)
import qualified Data.Map.Strict as Map
import qualified Data.Measure as Measure
import Data.Proxy (Proxy (..))
import qualified Data.Text as Text
import Ouroboros.Consensus.Block
  ( LeiosClosureInvalid (..)
  , LeiosClosureValidation (..)
  , LookupValidatedTxs
  , ReadLedgerTables
  , RecordValidatedTx
  , SlotNo
  , pointSlot
  )
import Ouroboros.Consensus.Ledger.Abstract
  ( ComputeLedgerEvents (OmitLedgerEvents)
  , GetTip (..)
  , LedgerConfig
  , LedgerState
  , applyChainTick
  )
import Ouroboros.Consensus.Ledger.Basics (TickedLedgerState)
import Ouroboros.Consensus.Ledger.SupportsMempool
  ( ByteSize32 (..)
  , GenTx
  , LedgerSupportsMempool (..)
  , TxEbMeasureByteSize (..)
  , TxLimits (..)
  , TxMeasure (..)
  , Validated
  , WhetherToIntervene (Intervene)
  )
import Ouroboros.Consensus.Ledger.Tables
  ( DiffMK (..)
  , EmptyMK
  , HasLedgerTables (..)
  , KeysMK (..)
  , LedgerTables
  , ValuesMK (..)
  , ltliftA2
  , ltliftA3
  )
import Ouroboros.Consensus.Ledger.Tables.Diff (Delta (..), Diff (..))
import Ouroboros.Consensus.Ledger.Tables.Utils (forgetLedgerTables, restrictValuesMK)
import Ouroboros.Consensus.Leios.Types
  ( LeiosTx
  , SerialisedEndorserBlock (..)
  , TxHash
  , TxOffset
  , deserialiseEndorserBlock
  , endorserBlockReferences
  , sizeEndorserBlockReference
  )

-- | Validate a closure on the ledger state of the block that announced it,
-- ticked to its slot, as the mempool validates transactions.
--
-- The references are checked against what an endorser block may hold before
-- any transaction is looked at.
validateClosureInMempool ::
  forall m blk.
  (Monad m, LedgerSupportsMempool blk) =>
  -- | Decode a transaction of the closure.
  (LeiosTx -> Either DecoderError (GenTx blk)) ->
  -- | The major protocol version transactions are validated under in the
  -- state.
  (TickedLedgerState blk EmptyMK -> Version) ->
  -- | A transaction validated before, to reapply rather than validate in full.
  ( LedgerConfig blk ->
    SlotNo ->
    TickedLedgerState blk ValuesMK ->
    GenTx blk ->
    Maybe (Validated (GenTx blk))
  ) ->
  LedgerConfig blk ->
  ReadLedgerTables m blk ->
  LookupValidatedTxs m ->
  RecordValidatedTx m ->
  SerialisedEndorserBlock ->
  LedgerState blk EmptyMK ->
  m LeiosClosureValidation
validateClosureInMempool decodeTx protVerIn assumeValidated cfg readTables wasValidated recordValidated closure st =
  case pointSlot (getTip st) of
    Origin -> pure $ rejected minBound (ClosureInvalid "no block announced the endorser block")
    At slot -> validateAt slot (applyChainTick OmitLedgerEvents cfg slot st)
 where
  MkSerialisedEndorserBlock{serialisedEndorsement, serialisedTransactions} = closure

  validateAt slot ticked = either (rejected protVer) id <$> runExceptT validate
   where
    protVer = protVerIn (forgetLedgerTables ticked)
    capacity = ebCapacityTxMeasure cfg ticked

    validate = do
      eb <-
        liftEither $ first invalid $ deserialiseEndorserBlock (Proxy @DijkstraEra) serialisedEndorsement
      let references = endorserBlockReferences eb
          -- What the references and the transactions they claim take, before
          -- loading any transaction.
          claimedBytes = sum [toInteger size | (_txHash, size) <- references]
          referencesBytes =
            sum
              [ toInteger . unByteSize32 $ sizeEndorserBlockReference (ByteSize32 size)
              | (_txHash, size) <- references
              ]
          capacityOf f = toInteger (unByteSize32 (f capacity))
      when
        ( claimedBytes > capacityOf txEbMeasureTxsByteSize
            || referencesBytes > capacityOf txEbMeasureByteSize
        )
        $ throwError ClosureOverCapacity
      when (length serialisedTransactions /= length references) $ throwError ClosureIncomplete
      -- The fetch checked the bytes against the references, so a transaction
      -- that does not decode is the block's fault: reject it before applying
      -- any.
      txs <- liftEither $ first invalid $ mapM decodeTx serialisedTransactions
      let txKeys = foldMap getTransactionKeySets txs
          txHashes = map fst references
      values <- lift $ readTables txKeys
      validated <- lift $ wasValidated protVer txHashes
      lift $
        applyClosure
          cfg
          assumeValidated
          (recordValidated protVer)
          slot
          protVer
          capacity
          (applyMempoolDiffs values txKeys ticked)
          (zip4 [0 ..] txHashes validated txs)

  rejected protVer reason =
    MkLeiosClosureValidation
      { closureVerdict = Left reason
      , closureProtVer = protVer
      , closureReapplied = 0
      , closureValidated = []
      }

  invalid :: Show e => e -> LeiosClosureInvalid
  invalid = ClosureInvalid . Text.pack . show

-- | Apply the transactions of a closure, one after the other, to the ledger
-- state of the block that announced it, ticked to its slot, as a certificate
-- for it would. And sum what they take of an endorser block's capacity, which
-- must hold them all.
--
-- A transaction the node validated before is reapplied: only the checks that
-- depend on the ledger state run again. Any other is applied with
-- 'Intervene', so that the ledger does not flip a transaction's validity
-- flag, which applying a certified closure would not either, and is recorded
-- as validated right away.
--
-- The closure's ledger table stays outside the state. Each transaction runs on
-- a state holding only its own inputs, as the mempool runs it, and its effect
-- is folded back into the table, so each step costs what the transaction
-- does, not what the closure does.
applyClosure ::
  forall m blk.
  (Monad m, LedgerSupportsMempool blk) =>
  LedgerConfig blk ->
  ( LedgerConfig blk ->
    SlotNo ->
    TickedLedgerState blk ValuesMK ->
    GenTx blk ->
    Maybe (Validated (GenTx blk))
  ) ->
  -- | Record a transaction validated in full
  (TxOffset -> TxHash -> m ()) ->
  SlotNo ->
  Version ->
  -- | What an endorser block may hold
  TxEbMeasure blk ->
  TickedLedgerState blk ValuesMK ->
  -- | Each transaction, at its offset, with whether the node validated it before
  [(TxOffset, TxHash, Bool, GenTx blk)] ->
  m LeiosClosureValidation
applyClosure cfg assumeValidated recordValidated slot protVer capacity st0 =
  go st0 (projectLedgerTables st0) Measure.zero 0 []
 where
  go ::
    TickedLedgerState blk mk ->
    LedgerTables blk ValuesMK ->
    TxEbMeasure blk ->
    Int ->
    [TxHash] ->
    [(TxOffset, TxHash, Bool, GenTx blk)] ->
    m LeiosClosureValidation
  go _ _ _ !reapplied applied [] = pure $ outcome (Right ()) reapplied applied
  go st values measured !reapplied applied ((off, txHash, validated, tx) : txs) =
    case measure of
      Left reason -> stop reapplied (Left reason)
      Right measured' -> case (validated, assumeValidated cfg slot stTx tx) of
        (True, Just vtx) -> case runExcept (reapplyTx cfg slot vtx stTx) of
          Left err -> stop reapplied (invalid err)
          Right st' ->
            go
              st'
              (ltliftA3 rebase values txKeys (projectLedgerTables st'))
              measured'
              (reapplied + 1)
              applied
              txs
        _ -> case runExcept (applyTx cfg Intervene slot tx stTx) of
          Left err -> stop reapplied (invalid err)
          Right (st', _vtx) -> do
            recordValidated off txHash
            go
              st'
              (ltliftA2 applyDelta values (projectLedgerTables st'))
              measured'
              reapplied
              (txHash : applied)
              txs
   where
    stop reapplied' verdict = pure $ outcome verdict reapplied' applied

    txKeys = getTransactionKeySets tx

    stTx = st `withLedgerTables` ltliftA2 restrictValuesMK values txKeys

    measure = do
      phase1 <- first showInvalid . runExcept $ txMeasurePhase1 cfg (forgetLedgerTables stTx) tx
      phase2 <- first showInvalid . runExcept $ txMeasurePhase2 cfg stTx tx
      let measured' = measured `Measure.plus` txEbMeasure (Proxy @blk) (TxMeasure phase1 phase2)
      if measured' Measure.<= capacity then Right measured' else Left ClosureOverCapacity

  outcome verdict reapplied applied =
    MkLeiosClosureValidation
      { closureVerdict = verdict
      , closureProtVer = protVer
      , closureReapplied = reapplied
      , closureValidated = applied
      }

  invalid = Left . showInvalid

  showInvalid = ClosureInvalid . Text.pack . show

  -- After a reapplied transaction: what 'reapplyTx' left of the transaction's
  -- own entries replaces them in the table. The inputs it consumed are gone,
  -- the ones it only read stay, and its outputs are new.
  rebase :: Ord k => ValuesMK k v -> KeysMK k v -> ValuesMK k v -> ValuesMK k v
  rebase (ValuesMK m) (KeysMK ks) (ValuesMK after) = ValuesMK (Map.union after (Map.withoutKeys m ks))

  -- After an applied transaction: one update per key it changed, where
  -- 'applyDiffs' would merge over the whole table.
  applyDelta :: Ord k => ValuesMK k v -> DiffMK k v -> ValuesMK k v
  applyDelta (ValuesMK m) (DiffMK (Diff ds)) = ValuesMK (Map.foldlWithKey' step m ds)
   where
    step acc k (Insert v) = Map.insert k v acc
    step acc k Delete = Map.delete k acc
