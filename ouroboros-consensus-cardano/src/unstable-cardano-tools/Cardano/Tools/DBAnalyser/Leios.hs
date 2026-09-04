{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Leios-aware block application for db-analyser.
--
-- A cert-RB has an empty wire body. The txs that it causes the ledger to apply
-- are in the EB that it certifies, and the LeiosDb holds them. So an analysis
-- that reads a block body, or that maintains a ledger state, must reach the EB
-- first. The functions here do that reach, and the analyses in
-- 'Cardano.Tools.DBAnalyser.Analysis' call them.
module Cardano.Tools.DBAnalyser.Leios
  ( -- * The EB that a block announces or certifies
    announcementAtPoint
  , certifiedEbHash
  , parentAnnouncement

    -- * Reading the EB
  , blockWithCertifiedEbTxs
  , certifiedEbTxSizes
  , readEbClosure

    -- * Applying the EB
  , ClosureApplied (..)
  , applyClosure
  , closureKeySets

    -- * Applying a whole block
  , applyBlockAtTip
  , applyBlockToTipForker
  , verifyCertRb
  ) where

import Cardano.Tools.DBAnalyser.Types (LedgerApplicationMode (..))
import Control.Monad (when)
import LeiosDemoDb (LeiosDbConnection, leiosDbLookupEbBody)
import LeiosDemoTypes
  ( BytesSize
  , EbHash
  , HasLeiosVoting (..)
  , LeiosExtValidationError (..)
  , LeiosPoint
  , minCertificationThreshold
  , pointEbHash
  , verifyLeiosCert
  )
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.HeaderValidation (HeaderState (..))
import Ouroboros.Consensus.Ledger.Abstract (ApplyBlock (getBlockKeySets))
import Ouroboros.Consensus.Ledger.Basics
import Ouroboros.Consensus.Ledger.Extended
import qualified Ouroboros.Consensus.Ledger.SupportsMempool as LedgerSupportsMempool
import Ouroboros.Consensus.Ledger.SupportsProtocol (LedgerSupportsProtocol)
import Ouroboros.Consensus.Ledger.Tables.Utils
import Ouroboros.Consensus.Storage.Common (BlockComponent (..))
import Ouroboros.Consensus.Storage.ImmutableDB (ImmutableDB)
import qualified Ouroboros.Consensus.Storage.ImmutableDB as ImmutableDB
import qualified Ouroboros.Consensus.Storage.LedgerDB as LedgerDB
import Ouroboros.Consensus.Storage.LedgerDB.Forker
  ( ResolveLeiosBlock
  , announcingRbHash
  , applyLeiosClosure
  , blockLeiosCert
  , headerLeiosAnnouncement
  , inlineLeiosClosure
  , leiosClosureTxKeySets
  , protocolStateLeiosAnnouncement
  , resolveLeiosClosure
  )
import qualified Ouroboros.Consensus.Util.IOLike as IOLike
import Ouroboros.Network.SizeInBytes

-- | The EB that the block at the given point announces. The analysis stream
-- starts /after/ that point, so this is the announcement that the first
-- streamed block sees.
announcementAtPoint ::
  (ResolveLeiosBlock blk, HasHeader blk) =>
  ImmutableDB IO blk ->
  Point blk ->
  IO (Maybe (LeiosPoint, BytesSize))
announcementAtPoint db = \case
  GenesisPoint -> pure Nothing
  BlockPoint slot hash ->
    headerLeiosAnnouncement
      <$> ImmutableDB.getKnownBlockComponent db GetHeader (RealPoint slot hash)

-- | The EB that the block at the given tip announces.
--
-- An analysis that holds a ledger state reads the announcement from here.
-- 'announcementAtPoint' answers the same question from a header, for an analysis
-- that builds no ledger state.
parentAnnouncement ::
  forall blk mk.
  ResolveLeiosBlock blk =>
  ExtLedgerState blk mk ->
  Maybe (LeiosPoint, BytesSize)
parentAnnouncement =
  protocolStateLeiosAnnouncement @blk . headerStateChainDep . headerState

-- | Verify the Leios certificate of a cert-RB against the committee of the
-- parent ledger state. 'Right ()' for a block that carries no certificate.
--
-- This repeats what 'LedgerDB.applyBlock' does on its 'LedgerDB.ApplyVal' path,
-- and it returns the same four rejection reasons. An analysis that calls
-- 'LedgerDB.applyBlock' gets the check from there and must not call this.
-- 'Cardano.Tools.DBAnalyser.Analysis.benchmarkLedgerOps' times each phase of
-- block application on its own, so it cannot call 'LedgerDB.applyBlock', and it
-- calls this instead.
--
-- The check matters because 'applyLeiosClosure' applies the txs of the EB
-- without validation: no signatures, no scripts, no balances.
verifyCertRb ::
  forall blk.
  ( ResolveLeiosBlock blk
  , HasLeiosVoting blk
  ) =>
  -- | The unticked parent state
  ExtLedgerState blk EmptyMK ->
  blk ->
  Either LeiosExtValidationError ()
verifyCertRb parent blk = case blockLeiosCert blk of
  Nothing -> Right ()
  Just cert -> case parentAnnouncement parent of
    -- A cert-RB certifies the EB that its predecessor announced. If the parent
    -- announced none, there is nothing to certify.
    Nothing -> Left (LeiosCertificateWithoutAnnouncement cert)
    Just (announcedPoint, _size) -> case getLeiosCommittee (ledgerState parent) of
      -- A cert-RB in an era with no Leios committee is a protocol violation.
      Nothing -> Left (LeiosMissingCommittee announcedPoint cert)
      Just committee -> case announcingRbHash blk of
        -- A cert-RB always has a non-genesis announcing parent.
        Nothing -> Left (LeiosCertificateAfterGenesis cert announcedPoint)
        Just rbHash ->
          case verifyLeiosCert committee minCertificationThreshold rbHash cert of
            Left invalid ->
              Left (LeiosInvalidCertificate cert announcedPoint rbHash invalid)
            Right _weight -> Right ()

-- | The EB that this block certifies. 'Nothing' for a block that carries no
-- Leios certificate.
certifiedEbHash ::
  ResolveLeiosBlock blk =>
  -- | The EB that the previous block announced
  Maybe (LeiosPoint, BytesSize) ->
  blk ->
  Maybe EbHash
certifiedEbHash prevAnnouncement blk =
  case blockLeiosCert blk of
    Nothing -> Nothing
    Just{} -> case prevAnnouncement of
      -- A CertRB's parent always announced an EB; its absence is a bug.
      Nothing -> error "certifiedEbHash: CertRB whose parent announced no EB"
      Just (announcedPoint, _size) -> Just (pointEbHash announcedPoint)

-- | The sizes of the txs in the EB that this block certifies. Empty for a block
-- that carries no Leios certificate.
--
-- Reads the EB body, which holds the size of every tx. The tx bytes stay on disk
-- and no tx is deserialised.
certifiedEbTxSizes ::
  ResolveLeiosBlock blk =>
  LeiosDbConnection IO ->
  -- | The EB that the previous block announced
  Maybe (LeiosPoint, BytesSize) ->
  blk ->
  IO [SizeInBytes]
certifiedEbTxSizes leiosConn prevAnnouncement blk =
  case certifiedEbHash prevAnnouncement blk of
    Nothing -> pure []
    Just ebHash ->
      leiosDbLookupEbBody leiosConn ebHash >>= \case
        -- 'leiosDbLookupEbBody' returns a bare list, so an absent EB and an EB
        -- with no txs both give []. Here the EB is a certified one, and a
        -- certified EB holds at least one tx: a committee member votes for an
        -- EB only when it is not empty (CIP-0164). So [] means absent.
        [] -> error (missingEbBodyError ebHash)
        ebBody -> pure [SizeInBytes size | (_txHash, size) <- ebBody]

-- | This block with the txs of the EB that it certifies in its body. 'Nothing'
-- for a block that carries no Leios certificate.
--
-- A cert-RB has an empty body, so the returned block holds the EB's txs and
-- nothing else. The caller then counts those txs with the 'HasAnalysis'
-- functions. Those functions take a block, and 'HasAnalysis' has no per-tx
-- counterpart, so the txs go back into a block rather than stay a list.
--
-- The header is untouched, so 'blockMatchesHeader' on the returned block is
-- False.
blockWithCertifiedEbTxs ::
  ResolveLeiosBlock blk =>
  LeiosDbConnection IO ->
  -- | The EB that the previous block announced
  Maybe (LeiosPoint, BytesSize) ->
  blk ->
  IO (Maybe blk)
blockWithCertifiedEbTxs leiosConn prevAnnouncement blk =
  case certifiedEbHash prevAnnouncement blk of
    Nothing -> pure Nothing
    Just ebHash -> Just . inlineLeiosClosure blk . fst <$> readEbClosure leiosConn ebHash

-- | The txs of the EB with the given hash, in the order they appear in the EB,
-- and their total size in bytes.
--
-- The EB body holds the size of every tx, and this function reads that body
-- anyway, so the total size costs no further read of the LeiosDb.
readEbClosure ::
  ResolveLeiosBlock blk =>
  LeiosDbConnection IO ->
  EbHash ->
  IO ([LedgerSupportsMempool.GenTx blk], BytesSize)
readEbClosure leiosConn ebHash = do
  -- Check that the EB is here before resolving it. On an absent EB
  -- 'resolveLeiosClosure' errors with "chain-sel selected a cert-RB without its
  -- EB closure". Report the absence here.
  ebBody <- leiosDbLookupEbBody leiosConn ebHash
  when (null ebBody) $ error (missingEbBodyError ebHash)
  txs <- map snd <$> resolveLeiosClosure leiosConn ebHash
  pure (txs, sum (snd <$> ebBody))

missingEbBodyError :: EbHash -> String
missingEbBodyError ebHash =
  "Could not resolve the EB "
    <> show ebHash
    <> ", because the LeiosDb holds no body for it. Either the analysis ran "
    <> "with --no-leios-db, which uses an empty in-memory LeiosDb, or the "
    <> "chain and the node's leios.db do not match: the node that applied the "
    <> "certifying block held that EB."

-- | The ledger keys that the given EB txs read.
closureKeySets ::
  ( ResolveLeiosBlock blk
  , HasLedgerTables (LedgerState blk)
  ) =>
  [LedgerSupportsMempool.GenTx blk] ->
  LedgerTables (ExtLedgerState blk) KeysMK
closureKeySets = castLedgerTables . foldMap leiosClosureTxKeySets

-- | What 'applyClosure' produces.
data ClosureApplied blk = ClosureApplied
  { caStateAfterEb :: ExtLedgerState blk EmptyMK
  -- ^ The ledger state after applying the EB txs, without its tables.
  , caTablesAfterEb :: LedgerTables (ExtLedgerState blk) ValuesMK
  -- ^ The tables of that state.
  , caClosureDiff :: Maybe (LedgerState blk DiffMK)
  -- ^ The change that applying the EB txs makes. The caller prepends this to
  -- the diff that it pushes to the LedgerDB. 'Nothing' when the block carries
  -- no certificate.
  }

-- | Apply the given EB txs to the parent tip. Returns the ledger
-- state after applying those txs, as a state with no tables plus the
-- tables themselves. Returns the parent tip and its tables unchanged
-- when the list is empty.
--
-- The returned action applies every tx before it returns. So a caller that times
-- the action measures the whole application.
applyClosure ::
  ( ResolveLeiosBlock blk
  , HasLedgerTables (LedgerState blk)
  , Show (LedgerErr (LedgerState blk))
  ) =>
  LedgerCfg (LedgerState blk) ->
  [LedgerSupportsMempool.GenTx blk] ->
  -- | Parent tip
  ExtLedgerState blk EmptyMK ->
  LedgerTables (ExtLedgerState blk) ValuesMK ->
  IO (ClosureApplied blk)
applyClosure _lcfg [] parent tables = pure (ClosureApplied parent tables Nothing)
applyClosure lcfg closureTxs parent tables =
  -- The pattern match forces 'applyLeiosClosure', so every tx applies before this
  -- function returns.
  case applyLeiosClosure lcfg closureTxs stateBeforeEb of
    -- 'applyLeiosClosure' applies the closure without validation: each tx passed
    -- validation when the node wrote it to the LeiosDb. So a failure here is
    -- unexpected, and the message carries the ledger error itself.
    Left err -> error ("applyClosure: " <> show err)
    Right stateAfterEb ->
      pure $
        ClosureApplied
          (parent{ledgerState = forgetLedgerTables stateAfterEb})
          (ltprj stateAfterEb)
          (Just (trackingToDiffs (calculateDifference stateBeforeEb stateAfterEb)))
 where
  stateBeforeEb = ledgerState parent `ltwith` castLedgerTables tables

-- | The 'LedgerDB.BlockApplicationMode' that a 'LedgerApplicationMode' selects.
--
-- The difference is not only the cost. On 'LedgerApply' the node verifies the
-- Leios certificate of a cert-RB, and on 'LedgerReapply' it does not. See
-- 'verifyCertRb'.
blockApplicationMode :: LedgerApplicationMode -> LedgerDB.BlockApplicationMode
blockApplicationMode = \case
  LedgerApply -> LedgerDB.ValidateBlock
  LedgerReapply -> LedgerDB.ReapplyBlock

-- | Read the ledger state at the tip (with the UTxOs this block
-- consumes) and apply the block to it via the node's own
-- 'LedgerDB.applyBlockToForker'. Fails on any error: an invalid block, an
-- invalid Leios certificate, or an EB closure that is missing or
-- won't apply.
applyBlockAtTip ::
  ( LedgerSupportsProtocol blk
  , ResolveLeiosBlock blk
  , HasLeiosVoting blk
  ) =>
  LeiosDbConnection IO ->
  LedgerApplicationMode ->
  TopLevelConfig blk ->
  LedgerDB.LedgerDB' IO blk ->
  blk ->
  IO (ExtLedgerState blk ValuesMK, ExtLedgerState blk DiffMK)
applyBlockAtTip leiosConn mode cfg ldb blk =
  LedgerDB.withTipForker ldb $ \frk -> do
    oldLedgerSt <- IOLike.atomically $ LedgerDB.forkerGetLedgerState frk
    oldLedgerTbs <- LedgerDB.forkerReadTables frk (getBlockKeySets blk)
    let preState = oldLedgerSt `withLedgerTables` oldLedgerTbs
    applied <-
      either (error . show . LedgerDB.annLedgerErr) id
        <$> applyBlockToTipForker leiosConn mode cfg frk blk
    pure (preState, applied)

-- | Apply the block to the given forker, in the given mode.
applyBlockToTipForker ::
  ( LedgerSupportsProtocol blk
  , ResolveLeiosBlock blk
  , HasLeiosVoting blk
  ) =>
  LeiosDbConnection IO ->
  LedgerApplicationMode ->
  TopLevelConfig blk ->
  LedgerDB.Forker' IO blk ->
  blk ->
  IO
    ( Either
        (LedgerDB.AnnLedgerError (ExtLedgerState blk) blk)
        (ExtLedgerState blk DiffMK)
    )
applyBlockToTipForker leiosConn mode cfg frk blk =
  LedgerDB.applyBlockToForker
    leiosConn
    (blockApplicationMode mode)
    OmitLedgerEvents
    (ExtLedgerCfg cfg)
    frk
    blk
