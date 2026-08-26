{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Tools.DBSynthesizer.Forging
  ( GenTxs
  , runForge
  ) where

import Cardano.Crypto.DSIGN.Class (deriveVerKeyDSIGN)
import Cardano.Tools.DBSynthesizer.Types
  ( ForgeLimit (..)
  , ForgeResult (..)
  )
import Control.Monad (when)
import Control.Monad.Except (runExcept)
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Trans.Class as Trans
import Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE)
import Control.Tracer (Tracer, nullTracer, traceWith)
import Data.ByteString.Short (fromShort)
import Data.Either (isRight)
import Data.Maybe (fromJust, isJust)
import Data.Proxy
import Data.Word (Word64)
import LeiosDemoDb (LeiosDbConnection)
import LeiosDemoTypes
  ( LeiosSigningKey
  , RbHash (MkRbHash)
  , TraceLeiosKernel (..)
  , getLeiosSeatId
  , leiosCommitteeSize
  , signLeiosVote
  )
import LeiosVoteState
  ( AddVoteResult (Added)
  , LeiosVoteState (addVote)
  , newLeiosVoteState
  )
import LeiosVoting (HasLeiosVoting (getLeiosCommittee))
import Ouroboros.Consensus.Block.Abstract as Block
import Ouroboros.Consensus.Block.Forging as Block
  ( BlockForging (..)
  , ForgeBlockArgs (..)
  , ShouldForge (..)
  , checkShouldForge
  )
import Ouroboros.Consensus.Config
  ( TopLevelConfig
  , configConsensus
  , configLedger
  )
import Ouroboros.Consensus.Forecast (forecastFor)
import Ouroboros.Consensus.HeaderValidation
  ( BasicEnvelopeValidation (..)
  , HeaderState (..)
  )
import Ouroboros.Consensus.Ledger.Abstract (Validated)
import Ouroboros.Consensus.Ledger.Basics
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Ledger.SupportsMempool (GenTx)
import Ouroboros.Consensus.Ledger.SupportsProtocol
import Ouroboros.Consensus.Ledger.Tables.Utils (forgetLedgerTables)
import Ouroboros.Consensus.NodeKernel.Forge (decideLeiosCertify)
import Ouroboros.Consensus.Protocol.Abstract
  ( ChainDepState
  , tickChainDepState
  )
import Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
  ( AddBlockResult (..)
  , ChainDB
  , addBlockAsync
  , blockProcessed
  , getCurrentChain
  , getCurrentLedger
  , getPastLedger
  , withReadOnlyForkerAtPoint
  )
import qualified Ouroboros.Consensus.Storage.ChainDB.API.Types.InvalidBlockPunishment as InvalidBlockPunishment
  ( noPunishment
  )
import Ouroboros.Consensus.Storage.LedgerDB
import Ouroboros.Consensus.Util.EarlyExit
  ( withEarlyExit
  )
import Ouroboros.Consensus.Util.IOLike (atomically)
import Ouroboros.Network.AnchoredFragment as AF
  ( Anchor (..)
  , AnchoredFragment
  , AnchoredSeq (..)
  , headPoint
  )
import Ouroboros.Network.Protocol.LocalStateQuery.Type

data ForgeState
  = ForgeState
  { currentSlot :: !SlotNo
  , forged :: !Word64
  , currentEpoch :: !Word64
  , processed :: !SlotNo
  }

initialForgeState :: ForgeState
initialForgeState = ForgeState 0 0 0 0

-- | An action to generate transactions for a given block.
--
-- The first list fills the ranking block. The second fills the endorser block
-- that the ranking block announces. An empty second list announces no endorser
-- block, because 'mkAndStoreEb' forges none for an empty list.
--
-- The 'Bool' says whether this block certifies the endorser block that its
-- parent announced. Such a block must get no transactions, because 'mkBody'
-- drops them and applies the certified endorser block's instead.
type GenTxs blk =
  SlotNo ->
  Bool ->
  ReadOnlyForker IO (ExtLedgerState blk) ->
  TickedLedgerState blk DiffMK ->
  IO ([Validated (GenTx blk)], [Validated (GenTx blk)])

-- DUPLICATE: runForge mirrors forging loop from ouroboros-consensus/src/Ouroboros/Consensus/NodeKernel.hs
-- For an extensive commentary of the forging loop, see there.

runForge ::
  forall blk.
  ( LedgerSupportsProtocol blk
  , HasLeiosVoting blk
  , ConvertRawHash blk
  , ResolveLeiosBlock blk
  ) =>
  EpochSize ->
  SlotNo ->
  ForgeLimit ->
  ChainDB IO blk ->
  [BlockForging IO blk] ->
  TopLevelConfig blk ->
  -- | The BLS key that this forger votes with, if it has one.
  Maybe LeiosSigningKey ->
  GenTxs blk ->
  LeiosDbConnection IO ->
  Tracer IO TraceLeiosKernel ->
  IO ForgeResult
runForge epochSize_ nextSlot opts chainDB blockForging cfg votingKey genTxs leiosDb leiosTracer = do
  putStrLn $ "--> epoch size: " ++ show epochSize_
  putStrLn $ "--> will process until: " ++ show opts
  leiosVoteState <- newLeiosVoteState committee
  reportCommittee
  endState <- go leiosVoteState initialForgeState{currentSlot = nextSlot}
  putStrLn $
    "--> forged and adopted "
      ++ show (forged endState)
      ++ " blocks; reached "
      ++ show (currentSlot endState)
  pure $ ForgeResult $ fromIntegral $ forged endState
 where
  epochSize = unEpochSize epochSize_

  -- The committee is rebuilt from the ledger state on every read,
  -- because it changes with the stake distribution snapshot at each
  -- epoch boundary.
  committee = getLeiosCommittee . ledgerState <$> getCurrentLedger chainDB

  -- A seat can exist without a key. If the pool registers no
  -- 'leiosKey', its seat is keyless. If its proof of possession does
  -- not verify, 'mkLeiosCommittee' also makes the seat keyless. If our
  -- key differs from the registered one, no seat matches ours. Each
  -- case ends with no vote counted.
  reportCommittee = do
    mCommittee <- atomically committee
    traceWith leiosTracer . MkTraceLeiosKernel $ case mCommittee of
      Nothing -> "committee: none; the era does not vote"
      Just c ->
        "committee: "
          ++ show (leiosCommitteeSize c)
          ++ " seats; our seat: "
          ++ case votingKey of
            Nothing -> "none, no voting key"
            Just sk -> case getLeiosSeatId (deriveVerKeyDSIGN sk) c of
              Nothing -> "none, our key holds no seat"
              Just seat -> show seat

  -- Vote for the endorser block that this block announces. The vote signs the
  -- announcing block's hash, not the endorser block's.
  --
  -- 'runLeiosVoting' applies four checks that this function does not. Three of
  -- them hold here already. 'forgeBlock' stores the closure before it returns,
  -- so the closure is on disk. The caller votes only after the ChainDB adopts
  -- the block, so the announcing block is the tip. The vote goes out in the
  -- announcing slot, so it is inside the vote window.
  --
  -- The fourth is the wait of 'lHdrWaitSlots' after the announcing slot, which
  -- gives an equivocating announcement time to arrive. One forger makes this
  -- chain, so no second announcement exists. A reader that compares this trace
  -- against a node log sees the vote that many slots early.
  voteFor ::
    LeiosVoteState IO ->
    blk ->
    [Validated (GenTx blk)] ->
    IO ()
  voteFor leiosVoteState newBlock ebTxs
    -- 'mkAndStoreEb' announces an endorser block exactly when it is given
    -- transactions. So an empty list means this block announced none, and there
    -- is nothing to vote for.
    | null ebTxs = pure ()
    | otherwise = do
        mCommittee <- atomically committee
        case (mCommittee, votingKey) of
          (Just c, Just sk)
            | Just seat <- getLeiosSeatId (deriveVerKeyDSIGN sk) c -> do
                let rbHash =
                      MkRbHash . fromShort . toShortRawHash (Proxy @blk) $
                        blockHash newBlock
                    vote = signLeiosVote sk seat rbHash
                addVote leiosVoteState vote >>= \case
                  Added weight mCert -> do
                    -- The same events 'runLeiosVoting' emits.
                    traceWith leiosTracer TraceLeiosVoted{vote, weight}
                    traceWith leiosTracer TraceLeiosVoteAcquired{vote}
                    case mCert of
                      Just _ -> traceWith leiosTracer TraceLeiosCertified{rbHash}
                      Nothing -> pure ()
                  -- 'reportCommittee' already showed the committee and the seat,
                  -- so any other result means the state changed under us. Stop,
                  -- rather than forge blocks that nobody certifies.
                  other -> fail $ "db-synthesizer: addVote returned " ++ show other
          -- No key or no seat. 'reportCommittee' said so at start-up, and the
          -- run emits no 'TraceLeiosVoted'.
          _ -> pure ()

  forgingDone :: ForgeState -> Bool
  forgingDone = case opts of
    ForgeLimitSlot s -> (s ==) . processed
    ForgeLimitBlock b -> (b ==) . forged
    ForgeLimitEpoch e -> (e ==) . currentEpoch

  go :: LeiosVoteState IO -> ForgeState -> IO ForgeState
  go leiosVoteState forgeState
    | forgingDone forgeState = pure forgeState
    | otherwise =
        go leiosVoteState . nextForgeState forgeState . isRight
          =<< runExceptT (goSlot leiosVoteState $ currentSlot forgeState)

  nextForgeState :: ForgeState -> Bool -> ForgeState
  nextForgeState ForgeState{currentSlot, forged, currentEpoch, processed} didForge =
    ForgeState
      { currentSlot = currentSlot + 1
      , forged = forged + if didForge then 1 else 0
      , currentEpoch = epoch'
      , processed = processed'
      }
   where
    processed' = processed + 1
    epoch' = currentEpoch + if unSlotNo processed' `rem` epochSize == 0 then 1 else 0

  -- just some shims; in this ported code, we use ExceptT instead of WithEarlyExit
  exitEarly' = throwE
  lift = liftIO

  goSlot :: LeiosVoteState IO -> SlotNo -> ExceptT String IO ()
  goSlot leiosVoteState currentSlot = do
    -- Figure out which block to connect to
    BlockContext{bcBlockNo, bcPrevPoint} <- do
      eBlkCtx <-
        lift $
          atomically $
            mkCurrentBlockContext currentSlot
              <$> ChainDB.getCurrentChain chainDB
      case eBlkCtx of
        Right blkCtx -> return blkCtx
        Left{} -> exitEarly' "no block context"

    -- Get corresponding ledger state, ledgder view and ticked 'ChainDepState'
    unticked <- do
      mExtLedger <- lift $ atomically $ ChainDB.getPastLedger chainDB bcPrevPoint
      case mExtLedger of
        Just l -> return l
        Nothing -> exitEarly' "no ledger state"

    ledgerView <-
      case runExcept $
        forecastFor
          ( ledgerViewForecastAt
              (configLedger cfg)
              (ledgerState unticked)
          )
          currentSlot of
        Left err -> exitEarly' $ "no ledger view: " ++ show err
        Right lv -> return lv

    let tickedChainDepState :: Ticked (ChainDepState (BlockProtocol blk))
        tickedChainDepState =
          tickChainDepState
            (configConsensus cfg)
            ledgerView
            currentSlot
            (headerStateChainDep (headerState unticked))

    -- Check if any forger is slot leader
    let
      checkShouldForge' f =
        checkShouldForge f nullTracer cfg currentSlot tickedChainDepState

    checks <- zip blockForging <$> liftIO (mapM checkShouldForge' blockForging)

    (blockForging', proof) <- case [(f, p) | (f, ShouldForge p) <- checks] of
      x : _ -> pure x
      _ -> exitEarly' "NoLeader"

    -- Tick the ledger state for the 'SlotNo' we're producing a block for
    let tickedLedgerState :: Ticked (LedgerState blk) DiffMK
        tickedLedgerState =
          applyChainTick
            OmitLedgerEvents
            (configLedger cfg)
            currentSlot
            (ledgerState unticked)

    -- Let the caller generate transactions
    let withReadOnlyForkerAtPoint' cdb tgt k =
          -- type legos just to reuse the same Forker combinator as
          -- the node's forging loop
          ExceptT . fmap (Right . fromJust) . withEarlyExit $
            withReadOnlyForkerAtPoint cdb tgt (Trans.lift . k)
    -- Decide before generating. A certifying block carries no transactions of
    -- its own, so the generator has to know.
    mCert <-
      lift $
        decideLeiosCertify
          leiosDb
          leiosVoteState
          leiosTracer
          currentSlot
          (headerState unticked)

    (rbTxs, ebTxs) <- withReadOnlyForkerAtPoint'
      chainDB
      (SpecificPoint bcPrevPoint)
      $ \case
        Left{} -> error "Impossible: we are forging on top of a block that the ChainDB cannot create forkers on!"
        Right frk ->
          genTxs
            currentSlot
            (isJust mCert)
            frk
            tickedLedgerState

    -- Actually produce the block
    newBlock <-
      lift $
        Block.forgeBlock
          blockForging'
          ForgeBlockArgs
            { fbConfig = cfg
            , fbCurrentBlockNo = bcBlockNo
            , fbCurrentSlotNo = currentSlot
            , fbCurrentTickedLedgerState = forgetLedgerTables tickedLedgerState
            , fbRbTxs = rbTxs
            , fbEbTxs = ebTxs
            , fbIsLeader = proof
            , fbChainDepState = Nothing
            , fbLeiosDb = leiosDb
            , fbLeiosTracer = leiosTracer
            , fbLeiosVoteState = leiosVoteState
            , fbMayLeiosCert = fst <$> mCert
            }

    -- Add the block to the chain DB (synchronously) and verify adoption
    let noPunish = InvalidBlockPunishment.noPunishment
    result <- lift $ ChainDB.addBlockAsync chainDB noPunish newBlock
    mbCurTip <- lift $ atomically $ ChainDB.blockProcessed result

    when (mbCurTip /= SuccesfullyAddedBlock (blockPoint newBlock)) $
      exitEarly' "block not adopted"

    lift $ voteFor leiosVoteState newBlock ebTxs

-- | Context required to forge a block
data BlockContext blk = BlockContext
  { bcBlockNo :: !BlockNo
  , bcPrevPoint :: !(Point blk)
  }

-- | Create the 'BlockContext' from the header of the previous block
blockContextFromPrevHeader ::
  HasHeader (Header blk) =>
  Header blk ->
  BlockContext blk
blockContextFromPrevHeader hdr =
  BlockContext (succ (blockNo hdr)) (headerPoint hdr)

-- | Determine the 'BlockContext' for a block about to be forged from the
-- current slot, ChainDB chain fragment, and ChainDB tip block number
mkCurrentBlockContext ::
  forall blk.
  ( GetHeader blk
  , BasicEnvelopeValidation blk
  ) =>
  SlotNo ->
  AnchoredFragment (Header blk) ->
  Either () (BlockContext blk)
mkCurrentBlockContext currentSlot c = case c of
  Empty AF.AnchorGenesis ->
    Right $ BlockContext (expectedFirstBlockNo (Proxy @blk)) GenesisPoint
  Empty (AF.Anchor anchorSlot anchorHash anchorBlockNo) ->
    let p :: Point blk = BlockPoint anchorSlot anchorHash
     in if anchorSlot < currentSlot
          then Right $ BlockContext (succ anchorBlockNo) p
          else Left ()
  c' :> hdr -> case blockSlot hdr `compare` currentSlot of
    LT -> Right $ blockContextFromPrevHeader hdr
    GT -> Left ()
    EQ ->
      Right $
        if isJust (headerIsEBB hdr)
          then blockContextFromPrevHeader hdr
          else BlockContext (blockNo hdr) $ castPoint $ AF.headPoint c'
