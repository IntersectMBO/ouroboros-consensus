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
import Control.Tracer as Trace (nullTracer)
import Data.ByteString.Short (fromShort)
import Data.Either (isRight)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Maybe (fromJust, isJust)
import Data.Proxy
import Data.Word (Word64)
import LeiosDemoDb (LeiosDbConnection)
import LeiosDemoTypes
  ( RbHash (MkRbHash)
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
  , topLevelConfigVotingKey
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

-- | What the forger's own votes achieved over a run.
data VoteTally = VoteTally
  { cast :: !Word64
  -- ^ Votes the forger cast, one for every endorser block it announced.
  , certified :: !Word64
  -- ^ Votes that took the tally for their announcing block to the
  -- certification threshold. A certificate exists for each one.
  }

-- | An action to generate transactions for a given block.
--
-- The first list fills the ranking block. The second fills the endorser block
-- that the ranking block announces. An empty second list announces no endorser
-- block, because 'mkAndStoreEb' forges none for an empty list.
type GenTxs blk =
  SlotNo ->
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
  ) =>
  EpochSize ->
  SlotNo ->
  ForgeLimit ->
  ChainDB IO blk ->
  [BlockForging IO blk] ->
  TopLevelConfig blk ->
  GenTxs blk ->
  LeiosDbConnection IO ->
  IO ForgeResult
runForge epochSize_ nextSlot opts chainDB blockForging cfg genTxs leiosDb = do
  putStrLn $ "--> epoch size: " ++ show epochSize_
  putStrLn $ "--> will process until: " ++ show opts
  leiosVoteState <- newLeiosVoteState committee
  reportCommittee
  -- 'goSlot' reports only success or failure to 'go', so the tally lives beside
  -- 'ForgeState' rather than in it.
  tally <- newIORef VoteTally{cast = 0, certified = 0}
  endState <- go leiosVoteState tally initialForgeState{currentSlot = nextSlot}
  putStrLn $
    "--> forged and adopted "
      ++ show (forged endState)
      ++ " blocks; reached "
      ++ show (currentSlot endState)
  VoteTally{cast, certified} <- readIORef tally
  putStrLn $
    "--> votes: " ++ show cast ++ " cast, " ++ show certified ++ " certified"
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
    -- TODO: shouldn't we be using other mechanism to report other than putStrLn?
    putStrLn $ case mCommittee of
      Nothing -> "--> committee: none; the era does not vote"
      Just c ->
        "--> committee: "
          ++ show (leiosCommitteeSize c)
          ++ " seats; our seat: "
          ++ case topLevelConfigVotingKey cfg of
            Nothing -> "none, no voting key"
            Just sk -> case getLeiosSeatId (deriveVerKeyDSIGN sk) c of
              Nothing -> "none, our key holds no seat"
              Just seat -> show seat

  -- Vote for the endorser block that this block announces. The vote signs the
  -- announcing block's hash, not the endorser block's.
  voteFor ::
    LeiosVoteState IO ->
    IORef VoteTally ->
    blk ->
    [Validated (GenTx blk)] ->
    IO ()
  voteFor leiosVoteState tally newBlock ebTxs
    -- 'mkAndStoreEb' announces an endorser block exactly when it is given
    -- transactions. So an empty list means this block announced none, and there
    -- is nothing to vote for.
    | null ebTxs = pure ()
    | otherwise = do
        mCommittee <- atomically committee
        case (mCommittee, topLevelConfigVotingKey cfg) of
          (Just c, Just sk)
            | Just seat <- getLeiosSeatId (deriveVerKeyDSIGN sk) c -> do
                let rbHash =
                      MkRbHash . fromShort . toShortRawHash (Proxy @blk) $
                        blockHash newBlock
                addVote leiosVoteState (signLeiosVote sk seat rbHash) >>= \case
                  Added _weight mCert ->
                    modifyIORef' tally $ \t ->
                      VoteTally
                        { cast = cast t + 1
                        , certified = certified t + if isJust mCert then 1 else 0
                        }
                  -- 'reportCommittee' already showed the committee and the seat,
                  -- so any other result means the state changed under us. Stop,
                  -- rather than forge blocks that nobody certifies.
                  other -> fail $ "db-synthesizer: addVote returned " ++ show other
          -- No key or no seat. 'reportCommittee' said so at start-up, and the
          -- run ends with "0 cast".
          _ -> pure ()

  forgingDone :: ForgeState -> Bool
  forgingDone = case opts of
    ForgeLimitSlot s -> (s ==) . processed
    ForgeLimitBlock b -> (b ==) . forged
    ForgeLimitEpoch e -> (e ==) . currentEpoch

  go :: LeiosVoteState IO -> IORef VoteTally -> ForgeState -> IO ForgeState
  go leiosVoteState tally forgeState
    | forgingDone forgeState = pure forgeState
    | otherwise =
        go leiosVoteState tally . nextForgeState forgeState . isRight
          =<< runExceptT (goSlot leiosVoteState tally $ currentSlot forgeState)

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

  goSlot :: LeiosVoteState IO -> IORef VoteTally -> SlotNo -> ExceptT String IO ()
  goSlot leiosVoteState tally currentSlot = do
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
    (rbTxs, ebTxs) <- withReadOnlyForkerAtPoint'
      chainDB
      (SpecificPoint bcPrevPoint)
      $ \case
        Left{} -> error "Impossible: we are forging on top of a block that the ChainDB cannot create forkers on!"
        Right frk ->
          genTxs
            currentSlot
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
            , fbLeiosTracer = Trace.nullTracer
            , fbLeiosVoteState = leiosVoteState
            , fbMayLeiosCert = Nothing
            }

    -- Add the block to the chain DB (synchronously) and verify adoption
    let noPunish = InvalidBlockPunishment.noPunishment
    result <- lift $ ChainDB.addBlockAsync chainDB noPunish newBlock
    mbCurTip <- lift $ atomically $ ChainDB.blockProcessed result

    when (mbCurTip /= SuccesfullyAddedBlock (blockPoint newBlock)) $
      exitEarly' "block not adopted"

    lift $ voteFor leiosVoteState tally newBlock ebTxs

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
