{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Tests of the node voting for endorser blocks, against the peers of
-- "Test.Consensus.Leios.Relay": the node votes with the BLS keys of its block
-- forging, and a downstream peer records the votes it relays.
--
-- One-second slots: the window to vote in the election of slot 1 opens 4 s in,
-- and closes 8 s in.
module Test.Consensus.Leios.Voting (tests) where

import Cardano.Crypto.DSIGN.Class (genKeyDSIGN)
import Cardano.Crypto.Hash (Hash (UnsafeHash))
import Cardano.Crypto.Leios (LeiosCert, LeiosSigningKey)
import Cardano.Crypto.Seed (mkSeedFromBytes)
import Control.Concurrent.Class.MonadSTM.Strict (atomically, modifyTVar, newTVarIO, readTVarIO)
import Control.Monad.Class.MonadAsync (forConcurrently, withAsync)
import Control.Monad.Class.MonadTime.SI (DiffTime, Time (..), getMonotonicTime)
import Control.Monad.Class.MonadTimer.SI (threadDelay)
import Control.Monad.IOSim (IOSim, runSimOrThrow)
import Control.Tracer (mkTracer)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
import Data.Foldable (for_)
import Ouroboros.Consensus.Block (blockSlot)
import Ouroboros.Consensus.Block.Forging
  ( BlockForging (..)
  , ForgeStateUpdateInfo (..)
  , MkBlockForging (..)
  )
import Ouroboros.Consensus.Leios.Types
  ( EndorserBlockHash
  , LeiosPoint (..)
  , TxHash
  , encodeEndorserBlockSize
  , mkEndorserBlock
  )
import Ouroboros.Consensus.Leios.VoteState (AddVoteResult (..), getCertificate)
import Ouroboros.Consensus.Leios.Voting (NotVotedReason (..), TraceLeiosVoting (..))
import Ouroboros.Consensus.Node.Tracers (Tracers' (..), nullTracers)
import Ouroboros.Consensus.NodeKernel (NodeKernel (..))
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import qualified Ouroboros.Consensus.Storage.ChainDB.API.Types.InvalidBlockPunishment as InvalidBlockPunishment
import Ouroboros.Consensus.Storage.LeiosDB.API
  ( LeiosDbWriter (..)
  , await
  , withWriter
  )
import Test.Consensus.Leios.NodeUnderTest
  ( Blk
  , NodeUnderTest (..)
  , PeerAddr (..)
  , defaultNodeUnderTestConfig
  , withNodeUnderTest
  )
import Test.Consensus.Leios.Relay
  ( Notification (..)
  , PeerOutcome (..)
  , PeerSpec (..)
  , announce
  , announceAt
  , equivocation
  , k
  , ledgerConfig
  , rbHashOf
  , runPeer
  , seatKeys
  , someAnnouncingBlock
  , voteOf
  , voteTimes
  , votes
  )
import Test.Consensus.Leios.TestBlock
  ( Header (..)
  , LeiosTestBlock (..)
  , invalidEbTx
  , setAnnouncement
  , successorLeiosBlock
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Util.ChainDB (emptyNodeDBs)

tests :: TestTree
tests =
  testGroup
    "Voting"
    [ testCase "the node votes for the endorser block its tip announces, once the window opens" $ do
        let Outcome{down} = runVoting voting
        votes down @?= [voteOf someAnnouncingBlock 0]
        voteTimes down @?= [Time 4]
    , testCase "the node validates a closure before the window opens" $ do
        let Outcome{down} = runVoting voting{validating = const 1.5}
        voteTimes down @?= [Time 4]
    , testCase "a slow validation delays no vote for another endorser block" $ do
        let Outcome{down, notVoted} =
              runVoting
                voting
                  { ups = [[announce someAnnouncingBlock, announceAt 2.2 nextAnnouncingBlock]]
                  , adopts = [(1.5, someAnnouncingBlock), (2.5, nextAnnouncingBlock)]
                  , closures = [(2, someAnnouncingBlock, validTxs), (3, nextAnnouncingBlock, validTxs)]
                  , validating = \ebHash -> if ebHash == ebHashOf someAnnouncingBlock then 10 else 0
                  }
        votes down @?= [voteOf nextAnnouncingBlock 0]
        -- The window of the election of slot 2 opens 5 s in.
        voteTimes down @?= [Time 5]
        notVoted @?= [TooLate]
    , testCase "the node votes with each of its keys that holds a seat" $ do
        let Outcome{down} = runVoting voting{keys = take 2 seatKeys}
        votes down @?= map (voteOf someAnnouncingBlock) [0, 1]
    , testCase "the node's votes count towards a certificate" $ do
        let Outcome{certified} = runVoting voting{keys = seatKeys}
        fmap (const ()) certified @?= Just ()
    , testCase "the node does not vote with a key that holds no seat" $ do
        let Outcome{down, notVoted} = runVoting voting{keys = [keyOfNoSeat]}
        votes down @?= []
        notVoted @?= [NotOnCommittee]
    , testCase "the node does not vote for an invalid endorser block" $ do
        let Outcome{down, notVoted} =
              runVoting voting{closures = [(2, someAnnouncingBlock, [invalidEbTx])]}
        votes down @?= []
        notVoted @?= [EbInvalid "invalid endorsed transaction"]
    , testCase "the node does not vote for an endorser block its tip does not announce" $ do
        let Outcome{down, notVoted} = runVoting voting{adopts = []}
        votes down @?= []
        notVoted @?= [TipDoesNotAnnounce]
    , testCase "the node does not vote when it saw an equivocation by the time the window opens" $ do
        let Outcome{down, notVoted} =
              runVoting voting{ups = [[announce someAnnouncingBlock, announceAt 2 equivocation]]}
        votes down @?= []
        notVoted @?= [Equivocated]
    , testCase "the node does not vote once the window closed" $ do
        let Outcome{down, notVoted} = runVoting voting{closures = [(8.5, someAnnouncingBlock, validTxs)]}
        votes down @?= []
        notVoted @?= [TooLate]
    , testCase "the node does not relay its vote for an announcement it did not relay" $ do
        let Outcome{down, notVoted} = runVoting voting{ups = []}
        votes down @?= []
        notVoted @?= [VoteNotAdded NotAnnounced]
    ]

-- | What happens around the node: the announcements its upstream peers send
-- it, when it adopts which blocks and acquires which closures, and how long it
-- takes to validate a closure.
data Voting = Voting
  { keys :: [LeiosSigningKey]
  , ups :: [[(DiffTime, Notification)]]
  , adopts :: [(DiffTime, LeiosTestBlock)]
  , closures :: [(DiffTime, LeiosTestBlock, [BS.ByteString])]
  , validating :: EndorserBlockHash -> DiffTime
  -- ^ Beyond what validating takes in the simulation.
  }

-- | The node holds seat 0, an upstream peer announces 'someAnnouncingBlock',
-- and the node adopts it, then acquires its closure.
voting :: Voting
voting =
  Voting
    { keys = take 1 seatKeys
    , ups = [[announce someAnnouncingBlock]]
    , adopts = [(1.5, someAnnouncingBlock)]
    , closures = [(2, someAnnouncingBlock, validTxs)]
    , validating = const 0
    }

data Outcome = Outcome
  { down :: PeerOutcome
  -- ^ What a downstream peer received.
  , notVoted :: [NotVotedReason]
  , certified :: Maybe LeiosCert
  -- ^ The certificate of 'someAnnouncingBlock', if any.
  }

-- | Run the node with the given keys for 20 s.
runVoting :: Voting -> Outcome
runVoting Voting{keys, ups, adopts, closures, validating} = runSimOrThrow $ do
  traced <- newTVarIO []
  nodeDBs <- emptyNodeDBs
  let tracers =
        nullTracers
          { leiosVotingTracer = mkTracer $ \ev -> do
              case ev of
                TraceLeiosEbValidated ebHash -> threadDelay (validating ebHash)
                _ -> pure ()
              atomically $ modifyTVar traced (ev :)
          }
  withNodeUnderTest (defaultNodeUnderTestConfig k ledgerConfig) tracers nodeDBs $ \nut -> do
    setBlockForging (nutKernel nut) [MkBlockForging (pure (votingOnly keys))]
    withAsync (adopt nut) $ \_ -> withAsync (acquire nut) $ \_ -> do
      outcomes <-
        forConcurrently (zip [1 ..] (map (PeerSpec 0) ups <> [PeerSpec 0 []])) $ \(i, spec) ->
          runPeer nut 20 (PeerAddr i) spec
      certified <-
        atomically $
          getCertificate (getLeiosVoteState (nutKernel nut)) (rbHashOf (ltbHeader someAnnouncingBlock))
      evs <- readTVarIO traced
      pure
        Outcome
          { down = last outcomes
          , notVoted = reverse [reason | TraceLeiosNotVoted _ reason <- evs]
          , certified
          }
 where
  adopt nut = for_ adopts $ \(at, blk) -> do
    waitUntil at
    ChainDB.addBlock_ (nutChainDB nut) InvalidBlockPunishment.noPunishment blk

  acquire nut = for_ closures $ \(at, blk, txs) -> do
    waitUntil at
    let eb = mkEndorserBlock [(txHashOf i, fromIntegral (BS.length tx)) | (i, tx) <- zip [0 ..] txs]
        point = MkLeiosPoint (blockSlot blk) (ebHashOf blk)
    withWriter (getLeiosDB (nutKernel nut)) $ \writer -> do
      await =<< writeEbPoint writer point (encodeEndorserBlockSize eb)
      _ <- await =<< writeEbBody writer point eb
      _ <- await =<< writeTxs writer point (zip [0 ..] txs)
      pure ()

waitUntil :: DiffTime -> IOSim s ()
waitUntil at = do
  Time now <- getMonotonicTime
  threadDelay (max 0 (at - now))

-- | A node that never leads, but votes with these keys.
votingOnly :: [LeiosSigningKey] -> BlockForging (IOSim s) Blk
votingOnly keys =
  BlockForging
    { forgeLabel = "voting"
    , canBeLeader = keys
    , updateForgeState = \_ _ _ -> pure (ForgeStateUpdated ())
    , checkCanForge = \_ _ _ _ _ -> Right ()
    , forgeBlock = \_ -> error "votingOnly: the node never leads"
    , finalize = pure ()
    }

-- | Announces an endorser block in slot 2, extending 'someAnnouncingBlock'.
nextAnnouncingBlock :: LeiosTestBlock
nextAnnouncingBlock = setAnnouncement (UnsafeHash (SBS.replicate 32 5)) 1000 (successorLeiosBlock someAnnouncingBlock)

ebHashOf :: LeiosTestBlock -> EndorserBlockHash
ebHashOf blk = case lthAnnouncement (ltbHeader blk) of
  Just (ebHash, _size) -> ebHash
  Nothing -> error "ebHashOf: the block announces nothing"

txHashOf :: Int -> TxHash
txHashOf i = UnsafeHash (SBS.toShort (BS.replicate 31 0 <> BS.singleton (fromIntegral i)))

validTxs :: [BS.ByteString]
validTxs = ["a valid transaction"]

-- | A key of no seat in the committee.
keyOfNoSeat :: LeiosSigningKey
keyOfNoSeat = genKeyDSIGN (mkSeedFromBytes (BS.replicate 32 42))
