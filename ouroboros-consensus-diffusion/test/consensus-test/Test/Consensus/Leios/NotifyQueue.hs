{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | The LeiosNotify server's vote placement, 'enqueueVote', run over random
-- histories of one peer's outgoing queue and checked against what it must
-- keep: the votes' order, the cap, the credit accounting, exactly when a
-- vote is dropped, and that a vote never waits for a later message.
module Test.Consensus.Leios.NotifyQueue (tests) where

import Data.Foldable (toList)
import Data.Maybe (isJust)
import qualified Data.Sequence as Seq
import LeiosDemoOnlyTestNotify (LeiosNotify, Message (..), leiosNotifyMaxVotesPerMessage)
import qualified LeiosDemoOnlyTestNotify as Notify
import LeiosDemoTypes
  ( LeiosFetchStaticEnv (..)
  , demoLeiosFetchStaticEnv
  , maxEncodedLeiosVoteSize
  )
import Ouroboros.Consensus.Network.NodeToNode
  ( enqueueVote
  , leiosNotifyPipelineDepth
  , leiosNotifyVoteBatch
  )
import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup
    "LeiosNotify queue"
    [ testProperty
        "a vote joins the tail message or starts one for a credit, in order, under the cap, else is dropped"
        prop_enqueueVote
    , testProperty
        "the vote cap fits the receiver's ingress queue and sits under the decoder's bound"
        prop_voteBatchBudget
    ]

-- | Votes are numbered in arrival order; points and announcements are not
-- looked at.
type Msg = Message (LeiosNotify () () Int) Notify.StBusy Notify.StIdle

type Queue = Seq.Seq Msg

-- | One step of a peer's history: a vote to place, the one thing under
-- test; another message the server enqueues for a credit; the sender
-- taking the head; the client granting a credit.
data Op = ArriveVote | ArriveOther | Send | Grant
  deriving (Show, Eq)

data History = History
  { hCap :: Int
  , hCredits :: Int
  , hOps :: [Op]
  }
  deriving Show

instance Arbitrary History where
  arbitrary = do
    cap <- frequency [(3, choose (1, 4)), (2, choose (5, 20)), (1, choose (21, 200))]
    credits <- choose (0, 6)
    ops <-
      listOf $
        frequency
          [ (6, pure ArriveVote)
          , (1, pure ArriveOther)
          , (2, pure Send)
          , (2, pure Grant)
          ]
    pure (History cap credits ops)
  shrink (History cap credits ops) =
    [History cap' credits ops | cap' <- shrink cap, cap' >= 1]
      ++ [History cap credits' ops | credits' <- shrink credits]
      ++ [History cap credits ops' | ops' <- shrinkList (const []) ops]

-- | The votes a message carries, if it is a votes message.
votesOf :: Msg -> Maybe [Int]
votesOf = \case
  MsgLeiosVotes vs -> Just vs
  _ -> Nothing

-- | Whether the tail of the queue is a votes message with room under the cap.
roomAtTail :: Int -> Queue -> Bool
roomAtTail cap q = case Seq.viewr q of
  _ Seq.:> MsgLeiosVotes vs -> length vs < cap
  _ -> False

data St = St
  { stCredits :: Int
  , stQueue :: Queue
  , stSent :: [Msg]
  -- ^ newest first
  , stAccepted :: [Int]
  -- ^ newest first
  , stNext :: Int
  , stGrants :: Int
  , stOthers :: Int
  , stJoined :: Int
  -- ^ votes that joined an existing message
  , stDropped :: Int
  , stAtCap :: Bool
  -- ^ some votes message reached the cap
  }

prop_enqueueVote :: History -> Property
prop_enqueueVote History{hCap = cap, hCredits = credits0, hOps} =
  case foldl step (Right st0) hOps of
    Left err -> counterexample err False
    Right st ->
      let msgs = reverse (stSent st) ++ toList (stQueue st)
          voteMsgs = [vs | Just vs <- map votesOf msgs]
       in checkCoverage
            . cover 30 (stJoined st > 0) "a vote joined an existing message"
            . cover 20 (stDropped st > 0) "a vote was dropped"
            . cover 10 (stAtCap st && cap > 1) "a message reached the cap"
            $ conjoin
              [ counterexample "votes out of order or lost" $
                  concat voteMsgs === reverse (stAccepted st)
              , counterexample "a votes message is empty or over the cap" $
                  all (\vs -> not (null vs) && length vs <= cap) voteMsgs
              , counterexample "credits do not account for the messages started" $
                  stCredits st === credits0 + stGrants st - stOthers st - length voteMsgs
              ]
 where
  st0 = St credits0 Seq.empty [] [] 1 0 0 0 0 False

  step :: Either String St -> Op -> Either String St
  step (Left err) = const (Left err)
  step (Right st@St{stCredits, stQueue, stSent, stAccepted, stNext}) = \case
    ArriveVote ->
      let v = stNext
          room = roomAtTail cap stQueue
          placed = enqueueVote cap v stCredits stQueue
          before q = fmap votesOf (Seq.take (Seq.length q - 1) q)
       in if isJust placed /= (room || stCredits > 0)
            then Left ("vote " ++ show v ++ ": dropped iff no room and no credit violated")
            else case placed of
              Nothing -> Right st{stNext = v + 1, stDropped = stDropped st + 1}
              Just (credits', q') ->
                let lastOk = case Seq.viewr q' of
                      _ Seq.:> MsgLeiosVotes vs -> not (null vs) && last vs == v
                      _ -> False
                    restOk =
                      before q' == (if room then before stQueue else fmap votesOf stQueue)
                    creditsOk = credits' == (if room then stCredits else stCredits - 1)
                    atCap = case Seq.viewr q' of
                      _ Seq.:> MsgLeiosVotes vs -> length vs == cap
                      _ -> False
                 in if not lastOk
                      then Left ("vote " ++ show v ++ " is not the last vote of the last message")
                      else
                        if not restOk
                          then Left ("vote " ++ show v ++ " changed a message other than the tail")
                          else
                            if not creditsOk
                              then Left ("vote " ++ show v ++ ": credits " ++ show credits')
                              else
                                Right
                                  st
                                    { stCredits = credits'
                                    , stQueue = q'
                                    , stAccepted = v : stAccepted
                                    , stNext = v + 1
                                    , stJoined = stJoined st + (if room then 1 else 0)
                                    , stAtCap = stAtCap st || atCap
                                    }
    ArriveOther
      | stCredits > 0 ->
          Right
            st
              { stCredits = stCredits - 1
              , stQueue = stQueue Seq.|> MsgLeiosBlockTxsOffer ()
              , stOthers = stOthers st + 1
              }
      | otherwise -> Right st
    Send -> case Seq.viewl stQueue of
      Seq.EmptyL -> Right st
      msg Seq.:< rest -> Right st{stQueue = rest, stSent = msg : stSent}
    Grant -> Right st{stCredits = stCredits + 1, stGrants = stGrants st + 1}

-- | The cap, the pipeline depth and the vote size together stay within the
-- receiver's ingress queue, and the decoder's bound stays four times above
-- the cap.
prop_voteBatchBudget :: Property
prop_voteBatchBudget =
  once $
    counterexample ("cap " ++ show leiosNotifyVoteBatch) $
      conjoin
        [ counterexample "cap below one" $ leiosNotifyVoteBatch >= 1
        , counterexample "depth times a full message exceeds the ingress queue" $
            leiosNotifyPipelineDepth * (16 + leiosNotifyVoteBatch * maxEncodedLeiosVoteSize)
              <= fromIntegral (maxLeiosNotifyIngressQueue demoLeiosFetchStaticEnv)
        , counterexample "decoder bound under four times the cap" $
            4 * leiosNotifyVoteBatch <= leiosNotifyMaxVotesPerMessage
        ]
