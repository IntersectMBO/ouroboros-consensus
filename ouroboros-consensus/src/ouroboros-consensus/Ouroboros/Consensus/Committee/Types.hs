{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Types common to any generic committee selection scheme
module Ouroboros.Consensus.Committee.Types
  ( PoolId (..)
  , LedgerStake (..)
  , VoteWeight (..)
  , TargetCommitteeSize (..)
  , Cumulative (..)

    -- * Committee selection interface
  , CryptoSupportsCommitteeSelection (..)
  , VotingWithCommitteeSelection (..)
  , VotingWithAggregation (..)
  , VotesWithSameTarget -- Hides inner fields since this is a smart constructor
  , getElectionIdFromVotes
  , getVoteMessageFromVotes
  , getRawVotes
  , ensureSameTarget
  , TargetMismatch (..)
  ) where

import Cardano.Ledger.BaseTypes.NonZero (HasZero (..))
import Cardano.Ledger.Core (KeyHash, KeyRole (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Word (Word64)
import Ouroboros.Consensus.Committee.Crypto
  ( CryptoSupportsVoteSigning
  , ElectionId
  , PrivateKeys
  , VoteMessage
  )

-- | Identifier of a given voter in the committee selection scheme
newtype PoolId = PoolId
  { unPoolId :: KeyHash StakePool
  }
  deriving (Show, Eq, Ord)

-- | Stake of a voter as reflected by the ledger state
newtype LedgerStake = LedgerStake
  { unLedgerStake :: Rational
  }
  deriving (Show, Eq)

instance HasZero LedgerStake where
  isZero (LedgerStake x) = isZero x

-- | Voting power of a voter in the committee selection scheme
newtype VoteWeight = VoteWeight
  { unVoteWeight :: Rational
  }
  deriving (Show, Eq)

-- | Target committee size
newtype TargetCommitteeSize = TargetCommitteeSize
  { unTargetCommitteeSize :: Word64
  }
  deriving (Show, Eq)

-- | Wrapper to tag accumulated resources
newtype Cumulative a = Cumulative
  { unCumulative :: a
  }
  deriving (Show, Eq)

-- * Committee selection interface

-- | Interface for committee selection schemes.
--
-- This class is parametrized by the crypto primitives and the committee
-- selection data structure. Instances define how to check whether a party
-- should vote and how to compute the voting weight of a committee member.
class
  (CryptoSupportsVoteSigning crypto, CryptoOf csContext ~ crypto) =>
  CryptoSupportsCommitteeSelection crypto csContext
    | csContext -> crypto
  where
  type CryptoOf csContext

  type CommitteeSelectionError csContext
  type CommitteeMember csContext

  -- | Check whether we should vote in a given election.
  checkShouldVote ::
    csContext ->
    PoolId ->
    PrivateKeys crypto ->
    ElectionId crypto ->
    Either (CommitteeSelectionError csContext) (Maybe (CommitteeMember csContext))

  committeeMemberWeight ::
    CommitteeMember csContext ->
    VoteWeight

-- | Interface for votes that can be validated under a committee selection scheme.
class
  ( CryptoSupportsCommitteeSelection crypto csContext
  , Eq (Vote csContext)
  , Show (Vote csContext)
  ) =>
  VotingWithCommitteeSelection crypto csContext
    | csContext -> crypto
  where
  type Vote csContext

  forgeVote ::
    CommitteeMember csContext ->
    PrivateKeys crypto ->
    ElectionId crypto ->
    VoteMessage crypto ->
    Vote csContext

  -- | Check the validity of a vote in a given election.
  verifyVote ::
    csContext ->
    Vote csContext ->
    Either (CommitteeSelectionError csContext) (CommitteeMember csContext)

  getElectionIdFromVote :: Vote csContext -> ElectionId crypto
  getVoteMessageFromVote :: Vote csContext -> VoteMessage crypto

data VotesWithSameTarget csContext = VotesWithSameTarget
  { vwstElectionId :: ElectionId (CryptoOf csContext)
  , vwstVoteMessage :: VoteMessage (CryptoOf csContext)
  , vwstVotes :: NonEmpty (Vote csContext)
  }

getElectionIdFromVotes :: VotesWithSameTarget csContext -> ElectionId (CryptoOf csContext)
getElectionIdFromVotes = vwstElectionId

getVoteMessageFromVotes :: VotesWithSameTarget csContext -> VoteMessage (CryptoOf csContext)
getVoteMessageFromVotes = vwstVoteMessage

getRawVotes :: VotesWithSameTarget csContext -> NonEmpty (Vote csContext)
getRawVotes = vwstVotes

deriving instance
  ( Eq (ElectionId (CryptoOf csContext))
  , Eq (VoteMessage (CryptoOf csContext))
  , Eq (Vote csContext)
  ) =>
  Eq (VotesWithSameTarget csContext)

deriving instance
  ( Show (ElectionId (CryptoOf csContext))
  , Show (VoteMessage (CryptoOf csContext))
  , Show (Vote csContext)
  ) =>
  Show (VotesWithSameTarget csContext)

-- | Mismatch between the target (election ID or vote message) of votes.
data TargetMismatch crypto
  = -- | Two votes have different election IDs
    ElectionIdMismatch (ElectionId crypto) (ElectionId crypto)
  | -- | Two votes have different vote messages
    VoteMessageMismatch (VoteMessage crypto) (VoteMessage crypto)

deriving instance
  (Eq (ElectionId crypto), Eq (VoteMessage crypto)) => Eq (TargetMismatch crypto)

deriving instance
  (Show (ElectionId crypto), Show (VoteMessage crypto)) => Show (TargetMismatch crypto)

ensureSameTarget ::
  forall crypto csContext.
  VotingWithCommitteeSelection crypto csContext =>
  NonEmpty (Vote csContext) ->
  Either (TargetMismatch crypto) (VotesWithSameTarget csContext)
ensureSameTarget votes@(v :| vs) =
  let eId0 = getElectionIdFromVote @crypto @csContext v
      msg0 = getVoteMessageFromVote @crypto @csContext v
   in go (1 :: Int) eId0 msg0 vs
 where
  go _ eId msg [] = Right (VotesWithSameTarget eId msg votes)
  go n eId1 msg1 (y : ys) =
    let eId2 = getElectionIdFromVote @crypto @csContext y
        msg2 = getVoteMessageFromVote @crypto @csContext y
     in if eId1 /= eId2
          then Left $ ElectionIdMismatch eId1 eId2
          else
            if msg1 /= msg2
              then Left $ VoteMessageMismatch msg1 msg2
              else go (n + 1) eId1 msg1 ys

-- | Interface for aggregating votes into certificates.
class
  ( VotingWithCommitteeSelection crypto csContext
  , Eq (Cert csContext)
  , Show (Cert csContext)
  ) =>
  VotingWithAggregation crypto csContext
    | csContext -> crypto
  where
  type Cert csContext

  getElectionIdFromCert :: Cert csContext -> ElectionId crypto
  getVoteMessageFromCert :: Cert csContext -> VoteMessage crypto

  forgeCert ::
    VotesWithSameTarget csContext ->
    Cert csContext

  verifyCert ::
    csContext ->
    Cert csContext ->
    Either (CommitteeSelectionError csContext) (NonEmpty (CommitteeMember csContext))
