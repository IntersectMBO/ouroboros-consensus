{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Helpers to derive @BlockSupportsPeras@ for block types without Peras support.
module Ouroboros.Consensus.Peras.Void
  ( VoidPerasVote (..)
  , VoidPerasCert (..)
  , VoidPerasError (..)
  , VoidPerasCrypto
  , VoidPerasVotingCommitteeScheme
  , absurdVoidPerasVotingCommitteeError
  , absurdVoidPerasCert
  ) where

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import Control.Exception.Base (Exception)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Typeable (Typeable)
import Data.Void (Void, absurd)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block.Abstract (Point)
import Ouroboros.Consensus.Committee.Class
  ( CryptoSupportsVotingCommittee (..)
  , VotingCommittee
  , getRawVotes
  )
import Ouroboros.Consensus.Committee.Crypto
  ( CryptoSupportsVoteSigning (..)
  , ElectionId
  , PrivateKey
  , PublicKey
  , VoteCandidate
  )
import Ouroboros.Consensus.Peras.Cert.Class
import Ouroboros.Consensus.Peras.Types (BoostedBlock, PerasRoundNo)
import Ouroboros.Consensus.Peras.Vote.Class
import Ouroboros.Consensus.Peras.Voting.Adapter
  ( PerasCertCompatibleWithVotingCommittee (..)
  , PerasVoteCompatibleWithVotingCommittee (..)
  )
import Ouroboros.Consensus.Util (ShowProxy)

-- | Imposible Peras vote for @blk@.
--
-- NOTE: the phantom @blk@ is used to keep the 'PerasVote' type family injective.
newtype VoidPerasVote blk
  = VoidPerasVote
  { unVoidPerasVote :: Void
  }
  deriving newtype (Show, Eq, NoThunks, ShowProxy)

-- | Imposible Peras certificate for @blk@.
--
-- NOTE: the phantom @blk@ is used to keep the 'PerasCert' type family injective.
newtype VoidPerasCert blk
  = VoidPerasCert
  { unVoidPerasCert :: Void
  }
  deriving newtype (Show, Eq, NoThunks, ShowProxy)

type instance BoostedBlock (VoidPerasVote blk) = Point blk
type instance BoostedBlock (VoidPerasCert blk) = Point blk

instance IsPerasVote (VoidPerasVote blk) blk where
  getPerasVoteRound = absurd . unVoidPerasVote
  getPerasVoteBlock = absurd . unVoidPerasVote
  getPerasVoteSeatIndex = absurd . unVoidPerasVote

instance IsPerasCert (VoidPerasCert blk) blk where
  getPerasCertRound = absurd . unVoidPerasCert
  getPerasCertBlock = absurd . unVoidPerasCert

-- | Void Peras error for @blk@.
--
-- NOTE: the phantom @blk@ is used to keep the 'PerasError' type family injective.
newtype VoidPerasError blk
  = VoidPerasError
  { unVoidPerasError :: Void
  }
  deriving newtype (Show, Eq, NoThunks, Generic, ShowProxy, Exception)

-- | Void Peras committee for @blk@.
data VoidPerasVotingCommitteeScheme
  deriving (Show, Eq, Generic, NoThunks)

data VoidPerasCrypto blk
  deriving (Show, Eq, Generic, NoThunks)

type instance ElectionId (VoidPerasCrypto blk) = PerasRoundNo
type instance VoteCandidate (VoidPerasCrypto blk) = Point blk

type instance PrivateKey (VoidPerasCrypto blk) = ()
type instance PublicKey (VoidPerasCrypto blk) = Void

instance CryptoSupportsVoteSigning (VoidPerasCrypto blk) where
  type VoteSigningKey (VoidPerasCrypto blk) = ()
  type VoteVerificationKey (VoidPerasCrypto blk) = Void
  data VoteSignature (VoidPerasCrypto blk) = VoidVoteSignature ()
  getVoteSigningKey _proxy _privateKey = ()
  getVoteVerificationKey _proxy publicKey = absurd publicKey
  signVote _signingKey _ _ = VoidVoteSignature ()
  verifyVoteSignature verificationKey _ _ _ = absurd verificationKey

newtype instance VotingCommittee (VoidPerasCrypto blk) VoidPerasVotingCommitteeScheme
  = VoidPerasVotingCommittee Void

instance CryptoSupportsVotingCommittee (VoidPerasCrypto blk) VoidPerasVotingCommitteeScheme where
  newtype VotingCommitteeError (VoidPerasCrypto blk) VoidPerasVotingCommitteeScheme
    = VoidPerasVotingCommitteeError Void
  newtype VotingCommitteeInput (VoidPerasCrypto blk) VoidPerasVotingCommitteeScheme
    = VoidPerasVotingCommitteeInput Void
  newtype EligibilityWitness (VoidPerasCrypto blk) VoidPerasVotingCommitteeScheme
    = VoidPerasEligibilityWitness Void
  newtype Cert (VoidPerasCrypto blk) VoidPerasVotingCommitteeScheme
    = VoidPerasCert' {unCommitteeCert :: VoidPerasCert blk}
  newtype Vote (VoidPerasCrypto blk) VoidPerasVotingCommitteeScheme
    = VoidPerasVote' {unCommitteeVote :: VoidPerasVote blk}

  mkVotingCommittee (VoidPerasVotingCommitteeInput void) = absurd void
  checkShouldVote (VoidPerasVotingCommittee void) _ _ _ = absurd void
  forgeVote (VoidPerasEligibilityWitness void) _ _ _ = absurd void
  verifyVote (VoidPerasVotingCommittee void) _ = absurd void
  eligiblePartyVoteWeight (VoidPerasVotingCommittee void) _ = absurd void
  forgeCert = absurd . telescope
   where
    telescope = unVoidPerasVote . unCommitteeVote . NonEmpty.head . getRawVotes
  verifyCert (VoidPerasVotingCommittee void) _ = absurd void
  voteTarget (VoidPerasVote' (VoidPerasVote void)) = absurd void
  compareVotesById (VoidPerasVote' (VoidPerasVote void)) _ = absurd void

-- | Exists solely to silence a 'defined-but-not-used' warning on the
-- 'VoidPerasVotingCommitteeError' constructor.
absurdVoidPerasVotingCommitteeError ::
  VotingCommitteeError (VoidPerasCrypto blk) VoidPerasVotingCommitteeScheme ->
  a
absurdVoidPerasVotingCommitteeError (VoidPerasVotingCommitteeError void) =
  absurd void

-- | Exists solely to silence a 'defined-but-not-used' warning on the
-- 'VoidPerasCert' constructor.
absurdVoidPerasCert ::
  Cert (VoidPerasCrypto blk) VoidPerasVotingCommitteeScheme ->
  a
absurdVoidPerasCert (VoidPerasCert' (VoidPerasCert void)) =
  absurd void

deriving newtype instance
  Show (VotingCommittee (VoidPerasCrypto blk) VoidPerasVotingCommitteeScheme)
deriving newtype instance
  Eq (VotingCommittee (VoidPerasCrypto blk) VoidPerasVotingCommitteeScheme)
deriving newtype instance
  NoThunks (VotingCommittee (VoidPerasCrypto blk) VoidPerasVotingCommitteeScheme)
deriving newtype instance
  Generic (VotingCommittee (VoidPerasCrypto blk) VoidPerasVotingCommitteeScheme)

deriving newtype instance
  Typeable blk =>
  FromCBOR (VotingCommittee (VoidPerasCrypto blk) VoidPerasVotingCommitteeScheme)
deriving newtype instance
  Typeable blk =>
  ToCBOR (VotingCommittee (VoidPerasCrypto blk) VoidPerasVotingCommitteeScheme)

instance
  PerasVoteCompatibleWithVotingCommittee
    (VoidPerasVote blk)
    (VoidPerasCrypto blk)
    VoidPerasVotingCommitteeScheme
  where
  toPerasVote = absurd . unVoidPerasVote . unCommitteeVote
  fromPerasVote = absurd . unVoidPerasVote

instance
  PerasCertCompatibleWithVotingCommittee
    (VoidPerasCert blk)
    (VoidPerasCrypto blk)
    VoidPerasVotingCommitteeScheme
  where
  toPerasCert = absurd . unVoidPerasCert . unCommitteeCert
  fromPerasCert = absurd . unVoidPerasCert
