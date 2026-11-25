-- | A Leios committee of four equal seats that needs half the weight, and the
-- votes and certificates its seats make.
module Test.Consensus.Leios.Committee
  ( certOf
  , testCommittee
  , testQuorumStakeThreshold
  , voteOf
  ) where

import Cardano.Crypto.DSIGN.Class
  ( createPossessionProofDSIGN
  , deriveVerKeyDSIGN
  , genKeyDSIGN
  , signDSIGN
  )
import Cardano.Crypto.Leios
  ( LeiosCert
  , LeiosCommittee
  , LeiosSeatId (..)
  , LeiosSigningKey
  , aggregateLeiosCert
  , leiosSignContext
  , mkLeiosCommittee
  )
import Cardano.Crypto.Seed (mkSeedFromBytes)
import Cardano.Ledger.BaseTypes (UnitInterval, boundRational)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Maybe.Strict (StrictMaybe (..))
import qualified Data.Vector.Strict as V
import Data.Word (Word16)
import Ouroboros.Consensus.Leios.Types (LeiosVote (..), RankingBlockHash)

signingKeys :: [LeiosSigningKey]
signingKeys = [genKeyDSIGN (mkSeedFromBytes (BS.replicate 32 i)) | i <- [0 .. 3]]

testCommittee :: LeiosCommittee
testCommittee =
  mkLeiosCommittee $
    V.fromList
      [ (SJust (deriveVerKeyDSIGN sk, createPossessionProofDSIGN leiosSignContext sk), 1 / 4)
      | sk <- signingKeys
      ]

testQuorumStakeThreshold :: UnitInterval
testQuorumStakeThreshold =
  fromMaybe (error "not a unit interval") (boundRational (1 / 2))

-- | The certificate the given seats make of the given announcing block.
certOf :: RankingBlockHash -> [Word16] -> LeiosCert
certOf rbHash seats =
  either (error . show) id $
    aggregateLeiosCert testCommittee $
      Map.fromList
        [(voteSeatId vote, voteSignature vote) | seat <- seats, let vote = voteOf rbHash seat]

-- | The vote of the given seat for the given announcing block.
--
-- TODO: call 'Ouroboros.Consensus.Leios.Voting.signVote' here, once the
-- branch ch1bo/leios-voting merges and its module exports that function.
voteOf :: RankingBlockHash -> Word16 -> LeiosVote
voteOf rbHash seat =
  MkLeiosVote
    { voteAnnouncingRbHash = rbHash
    , voteSeatId = LeiosSeatId seat
    , voteSignature = signDSIGN leiosSignContext rbHash (signingKeys !! fromIntegral seat)
    }
