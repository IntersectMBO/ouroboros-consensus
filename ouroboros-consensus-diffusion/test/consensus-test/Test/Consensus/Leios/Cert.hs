{-# LANGUAGE TypeApplications #-}

-- | 'verifyLeiosClaim' on certificates made by the test committee.
module Test.Consensus.Leios.Cert (tests) where

import Cardano.Crypto.Leios (VerificationError (..))
import Cardano.Ledger.BaseTypes (Milliseconds32 (..))
import Control.Monad.Except (runExcept)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Leios.Cert
  ( LeiosCertRejection (..)
  , LeiosClaim (..)
  , verifyLeiosClaim
  )
import Ouroboros.Consensus.Leios.Protocol (LeiosLedgerView (..))
import Ouroboros.Consensus.Leios.Types (RankingBlockHash, toRankingBlockHash)
import Test.Consensus.Leios.Committee
  ( certOf
  , testCommittee
  , testQuorumStakeThreshold
  )
import Test.Consensus.Leios.TestBlock (LeiosTestBlock, firstLeiosBlock)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Leios certificates"
    [ testCase "the certificate a quorum makes of the predecessor is valid" $
        verify (certOf (rbHashOf announcing) [0, 1, 2]) claim
          @?= Right ()
    , testCase "a certificate of a block other than the predecessor is invalid" $
        verify (certOf (rbHashOf other) [0, 1, 2]) claim
          @?= Left (LeiosCertInvalid InvalidSignature)
    , testCase "the certificate a minority makes is invalid" $
        verify (certOf (rbHashOf announcing) [0]) claim
          @?= Left (LeiosCertInvalid (InsufficientWeight (1 / 4)))
    ]
 where
  verify cert claim' = runExcept $ verifyLeiosClaim view cert claim'

  claim = LeiosClaim announcingSlot (rbHashOf announcing)

  announcingSlot = blockSlot (firstLeiosBlock 0)
  announcing = blockHash (firstLeiosBlock 0)
  other = blockHash (firstLeiosBlock 1)

rbHashOf :: HeaderHash LeiosTestBlock -> RankingBlockHash
rbHashOf = toRankingBlockHash (Proxy @LeiosTestBlock)

view :: LeiosLedgerView
view =
  MkLeiosLedgerView
    { llvCommittee = testCommittee
    , llvQuorumStakeThreshold = testQuorumStakeThreshold
    , llvAnnouncementPeriod = Milliseconds32 1000
    , llvVotePeriod = Milliseconds32 4000
    , llvDiffusionPeriod = Milliseconds32 1000
    , llvMaxEbBodySize = 5000
    , llvMaxEbTxsSize = 5000
    }
