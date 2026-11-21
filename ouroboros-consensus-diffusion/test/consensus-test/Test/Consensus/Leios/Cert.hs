{-# LANGUAGE TypeApplications #-}

-- | 'verifyLeiosClaim' on certificates made by the test committee.
module Test.Consensus.Leios.Cert (tests) where

import Cardano.Crypto.Hash (Hash (UnsafeHash))
import Cardano.Crypto.Leios (VerificationError (..))
import Cardano.Ledger.BaseTypes (Milliseconds32 (..))
import Control.Monad.Except (runExcept)
import qualified Data.ByteString.Short as SBS
import Data.Maybe (fromJust)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Leios.Cert
  ( LeiosCertRejection (..)
  , LeiosClaim (..)
  , verifyLeiosClaim
  )
import Ouroboros.Consensus.Leios.Protocol (LeiosLedgerView (..), LeiosProtocol (..))
import Ouroboros.Consensus.Leios.Types (RankingBlockHash, toRankingBlockHash)
import Test.Consensus.Leios.Committee
  ( certOf
  , testCommittee
  , testQuorumStakeThreshold
  )
import Test.Consensus.Leios.TestBlock
  ( LeiosTestBlock
  , LeiosTestProtocol
  , firstLeiosBlock
  , setAnnouncement
  )
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Leios certificates"
    [ testCase "the certificate a quorum makes of the predecessor is valid" $
        verify (certOf (rbHashOf announcing) [0, 1, 2])
          @?= Right ()
    , testCase "a certificate of a block other than the predecessor is invalid" $
        verify (certOf (rbHashOf other) [0, 1, 2])
          @?= Left (LeiosCertInvalid InvalidSignature)
    , testCase "the certificate a minority makes is invalid" $
        verify (certOf (rbHashOf announcing) [0])
          @?= Left (LeiosCertInvalid (InsufficientWeight (1 / 4)))
    ]
 where
  verify cert = runExcept $ verifyLeiosClaim cert claim (rbHashOf announcing)

  claim = LeiosClaim (blockSlot announcingBlock) announcement view
  announcement =
    fromJust $ getLeiosAnnouncement (Proxy @LeiosTestProtocol) (getHeader announcingBlock)
  announcingBlock = setAnnouncement (UnsafeHash (SBS.replicate 32 1)) 0 (firstLeiosBlock 0)
  announcing = blockHash announcingBlock
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
