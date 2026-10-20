{-# LANGUAGE TypeApplications #-}

-- | 'leiosClaim', and 'verifyLeiosClaim' on certificates made by the test
-- committee.
module Test.Consensus.Leios.Cert (tests) where

import Cardano.Crypto.Leios (VerificationError (..))
import Cardano.Ledger.BaseTypes (Milliseconds32 (..))
import Control.Monad.Except (runExcept)
import Data.Maybe.Strict (StrictMaybe (..))
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Leios.Cert
  ( LeiosCertRejection (..)
  , LeiosClaim (..)
  , leiosClaim
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
    [ testCase "a block claims its predecessor's announcement" $
        claimOf (BlockHash announcing) (NotOrigin announcingSlot)
          @?= Right claim
    , testCase "a block whose predecessor is genesis claims nothing" $
        claimOf GenesisHash Origin
          @?= Left LeiosCertAtGenesis
    , testCase "the certificate a quorum makes of the predecessor is valid" $
        verify (SJust view) (certOf (rbHashOf announcing) [0, 1, 2]) claim
          @?= Right ()
    , testCase "a certificate of a block other than the predecessor is invalid" $
        verify (SJust view) (certOf (rbHashOf other) [0, 1, 2]) claim
          @?= Left (LeiosCertInvalid InvalidSignature)
    , testCase "the certificate a minority makes is invalid" $
        verify (SJust view) (certOf (rbHashOf announcing) [0]) claim
          @?= Left (LeiosCertInvalid (InsufficientWeight (1 / 4)))
    , testCase "a certificate without a committee is invalid" $
        verify SNothing (certOf (rbHashOf announcing) [0, 1, 2]) claim
          @?= Left LeiosCertWithoutCommittee
    ]
 where
  claimOf prevHash predecessorSlot =
    runExcept $ leiosClaim @LeiosTestBlock prevHash predecessorSlot
  verify mbView cert claim' = runExcept $ verifyLeiosClaim mbView cert claim'

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
