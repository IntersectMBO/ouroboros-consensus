{-# LANGUAGE TypeApplications #-}

-- | 'leiosClaim', and 'verifyLeiosClaim' on certificates made by a committee of
-- four equal seats that needs half the weight.
module Test.Consensus.Leios.Cert (tests) where

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
  , VerificationError (..)
  , aggregateLeiosCert
  , leiosSignContext
  , mkLeiosCommittee
  )
import Cardano.Crypto.Seed (mkSeedFromBytes)
import Cardano.Ledger.BaseTypes (Milliseconds32 (..), boundRational)
import Control.Monad.Except (runExcept)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Maybe.Strict (StrictMaybe (..))
import qualified Data.Vector.Strict as V
import Data.Word (Word16)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Leios.Cert
  ( LeiosCertRejection (..)
  , LeiosClaim (..)
  , leiosClaim
  , verifyLeiosClaim
  )
import Ouroboros.Consensus.Leios.Protocol (LeiosLedgerView (..))
import Ouroboros.Consensus.Leios.Types (RankingBlockHash, toRankingBlockHash)
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
        verify (SJust view) (certOf announcing [0, 1, 2]) claim
          @?= Right ()
    , testCase "a certificate of a block other than the predecessor is invalid" $
        verify (SJust view) (certOf other [0, 1, 2]) claim
          @?= Left (LeiosCertInvalid InvalidSignature)
    , testCase "the certificate a minority makes is invalid" $
        verify (SJust view) (certOf announcing [0]) claim
          @?= Left (LeiosCertInvalid (InsufficientWeight (1 / 4)))
    , testCase "a certificate without a committee is invalid" $
        verify SNothing (certOf announcing [0, 1, 2]) claim
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

signingKeys :: [LeiosSigningKey]
signingKeys = [genKeyDSIGN (mkSeedFromBytes (BS.replicate 32 i)) | i <- [0 .. 3]]

committee :: LeiosCommittee
committee =
  mkLeiosCommittee $
    V.fromList
      [ (SJust (deriveVerKeyDSIGN sk, createPossessionProofDSIGN leiosSignContext sk), 1 / 4)
      | sk <- signingKeys
      ]

-- | The certificate the given seats make of the given block.
certOf :: HeaderHash LeiosTestBlock -> [Word16] -> LeiosCert
certOf hash seats =
  either (error . show) id $
    aggregateLeiosCert committee $
      Map.fromList
        [ (LeiosSeatId seat, signDSIGN leiosSignContext (rbHashOf hash) sk)
        | (seat, sk) <- zip [0 ..] signingKeys
        , seat `elem` seats
        ]

view :: LeiosLedgerView
view =
  MkLeiosLedgerView
    { llvCommittee = committee
    , llvQuorumStakeThreshold =
        fromMaybe (error "not a unit interval") (boundRational (1 / 2))
    , llvAnnouncementPeriod = Milliseconds32 1000
    , llvVotePeriod = Milliseconds32 4000
    , llvDiffusionPeriod = Milliseconds32 1000
    , llvMaxEbBodySize = 5000
    , llvMaxEbTxsSize = 5000
    }
