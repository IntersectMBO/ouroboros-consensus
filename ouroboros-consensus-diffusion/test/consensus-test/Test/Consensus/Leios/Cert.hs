{-# LANGUAGE TypeApplications #-}

-- | 'verifyBlockLeiosCert', on certificates made by a committee of four equal
-- seats that needs half the weight.
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
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Maybe.Strict (StrictMaybe (..))
import qualified Data.Vector.Strict as V
import Data.Word (Word16)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Leios.Protocol (LeiosLedgerView (..))
import Ouroboros.Consensus.Leios.Types (RbHash (..))
import Ouroboros.Consensus.Storage.ChainDB.Impl.Leios
  ( LeiosCertRejection (..)
  , verifyBlockLeiosCert
  )
import Test.Consensus.Leios.TestBlock (LeiosTestBlock, firstLeiosBlock)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Leios certificates"
    [ testCase "the certificate a quorum makes of the predecessor is valid" $
        verify (BlockHash announcing) (Just view) (certOf announcing [0, 1, 2])
          @?= Right (rbHashOf announcing)
    , testCase "a certificate of a block other than the predecessor is invalid" $
        verify (BlockHash announcing) (Just view) (certOf other [0, 1, 2])
          @?= Left (LeiosCertInvalid InvalidSignature)
    , testCase "the certificate a minority makes is invalid" $
        verify (BlockHash announcing) (Just view) (certOf announcing [0])
          @?= Left (LeiosCertInvalid (InsufficientWeight (1 / 4)))
    , testCase "a certificate whose predecessor is genesis is invalid" $
        verify GenesisHash (Just view) (certOf announcing [0, 1, 2])
          @?= Left LeiosCertAtGenesis
    , testCase "a certificate without a committee is invalid" $
        verify (BlockHash announcing) Nothing (certOf announcing [0, 1, 2])
          @?= Left LeiosCertWithoutCommittee
    ]
 where
  verify = verifyBlockLeiosCert @LeiosTestBlock

  announcing = blockHash (firstLeiosBlock 0)
  other = blockHash (firstLeiosBlock 1)

rbHashOf :: HeaderHash LeiosTestBlock -> RbHash
rbHashOf = MkRbHash . toRawHash (Proxy @LeiosTestBlock)

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
