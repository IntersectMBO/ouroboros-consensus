{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

-- | Test properties for the weighted Fait-Accompli committee selection implementation
module Test.Consensus.Committee.WFALS.Test (tests) where

import Cardano.Ledger.BaseTypes (Nonce (..), mkNonceFromNumber)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ratio ((%))
import Data.Word (Word64)
import GHC.Natural (Natural)
import Ouroboros.Consensus.Committee.Crypto
  ( CryptoSupportsVRF (..)
  , CryptoSupportsVoteSigning (..)
  , ElectionId
  , NormalizedVRFOutput (..)
  , VRFPoolContext (..)
  )
import qualified Ouroboros.Consensus.Committee.LS as LS
import Ouroboros.Consensus.Committee.Types
  ( LedgerStake (..)
  , PoolId
  , TargetCommitteeSize (..)
  )
import Ouroboros.Consensus.Committee.WFA (WFATiebreaker (..))
import qualified Ouroboros.Consensus.Committee.WFA as WFA
import Ouroboros.Consensus.Committee.WFALS (CommitteeMember (..))
import qualified Ouroboros.Consensus.Committee.WFALS as WFALS
import Test.Consensus.Committee.WFALS.Utils (mkPoolId)
import Test.QuickCheck
  ( Arbitrary (..)
  , Gen
  , Property
  , Testable (..)
  , choose
  , elements
  , forAll
  , frequency
  , tabulate
  , vectorOf
  , (===)
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.Util.QuickCheck (geometric)
import Test.Util.TestEnv (adjustQuickCheckTests)

-- * Mocked up crypto implementation

data MockCrypto

data MockVote
  = PersistentMockVote
      WFA.SeatIndex
      (ElectionId MockCrypto)
      (VoteMessage MockCrypto)
      (VoteSignature MockCrypto)
  | NonPersistentMockVote
      PoolId
      (ElectionId MockCrypto)
      (VoteMessage MockCrypto)
      (VRFOutput MockCrypto)
      (VoteSignature MockCrypto)
  deriving (Eq, Show)

type instance ElectionId MockCrypto = Word64

instance WFALS.CryptoSupportsWFALS MockCrypto where
  type PrivateKey MockCrypto = (String, String)
  type PublicKey MockCrypto = (String, String)

  getVoteSignaturePrivateKey _ = fst
  getVoteSignaturePublicKey _ = fst
  getVRFSigningKey _ = snd
  getVRFVerifyKey _ = snd

instance WFALS.VoteSupportsWFALS MockCrypto MockVote where
  getVoteView vote k =
    case vote of
      PersistentMockVote seatIndex electionId msg sig ->
        k $ WFALS.PersistentVote seatIndex electionId msg sig
      NonPersistentMockVote poolId electionId msg vrfOutput sig ->
        k $ WFALS.NonPersistentVote poolId electionId msg vrfOutput sig

instance CryptoSupportsVoteSigning MockCrypto where
  type VoteSignaturePrivateKey MockCrypto = String
  type VoteSignaturePublicKey MockCrypto = String -- reverse of the private key

  newtype VoteMessage MockCrypto
    = MockVoteMessage String
    deriving (Eq, Show)

  newtype VoteSignature MockCrypto
    = MockVoteSignature String
    deriving (Eq, Show)

  signVote sk electionId (MockVoteMessage str) =
    MockVoteSignature $
      show electionId <> "|" <> sk <> "|" <> str

  verifyVoteSignature pk electionId (MockVoteMessage str) (MockVoteSignature sig)
    | sig == expectedSig =
        Right ()
    | otherwise =
        Left $
          concat $
            [ "could not verify vote signature"
            , ", expected signature: " <> show expectedSig
            , ", actual signature: " <> sig
            ]
   where
    expectedSig =
      show electionId <> "|" <> reverse pk <> "|" <> str

instance CryptoSupportsVRF MockCrypto where
  type VRFSigningKey MockCrypto = String
  type VRFVerifyKey MockCrypto = String -- reverse of the signing key

  newtype VRFElectionInput MockCrypto
    = MockVoteVRFElectionInput (Nonce, ElectionId MockCrypto)
    deriving (Eq, Show)

  -- Number of digits in the concatenation of all the inputs, plus its length.
  -- This is just a dummy way to deterministically derive a rational value in
  -- the range [0, 1].
  newtype VRFOutput MockCrypto
    = MockVRFOutput (Natural, Natural)
    deriving (Eq, Show)

  mkVRFElectionInput epochNonce electionId =
    MockVoteVRFElectionInput (epochNonce, electionId)

  evalVRF context (MockVoteVRFElectionInput (epochNonce, electionId)) =
    case context of
      VRFSignContext sk -> do
        Right $ MockVRFOutput (countDigits msg, fromIntegral (length msg))
       where
        msg = show epochNonce <> show electionId <> sk
      VRFVerifyContext pk (MockVRFOutput output)
        | output == expectedOutput ->
            Right $ MockVRFOutput expectedOutput
        | otherwise ->
            Left $
              concat $
                [ "could not verify VRF output"
                , ", expected output: " <> show expectedOutput
                , ", actual output: " <> show output
                ]
       where
        expectedOutput = (countDigits msg, fromIntegral (length msg))
        msg = show epochNonce <> show electionId <> pk

  normalizeVRFOutput (MockVRFOutput (count, total)) =
    NormalizedVRFOutput $
      fromIntegral count
        % fromIntegral total

countDigits :: String -> Natural
countDigits =
  fromIntegral
    . length
    . filter (`elem` ['0' .. '9'])

mockWFATiebreaker :: WFA.WFATiebreaker
mockWFATiebreaker =
  WFATiebreaker $ \_ _ ->
    LT

castVote ::
  PoolId ->
  ElectionId MockCrypto ->
  VoteMessage MockCrypto ->
  WFALS.CommitteeMember MockCrypto ->
  MockVote
castVote poolId electionId msg committeeMember =
  case committeeMember of
    WFALS.PersistentCommitteeMember proof _stake ->
      PersistentMockVote
        (WFALS.membershipProofSeatIndex proof)
        electionId
        msg
        (WFALS.membershipProofVoteSignature proof)
    WFALS.NonPersistentCommitteeMember proof _stake ->
      NonPersistentMockVote
        poolId
        electionId
        msg
        (WFALS.membershipProofVRFOutput proof)
        (WFALS.membershipProofVoteSignature proof)

-- * Generators

alphaNumString :: Gen String
alphaNumString =
  vectorOf 8 $
    elements $
      ['a' .. 'z']
        <> ['A' .. 'Z']
        <> ['0' .. '9']

genNonce :: Gen Nonce
genNonce =
  frequency
    [ (1, pure NeutralNonce)
    , (9, mkNonceFromNumber <$> arbitrary)
    ]

genPositiveStake :: Gen LedgerStake
genPositiveStake =
  LedgerStake
    . toRational
    . (+ 1)
    <$> geometric 0.25

genElectionId :: Gen (ElectionId MockCrypto)
genElectionId =
  arbitrary

genOnePool ::
  Gen LedgerStake ->
  Gen
    ( PoolId
    , ( WFALS.PrivateKey MockCrypto
      , WFALS.PublicKey MockCrypto
      , LedgerStake
      )
    )
genOnePool genStake = do
  poolId <- alphaNumString
  privateKeySign <- alphaNumString
  signKeyVRF <- alphaNumString
  stake <- genStake
  pure $
    ( mkPoolId poolId
    ,
      ( (privateKeySign, signKeyVRF)
      , (reverse privateKeySign, reverse signKeyVRF)
      , stake
      )
    )

genPools ::
  Int ->
  Gen
    ( Map
        PoolId
        ( WFALS.PrivateKey MockCrypto
        , WFALS.PublicKey MockCrypto
        , LedgerStake
        )
    )
genPools maxPools = do
  numPools <- choose (1, maxPools)
  numPoolsWithZeroStake <-
    choose (0, numPools - 1)
  poolsWithZeroStake <-
    vectorOf
      numPoolsWithZeroStake
      (genOnePool (pure (LedgerStake 0)))
  poolsWithPositiveStake <-
    vectorOf
      (numPools - numPoolsWithZeroStake)
      (genOnePool genPositiveStake)
  pure $
    Map.fromList (poolsWithZeroStake <> poolsWithPositiveStake)

genTargetCommitteeSize ::
  Map
    PoolId
    ( WFALS.PrivateKey MockCrypto
    , WFALS.PublicKey MockCrypto
    , LedgerStake
    ) ->
  Gen TargetCommitteeSize
genTargetCommitteeSize pools = do
  let hasPositiveStake (_, _, LedgerStake stake) =
        stake > 0
  let poolsWithPositiveStake =
        Map.filter hasPositiveStake pools
  numPools <-
    choose (1, Map.size poolsWithPositiveStake)
  pure $ TargetCommitteeSize (fromIntegral numPools)

genPool ::
  Map
    PoolId
    ( WFALS.PrivateKey MockCrypto
    , WFALS.PublicKey MockCrypto
    , LedgerStake
    ) ->
  Gen
    ( PoolId
    , WFALS.PrivateKey MockCrypto
    , WFALS.PublicKey MockCrypto
    , LedgerStake
    )
genPool pools = do
  (poolId, (privateKey, publicKey, stake)) <- elements (Map.toList pools)
  pure (poolId, privateKey, publicKey, stake)

genMockVoteMessage :: Gen (VoteMessage MockCrypto)
genMockVoteMessage =
  MockVoteMessage <$> alphaNumString

-- * Tabulate helpers

mkBucket :: Integer -> Integer -> String
mkBucket size val
  | val <= 0 =
      "<= 0"
  | otherwise =
      "[ " <> show lo <> ", " <> show hi <> " )"
 where
  lo = (val `div` size) * size
  hi = lo + size

tabulateTargetCommitteeSize ::
  TargetCommitteeSize ->
  Property ->
  Property
tabulateTargetCommitteeSize (TargetCommitteeSize size) =
  tabulate
    "target committee size"
    [mkBucket 10 (fromIntegral size)]

tabulateNumPools ::
  Map
    PoolId
    ( WFALS.PrivateKey MockCrypto
    , WFALS.PublicKey MockCrypto
    , LedgerStake
    ) ->
  Property ->
  Property
tabulateNumPools pools =
  tabulate
    "number of pools"
    [mkBucket 10 (fromIntegral (Map.size pools))]

tabulatePoolStake ::
  LedgerStake ->
  Property ->
  Property
tabulatePoolStake (LedgerStake stake) =
  tabulate
    "pool stake"
    [mkBucket 10 (floor stake)]

tabulateShouldVote ::
  Maybe (WFALS.CommitteeMember MockCrypto) ->
  Property ->
  Property
tabulateShouldVote shouldVote =
  tabulate
    "should vote"
    [ case shouldVote of
        Nothing ->
          "NoVote"
        Just (PersistentCommitteeMember _ _) ->
          "PersistentVote"
        Just (NonPersistentCommitteeMember proof _) ->
          let numSeats = LS.unLocalSortitionNumSeats (WFALS.membershipProofNumSeats proof)
           in "NonPersistentVote(NumSeats=" <> show numSeats <> ")"
    ]

-- * Test properties

onError :: Either err a -> (err -> a) -> a
onError action onLeft =
  case action of
    Left err -> onLeft err
    Right val -> val

-- | If a pool is entitled to vote in a given committee selection, then the vote
-- it casts should be verifiable under the same committee selection.
prop_checkShouldVote_verifyVote :: Property
prop_checkShouldVote_verifyVote = do
  forAll genNonce $ \epochNonce -> do
    forAll (genPools 100) $ \pools -> do
      forAll (genTargetCommitteeSize pools) $ \targetCommitteeSize -> do
        forAll (genPool pools) $ \(poolId, poolPrivateKey, _, poolStake) -> do
          forAll genElectionId $ \electionId -> do
            forAll genMockVoteMessage $ \voteMsg -> do
              let extWFAStakeDistr =
                    WFA.mkExtWFAStakeDistr
                      mockWFATiebreaker
                      (Map.map (\(_, pubKey, stake) -> (stake, pubKey)) pools)
                      `onError` \err ->
                        error ("mkExtWFAStakeDistr failed: " <> show err)
              let committeeSelection =
                    WFALS.mkCommitteeSelection
                      epochNonce
                      targetCommitteeSize
                      extWFAStakeDistr
                      `onError` \err ->
                        error ("mkCommitteeSelection failed: " <> show err)
              let shouldVote =
                    WFALS.checkShouldVote
                      poolId
                      poolPrivateKey
                      electionId
                      voteMsg
                      committeeSelection
                      `onError` \err ->
                        error ("checkShouldVote failed: " <> show err)
              let addTables =
                    tabulateNumPools pools
                      . tabulateTargetCommitteeSize targetCommitteeSize
                      . tabulatePoolStake poolStake
                      . tabulateShouldVote shouldVote
              -- If so, a vote cast by this pool should be verifiable undert the
              -- same committee selection (which exists on the receiving peers)
              case shouldVote of
                Nothing -> do
                  addTables $
                    property True
                Just committeeMember -> do
                  let vote =
                        castVote poolId electionId voteMsg committeeMember
                  let committeeMember' =
                        WFALS.verifyVote
                          vote
                          committeeSelection
                          `onError` \err ->
                            error ("vote verification failed: " <> show err)
                  addTables $
                    Just committeeMember === committeeMember'

-- | Test suite
tests :: TestTree
tests =
  adjustQuickCheckTests (* 100) $
    testGroup
      "weighted Fait-Accompli committee selection implementation property tests"
      [ testProperty
          "if a pool is entitled to vote, then the vote it casts should verifiable"
          prop_checkShouldVote_verifyVote
      ]
