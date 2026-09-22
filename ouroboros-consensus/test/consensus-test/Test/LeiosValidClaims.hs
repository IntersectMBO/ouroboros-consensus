{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Tests for 'LeiosValidClaims': the claim cache and the verdict ChainSel
-- reaches about a CertRB's certificate.
module Test.LeiosValidClaims (mkCert, tests, wholeCommittee) where

import Cardano.Crypto.DSIGN (signDSIGN)
import qualified Data.Map.Strict as Map
import Data.Ratio ((%))
import LeiosDemoTypes
  ( LeiosCert
  , LeiosCommittee
  , LeiosForecastRejection (..)
  , LeiosSeatId (..)
  , RbHash
  , Weight
  , aggregateLeiosCert
  )
import LeiosValidClaims
  ( Announcing (..)
  , ClaimVerdict (..)
  , ValidClaims
  , decideClaim
  , emptyValidClaims
  , insertValidClaim
  , memberValidClaim
  , pruneValidClaims
  , sizeValidClaims
  )
import Ouroboros.Consensus.Block (RealPoint (..), SlotNo (..))
import Test.Cardano.Crypto.Leios.Gen (TestCommittee (..), genCommittee)
import Test.LeiosDemoDb (genRbHash)
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)
import Test.Util.TestBlock (TestBlock, TestHash (..))

tests :: TestTree
tests =
  testGroup
    "LeiosValidClaims"
    [ testGroup
        "ValidClaims"
        [ testProperty "agrees with a Map model under inserts and prunes" prop_model
        , testProperty "insert is idempotent and keeps the first slot" prop_insertIdempotent
        , testProperty "prune keeps the immutable tip's own announcement" prop_pruneIsStrict
        ]
    , testGroup
        "decideClaim"
        [ testProperty "certifying at genesis is rejected" prop_genesisRejected
        , testProperty "a known claim short-circuits even an invalid cert" prop_knownShortCircuits
        , testProperty "a view with no committee is rejected" prop_noCommitteeRejected
        , testProperty "a cert over another claim is rejected" prop_wrongMessageRejected
        , testProperty "an unmeetable threshold is rejected" prop_thresholdRejected
        , testProperty "a good cert establishes exactly its own claim" prop_goodCertEstablishes
        ]
    ]

{-------------------------------------------------------------------------------
  ValidClaims against a model
-------------------------------------------------------------------------------}

-- | The claim cache is a 'Map' from claim to announcing slot, with pruning
-- dropping everything announced strictly below a slot.
type Model = Map.Map RbHash SlotNo

data Op = Insert SlotNo RbHash | Prune SlotNo
  deriving Show

-- | Draws claims from a small pool, so that inserts collide and prunes bite.
genOps :: [RbHash] -> Gen [Op]
genOps pool =
  listOf $
    oneof
      [ Insert <$> genSlot <*> elements pool
      , Prune <$> genSlot
      ]
 where
  genSlot = SlotNo <$> choose (0, 20)

-- | Shrinks the list of operations and the slots they mention; the claims
-- themselves stay as drawn, since they come from a fixed pool.
shrinkOps :: [Op] -> [[Op]]
shrinkOps = shrinkList shrinkOp
 where
  shrinkOp = \case
    Insert (SlotNo s) rbHash -> [Insert (SlotNo s') rbHash | s' <- shrink s]
    Prune (SlotNo s) -> [Prune (SlotNo s') | s' <- shrink s]

applyOp :: (ValidClaims, Model) -> Op -> (ValidClaims, Model)
applyOp (vc, m) = \case
  Insert slot rbHash ->
    ( insertValidClaim slot rbHash vc
    , Map.insertWith (\_new old -> old) rbHash slot m
    )
  Prune slot ->
    ( pruneValidClaims slot vc
    , Map.filter (>= slot) m
    )

prop_model :: Property
prop_model =
  forAllShrink (vectorOf 4 genRbHash) (const []) $ \pool ->
    forAllShrink (genOps pool) shrinkOps $ \ops ->
      let (vc, m) = foldl applyOp (emptyValidClaims, Map.empty) ops
       in conjoin
            [ counterexample "size" $ sizeValidClaims vc === Map.size m
            , conjoin
                [ counterexample ("member " <> show rbHash) $
                    memberValidClaim rbHash vc === Map.member rbHash m
                | rbHash <- pool
                ]
            ]

prop_insertIdempotent :: Property
prop_insertIdempotent =
  forAll genRbHash $ \rbHash ->
    forAll (choose (0, 20)) $ \s0 ->
      forAll (choose (0, 20)) $ \s1 ->
        let vc1 = insertValidClaim (SlotNo s0) rbHash emptyValidClaims
            vc2 = insertValidClaim (SlotNo s1) rbHash vc1
         in conjoin
              [ counterexample "size" $ sizeValidClaims vc2 === 1
              , -- The first slot decides, so pruning at it keeps the claim
                -- whatever the second insert asked for.
                counterexample "first slot wins" $
                  memberValidClaim rbHash (pruneValidClaims (SlotNo s0) vc2)
              ]

-- | Pruning is strict below the immutable tip, so a claim announced /at/ the tip
-- survives --- it is the tip's own announcement, which a CertRB extending the
-- tip certifies.
prop_pruneIsStrict :: Property
prop_pruneIsStrict =
  forAll genRbHash $ \rbHash ->
    forAll (choose (1, 20)) $ \s ->
      let vc = insertValidClaim (SlotNo s) rbHash emptyValidClaims
       in conjoin
            [ counterexample "at the tip: kept" $
                memberValidClaim rbHash (pruneValidClaims (SlotNo s) vc)
            , counterexample "below the tip: dropped" $
                not $
                  memberValidClaim rbHash (pruneValidClaims (SlotNo (s + 1)) vc)
            ]

{-------------------------------------------------------------------------------
  decideClaim
-------------------------------------------------------------------------------}

-- | A CertRB's certificate need not be valid for most of these, which is the
-- point: the verdict is reachable without a node.
mkCert :: TestCommittee -> RbHash -> LeiosCert
mkCert TestCommittee{committee, allKeys} msg =
  case aggregateLeiosCert committee sigs of
    Right cert -> cert
    Left e -> error $ "mkCert: aggregation failed: " <> show e
 where
  sigs =
    Map.fromList
      [ (LeiosSeatId (fromIntegral i), signDSIGN () msg sk)
      | (i, sk) <- zip [0 :: Int ..] allKeys
      ]

-- | Every seat of 'genCommittee' has weight @1/n@, so the whole committee
-- signing meets a threshold of 1.
wholeCommittee :: Weight
wholeCommittee = 1 % 1

announcingAt :: SlotNo -> RbHash -> Maybe (LeiosCommittee, Weight) -> Announcing TestBlock
announcingAt slot = Announcing (RealPoint slot (TestHash (pure 0)))

prop_genesisRejected :: Property
prop_genesisRejected =
  forAll genCommittee $ \tc ->
    forAll genRbHash $ \rbHash ->
      decideClaim @TestBlock emptyValidClaims AnnouncingAtGenesis (mkCert tc rbHash)
        === ClaimRejected LeiosForecastAfterGenesis

prop_knownShortCircuits :: Property
prop_knownShortCircuits =
  forAll genCommittee $ \tc ->
    forAll genRbHash $ \rbHash ->
      forAll genRbHash $ \otherHash ->
        rbHash /= otherHash ==>
          let vc = insertValidClaim (SlotNo 7) rbHash emptyValidClaims
              -- Signed over the wrong claim, so it would not verify.
              badCert = mkCert tc otherHash
           in decideClaim
                vc
                (announcingAt (SlotNo 7) rbHash (Just (tc.committee, wholeCommittee)))
                badCert
                === ClaimAlreadyEstablished

prop_noCommitteeRejected :: Property
prop_noCommitteeRejected =
  forAll genCommittee $ \tc ->
    forAll genRbHash $ \rbHash ->
      decideClaim
        emptyValidClaims
        (announcingAt (SlotNo 7) rbHash Nothing)
        (mkCert tc rbHash)
        === ClaimRejected (LeiosForecastMissingCommittee rbHash)

prop_wrongMessageRejected :: Property
prop_wrongMessageRejected =
  forAll genCommittee $ \tc ->
    forAll genRbHash $ \rbHash ->
      forAll genRbHash $ \otherHash ->
        rbHash /= otherHash ==>
          case decideClaim
            emptyValidClaims
            (announcingAt (SlotNo 7) rbHash (Just (tc.committee, wholeCommittee)))
            (mkCert tc otherHash) of
            ClaimRejected (LeiosForecastInvalidCertificate h _) -> h === rbHash
            verdict -> counterexample (show verdict) False

prop_thresholdRejected :: Property
prop_thresholdRejected =
  forAll genCommittee $ \tc ->
    forAll genRbHash $ \rbHash ->
      -- More weight than the whole committee can supply.
      case decideClaim
        emptyValidClaims
        (announcingAt (SlotNo 7) rbHash (Just (tc.committee, 2 % 1)))
        (mkCert tc rbHash) of
        ClaimRejected (LeiosForecastInvalidCertificate h _) -> h === rbHash
        verdict -> counterexample (show verdict) False

prop_goodCertEstablishes :: Property
prop_goodCertEstablishes =
  forAll genCommittee $ \tc ->
    forAll genRbHash $ \rbHash ->
      forAll (choose (0, 20)) $ \s ->
        let point :: RealPoint TestBlock
            point = RealPoint (SlotNo s) (TestHash (pure 0))
         in decideClaim
              emptyValidClaims
              (Announcing point rbHash (Just (tc.committee, wholeCommittee)))
              (mkCert tc rbHash)
              === ClaimEstablished point rbHash
