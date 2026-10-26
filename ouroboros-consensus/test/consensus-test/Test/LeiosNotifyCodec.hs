{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The LeiosNotify codec on 'MsgLeiosVotes': every vote count the decoder
-- admits round-trips, and it refuses an empty message and one above its
-- bound.
module Test.LeiosNotifyCodec (tests) where

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import Control.Monad.ST (ST, runST)
import qualified Data.ByteString.Lazy as LBS
import Data.Either (isLeft)
import LeiosDemoOnlyTestNotify
  ( LeiosNotify
  , Message (..)
  , SingLeiosNotify (..)
  , codecLeiosNotify
  , leiosNotifyMaxVotesPerMessage
  )
import qualified LeiosDemoOnlyTestNotify as Notify
import LeiosDemoTypes
  ( LeiosSeatId (..)
  , LeiosVote
  , decodeLeiosVote
  , encodeLeiosVote
  , signLeiosVote
  )
import Network.TypedProtocol.Codec (Codec, CodecF (..), SomeMessage (..), runDecoder)
import Test.Cardano.Crypto.Leios.Gen (genLeiosSigningKey)
import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.Util.LeiosHash (genRbHash)

tests :: TestTree
tests =
  testGroup
    "LeiosNotify codec"
    [ testProperty
        "MsgLeiosVotes round-trips at every count the decoder admits"
        prop_votesRoundTrip
    , testProperty
        "MsgLeiosVotes with no votes or more than the bound is refused"
        prop_votesCountRefused
    ]

type Protocol = LeiosNotify Int Int LeiosVote

codec :: Codec Protocol CBOR.DeserialiseFailure (ST s) LBS.ByteString
codec =
  codecLeiosNotify
    CBOR.encodeInt
    CBOR.decodeInt
    CBOR.encodeInt
    CBOR.decodeInt
    encodeLeiosVote
    decodeLeiosVote

encodeVotes :: [LeiosVote] -> LBS.ByteString
encodeVotes vs = runST (pure (encode codec (MsgLeiosVotes vs :: Message Protocol Notify.StBusy Notify.StIdle)))

decodeBusy ::
  LBS.ByteString -> Either CBOR.DeserialiseFailure (SomeMessage (Notify.StBusy :: Protocol))
decodeBusy bytes = runST $ do
  step <- decode codec SingBusy
  runDecoder [bytes] step

-- | @n@ signed votes with arbitrary seats and points, from fresh keys.
genVotes :: Int -> Gen [LeiosVote]
genVotes n =
  vectorOf n $
    signLeiosVote <$> genLeiosSigningKey <*> (LeiosSeatId <$> arbitrary) <*> genRbHash

prop_votesRoundTrip :: Property
prop_votesRoundTrip =
  forAllShrink
    (choose (1, leiosNotifyMaxVotesPerMessage) >>= genVotes)
    (filter (not . null) . shrinkList (const []))
    $ \vs ->
      case decodeBusy (encodeVotes vs) of
        Right (SomeMessage (MsgLeiosVotes vs')) -> vs' === vs
        Right (SomeMessage _) -> counterexample "decoded to another message" False
        Left err -> counterexample (show err) False

prop_votesCountRefused :: Property
prop_votesCountRefused =
  forAll (elements (0 : [leiosNotifyMaxVotesPerMessage + 1 .. leiosNotifyMaxVotesPerMessage + 8])) $ \n ->
    forAll (genVotes n) $ \vs ->
      counterexample (show n ++ " votes decoded") $
        isLeft (decodeBusy (encodeVotes vs))
