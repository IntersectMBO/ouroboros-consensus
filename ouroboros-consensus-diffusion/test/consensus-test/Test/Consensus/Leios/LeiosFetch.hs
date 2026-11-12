-- | Tests of LeiosFetch's codec, its decision logic, and its client and server
-- as the node runs them.
module Test.Consensus.Leios.LeiosFetch (tests) where

import qualified Test.Consensus.Leios.LeiosFetch.Assignment as Assignment
import qualified Test.Consensus.Leios.LeiosFetch.Client as Client
import qualified Test.Consensus.Leios.LeiosFetch.Codec as Codec
import qualified Test.Consensus.Leios.LeiosFetch.Coverage as Coverage
import qualified Test.Consensus.Leios.LeiosFetch.Decision as Decision
import qualified Test.Consensus.Leios.LeiosFetch.LocalReuse as LocalReuse
import qualified Test.Consensus.Leios.LeiosFetch.Node as Node
import qualified Test.Consensus.Leios.LeiosFetch.PeerPenalty as PeerPenalty
import qualified Test.Consensus.Leios.LeiosFetch.Replies as Replies
import qualified Test.Consensus.Leios.LeiosFetch.RequestState as RequestState
import qualified Test.Consensus.Leios.LeiosFetch.Runtime as Runtime
import qualified Test.Consensus.Leios.LeiosFetch.Timing as Timing
import qualified Test.Consensus.Leios.LeiosFetch.Wait as Wait
import qualified Test.Consensus.Leios.LeiosFetch.Wire as Wire
import Test.Tasty

tests :: TestTree
tests =
  testGroup
    "LeiosFetch"
    [ Coverage.tests
    , Assignment.tests
    , Wire.tests
    , Runtime.tests
    , Timing.tests
    , PeerPenalty.tests
    , Wait.tests
    , RequestState.tests
    , Codec.tests
    , Decision.tests
    , Replies.tests
    , LocalReuse.tests
    , Client.tests
    , Node.tests
    ]
