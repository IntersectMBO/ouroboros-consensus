-- | One configuration authority for ordinary LeiosFetch admission and timing.
-- Rationale: @docs/fetch-policy/leiosfetch-improved.md#requests-and-admission@.
module Ouroboros.Consensus.MiniProtocol.LeiosFetch.Config (LeiosFetchStaticEnv (..), defaultLeiosFetchStaticEnv, validateLeiosFetchStaticEnv) where

import Control.Monad (unless)
import Data.Word (Word32, Word64)
import Ouroboros.Consensus.Leios.Types (BytesSize)
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Type (maxLeiosTxsRequestBytesSize)

data LeiosFetchStaticEnv = MkLeiosFetchStaticEnv
  { maxRequestedBytesSizePerPeer :: BytesSize
  -- ^ At most this many outstanding bytes requested from each non-big-ledger
  -- peer
  , maxRequestedBytesSizePerBigLedgerPeer :: BytesSize
  -- ^ At most this many outstanding bytes requested from each big-ledger peer.
  --
  -- At most the LeiosFetch ingress queue's size: the scheduler must never
  -- leave more requested-but-unconsumed response bytes outstanding than that
  -- queue can hold, else the mux tears the connection down (see
  -- @Network.Mux.Ingress@'s @IngressQueueOverRun@).
  --
  -- Larger than 'maxRequestedBytesSizePerPeer' so a high-stake peer can be asked
  -- for multiple whole EB closures at once, but still bounded so an adversarial
  -- peer can't drown us.
  , maxRequestBytesSize :: BytesSize
  -- ^ At most this many outstanding bytes per request.
  --
  -- At most 'maxLeiosTxsRequestBytesSize', by which the codec bounds the reply
  -- ('Ouroboros.Consensus.MiniProtocol.LeiosFetch.Type.byteLimitsLeiosFetch'):
  -- an honest peer's reply to a larger request could cost it its connection.
  , maxJobBytesSize :: BytesSize
  -- ^ At most this many bytes of txs per job
  , maxBodiesPerPeer :: !Word32
  -- ^ Body count credit, independent of transaction bytes across all EBs.
  , minimumServiceRate :: !Word32
  -- ^ Requested payload service benchmark in bits per second.
  , maxJobTxCount :: Int
  -- ^ At most this many txs per job
  }
  deriving (Eq, Show)

-- | Conservative initial policy settings. The ingress allowance is validated
-- separately by the runtime; it does not enlarge a peer's TX credit.
defaultLeiosFetchStaticEnv :: BytesSize -> LeiosFetchStaticEnv
defaultLeiosFetchStaticEnv _ingress =
  MkLeiosFetchStaticEnv
    { maxRequestedBytesSizePerPeer = 262144
    , maxRequestedBytesSizePerBigLedgerPeer = 262144
    , maxRequestBytesSize = maxLeiosTxsRequestBytesSize
    , maxJobBytesSize = 64 * thousandBase2
    , maxBodiesPerPeer = 4
    , minimumServiceRate = 300000
    , maxJobTxCount = 20000 -- TODO do we want this to be low enough to matter?
    }
 where
  thousandBase2 :: Num a => a
  thousandBase2 = 2 ^ (10 :: Int)

-- | Validate before peer admission. Arithmetic is widened before summation.
-- Reserve conservative CBOR overhead even for zero-byte adversarial TXs:
-- payload credit alone does not bound their count. The client pipeline has at
-- most 150 responses, each with at most 'maxJobTxCount' transaction elements.
validateLeiosFetchStaticEnv :: Word64 -> LeiosFetchStaticEnv -> Either String LeiosFetchStaticEnv
validateLeiosFetchStaticEnv ingress c = do
  unless (maxJobBytesSize c > 0 && maxJobBytesSize c <= maxLeiosTxsRequestBytesSize) $
    Left "LeiosFetch job target must be in 1..500000 bytes"
  unless
    ( all
        (>= max 32768 (maxJobBytesSize c))
        [maxRequestedBytesSizePerPeer c, maxRequestedBytesSizePerBigLedgerPeer c]
    )
    $ Left "LeiosFetch peer credit must hold the job target and a 32768-byte transaction"
  unless
    ( maxRequestBytesSize c >= max 32768 (maxJobBytesSize c)
        && maxRequestBytesSize c <= maxLeiosTxsRequestBytesSize
    )
    $ Left "LeiosFetch request bound must hold a legal job and fit the protocol limit"
  unless (maxBodiesPerPeer c > 0 && maxBodiesPerPeer c <= 32767) $
    Left "LeiosFetch body count must be in 1..32767"
  unless (maxJobTxCount c > 0 && maxJobTxCount c <= 20000) $
    Left "LeiosFetch job transaction count must be in 1..20000"
  unless
    (minimumServiceRate c > 0 && 4800000 / fromIntegral (minimumServiceRate c) + 0.5 < (60 :: Rational))
    $ Left "LeiosFetch service allowance must fit within the 60-second driver guard"
  let bodyBytes = fromIntegral (maxBodiesPerPeer c) * 600003 :: Word64
      txBytes =
        fromIntegral (max (maxRequestedBytesSizePerPeer c) (maxRequestedBytesSizePerBigLedgerPeer c)) ::
          Word64
      framing = 150 * (12 * fromIntegral (maxJobTxCount c) + 32) :: Word64
  unless (bodyBytes + txBytes + framing <= ingress) $
    Left "LeiosFetch outstanding replies exceed the mux ingress limit"
  pure c
