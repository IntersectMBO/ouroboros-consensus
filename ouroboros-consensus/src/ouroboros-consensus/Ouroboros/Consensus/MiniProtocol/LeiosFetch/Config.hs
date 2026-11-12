-- | One configuration authority for ordinary LeiosFetch admission and timing.
-- Rationale: @docs/fetch-policy/leiosfetch-improved.md#requests-and-admission@.
module Ouroboros.Consensus.MiniProtocol.LeiosFetch.Config
  ( LeiosFetchStaticEnv (..)
  , defaultLeiosFetchStaticEnv
  , validateLeiosFetchStaticEnv
  , maxOutstandingRequests
  , maxIngressBytes
  ) where

import Control.Monad (unless)
import Data.Word (Word32, Word64)
import Ouroboros.Consensus.Leios.Types (BytesSize, maxEndorserBlockBytesSize)
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Type (maxLeiosTxsRequestBytesSize)

-- | Node-local tuning. Transaction payload credit and body counts are
-- independent and shared across all EBs at the original peer registration.
-- The codec/count bounds remain internal rather than operator settings.
data LeiosFetchStaticEnv = MkLeiosFetchStaticEnv
  { maxRequestedBytesSizePerPeer :: BytesSize
  -- ^ Outstanding queued/sent TX payload bytes at an ordinary peer.
  , maxRequestedBytesSizePerBigLedgerPeer :: BytesSize
  -- ^ The corresponding independently configurable big-ledger-peer credit.
  -- Its default is equal to ordinary credit; neither includes body payload.
  , maxJobBytesSize :: BytesSize
  -- ^ Whole-transaction packing target for one immutable wire request. An
  -- indivisible TX may overhang; peer credit must still hold a legal TX.
  , maxBodiesPerPeer :: !Word32
  -- ^ Outstanding body requests across EBs, independently of TX payload.
  , minimumServiceRate :: !Word32
  -- ^ Requested payload service benchmark in bits per second.
  , maxJobTxCount :: Int
  -- ^ Internal codec-safety transaction count per job, not a tuning option.
  }
  deriving (Eq, Show)

defaultLeiosFetchStaticEnv :: LeiosFetchStaticEnv
defaultLeiosFetchStaticEnv =
  MkLeiosFetchStaticEnv
    { maxRequestedBytesSizePerPeer = 262144
    , maxRequestedBytesSizePerBigLedgerPeer = 262144
    , maxJobBytesSize = 65536
    , maxBodiesPerPeer = 4
    , minimumServiceRate = 300000
    , maxJobTxCount = 20000
    }

-- | Physical queued/sent requests, including zero-byte TX jobs. The actual
-- wire pipeline uses this same bound; there is no second policy depth knob.
maxOutstandingRequests :: Int
maxOutstandingRequests = 150

-- | Existing mux ingress allowance. Policy credit is deliberately smaller;
-- validation below includes body maxima and conservative encoding exposure.
maxIngressBytes :: Word64
maxIngressBytes = 60000000

-- | Validate before applications start. Arithmetic widens before multiplication
-- and summation. Reserve CBOR overhead even for zero-byte adversarial TXs: the
-- payload credit alone cannot bound their element count.
validateLeiosFetchStaticEnv :: Word64 -> LeiosFetchStaticEnv -> Either String LeiosFetchStaticEnv
validateLeiosFetchStaticEnv ingress c = do
  unless (maxJobBytesSize c > 0 && maxJobBytesSize c <= maxLeiosTxsRequestBytesSize) $
    Left "LeiosFetch job target must be in 1..500000 bytes"
  unless
    ( all
        (>= max 32768 (maxJobBytesSize c))
        [maxRequestedBytesSizePerPeer c, maxRequestedBytesSizePerBigLedgerPeer c]
    ) $
    Left "LeiosFetch peer credit must hold the job target and a 32768-byte transaction"
  unless (maxBodiesPerPeer c > 0 && maxBodiesPerPeer c <= 32767) $
    Left "LeiosFetch body count must be in 1..32767"
  unless (maxJobTxCount c > 0 && maxJobTxCount c <= 20000) $
    Left "LeiosFetch job transaction count must be in 1..20000"
  let bodyMax = fromIntegral maxEndorserBlockBytesSize :: Word64
  unless
    ( minimumServiceRate c > 0
        && 8 * fromIntegral bodyMax / fromIntegral (minimumServiceRate c) + 0.5 < (60 :: Rational)
    ) $
    Left "LeiosFetch service allowance must fit within the 60-second driver guard"
  let bodyBytes = fromIntegral (maxBodiesPerPeer c) * (bodyMax + 3)
      txBytes = fromIntegral (max (maxRequestedBytesSizePerPeer c) (maxRequestedBytesSizePerBigLedgerPeer c))
      framing = fromIntegral maxOutstandingRequests * (12 * fromIntegral (maxJobTxCount c) + 32)
  unless (bodyBytes + txBytes + framing <= ingress) $
    Left "LeiosFetch outstanding replies exceed the mux ingress limit"
  pure c
