{-# LANGUAGE ScopedTypeVariables #-}

module Test.Util.SupportedNetworkProtocolVersion (contiguousSupportedNetworkProtocolVersions) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy
import qualified Data.Set as Set
import Data.Typeable
import Ouroboros.Consensus.Node.NetworkProtocolVersion
import Test.Tasty.HUnit

-- | Make sure that 'supportedNodeToNodeVersions' and
-- 'supportedNodeToClientVersions' each map a contiguous range of versions
-- ending at 'maxBound'.
--
-- We deliberately do /not/ require every version of the network layer's enum to
-- be mapped. Dropping the oldest versions once no client still negotiates them
-- is a recurring, intentional operation, and the network layer keeps the
-- constructors around for longer than we keep supporting them.
--
-- What must hold is that the supported versions are a suffix without holes:
--
--  * ending at 'maxBound', so that adding a version to the network layer's enum
--    and forgetting to map it here is a build-time-visible failure, which is
--    the mistake this test was originally written to catch;
--
--  * without holes, since the handshake picks the highest version both sides
--    know, and a gap in the middle only makes the negotiated version harder to
--    reason about for no benefit.
contiguousSupportedNetworkProtocolVersions ::
  forall blk.
  (Typeable blk, SupportedNetworkProtocolVersion blk) =>
  Proxy blk ->
  Assertion
contiguousSupportedNetworkProtocolVersions p = do
  testVersions supportedNodeToNodeVersions
  testVersions supportedNodeToClientVersions
 where
  testVersions ::
    (Show v, Ord v, Enum v, Bounded v) =>
    (Proxy blk -> Map v a) ->
    Assertion
  testVersions f = do
    assertBool
      ("no supported versions for " <> blkName)
      (not (Set.null mappedVersions))
    assertBool
      ( "the newest version is not supported by "
          <> blkName
          <> ": "
          <> show (Set.toList unsupportedNewerVersions)
      )
      (Set.null unsupportedNewerVersions)
    assertBool
      ( "holes in the supported versions of "
          <> blkName
          <> ": "
          <> show (Set.toList holes)
      )
      (Set.null holes)
   where
    blkName = show (typeRep p)
    mappedVersions = Map.keysSet $ f p
    -- Versions newer than the newest one we support. Empty iff we support
    -- 'maxBound'. Note we cannot use @succ (Set.findMax mappedVersions)@ here,
    -- as that throws when the newest version is already 'maxBound'.
    unsupportedNewerVersions
      | Set.null mappedVersions = Set.empty
      | otherwise =
          Set.fromList [Set.findMax mappedVersions .. maxBound]
            Set.\\ mappedVersions
    holes
      | Set.null mappedVersions = Set.empty
      | otherwise =
          Set.fromList [Set.findMin mappedVersions .. Set.findMax mappedVersions]
            Set.\\ mappedVersions
