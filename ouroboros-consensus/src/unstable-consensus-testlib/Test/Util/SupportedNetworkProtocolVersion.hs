{-# LANGUAGE ScopedTypeVariables #-}

module Test.Util.SupportedNetworkProtocolVersion (exhaustiveSupportedNetworkProtocolVersions) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy
import qualified Data.Set as Set
import Data.Typeable
import Ouroboros.Consensus.Node.NetworkProtocolVersion
import Test.Tasty.HUnit

-- | Make sure that 'supportedNodeToNodeVersions' and
-- 'supportedNodeToClientVersions' contain entries for all 'NodeToNodeVersion's
-- and 'NodeToClientVersion', respectively.
exhaustiveSupportedNetworkProtocolVersions ::
  forall blk.
  (Typeable blk, SupportedNetworkProtocolVersion blk) =>
  Proxy blk ->
  Assertion
exhaustiveSupportedNetworkProtocolVersions p = do
  testVersions supportedNodeToNodeVersions
  testVersions supportedNodeToClientVersions
 where
  testVersions ::
    (Show v, Ord v, Enum v, Bounded v) =>
    (Proxy blk -> Map v a) ->
    Assertion
  testVersions f =
    assertBool
      ( "unmapped versions for "
          <> show (typeRep p)
          <> ": "
          <> show (Set.toList unmappedVersions)
      )
      (Set.null unmappedVersions)
   where
    unmappedVersions = allVersions Set.\\ mappedVersions
    allVersions = Set.fromList [minBound .. maxBound]
    mappedVersions = Map.keysSet $ f p
