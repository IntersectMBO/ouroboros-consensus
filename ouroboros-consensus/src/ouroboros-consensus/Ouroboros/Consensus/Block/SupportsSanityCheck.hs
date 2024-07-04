{-# LANGUAGE LambdaCase #-}
-- | This module adds support for sanity checking consensus configuration
--   on node startup. These checks should primarily look for unusual
--   configuration choices that may point to an accidentally-misconfigured node
--   and quietly cause problems, rather than incoherent configurations that will
--   result in fatal errors at a later point.
--
--   While in most situations they can be handled as fatal issues, there are
--   situations when intentionally configuring a node "weirdly" can be useful,
--   and so the user should be able to opt out of the sanity checks at their
--   own peril.
module Ouroboros.Consensus.Block.SupportsSanityCheck (
    BlockSupportsSanityCheck (..)
  , SanityCheckIssue (..)
  , checkSecurityParamConsistency
  , sanityCheckConfig
  ) where

import           Control.Exception
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (catMaybes)
import           Ouroboros.Consensus.Config (TopLevelConfig)
import           Ouroboros.Consensus.Config.SecurityParam

-- | An issue found in the 'TopLevelConfig' for a block. See 'displayException'
--   for human-readable descriptions of each of these cases, especially when
--   presenting these to users.
data SanityCheckIssue
  -- | Configuration contains multiple security parameters. This may cause
  --   strange behaviour around era boundaries.
  = InconsistentSecurityParam (NonEmpty SecurityParam)
  deriving (Show, Eq)

instance Exception SanityCheckIssue where
  displayException = \case
    InconsistentSecurityParam ks -> mconcat
      [ "InconsistentSecurityParam: "
      , "SecurityParams (K) were found to be inconsistent between constituent "
      , "eras of a HardForkBlock: "
      , show (NonEmpty.toList ks)
      ]

-- | 'BlockSupportsSanityCheck' provides evidence that a block can be sanity
--   checked for common issues on node startup. 'sanityCheckConfig', which runs
--   performs each check and returns a list with each 'SanityCheckIssue' found,
--   should be preferred over using these methods directly.
class BlockSupportsSanityCheck blk where

  -- | Generate a 'NonEmpty' list of security parameters for a given block type.
  --   For individual eras' block types, this is simply a singleton list
  --   containing the chosen 'SecurityParam', but combined block types (i.e.
  --   the 'HardForkCombinator') will return all of their constituent eras'
  --   configurations' security parameters.
  configAllSecurityParams
    :: TopLevelConfig blk
    -> NonEmpty SecurityParam

-- | Check a 'TopLevelConfig' for any inconsistency in constituent choices for
--   'SecurityParam' (colloquially @k@). For a block type to be considered
--   "sane" in this regard, its configuration's security parameter as well as
--   all of its childrens' configurations (if applicable) should be the same.
checkSecurityParamConsistency
  :: BlockSupportsSanityCheck blk
  => TopLevelConfig blk
  -> Maybe SanityCheckIssue
checkSecurityParamConsistency cfg = do
  let allParams = configAllSecurityParams cfg
  if allSame allParams
    then Nothing
    else Just (InconsistentSecurityParam allParams)

allSame :: Eq a => NonEmpty a -> Bool
allSame (x :| xs) = all (x ==) xs

-- | Run all supported sanity checks on a given 'TopLevelConfig'.
sanityCheckConfig
  :: BlockSupportsSanityCheck blk
  => TopLevelConfig blk
  -> [SanityCheckIssue]
sanityCheckConfig cfg =
  catMaybes [checkSecurityParamConsistency cfg]
