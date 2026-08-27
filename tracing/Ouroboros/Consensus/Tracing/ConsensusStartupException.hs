{-# LANGUAGE OverloadedStrings #-}

-- | Consensus startup exception
module Ouroboros.Consensus.Tracing.ConsensusStartupException
  ( ConsensusStartupException (..)
  ) where

import           Cardano.Logging.Types
import           Control.Exception (SomeException)
import           Data.Aeson (Value (String), (.=))
import qualified Data.Text as Text

-- | Exceptions logged when the consensus is initialising.
--
newtype ConsensusStartupException = ConsensusStartupException SomeException
  deriving Show

instance LogFormatting ConsensusStartupException where
  forMachine _ (ConsensusStartupException err) =
    mconcat [ "kind" .= String "ConsensusStartupException"
            , "error" .= String (Text.pack . show $ err)
            ]
  forHuman = Text.pack . show

instance MetaTrace ConsensusStartupException where
  namespaceFor ConsensusStartupException {} = Namespace [] ["ConsensusStartupException"]

  severityFor (Namespace _ ["ConsensusStartupException"]) _ = Just Error
  severityFor _ _ = Nothing

  documentFor (Namespace _ ["ConsensusStartupException"]) = Just
    "An exception was thrown while the Consensus layer was starting up. The node\
    \ does not come up when this is traced."
  documentFor _ = Nothing

  allNamespaces = [Namespace [] ["ConsensusStartupException"]]
