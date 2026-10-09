{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Ouroboros.Consensus.Shelley.Protocol.EnvelopeChecks
  ( EnvelopeError (..)
  , EnvelopeHeaderView (..)
  , envelopeCheck
  ) where

import Cardano.Ledger.BaseTypes (Version)
import Cardano.Ledger.Chain (ChainChecksPParams (ccMaxBBSize, ccMaxBHSize))
import Control.Monad (unless)
import Control.Monad.Except (Except, throwError)
import Data.Word (Word16, Word32)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)

data EnvelopeError
  = -- | This is a subtle case.
    --
    -- This node is explicitly rejecting the header, but the header isn't
    -- necessarily _directly_ at fault.
    --
    -- This rejection specifically happens when the ticked ledger state being
    -- used to validate this header contains a protocol major version (the
    -- first 'Version') that exceeds the maximum major protocol version allowed
    -- for this era this specific node's configuration (the second 'Version').
    -- The only thing the header did "wrong" was extend such a ledger state.
    --
    -- Note that the ChainSync client ensures that that ledger state is ticked
    -- starting from one of the latest k+1 ledger states on the node's current
    -- chain (modulo STM scheduling).
    --
    -- For Cardano and for now at least, this max major prot ver is typically
    -- hardcoded in the source code (subject only to whether or not the
    -- run-time config files enable "experimental" eras).
    --
    -- Hence, most likely, the appropriate rectifying action is for the node
    -- operator to update their node software and/or config; hence the name
    -- 'ObsoleteNode'. (Or if they're intentionally testing an experimental
    -- era, they forgot to set the appropriate config flag.)
    --
    -- TODO Would it be more intuitive to instead enforce this when validating
    -- the block that results in a ledger state with a major prot ver that
    -- violates the config's limit? Would the errors the user sees be more or
    -- less helpful? Etc.
    --
    -- TODO (cont'd) It's not even obviously that specific ledger
    -- state's/block's fault, since the protocol version is the consequence of
    -- on-chain governance. Is it the voters' fault? Is the fault of the first
    -- block that was after the voting deadline? So "extending the ledger state
    -- that resulting from ticking after applying the block after the epoch
    -- that extended the ancestor block that was after the voting deadline that
    -- ..." is merely one step more removed. And this 'envelopeChecks' approach
    -- does avoid the surprise (since the rejection doesn't even depend on the
    -- block's non-header content either) where the header could be validated
    -- but its underlying block could not. See
    -- <https://github.com/IntersectMBO/ouroboros-consensus/issues/325>.
    ObsoleteNode !Version !Version
  | HeaderSizeTooLarge !Int !Word16
  | BlockSizeTooLarge !Word32 !Word32
  deriving (Eq, Generic, Show)

instance NoThunks EnvelopeError

data EnvelopeHeaderView = EnvelopeHeaderView
  { ehvProtVer :: !Version
  -- ^ The version against which to compare the node's max.
  , ehvHeaderSize :: !Int
  , ehvBodySize :: !Word32
  }

-- | Shared envelope-check logic between Praos and TPraos.
--   'ehvProtVer' is the block header declared protocol version in TPraos, and
--   the ledger view's protocol version in Praos - see docs for EnvelopeError
envelopeCheck ::
  Version ->
  ChainChecksPParams ->
  EnvelopeHeaderView ->
  Except EnvelopeError ()
envelopeCheck maxpv ccd EnvelopeHeaderView{ehvProtVer, ehvHeaderSize, ehvBodySize} = do
  unless (ehvProtVer <= maxpv) $
    throwError $
      ObsoleteNode ehvProtVer maxpv
  unless (ehvHeaderSize <= fromIntegral @Word16 @Int (ccMaxBHSize ccd)) $
    throwError $
      HeaderSizeTooLarge ehvHeaderSize (ccMaxBHSize ccd)
  unless (ehvBodySize <= ccMaxBBSize ccd) $
    throwError $
      BlockSizeTooLarge ehvBodySize (ccMaxBBSize ccd)
