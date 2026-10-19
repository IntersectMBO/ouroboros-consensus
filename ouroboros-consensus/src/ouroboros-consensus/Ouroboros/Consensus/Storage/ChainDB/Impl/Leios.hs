{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | What the ChainDB knows about Leios endorser blocks.
module Ouroboros.Consensus.Storage.ChainDB.Impl.Leios
  ( -- * The Leios facts of a block
    leiosBlockInfo
  ) where

import Data.Maybe.Strict (maybeToStrictMaybe)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Leios.Protocol
  ( LeiosAnnouncement (..)
  , LeiosProtocol (..)
  )
import Ouroboros.Consensus.Storage.VolatileDB (LeiosBlockInfo (..))

{-------------------------------------------------------------------------------
  The Leios facts of a block
-------------------------------------------------------------------------------}

-- | What the block says about Leios endorser blocks, read off its header.
leiosBlockInfo ::
  forall blk.
  ( BlockSupportsProtocol blk
  , LeiosProtocol (BlockProtocol blk)
  ) =>
  BlockConfig blk ->
  blk ->
  LeiosBlockInfo
leiosBlockInfo bcfg blk =
  LeiosBlockInfo
    { lbiHasCert = getContainsLeiosCert p view
    , lbiAnnouncedEb =
        maybeToStrictMaybe $ announcementEbHash <$> getLeiosAnnouncement p view
    }
 where
  p = Proxy @(BlockProtocol blk)
  view = validateView bcfg (getHeader blk)
