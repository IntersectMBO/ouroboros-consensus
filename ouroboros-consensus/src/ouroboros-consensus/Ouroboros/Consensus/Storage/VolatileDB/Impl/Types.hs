{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module Ouroboros.Consensus.Storage.VolatileDB.Impl.Types
  ( -- * Blocks per file
    mkBlocksPerFile
  , unBlocksPerFile

    -- ** opaque
  , BlocksPerFile

    -- * Block validation policy
  , BlockValidationPolicy (..)

    -- * Parse error
  , ParseError (..)

    -- * Tracing
  , TraceEvent (..)

    -- * Internal indices
  , BlockOffset (..)
  , BlockSize (..)
  , FileId
  , InternalBlockInfo (..)
  , LeiosAnnouncerIndex (..)
  , ReverseIndex
  , SuccessorsIndex
  ) where

import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Word (Word32, Word64)
import GHC.Generics (Generic)
import LeiosDemoTypes (EbHash)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Storage.VolatileDB.API (BlockInfo)
import Ouroboros.Consensus.Util.CBOR (ReadIncrementalErr (..))
import System.FS.API.Types (FsPath)

{------------------------------------------------------------------------------
  Blocks per file
------------------------------------------------------------------------------}

-- | The maximum number of blocks to store per file.
newtype BlocksPerFile = BlocksPerFile {unBlocksPerFile :: Word32}
  deriving (Generic, Show)

-- | Create a 'BlocksPerFile'.
--
-- PRECONDITION: the given number must be greater than 0, if not, this
-- function will throw an 'error'.
mkBlocksPerFile :: Word32 -> BlocksPerFile
mkBlocksPerFile 0 = error "BlocksPerFile must be positive"
mkBlocksPerFile n = BlocksPerFile n

{------------------------------------------------------------------------------
  Block validation policy
------------------------------------------------------------------------------}

-- | When block validation is enabled, the parser checks for each block a
-- number of properties and stops parsing if it finds any invalid blocks.
data BlockValidationPolicy
  = NoValidation
  | ValidateAll
  deriving Eq

{------------------------------------------------------------------------------
  Parse error
------------------------------------------------------------------------------}

-- | Note that we recover from the error, and thus never throw it as an
-- 'Exception'.
--
-- Defined here instead of in the @Parser@ module because 'TraceEvent' depends
-- on it.
data ParseError blk
  = -- | A block could not be parsed.
    BlockReadErr ReadIncrementalErr
  | -- | A block was corrupted, e.g., checking its signature and/or hash
    -- failed.
    BlockCorruptedErr (HeaderHash blk)
  | -- | A block with the same hash occurred twice in the VolatileDB files.
    --
    -- We include the file in which it occurred first and the file in which it
    -- occured the second time. The two files can be the same.
    DuplicatedBlock (HeaderHash blk) FsPath FsPath

deriving instance StandardHash blk => Eq (ParseError blk)
deriving instance StandardHash blk => Show (ParseError blk)

{------------------------------------------------------------------------------
  Tracing
------------------------------------------------------------------------------}

data TraceEvent blk
  = DBAlreadyClosed
  | BlockAlreadyHere (HeaderHash blk)
  | Truncate (ParseError blk) FsPath BlockOffset
  | InvalidFileNames [FsPath]
  | DBClosed
  deriving (Eq, Generic, Show)

{------------------------------------------------------------------------------
  Internal indices
------------------------------------------------------------------------------}

-- | The 'FileId' is the unique identifier of each file found in the db.
-- For example, the file @blocks-42.dat@ has 'FileId' @42@.
type FileId = Int

-- | We map the header hash of each block to the 'InternalBlockInfo'.
type ReverseIndex blk = Map (HeaderHash blk) (InternalBlockInfo blk)

-- | For each block, we store the set of all blocks which have this block as
-- a predecessor (set of successors).
type SuccessorsIndex blk = Map (ChainHash blk) (Set (HeaderHash blk))

-- | For each announced endorser block (EB, by hash), the 'Point's of the blocks
-- (RBs) in the VolatileDB that announce it. Not a bijection: many RBs may
-- announce the same EB. Used by ChainSel to find the RBs to reconsider once an
-- EB's closure has been acquired, and to prune the acquired-EB set
-- ('cdbAcquiredLeiosEbs') by announcer slot. The announcers are 'Point's (not
-- bare hashes) so a consumer can filter them by slot -- e.g. against the
-- immutable tip -- without a separate 'getBlockInfo' lookup. Keyed by 'EbHash'
-- (not 'LeiosPoint') to match the acquired-EB set, which is what drives
-- reprocessing.
--
-- TODO(EbAnnouncement): see the note on 'biLeiosAnnouncedEb' — this index
-- should likely move out of the VolatileDB once the EbAnnouncement data flow
-- exists.
newtype LeiosAnnouncerIndex blk = LeiosAnnouncerIndex
  { getLeiosAnnouncerIndex :: Map EbHash (Set (Point blk))
  }
  deriving (Generic, NoThunks)

newtype BlockSize = BlockSize {unBlockSize :: Word32}
  deriving (Eq, Show, Generic, NoThunks)

-- | The offset at which a block is stored in a file.
newtype BlockOffset = BlockOffset {unBlockOffset :: Word64}
  deriving (Eq, Show, Generic, NoThunks)

-- | The internal information the db keeps for each block.
data InternalBlockInfo blk = InternalBlockInfo
  { ibiFile :: !FsPath
  , ibiBlockOffset :: !BlockOffset
  , ibiBlockSize :: !BlockSize
  , ibiBlockInfo :: !(BlockInfo blk)
  , ibiNestedCtxt :: !(SomeSecond (NestedCtxt Header) blk)
  }
  deriving (Generic, NoThunks)
