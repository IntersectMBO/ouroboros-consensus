-- | The storage layer is a highly specialized database for storing the
--   blockchain. It consists of five subcomponents:
--
-- * An abstract file system API, 'System.FS.API.HasFS',
--   that smooths out over some differences between the file systems of
--   different operating systems and, more importantly, allows us to simulate
--   all kinds of failures. This is then used for stress-testing the other
--   components below.
--
-- * The __[Immutable DB]("Ouroboros.Consensus.Storage.ImmutableDB")__, stores
--   the part of the chain that is immutable, that is, no longer subject to
--   rollback. It is an append-only database, providing efficient access to the
--   chain. 'Ouroboros.Consensus.Storage.ImmutableDB.API.ImmutableDB' defines
--   the immutable DB API.
--
-- * The __[Volatile DB]("Ouroboros.Consensus.Storage.VolatileDB")__, stores the
--   part of the chain near its tip. This doesn't really store a __chain__ as
--   such, but rather simply a collection of blocks from which we might
--   __construct__ a chain.
--   'Ouroboros.Consensus.Storage.VolatileDB.API.VolatileDB' defines the
--   volatile DB API.
--
-- * The __[Ledger DB]("Ouroboros.Consensus.Storage.LedgerDB")__, stores the
--   \(k\) last ledger states corresponding to the blocks on the current chain
--   (which are part of the volatile DB), and means to read
--   'Ouroboros.Consensus.Ledger.Tables.Basics.LedgerTables' for
--   them. 'Ouroboros.Consensus.Storage.LedgerDB.LedgerDB' defines the ledger DB
--   API.
--
-- * The Chain DB finally combines all of these components. It makes decisions
--   about which chains to adopt (chain selection), switches to forks when
--   needed, deals with clock skew, and provides various interfaces to the rest
--   of the consensus layer for things like finding out which blocks were
--   invalid (so we can disconnect from the clients who sent them), cursors that
--   follow the tip of the chain (so that we can inform our downstream peers of
--   how our chain evolves), etc. In many ways, the chain DB is the component
--   that is responsible for "consensus": deciding which chain is the one true
--   chain. 'Ouroboros.Consensus.Storage.ChainDB.API.ChainDB' defines the chain
--   DB API.
--
-- == Resource Management in the ChainDB
--
-- Clients of the ChainDB can allocate resources from the databases
-- it contains (LedgerDB, VolatileDB, and ImmutableDB):
--
-- - The LedgerDB is used to create 'Forker's.
--
-- - The ChainDB is used to create 'Follower's (which in turn contain
--   'Iterator's).
--
-- These resources must eventually be freed. See function
-- 'Ouroboros.Consensus.Node.runWith' for an example of an approach
-- taken to resource management using a resource registry.
--
module Ouroboros.Consensus.Storage.ChainDB
  ( module Ouroboros.Consensus.Storage.ChainDB.API
  , module Ouroboros.Consensus.Storage.ChainDB.Impl
  ) where

import Ouroboros.Consensus.Storage.ChainDB.API
import Ouroboros.Consensus.Storage.ChainDB.Impl
