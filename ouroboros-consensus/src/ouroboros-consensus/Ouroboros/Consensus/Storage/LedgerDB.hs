{-# LANGUAGE PatternSynonyms #-}

-- | The Ledger DB is responsible for the following tasks:
--
-- - __Maintaining the in-memory ledger state at the tip__: When we try to
--     extend our chain with a new block fitting onto our tip, the block must
--     first be validated using the right ledger state, i.e., the ledger state
--     corresponding to the tip.
--
-- - __Maintaining the past \(k\) in-memory ledger states__: we might roll back
--     up to \(k\) blocks when switching to a more preferable fork. Consider the
--     example below:
--
--     <<docs/haddocks/ledgerdb-switch.svg>>
--
--     Our current chain's tip is \(C_2\), but the fork containing blocks
--     with tags \(F_1\), \(F_2\), and \(F_3\) is more preferable. We roll back
--     our chain to the intersection point of the two chains, \(I\), which must
--     be not more than \(k\) blocks back from our current tip. Next, we must
--     validate block \(F_1\) using the ledger state at block \(I\), after which
--     we can validate \(F_2\) using the resulting ledger state, and so on.
--
--     This means that we need access to all ledger states of the past \(k\)
--     blocks, i.e., the ledger states corresponding to the volatile part of the
--     current chain. Note that applying a block to a ledger state is not an
--     invertible operation, so it is not possible to simply /unapply/ \(C_1\)
--     and \(C_2\) to obtain \(I\).
--
--     Access to the last \(k\) ledger states is not only needed for validating
--     candidate chains, but also by the:
--
--     - __Local state query server__: To query any of the past \(k\) ledger
--       states.
--
--     - __Chain sync client__: To validate headers of a chain that intersects
--        with any of the past \(k\) blocks.
--
-- - __Storing snapshots on disk__: To obtain a ledger state for the current tip
--     of the chain, one has to apply /all blocks in the chain/ one-by-one to
--     the initial ledger state. When starting up the system with an on-disk
--     chain containing millions of blocks, all of them would have to be read
--     from disk and applied. This process can take hours, depending on the
--     storage and CPU speed, and is thus too costly to perform on each startup.
--
--     For this reason, a recent snapshot of the ledger state should be
--     periodically written to disk. Upon the next startup, that snapshot can be
--     read and used to restore the current ledger state, as well as the past
--     volatile \(k\) ledger states.
--
-- === __(image code)__
-- >>> import Image.LaTeX.Render
-- >>> import Control.Monad
-- >>> import System.Directory
-- >>>
-- >>> createDirectoryIfMissing True "docs/haddocks/"
-- >>> :{
-- >>> either (error . show) pure =<<
-- >>>  renderToFile "docs/haddocks/ledgerdb-switch.svg" defaultEnv (tikz ["positioning", "arrows"]) "\
-- >>> \ \\draw (0, 0) -- (50pt, 0) coordinate (I);\
-- >>> \  \\draw (I) -- ++(20pt,  20pt) coordinate (C1) -- ++(20pt, 0) coordinate (C2);\
-- >>> \  \\draw (I) -- ++(20pt, -20pt) coordinate (F1) -- ++(20pt, 0) coordinate (F2) -- ++(20pt, 0) coordinate (F3);\
-- >>> \  \\node at (I)  {$\\bullet$};\
-- >>> \  \\node at (C1) {$\\bullet$};\
-- >>> \  \\node at (C2) {$\\bullet$};\
-- >>> \  \\node at (F1) {$\\bullet$};\
-- >>> \  \\node at (F2) {$\\bullet$};\
-- >>> \  \\node at (F3) {$\\bullet$};\
-- >>> \  \\node at (I) [above left] {$I$};\
-- >>> \  \\node at (C1) [above] {$C_1$};\
-- >>> \  \\node at (C2) [above] {$C_2$};\
-- >>> \  \\node at (F1) [below] {$F_1$};\
-- >>> \  \\node at (F2) [below] {$F_2$};\
-- >>> \  \\node at (F3) [below] {$F_3$};\
-- >>> \  \\draw (60pt, 50pt) node {$\\overbrace{\\hspace{60pt}}$};\
-- >>> \  \\draw (60pt, 60pt) node[fill=white] {$k$};\
-- >>> \  \\draw [dashed] (30pt, -40pt) -- (30pt, 45pt);"
-- >>> :}
--

module Ouroboros.Consensus.Storage.LedgerDB (
    -- * LedgerDB
    Checkpoint (..)
  , LedgerDB (..)
  , LedgerDB'
  , LedgerDbCfg (..)
  , configLedgerDb
    -- * Initialization
  , InitLog (..)
  , ReplayStart (..)
  , initLedgerDB
    -- * Trace
  , ReplayGoal (..)
  , TraceReplayEvent (..)
  , decorateReplayTracerWithGoal
  , decorateReplayTracerWithStart
    -- * Querying
  , ledgerDbAnchor
  , ledgerDbCurrent
  , ledgerDbIsSaturated
  , ledgerDbMaxRollback
  , ledgerDbPast
  , ledgerDbSnapshots
  , ledgerDbTip
    -- * Updates
    -- ** Construct
  , ledgerDbWithAnchor
    -- ** Applying blocks
  , AnnLedgerError (..)
  , AnnLedgerError'
  , Ap (..)
  , ExceededRollback (..)
  , ThrowsLedgerError (..)
  , defaultThrowLedgerErrors
    -- ** Block resolution
  , ResolveBlock
  , ResolvesBlocks (..)
  , defaultResolveBlocks
    -- ** Operations
  , defaultResolveWithErrors
  , ledgerDbBimap
  , ledgerDbPrune
  , ledgerDbPush
  , ledgerDbSwitch
    -- ** Pure API
  , ledgerDbPush'
  , ledgerDbPushMany'
  , ledgerDbSwitch'
    -- ** Trace
  , PushGoal (..)
  , PushStart (..)
  , Pushing (..)
  , UpdateLedgerDbTraceEvent (..)
    -- * Snapshots
  , DiskSnapshot (..)
    -- ** Read from disk
  , SnapshotFailure (..)
  , diskSnapshotIsTemporary
  , listSnapshots
  , pattern DiskSnapshotChecksum
  , pattern NoDiskSnapshotChecksum
  , readSnapshot
    -- ** Write to disk
  , takeSnapshot
  , trimSnapshots
  , writeSnapshot
    -- ** Low-level API (primarily exposed for testing)
  , decodeSnapshotBackwardsCompatible
  , deleteSnapshot
  , encodeSnapshot
  , snapshotToFileName
  , snapshotToPath
    -- ** Trace
  , TraceSnapshotEvent (..)
    -- * Disk policy
  , DiskPolicy (..)
  , DiskPolicyArgs (..)
  , NumOfDiskSnapshots (..)
  , SnapshotInterval (..)
  , TimeSinceLast (..)
  , defaultDiskPolicyArgs
  , mkDiskPolicy
  ) where

import           Ouroboros.Consensus.Storage.LedgerDB.DiskPolicy
                     (DiskPolicy (..), DiskPolicyArgs (..),
                     NumOfDiskSnapshots (..), SnapshotInterval (..),
                     TimeSinceLast (..), defaultDiskPolicyArgs, mkDiskPolicy,
                     pattern DiskSnapshotChecksum,
                     pattern NoDiskSnapshotChecksum)
import           Ouroboros.Consensus.Storage.LedgerDB.Init (InitLog (..),
                     ReplayGoal (..), ReplayStart (..), TraceReplayEvent (..),
                     decorateReplayTracerWithGoal,
                     decorateReplayTracerWithStart, initLedgerDB)
import           Ouroboros.Consensus.Storage.LedgerDB.LedgerDB (Checkpoint (..),
                     LedgerDB (..), LedgerDB', LedgerDbCfg (..), configLedgerDb)
import           Ouroboros.Consensus.Storage.LedgerDB.Query (ledgerDbAnchor,
                     ledgerDbCurrent, ledgerDbIsSaturated, ledgerDbMaxRollback,
                     ledgerDbPast, ledgerDbSnapshots, ledgerDbTip)
import           Ouroboros.Consensus.Storage.LedgerDB.Snapshots
                     (DiskSnapshot (..), SnapshotFailure (..),
                     TraceSnapshotEvent (..), decodeSnapshotBackwardsCompatible,
                     deleteSnapshot, diskSnapshotIsTemporary, encodeSnapshot,
                     listSnapshots, readSnapshot, snapshotToFileName,
                     snapshotToPath, takeSnapshot, trimSnapshots, writeSnapshot)
import           Ouroboros.Consensus.Storage.LedgerDB.Update
                     (AnnLedgerError (..), AnnLedgerError', Ap (..),
                     ExceededRollback (..), PushGoal (..), PushStart (..),
                     Pushing (..), ResolveBlock, ResolvesBlocks (..),
                     ThrowsLedgerError (..), UpdateLedgerDbTraceEvent (..),
                     defaultResolveBlocks, defaultResolveWithErrors,
                     defaultThrowLedgerErrors, ledgerDbBimap, ledgerDbPrune,
                     ledgerDbPush, ledgerDbPush', ledgerDbPushMany',
                     ledgerDbSwitch, ledgerDbSwitch', ledgerDbWithAnchor)
