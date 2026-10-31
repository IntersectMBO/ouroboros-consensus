{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

-- | A ChainDB of 'LeiosTestBlock's, and the LeiosDB writes a test needs to
-- give it the EBs its CertRBs certify.
module Test.LeiosTestNode
  ( -- * The node
    Node (..)
  , withNode
  , testCommittee

    -- * Certificates
  , certFor
  , claimOf

    -- * Adding blocks
  , addAtGenesis
  , addBlockWith
  , addWithCommittee

    -- * The LeiosDB
  , storeEb
  ) where

import Control.Monad (void)
import Control.ResourceRegistry (forkLinkedThread, withRegistry)
import Control.Tracer (nullTracer)
import Data.ByteString (ByteString)
import qualified Data.IntMap.Strict as IntMap
import LeiosDemoTypes (BytesSize, LeiosCert, LeiosEb, LeiosPoint, RbHash (..))
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.BlockchainTime (slotLengthFromSec)
import Ouroboros.Consensus.Config (SecurityParam (..))
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import Ouroboros.Consensus.Storage.ChainDB.API (ChainDB, Predecessor (..))
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import qualified Ouroboros.Consensus.Storage.ChainDB.API.Types.InvalidBlockPunishment as Punishment
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl as ChainDBImpl
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.Args as ChainDB
import Ouroboros.Consensus.Storage.ImmutableDB (simpleChunkInfo)
import Ouroboros.Consensus.Storage.LeiosDB
  ( LeiosDbHandle
  , LeiosDbWriter (..)
  , await
  , withWriter
  )
import Ouroboros.Consensus.Util.IOLike
import System.FS.Sim.MockFS (MockFS)
import Test.Cardano.Crypto.Leios.Gen (TestCommittee (..), genCommittee)
import Test.LeiosValidClaims (mkCert, wholeCommittee)
import Test.QuickCheck.Gen (unGen)
import Test.QuickCheck.Random (mkQCGen)
import Test.Util.ChainDB
import Test.Util.LeiosTestBlock

type Blk = LeiosTestBlock

{-------------------------------------------------------------------------------
  The node
-------------------------------------------------------------------------------}

data Node m = Node
  { nChainDB :: ChainDB m Blk
  , nInternal :: ChainDBImpl.Internal m Blk
  }

-- | One committee for every epoch of every test. Drawn from a fixed seed, so
-- tests using it are unit tests rather than properties.
testCommittee :: TestCommittee
testCommittee = unGen genCommittee (mkQCGen 42) 10

-- | Open a ChainDB with security parameter @k@ over the given mocked
-- filesystems and LeiosDB, run the body, and close it. Calling this twice over
-- the same 'NodeDBs' is a node restart.
--
-- The ChainDB's background tasks are not started, so the test decides when
-- blocks become immutable ('ChainDBImpl.intCopyToImmutableDB').
withNode ::
  IOLike m =>
  SecurityParam ->
  NodeDBs (StrictTMVar m MockFS) ->
  LeiosDbHandle m ->
  (Node m -> m a) ->
  m a
withNode securityParam mcdbNodeDBs mcdbLeiosDb body =
  withRegistry $ \registry -> do
    let mcdbTopLevelConfig = singleNodeLeiosTestConfig ledgerConfig securityParam
        mcdbChunkInfo = simpleChunkInfo (HardFork.eraEpochSize eraParams)
        mcdbInitLedger = leiosTestInitExtLedger ledgerConfig IntMap.empty
        mcdbRegistry = registry
        args =
          ChainDB.updateTracer nullTracer $
            fromMinimalChainDbArgs MinimalChainDbArgs{..}
    bracket
      (ChainDBImpl.openDBInternal args False)
      (ChainDB.closeDB . fst)
      $ \(nChainDB, nInternal) -> do
        _ <-
          forkLinkedThread registry "AddBlockRunner" $
            ChainDBImpl.intAddBlockRunner nInternal
        body Node{nChainDB, nInternal}
 where
  eraParams =
    HardFork.defaultEraParams securityParam (slotLengthFromSec 1) HardFork.NoPerasEnabled
  ledgerConfig =
    leiosTestLedgerConfig
      eraParams
      -- Generously wide: these tests are not about the forecast horizon.
      100
      (const (Just (testCommittee.committee, wholeCommittee)))

{-------------------------------------------------------------------------------
  Certificates
-------------------------------------------------------------------------------}

-- | The claim a CertRB extending this block makes: that this block announced
-- the EB the CertRB certifies.
claimOf :: Blk -> RbHash
claimOf blk = MkRbHash $ toRawHash (Proxy @Blk) (blockHash blk)

-- | A certificate from 'testCommittee' for 'claimOf' this block, which a
-- CertRB extending it carries.
certFor :: Blk -> LeiosCert
certFor = mkCert testCommittee . claimOf

{-------------------------------------------------------------------------------
  Adding blocks
-------------------------------------------------------------------------------}

-- | Add a block the way BlockFetch would: with the ledger view forecast at its
-- predecessor's slot.
addWithCommittee :: IOLike m => Node m -> Blk -> Blk -> m ()
addWithCommittee node predecessor blk =
  addBlockWith
    node
    ( Predecessor
        (blockSlot predecessor)
        (LeiosTestView (Just (testCommittee.committee, wholeCommittee)) maxBound)
    )
    blk

addBlockWith :: IOLike m => Node m -> Predecessor Blk -> Blk -> m ()
addBlockWith Node{nChainDB} predecessor =
  ChainDB.addBlock_ nChainDB Punishment.noPunishment predecessor

-- | Add a block that extends genesis.
addAtGenesis :: IOLike m => Node m -> Blk -> m ()
addAtGenesis node = addBlockWith node NoPredecessor

{-------------------------------------------------------------------------------
  The LeiosDB
-------------------------------------------------------------------------------}

-- | Store an EB, announced at the given size, and all of its transactions, in
-- the EB's order.
storeEb :: IOLike m => LeiosDbHandle m -> LeiosPoint -> LeiosEb -> BytesSize -> [ByteString] -> m ()
storeEb db point eb size txs =
  withWriter db $ \writer -> do
    void $ await =<< writeEbPoint writer point size
    void $ await =<< writeEbBody writer point eb []
    void $ await =<< writeTxs writer point (zip [0 ..] txs)
