{-# LANGUAGE DeriveAnyClass           #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE DeriveTraversable        #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE ViewPatterns             #-}

{-# OPTIONS_GHC -Wno-orphans -Wno-partial-fields #-}

module Test.Ouroboros.Storage.ImmutableDB.Index.StateMachine (tests) where

import           Control.Monad
import           Control.Tracer
import qualified Data.ByteString as BS
import           Data.IORef
import           Data.Kind
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Sequence.Strict (StrictSeq)
import           Data.TreeDiff.Class
import           Data.TreeDiff.Expr
import           Data.Typeable
import           GHC.Generics
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Storage.ImmutableDB.API
import           Ouroboros.Consensus.Storage.ImmutableDB.Chunks
import           Ouroboros.Consensus.Storage.ImmutableDB.Chunks.Internal
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index.Secondary hiding
                     (appendEntry, readAllEntries, readEntries)
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.Util
                     (fsPathChunkFile, fsPathPrimaryIndexFile,
                     fsPathSecondaryIndexFile)
import           Ouroboros.Consensus.Util.MonadSTM.NormalForm
import           Ouroboros.Consensus.Util.ResourceRegistry
import           System.FS.API.Lazy
import           System.FS.IO
import           System.IO.Temp
import           Test.Ouroboros.Storage.TestBlock
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Test.StateMachine
import qualified Test.StateMachine.Types as QSM
import qualified Test.StateMachine.Types.Rank2 as Rank2
import           Test.Tasty
import           Test.Tasty.QuickCheck

data Two a = Two { one :: a, two :: a }
  deriving (Generic, Show, Functor, Foldable, Traversable)

deriving instance ToExpr a => ToExpr (Two a)

{-------------------------------------------------------------------------------
  Model
-------------------------------------------------------------------------------}

type Model :: Type -> (Type -> Type) -> Type
data Model blk r =
    UnInitModel
  | Model {
        chunks     :: Map ChunkNo [SecondaryOffset]
      , currHandle :: Maybe (Reference (Opaque (Two (Handle HandleIO))) r)
      , testDir    :: Maybe FilePath
        -- | There can only be one thread writing when runnning on parallel.
        -- This ensures 'AppendOffsets'/'OpenPrimaryIndex' is not in in both
        -- parallel branches.
      , writing    :: Bool
      }
  deriving (Generic, Show)

deriving instance ToExpr (Model TestBlock Symbolic)
deriving instance ToExpr (Model TestBlock Concrete)
instance ToExpr (Symbolic (Opaque v)) where
  toExpr _ = App "<OPAQUE>" []

{-------------------------------------------------------------------------------
  Command
-------------------------------------------------------------------------------}

type Command :: Type -> (Type -> Type) -> Type
data Command blk r =

    Init

    -- | We will read the last offset known in 'tipSVar' which models the query to
    -- 'ImmutableDB.getTip'.
  | ReadTipOffset

    -- | Open a new primary index
  | OpenPrimaryIndex
      ChunkNo

    -- | Append some offsets to the latest primary index, sometimes it will be a
    -- consecutive offset, fewer times it will be a one-slot backfilled offset.
  | AppendOffsets
      (Reference (Opaque (Two (Handle HandleIO))) r)
      [SecondaryOffset]

  deriving (Generic, Show)

deriving instance Generic1          (Command blk)
deriving instance Rank2.Foldable    (Command blk)
deriving instance Rank2.Functor     (Command blk)
deriving instance Rank2.Traversable (Command blk)
deriving instance CommandNames      (Command blk)

{-------------------------------------------------------------------------------
  Response
-------------------------------------------------------------------------------}

type Response :: Type -> (Type -> Type) -> Type
data Response blk r =

    RInit FilePath

  | RReadTipOffset
      (Two ([Maybe SecondaryOffset], Maybe (StrictSeq SecondaryOffset)))

  | ROpenPrimaryIndex
      (Reference (Opaque (Two (Handle HandleIO))) r)

  | RAppendOffsets

  deriving (Generic, Show)

deriving instance Generic1          (Response blk)
deriving instance Rank2.Foldable    (Response blk)
deriving instance Rank2.Functor     (Response blk)
deriving instance Rank2.Traversable (Response blk)

-- | A expected undefined
die :: a
die = error "die: Impossible"

defaultChunkSize :: ChunkSize
defaultChunkSize = ChunkSize False 100

defaultChunkInfo :: ChunkInfo
defaultChunkInfo = simpleChunkInfo (EpochSize 100)

{-------------------------------------------------------------------------------
  State machine functions
-------------------------------------------------------------------------------}

transition ::
     Model    TestBlock r
  -> Command  TestBlock r
  -> Response TestBlock r
  -> Model    TestBlock r
transition UnInitModel cmd resp =
  case (cmd, resp) of
    (Init, RInit f) -> Model (Map.singleton (ChunkNo 0) []) Nothing (Just f) False
    _               -> die
transition model@Model{chunks} cmd resp =
  case (cmd, resp) of
    (ReadTipOffset{}      ,  _)                   -> model { writing = False }
    (OpenPrimaryIndex cn,  ROpenPrimaryIndex h) -> model {
        chunks     = maybe (Map.insert cn ([0]) chunks) (const chunks) (Map.lookup cn chunks)
      , currHandle = Just h
      , writing    = True
      }
    (AppendOffsets _ offs, RAppendOffsets)      -> model {
        chunks  = Map.updateMax (\x -> Just (x ++ offs)) chunks
      , writing = True
      }
    _                                           -> die

precondition ::
     Model   TestBlock Symbolic
  -> Command TestBlock Symbolic
  -> Logic
precondition UnInitModel Init = Top
precondition Model{} ReadTipOffset = Top
precondition Model{chunks, writing} (OpenPrimaryIndex cn) =
  cn `notMember` Map.keysSet chunks .&& Not (Boolean writing)
precondition Model{currHandle, writing} AppendOffsets{} =
  Not (Boolean (isNothing currHandle)) .&& Not (Boolean writing)
precondition _ _ = Bot

postcondition ::
     Model    TestBlock Concrete
  -> Command  TestBlock Concrete
  -> Response TestBlock Concrete
  -> Logic
postcondition UnInitModel Init RInit{} = Top
postcondition Model{} ReadTipOffset (RReadTipOffset (Two (fst -> a) (fst -> b))) =
   (a .== b) .|| Boolean (and (zipWith (\x y -> x == Nothing || x == y) a b))
postcondition Model{} OpenPrimaryIndex{} ROpenPrimaryIndex{} = Top
postcondition Model{} AppendOffsets{} RAppendOffsets = Top
postcondition _ _ _ = Bot

generator :: Model TestBlock s -> Maybe (Gen (Command TestBlock s))
generator UnInitModel = Just $ pure Init
generator Model{chunks, currHandle} =
  if Map.null chunks
  then Just (OpenPrimaryIndex <$> pure (ChunkNo 0))
  else
    let genReadTipOffset =
          pure ReadTipOffset

        -- Open next chunk
        genOpenPrimaryIndex =
          OpenPrimaryIndex <$> (pure $ (ChunkNo . (+1) . unChunkNo) $ fst $ Map.findMax chunks)

        -- Few times add spaced offsets, more times add consecutive offsets
        genAppendOffsets h =
          let cn = fst $ Map.findMax chunks
              offs = chunks Map.! cn
          in frequency [(1, pure $ AppendOffsets h [last offs, last offs + entrySize (Proxy @TestBlock)])
                       , (3, pure $ AppendOffsets h [last offs + entrySize (Proxy @TestBlock)])]
    in Just $ oneof $ [ genReadTipOffset
                      , genOpenPrimaryIndex
                      ]
                      ++
                      maybe [] (\x -> [genAppendOffsets x]) currHandle

shrinker ::
      Model   TestBlock Symbolic
  ->  Command TestBlock Symbolic
  -> [Command TestBlock Symbolic]
shrinker _ _ = []

-- | Touch a new chunk, primary and secondary files
touchChunk :: Two (HasFS IO HandleIO) -> ChunkNo -> Bool -> IO ()
touchChunk fs cn mkPrimary = do
  void
    $ traverse
      (\f -> withFile f (fsPathChunkFile cn) (AppendMode MustBeNew) $ \_ -> pure ())
      fs

  -- Only when creating the chunk 00000 we have to touch this file. In the other
  -- invocations the Index will create it.
  when mkPrimary
    $ void
    $ traverse
      (\f -> withFile f (fsPathPrimaryIndexFile cn) (AppendMode MustBeNew) $ \h ->
                hPutSome (one fs) h (BS.pack [1,0,0,0,0]))
      fs

  void
    $ traverse
      (\f -> withFile f (fsPathSecondaryIndexFile cn) (AppendMode MustBeNew) $ \_ -> pure ())
      fs

semantics ::
     ( ConvertRawHash blk
     , StandardHash blk
     , Typeable blk
     )
  => Env blk
  -> Command blk Concrete
  -> IO (Response blk Concrete)
semantics env Init = do
  let fileIndex = fileBackedIndex (one $ fs env) defaultChunkInfo

  touchChunk (fs env) (ChunkNo 0) True

  cacheIndex <- cachedIndex (two $ fs env) (registry env) nullTracer (CacheConfig maxBound 100000000) defaultChunkInfo firstChunkNo
  atomicWriteIORef (ref env) (Two fileIndex cacheIndex)
  pure (RInit $ envTestDir env)
semantics Env{ref, tipSVar} ReadTipOffset = do
  Two a b <- readIORef ref
  -- Read the latest known offset
  (cn, t) <- readSVar tipSVar
  r1 <- readOffsets a cn [t]
  r2 <- readOffsets b cn [t]
  pure $ RReadTipOffset $ Two r1 r2
semantics Env{fs, ref, tipSVar} (OpenPrimaryIndex cn) = do
  touchChunk fs cn False
  Two a b <- readIORef ref
  h1 <- openPrimaryIndex a cn MustBeNew
  h2 <- openPrimaryIndex b cn MustBeNew
  -- Update the latest known offset
  void $ swapSVar tipSVar (cn, RelativeSlot cn defaultChunkSize 0)
  pure $ ROpenPrimaryIndex (reference $ Opaque $ Two h1 h2)
semantics Env{ref, tipSVar} (AppendOffsets (unOpaque . concrete -> Two h1 h2) off) = do
  Two a b <- readIORef ref
  appendOffsets a h1 off
  appendOffsets b h2 off
  -- Update the latest known offset
  updateSVar_ tipSVar
    (\(cn, RelativeSlot _ cs n) -> (cn, RelativeSlot cn cs (n + fromIntegral (length off))))
  pure RAppendOffsets

mock ::
     Model TestBlock Symbolic
  -> Command TestBlock Symbolic
  -> GenSym (Response TestBlock Symbolic)
mock UnInitModel Init = pure (RInit "whatever")
mock Model{chunks} (ReadTipOffset) =
  pure $
   case Map.lookupMax chunks of
     Nothing -> die
     Just (_, vec) ->
       let res = [Just $ last vec]
       in RReadTipOffset (Two (res, Nothing) (res, Nothing))
mock Model{} OpenPrimaryIndex{} =
  ROpenPrimaryIndex <$> genSym
mock Model{} AppendOffsets{} =
  pure RAppendOffsets
mock _ _ = die

data Env blk = Env {
    registry   :: ResourceRegistry IO
  , fs         :: Two (HasFS IO HandleIO)
  , ref        :: IORef (Two (Index IO blk HandleIO))
  , envTestDir :: FilePath
    -- | Equivalent to the 'OpenState' in the ImmutableDB, from which we get the
    -- current last known block via 'ImmutableDB.getTip'
  , tipSVar    :: StrictSVar IO (ChunkNo, RelativeSlot)
  }

newEnv :: IO (Env blk)
newEnv = do
  -- make ./temp directory
  let fs0 = ioHasFS $ MountPoint "."
  createDirectoryIfMissing fs0 True $ mkFsPath ["temp"]
  -- make this test temporal directory, with file-backed and cached subdirs
  d <- createTempDirectory "temp" "temp"
  let fs1 = ioHasFS $ MountPoint d
  createDirectoryIfMissing fs1 True $ mkFsPath ["file-backed"]
  createDirectoryIfMissing fs1 True $ mkFsPath ["cached"]
  -- create corresponding filesystems
  let f1 = ioHasFS $ MountPoint (d <> "/file-backed")
      f2 = ioHasFS $ MountPoint (d <> "/cached")
      fs = Two f1 f2

  -- Create an untracked registry, needed for the parallel testing
  registry <- unsafeNewRegistry' False
  ref <- newIORef die

  -- Initialize "immutable db tip"
  tipSVar <- newSVar (ChunkNo 0, RelativeSlot (ChunkNo 0) defaultChunkSize 0)

  pure Env {registry, ref, fs, envTestDir = d, tipSVar}

sm :: StateMachine (Model TestBlock) (Command TestBlock) IO (Response TestBlock)
sm = StateMachine UnInitModel transition precondition postcondition
           Nothing generator shrinker die mock noCleanup

prop_sequential :: Property
prop_sequential = forAllCommands sm Nothing $ \cmds -> monadicIO $ do
  env <- run newEnv
  (hist, _model, res) <- runCommands (sm {QSM.semantics = semantics env}) cmds
  prettyCommands sm hist (checkCommandNames cmds (res === Ok))

prop_parallel :: Property
prop_parallel = withMaxSuccess 400 $ noShrinking $ forAllParallelCommands sm Nothing $ \cmds -> monadicIO $ do
  env <- run newEnv
  prettyParallelCommands cmds =<< runParallelCommands (sm {QSM.semantics = semantics env}) cmds

tests :: TestTree
tests = testGroup "Index" [
    testProperty "Sequential index" prop_sequential
  , testProperty "Parallel index"   prop_parallel
  ]
