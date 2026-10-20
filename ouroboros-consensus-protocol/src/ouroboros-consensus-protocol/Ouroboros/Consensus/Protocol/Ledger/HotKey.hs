{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Hot key
--
-- Intended for qualified import
module Ouroboros.Consensus.Protocol.Ledger.HotKey
  ( -- * KES Info
    KESEvolution
  , KESInfo (..)
  , kesAbsolutePeriod

    -- * KES Status
  , KESStatus (..)
  , kesStatus

    -- * Hot Key
  , HotKey (..)
  , KESEvolutionError (..)
  , KESEvolutionInfo
  , KESPersistCallback
  , getOCert
  , mkDynamicHotKey
  , mkEmptyHotKey
  , mkHotKey
  , mkHotKeyAtEvolution
  , mkHotKeyAtEvolutionWithPersist
  , sign
  ) where

import Cardano.Crypto.DirectSerialise (DirectSerialise)
import qualified Cardano.Crypto.KES as KES
import qualified Cardano.Crypto.KES as Relative (Period)
import Cardano.Crypto.KES.Locked (serialiseSignKeyKESToLockedBuf)
import Cardano.Crypto.Libsodium.Memory (MLockedForeignPtr)
import Cardano.Protocol.Crypto (Crypto (..))
import qualified Cardano.Protocol.TPraos.OCert as Absolute (KESPeriod (..))
import qualified Cardano.Protocol.TPraos.OCert as OCert
import Control.Monad (forM_)
import Data.Word (Word64)
import Foreign.C.Types (CChar)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import NoThunks.Class (OnlyCheckWhnfNamed (..))
import Ouroboros.Consensus.Block.Forging (UpdateInfo (..))
import Ouroboros.Consensus.Util.IOLike

{-------------------------------------------------------------------------------
  KES Info
-------------------------------------------------------------------------------}

-- | We call the relative periods that a KES key is valid its evolution, to
-- avoid confusion with absolute periods.
type KESEvolution = Relative.Period

data KESInfo = KESInfo
  { kesStartPeriod :: !Absolute.KESPeriod
  , kesEndPeriod :: !Absolute.KESPeriod
  -- ^ Currently derived from 'TPraosParams':
  -- > kesEndPeriod = kesStartPeriod + tpraosMaxKESEvo
  , kesEvolution :: !KESEvolution
  -- ^ Current evolution or /relative period/.
  --
  -- Invariant:
  -- > kesStartPeriod + kesEvolution in [kesStartPeriod, kesEndPeriod)
  }
  deriving (Show, Generic, NoThunks)

-- | Return the absolute KES period
kesAbsolutePeriod :: KESInfo -> Absolute.KESPeriod
kesAbsolutePeriod KESInfo{kesStartPeriod, kesEvolution} =
  Absolute.KESPeriod $ start + kesEvolution
 where
  Absolute.KESPeriod start = kesStartPeriod

{-------------------------------------------------------------------------------
  KES Status
-------------------------------------------------------------------------------}

data KESStatus
  = -- | The given period is before the start period of the KES key.
    BeforeKESStart
      -- | Given period
      Absolute.KESPeriod
      -- | Start period of the KES key
      Absolute.KESPeriod
  | -- | The given period is in the range of the KES key.
    InKESRange
      -- | Relative period or evolution corresponding to the
      -- given absolute period
      KESEvolution
  | -- | The given period is after the end period of the KES key.
    AfterKESEnd
      -- | Given period
      Absolute.KESPeriod
      -- | End period of the KES key
      Absolute.KESPeriod

-- | Return the evolution of the given KES period, /when/ it falls within the
-- range of the 'HotKey' (@[hkStart, hkEnd)@).
--
-- Note that the upper bound is exclusive, the spec says:
-- > c0 <= kesPeriod s < c0 + MaxKESEvo
kesStatus :: KESInfo -> Absolute.KESPeriod -> KESStatus
kesStatus
  KESInfo
    { kesStartPeriod = lo'@(Absolute.KESPeriod lo)
    , kesEndPeriod = hi'@(Absolute.KESPeriod hi)
    }
  cur'@(Absolute.KESPeriod cur)
    | cur < lo = BeforeKESStart cur' lo'
    | cur >= hi = AfterKESEnd cur' hi'
    | otherwise = InKESRange (cur - lo)

{-------------------------------------------------------------------------------
  Hot Key
-------------------------------------------------------------------------------}

-- | Failed to evolve the KES key.
data KESEvolutionError
  = -- | The KES key could not be evolved to the target period.
    KESCouldNotEvolve
      KESInfo
      -- | Target period outside the range of the current KES key. Typically
      -- the current KES period according to the wallclock slot.
      Absolute.KESPeriod
  | -- | The KES key was already poisoned.
    KESKeyAlreadyPoisoned
      KESInfo
      -- | Target period outside the range of the current KES key. Typically
      -- the current KES period according to the wallclock slot.
      Absolute.KESPeriod
  | -- | The KES key is already at an evolution /after/ the target period: the
    -- key comes from the future relative to the node's clock. A KES key can
    -- only evolve forward, so we cannot produce the evolution the verifiers
    -- will expect for this period. Signing with the current (higher)
    -- evolution would produce invalid blocks, so we refuse instead. The key
    -- is left untouched and becomes usable again once the target period
    -- catches up with its evolution.
    --
    -- Typical causes: a wallclock stepped backwards across a KES period
    -- boundary, a KES agent delivering a key evolved ahead of this node's
    -- clock, or a persisted evolved key file copied from a node that is
    -- ahead.
    KESKeyFromTheFuture
      KESInfo
      -- | Target period, below the key's current period. Typically the
      -- current KES period according to the wallclock slot.
      Absolute.KESPeriod
  deriving Show

-- | Result of evolving the KES key.
type KESEvolutionInfo = UpdateInfo KESInfo KESEvolutionError

-- | A callback that receives a serialised copy of the signing key after an
-- evolution: the absolute KES period the key is now at, a locked buffer with
-- the raw serialisation of the key, and the length of that buffer.
--
-- The callback owns the buffer and must erase it (with
-- 'Cardano.Crypto.Libsodium.Memory.finalizeMLockedForeignPtr') once it is
-- done. The node uses this to persist the evolved key to disk. A 'HotKey'
-- built with 'mkHotKeyAtEvolutionWithPersist' invokes it once per 'evolve'
-- that advanced the key, after the key has been evolved and the hot key's
-- lock released, in the thread that called 'evolve'. It is not invoked when
-- 'evolve' did not change the key (same period, a period before the start, a
-- period the key is already past, a poisoned or expired key). The callback
-- should not throw; an exception propagates to the caller of 'evolve' after
-- the key has already been evolved.
type KESPersistCallback m =
  Absolute.KESPeriod -> MLockedForeignPtr CChar -> Int -> m ()

-- | How a 'HotKey' hands its evolved key to a 'KESPersistCallback': a
-- serialiser for the key (into a fresh locked buffer the callback will own)
-- and the callback itself.
data Persister c m = Persister
  { persistSerialise :: KES.SignKeyKES (KES c) -> m (MLockedForeignPtr CChar, Int)
  , persistCallback :: KESPersistCallback m
  }

-- | API to interact with the key.
data HotKey c m = HotKey
  { evolve :: Absolute.KESPeriod -> m KESEvolutionInfo
  -- ^ Evolve the KES signing key to the given absolute KES period.
  --
  -- When the key cannot evolve anymore, we poison it.
  --
  -- When the key is already /past/ the given period, we return
  -- 'UpdateFailed' with 'KESKeyFromTheFuture' and leave the key untouched.
  , getInfo :: m KESInfo
  -- ^ Return 'KESInfo' of the signing key.
  , getOCertMaybe :: m (Maybe (OCert.OCert c))
  -- ^ Return the 'OCert' corresponding to the KES signing key, if any.
  , isPoisoned :: m Bool
  -- ^ Check whether a valid KES signing key exists. "Poisoned" means no
  -- key exists; reasons for this could be:
  -- - no signing key has been set yet
  -- - the signing key has been explicitly erased ('forget')
  -- - the signing key has been evolved past the end of the available
  --   evolutions
  , sign_ ::
      forall toSign.
      (KES.Signable (KES c) toSign, HasCallStack) =>
      toSign ->
      m (KES.SignedKES (KES c) toSign)
  -- ^ Sign the given @toSign@ with the current signing key.
  --
  -- PRECONDITION: the key is not poisoned.
  --
  -- POSTCONDITION: the signature is in normal form.
  , finalize :: m ()
  -- ^ Release any resources held by the 'HotKey', including the signing key
  -- itself: the key is poisoned (its mlocked memory erased) and any key
  -- producer thread is stopped. This should be called exactly once per
  -- 'HotKey' instance; calling it again is a no-op.
  }

deriving via (OnlyCheckWhnfNamed "HotKey" (HotKey c m)) instance NoThunks (HotKey c m)

getOCert :: Monad m => HotKey c m -> m (OCert.OCert c)
getOCert hotKey = do
  ocertMay <- getOCertMaybe hotKey
  case ocertMay of
    Just ocert -> return ocert
    Nothing -> error "trying to read OpCert for poisoned key"

sign ::
  (KES.Signable (KES c) toSign, HasCallStack) =>
  HotKey c m ->
  toSign ->
  m (KES.SignedKES (KES c) toSign)
sign = sign_

-- | The actual KES key, unless it expired, in which case it is replaced by
-- \"poison\".
data KESKey c
  = KESKey !(OCert.OCert c) !(KES.SignKeyKES (KES c))
  | KESKeyPoisoned
  deriving Generic

instance (NoThunks (KES.SignKeyKES (KES c)), Crypto c) => NoThunks (KESKey c)

kesKeyIsPoisoned :: KESKey c -> Bool
kesKeyIsPoisoned KESKeyPoisoned = True
kesKeyIsPoisoned (KESKey _ _) = False

data KESState c = KESState
  { kesStateInfo :: !KESInfo
  , kesStateKey :: !(KESKey c)
  }
  deriving Generic

instance (NoThunks (KES.SignKeyKES (KES c)), Crypto c) => NoThunks (KESState c)

-- Create a new 'HotKey' and initialize it to the given initial KES key. The
-- initial key must be at evolution 0 (i.e., freshly generated and never
-- evolved).
mkHotKey ::
  forall m c.
  (Crypto c, IOLike m) =>
  OCert.OCert c ->
  KES.SignKeyKES (KES c) ->
  -- | Start period
  Absolute.KESPeriod ->
  -- | Max KES evolutions
  Word64 ->
  m (HotKey c m)
mkHotKey = mkHotKeyAtEvolution 0

-- Create a new 'HotKey' and initialize it to the given initial KES key. The
-- initial key should be at the given evolution.
mkHotKeyAtEvolution ::
  forall m c.
  (Crypto c, IOLike m) =>
  Word ->
  OCert.OCert c ->
  KES.SignKeyKES (KES c) ->
  -- | Start period
  Absolute.KESPeriod ->
  -- | Max KES evolutions
  Word64 ->
  m (HotKey c m)
mkHotKeyAtEvolution evolution ocert initKey startPeriod maxKESEvolutions =
  mkHotKeyWith
    Nothing
    (Just (ocert, initKey, evolution, startPeriod))
    maxKESEvolutions
    Nothing

-- | 'mkHotKeyAtEvolution' with an optional 'KESPersistCallback': after every
-- 'evolve' that advanced the key, the new key is serialised into a fresh
-- locked buffer (a copy; the hot key keeps its own) and the callback is
-- invoked with the key's absolute period and that buffer, outside the hot
-- key's lock. The serialisation happens while the lock is held, so the
-- buffer always holds the key the hot key had right after that evolution. If
-- serialising fails, the evolution still succeeds and the callback is not
-- invoked for it.
--
-- With 'Nothing' this is exactly 'mkHotKeyAtEvolution'.
mkHotKeyAtEvolutionWithPersist ::
  forall m c.
  (Crypto c, IOLike m, DirectSerialise (KES.SignKeyKES (KES c))) =>
  Maybe (KESPersistCallback m) ->
  Word ->
  OCert.OCert c ->
  KES.SignKeyKES (KES c) ->
  -- | Start period
  Absolute.KESPeriod ->
  -- | Max KES evolutions
  Word64 ->
  m (HotKey c m)
mkHotKeyAtEvolutionWithPersist mCallback evolution ocert initKey startPeriod maxKESEvolutions =
  mkHotKeyWith
    (mkPersister <$> mCallback)
    (Just (ocert, initKey, evolution, startPeriod))
    maxKESEvolutions
    Nothing
 where
  mkPersister callback =
    Persister
      { persistSerialise = serialiseSignKeyKESToLockedBuf
      , persistCallback = callback
      }

-- | Create a new 'HotKey' and initialize it to a poisoned state (containing no
-- valid KES sign key).
mkEmptyHotKey ::
  forall m c.
  (Crypto c, IOLike m) =>
  -- | Max KES evolutions
  Word64 ->
  m (HotKey c m)
mkEmptyHotKey maxKESEvolutions =
  mkDynamicHotKey maxKESEvolutions Nothing

mkKESState ::
  Word64 -> OCert.OCert c -> KES.SignKeyKES (KES c) -> Word -> Absolute.KESPeriod -> KESState c
mkKESState maxKESEvolutions newOCert newKey evolution startPeriod@(Absolute.KESPeriod start) =
  KESState
    { kesStateInfo =
        KESInfo
          { kesStartPeriod = startPeriod
          , kesEndPeriod = Absolute.KESPeriod (start + fromIntegral maxKESEvolutions)
          , kesEvolution = evolution
          }
    , kesStateKey = KESKey newOCert newKey
    }

type KeyProducer c m =
  -- | Callback that will be invoked when a new key has been received
  (OCert.OCert c -> KES.SignKeyKES (KES c) -> Word -> Absolute.KESPeriod -> m ()) ->
  -- | Callback that will be invoked when a key deletion has been received
  m () ->
  m ()

-- | Create a new 'HotKey' that runs a key-producer action on a separate thread.
-- The key producer action will receive a callback that can be used to pass
-- keys into the HotKey; the HotKey will dynamically update its internal state
-- to reflect new keys as they arrive.
mkDynamicHotKey ::
  forall m c.
  (Crypto c, IOLike m) =>
  -- | Max KES evolutions
  Word64 ->
  Maybe (KeyProducer c m) ->
  m (HotKey c m)
mkDynamicHotKey = mkHotKeyWith Nothing Nothing

-- | The most general function for creating a new 'HotKey', accepting an
-- optional persister for evolved keys, an initial set of credentials and a
-- key producer action.
mkHotKeyWith ::
  forall m c.
  (Crypto c, IOLike m) =>
  Maybe (Persister c m) ->
  Maybe (OCert.OCert c, KES.SignKeyKES (KES c), Word, Absolute.KESPeriod) ->
  -- | Max KES evolutions
  Word64 ->
  Maybe (KeyProducer c m) ->
  m (HotKey c m)
mkHotKeyWith persisterMay initialStateMay maxKESEvolutions keyThreadMay = do
  varKESState <- newMVar initKESState

  let set newOCert newKey evolution startPeriod =
        modifyMVar_ varKESState $ \oldState -> do
          _ <- poisonState oldState
          return $ mkKESState maxKESEvolutions newOCert newKey evolution startPeriod
      unset =
        modifyMVar_ varKESState poisonState

  forM_ initialStateMay $ \(newOCert, newKey, evolution, startPeriod) ->
    set newOCert newKey evolution startPeriod

  finalizer' <- case keyThreadMay of
    Just keyThread -> do
      keyThreadAsync <- async $ do
        labelThisThread "HotKey receiver"
        keyThread set unset

      -- 'cancel' cannot throw exceptions, so we don't need to use 'finally'
      -- here
      pure (cancel keyThreadAsync >> unset)
    Nothing ->
      -- No key-producer thread to stop, but the key itself must still be
      -- erased: without this, a 'HotKey' built from an initial key (the
      -- file-based credentials) would keep its mlocked signing key alive
      -- after 'finalize' until the GC happens to run the allocator's
      -- finaliser. Poisoning here erases it deterministically, e.g. when the
      -- node reloads its credentials on SIGHUP.
      pure unset

  return
    HotKey
      { evolve = evolveKey persisterMay varKESState
      , getInfo = kesStateInfo <$> readMVar varKESState
      , getOCertMaybe =
          kesStateKey <$> readMVar varKESState >>= \case
            KESKeyPoisoned -> return Nothing
            KESKey ocert _ -> return (Just ocert)
      , isPoisoned = kesKeyIsPoisoned . kesStateKey <$> readMVar varKESState
      , sign_ = \toSign -> do
          withMVar varKESState $ \KESState{kesStateInfo, kesStateKey} -> do
            case kesStateKey of
              KESKeyPoisoned ->
                error "trying to sign with a poisoned key"
              KESKey _ key -> do
                let evolution = kesEvolution kesStateInfo
                KES.signedKES () evolution toSign key
      , finalize = finalizer'
      }
 where
  initKESState :: KESState c
  initKESState =
    KESState
      { kesStateInfo =
          KESInfo
            { kesStartPeriod = Absolute.KESPeriod 0
            , kesEndPeriod = Absolute.KESPeriod 0
            , kesEvolution = 0
            }
      , kesStateKey = KESKeyPoisoned
      }

poisonState ::
  forall m c.
  (KES.KESAlgorithm (KES c), IOLike m) =>
  KESState c -> m (KESState c)
poisonState kesState = do
  case kesStateKey kesState of
    KESKeyPoisoned -> do
      -- already poisoned
      return kesState
    KESKey _ sk -> do
      forgetSignKeyKES sk
      return kesState{kesStateKey = KESKeyPoisoned}

-- | Evolve the 'HotKey' so that its evolution matches the given KES period.
--
-- When the given KES period is after the end period of the 'HotKey', we
-- poison the key and return 'UpdateFailed'.
--
-- When the given KES period is before the start period of the 'HotKey' or
-- when the given period is equal to the key's period, we don't evolve the key
-- and return 'Updated'.
--
-- When the given KES period is within the range of the 'HotKey' but before
-- the key's current period, we don't touch the key and return 'UpdateFailed'
-- with 'KESKeyFromTheFuture': a KES key cannot evolve backwards, and signing
-- with a higher evolution than the verifiers expect would produce invalid
-- blocks. The key is not poisoned; it becomes usable again once the given
-- period catches up with it.
--
-- When the given KES period is within the range of the 'HotKey' and the given
-- period is after the key's period, we evolve the key and return 'Updated'.
--
-- When the key is poisoned, we always return 'UpdateFailed'.
--
-- When the key was evolved and a persister is given, the evolved key is
-- serialised while the state is still locked and the persist callback is
-- invoked with the result once the lock has been released.
evolveKey ::
  forall m c.
  (IOLike m, KES.ContextKES (KES c) ~ (), KES.KESAlgorithm (KES c)) =>
  Maybe (Persister c m) ->
  StrictMVar m (KESState c) ->
  Absolute.KESPeriod ->
  m KESEvolutionInfo
evolveKey persisterMay varKESState targetPeriod = do
  (result, persist) <- modifyMVar varKESState $ \kesState -> do
    let info = kesStateInfo kesState
    -- We mask the evolution process because if we got interrupted after
    -- calling 'forgetSignKeyKES', which destructively updates the current
    -- signing key, we would leave an erased key in the state, which might
    -- cause a segfault when used afterwards.
    uninterruptibleMask_ $ case kesStateKey kesState of
      KESKeyPoisoned ->
        let err = KESKeyAlreadyPoisoned info targetPeriod
         in return (kesState, (UpdateFailed err, Nothing))
      KESKey ocert key -> case kesStatus info targetPeriod of
        -- When the absolute period is before the start period, we can't
        -- update the key. 'checkCanForge' will say we can't forge because the
        -- key is not valid yet.
        BeforeKESStart{} ->
          return (kesState, (Updated info, Nothing))
        -- When the absolute period is after the end period, we can't evolve
        -- anymore and poison the expired key.
        AfterKESEnd{} -> do
          let err = KESCouldNotEvolve info targetPeriod
          poisonedState <- poisonState kesState
          return (poisonedState, (UpdateFailed err, Nothing))
        InKESRange targetEvolution
          -- No evolving needed
          | targetEvolution == kesEvolution info ->
              return (kesState, (Updated info, Nothing))
          -- The key is already past the target period. It cannot be evolved
          -- backwards, and signing with the current evolution would produce
          -- blocks the verifiers reject, so refuse to update and leave the key
          -- as it is.
          | targetEvolution < kesEvolution info ->
              let err = KESKeyFromTheFuture info targetPeriod
               in return (kesState, (UpdateFailed err, Nothing))
          -- Evolving needed
          | otherwise -> do
              kesState' <- go targetEvolution info ocert key
              persist <- serialiseForPersist kesState'
              return (kesState', (Updated (kesStateInfo kesState'), persist))
  -- The callback runs after the lock has been released and outside the mask,
  -- so a slow or throwing callback cannot hold up or poison the hot key.
  sequence_ persist
  return result
 where
  -- Serialise the evolved key for the persister, still under the lock, and
  -- return the callback invocation to run once the lock is released. A
  -- failure to serialise must not fail the evolution: the state's previous
  -- key has already been erased, so 'modifyMVar' restoring it would leave a
  -- dangling key behind. Instead the callback is simply not invoked.
  serialiseForPersist :: KESState c -> m (Maybe (m ()))
  serialiseForPersist kesState' = case (persisterMay, kesStateKey kesState') of
    (Just persister, KESKey _ key') ->
      try (persistSerialise persister key') >>= \case
        Left (_ :: SomeException) -> return Nothing
        Right (buf, len) ->
          return $
            Just $
              persistCallback persister (kesAbsolutePeriod (kesStateInfo kesState')) buf len
    _ -> return Nothing

  -- \| PRECONDITION:
  --
  -- > targetEvolution >= curEvolution
  go :: KESEvolution -> KESInfo -> OCert.OCert c -> KES.SignKeyKES (KES c) -> m (KESState c)
  go targetEvolution info ocert key
    | targetEvolution <= curEvolution =
        return $ KESState{kesStateInfo = info, kesStateKey = KESKey ocert key}
    | otherwise =
        do
          maybeKey' <- KES.updateKES () key curEvolution
          case maybeKey' of
            Nothing ->
              -- This cannot happen
              error "Could not update KES key"
            Just !key' -> do
              -- Clear the memory associated with the old key
              forgetSignKeyKES key
              let info' = info{kesEvolution = curEvolution + 1}
              go targetEvolution info' ocert key'
   where
    curEvolution = kesEvolution info
