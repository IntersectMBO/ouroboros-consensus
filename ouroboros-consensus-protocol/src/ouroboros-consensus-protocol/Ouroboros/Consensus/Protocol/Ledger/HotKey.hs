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
  , finalize
  , getOCert
  , mkDynamicHotKey
  , mkEmptyHotKey
  , mkHotKey
  , mkHotKeyAtEvolution
  , sign
  ) where

import qualified Cardano.Crypto.KES as KES
import qualified Cardano.Crypto.KES as Relative (Period)
import Cardano.Protocol.Crypto (Crypto (..))
import qualified Cardano.Protocol.TPraos.OCert as Absolute (KESPeriod (..))
import qualified Cardano.Protocol.TPraos.OCert as OCert
import Control.Monad (forM_)
import Data.Word (Word64)
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
  deriving Show

-- | Result of evolving the KES key.
type KESEvolutionInfo = UpdateInfo KESInfo KESEvolutionError

-- | API to interact with the key.
data HotKey c m = HotKey
  { evolve :: Absolute.KESPeriod -> m KESEvolutionInfo
  -- ^ Evolve the KES signing key to the given absolute KES period.
  --
  -- When the key cannot evolve anymore, we poison it.
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
  , forget :: m ()
  -- ^ Securely erase the key and release its memory.
  , finalize_ :: m ()
  -- ^ Release any resources held by the 'HotKey', except for the signing
  -- key itself. User code should use 'finalize' instead.
  }

-- | Release all resources held by the 'HotKey', including the signing key
-- itself. Use this exactly once per 'HotKey' instance.
finalize :: Monad m => HotKey c m -> m ()
finalize hotKey = forget hotKey >> finalize_ hotKey

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
    (Just (ocert, initKey, evolution, startPeriod))
    maxKESEvolutions
    Nothing
    (pure ())

-- | Create a new 'HotKey' and initialize it to a poisoned state (containing no
-- valid KES sign key).
mkEmptyHotKey ::
  forall m c.
  (Crypto c, IOLike m) =>
  -- | Max KES evolutions
  Word64 ->
  m () ->
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
  m () ->
  m (HotKey c m)
mkDynamicHotKey = mkHotKeyWith Nothing

-- | The most general function for creating a new 'HotKey', accepting an initial
-- set of credentials, a key producer action, and a custom finalizer.
mkHotKeyWith ::
  forall m c.
  (Crypto c, IOLike m) =>
  Maybe (OCert.OCert c, KES.SignKeyKES (KES c), Word, Absolute.KESPeriod) ->
  -- | Max KES evolutions
  Word64 ->
  Maybe (KeyProducer c m) ->
  m () ->
  m (HotKey c m)
mkHotKeyWith initialStateMay maxKESEvolutions keyThreadMay finalizer = do
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
      return (cancel keyThreadAsync >> finalizer)
    Nothing ->
      return finalizer

  return
    HotKey
      { evolve = evolveKey varKESState
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
      , forget = unset
      , finalize_ = finalizer'
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
-- when the given period is before the key's period, we don't evolve the key
-- and return 'Updated'.
--
-- When the given KES period is within the range of the 'HotKey' and the given
-- period is after the key's period, we evolve the key and return 'Updated'.
--
-- When the key is poisoned, we always return 'UpdateFailed'.
evolveKey ::
  forall m c.
  (IOLike m, KES.ContextKES (KES c) ~ (), KES.KESAlgorithm (KES c)) =>
  StrictMVar m (KESState c) -> Absolute.KESPeriod -> m KESEvolutionInfo
evolveKey varKESState targetPeriod = modifyMVar varKESState $ \kesState -> do
  let info = kesStateInfo kesState
  -- We mask the evolution process because if we got interrupted after
  -- calling 'forgetSignKeyKES', which destructively updates the current
  -- signing key, we would leave an erased key in the state, which might
  -- cause a segfault when used afterwards.
  uninterruptibleMask_ $ case kesStateKey kesState of
    KESKeyPoisoned ->
      let err = KESKeyAlreadyPoisoned info targetPeriod
       in return (kesState, UpdateFailed err)
    KESKey ocert key -> case kesStatus info targetPeriod of
      -- When the absolute period is before the start period, we can't
      -- update the key. 'checkCanForge' will say we can't forge because the
      -- key is not valid yet.
      BeforeKESStart{} ->
        return (kesState, Updated info)
      -- When the absolute period is after the end period, we can't evolve
      -- anymore and poison the expired key.
      AfterKESEnd{} -> do
        let err = KESCouldNotEvolve info targetPeriod
        poisonedState <- poisonState kesState
        return (poisonedState, UpdateFailed err)
      InKESRange targetEvolution
        -- No evolving needed
        | targetEvolution <= kesEvolution info ->
            return (kesState, Updated info)
        -- Evolving needed
        | otherwise ->
            (\s' -> (s', Updated (kesStateInfo s')))
              <$> go targetEvolution info ocert key
 where
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
