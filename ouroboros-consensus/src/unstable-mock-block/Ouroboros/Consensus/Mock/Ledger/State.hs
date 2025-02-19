{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Mock.Ledger.State (
    -- * Config for the mock ledger
    MockConfig (..)
  , defaultMockConfig
    -- * State of the mock ledger
  , MockError (..)
  , MockState (..)
  , updateMockState
  , updateMockTip
  , updateMockUTxO
    -- * Supporting definitions
  , checkTxSize
  , txSize
    -- * Genesis state
  , genesisMockState
  ) where

import           Cardano.Binary (toCBOR)
import           Cardano.Crypto.Hash
import           Codec.Serialise (Serialise, serialise)
import           Control.Monad (guard)
import           Control.Monad.Except (Except, throwError, withExcept)
import qualified Data.ByteString.Lazy as BL
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.SupportsMempool (ByteSize32 (..))
import           Ouroboros.Consensus.Mock.Ledger.Address
import           Ouroboros.Consensus.Mock.Ledger.UTxO
import           Ouroboros.Consensus.Util (ShowProxy (..), repeatedlyM)
import           Test.Util.Orphans.Serialise ()

{-------------------------------------------------------------------------------
  Config of the mock block
-------------------------------------------------------------------------------}

-- | Parameters needed to validate blocks/txs
data MockConfig = MockConfig {
    mockCfgMaxTxSize :: !(Maybe ByteSize32)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NoThunks, Serialise)

defaultMockConfig :: MockConfig
defaultMockConfig = MockConfig {
      mockCfgMaxTxSize = Nothing
    }

{-------------------------------------------------------------------------------
  State of the mock ledger
-------------------------------------------------------------------------------}

data MockState blk = MockState {
      mockUtxo      :: !Utxo
    , mockConfirmed :: !(Set TxId)
    , mockTip       :: !(Point blk)
    }
  deriving (Show, Eq, Generic, NoThunks)

deriving instance Serialise (HeaderHash blk) => Serialise (MockState blk)

data MockError blk =
    MockExpired !SlotNo !SlotNo
    -- ^ The transaction expired in the first 'SlotNo', and it failed to
    -- validate in the second 'SlotNo'.
  | MockUtxoError UtxoError
  | MockInvalidHash (ChainHash blk) (ChainHash blk)
  | MockTxSizeTooBig ByteSize32 ByteSize32
  deriving (Generic, NoThunks)

deriving instance StandardHash blk => Show (MockError blk)
deriving instance StandardHash blk => Eq   (MockError blk)
deriving instance Serialise (HeaderHash blk) => Serialise (MockError blk)

instance Typeable blk => ShowProxy (MockError blk) where

updateMockState :: (GetPrevHash blk, HasMockTxs blk)
                => MockConfig
                -> blk
                -> MockState blk
                -> Except (MockError blk) (MockState blk)
updateMockState cfg blk st = do
    let hdr = getHeader blk
    st' <- updateMockTip hdr st
    updateMockUTxO cfg (blockSlot hdr) blk st'

updateMockTip :: GetPrevHash blk
              => Header blk
              -> MockState blk
              -> Except (MockError blk) (MockState blk)
updateMockTip hdr (MockState u c t)
    | headerPrevHash hdr == pointHash t
    = return $ MockState u c (headerPoint hdr)
    | otherwise
    = throwError $ MockInvalidHash (headerPrevHash hdr) (pointHash t)

updateMockUTxO :: HasMockTxs a
               => MockConfig
               -> SlotNo
               -> a
               -> MockState blk
               -> Except (MockError blk) (MockState blk)
updateMockUTxO cfg now = repeatedlyM (updateMockUTxO1 cfg now) . getMockTxs

updateMockUTxO1 :: forall blk.
                   MockConfig
                -> SlotNo
                -> Tx
                -> MockState blk
                -> Except (MockError blk) (MockState blk)
updateMockUTxO1 cfg now tx (MockState u c t) = case hasExpired of
    Just e  -> throwError e
    Nothing -> do
      _ <- checkTxSize cfg tx
      u' <- withExcept MockUtxoError $ updateUtxo tx u
      return $ MockState u' (c `Set.union` confirmed tx) t
  where
      Tx expiry _ins _outs = tx

      hasExpired :: Maybe (MockError blk)
      hasExpired = case expiry of
          DoNotExpire       -> Nothing
          ExpireAtOnsetOf s -> do
            guard $ s <= now
            Just $ MockExpired s now

checkTxSize :: MockConfig -> Tx -> Except (MockError blk) ByteSize32
checkTxSize cfg tx
  | Just maxTxSize <- mockCfgMaxTxSize cfg
  , actualTxSize > maxTxSize =
      throwError $ MockTxSizeTooBig actualTxSize maxTxSize
  | otherwise = pure actualTxSize
  where
    actualTxSize = txSize tx

{-------------------------------------------------------------------------------
  Supporting definitions
-------------------------------------------------------------------------------}

txSize :: Tx -> ByteSize32
txSize = ByteSize32 . fromIntegral . BL.length . serialise

{-------------------------------------------------------------------------------
  Genesis
-------------------------------------------------------------------------------}

genesisMockState :: AddrDist -> MockState blk
genesisMockState addrDist = MockState {
      mockUtxo      = genesisUtxo addrDist
    , mockConfirmed = Set.singleton (hashWithSerialiser toCBOR (genesisTx addrDist))
    , mockTip       = GenesisPoint
    }
