{-# LANGUAGE BangPatterns #-}

module LeiosDemoLogic (module LeiosDemoLogic) where

import           Cardano.Slotting.Slot (SlotNo (..))
import           Control.Concurrent.Class.MonadMVar (MVar, MonadMVar)
import qualified Control.Concurrent.Class.MonadMVar as MVar
import qualified Data.Bits as Bits
import           Data.Functor ((<&>))
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import           Data.Word (Word64)
import           LeiosDemoTypes (EbId (..), LeiosEbBodies, LeiosPoint (..))
import qualified LeiosDemoTypes as Leios

ebIdSlot :: EbId -> SlotNo
ebIdSlot (MkEbId y) =
    SlotNo (fromIntegral (y - minBound :: Int) `Bits.unsafeShiftR` 20 :: Word64)

ebIdToPoint :: EbId -> LeiosEbBodies -> Maybe LeiosPoint
ebIdToPoint (MkEbId i) x =
        (\h -> MkLeiosPoint (ebIdSlot (MkEbId i)) h)
    <$>
        IntMap.lookup i (Leios.ebPointsInverse x)

ebIdToPointM :: MonadMVar m => MVar m LeiosEbBodies -> EbId -> m (Maybe LeiosPoint)
ebIdToPointM mvar ebId =
    MVar.readMVar mvar <&> ebIdToPoint ebId

ebIdFromPoint :: LeiosPoint -> LeiosEbBodies -> (EbId, Maybe LeiosEbBodies)
ebIdFromPoint p x =
    case IntMap.lookup islot (Leios.ebPoints x) of
        Just m -> case Map.lookup ebHash m of
            Just y -> (y, Nothing)
            Nothing -> gen $ MkEbId $ zero + (2^(20 :: Int) - 1) - Map.size m
        Nothing -> gen $ MkEbId $ zero + (2^(20 :: Int) - 1)
  where
    MkLeiosPoint ebSlot ebHash = p
    SlotNo wslot = ebSlot
    islot = fromIntegral (wslot :: Word64)

    zero = fromIntegral (wslot `Bits.unsafeShiftL` 20) + minBound :: Int

    gen y =
        let !x' = x {
                  Leios.ebPoints =
                      IntMap.insertWith
                          Map.union
                          islot
                          (Map.singleton ebHash y)
                          (Leios.ebPoints x)
               ,
                  Leios.ebPointsInverse =
                      let MkEbId z = y
                      in
                      IntMap.insert z ebHash (Leios.ebPointsInverse x)
               }
        in (y, Just x')

ebIdFromPointM :: MonadMVar m => MVar m LeiosEbBodies -> LeiosPoint -> m EbId
ebIdFromPointM mvar p =
    MVar.modifyMVar mvar $ \ebBodies -> do
        let (ebId, mbEbBodies') = ebIdFromPoint p ebBodies
        case mbEbBodies' of
            Nothing -> pure (ebBodies, ebId)
            Just ebBodies' -> do
                -- TODO when to INSERT INTO ebPoints?
                pure (ebBodies', ebId)
