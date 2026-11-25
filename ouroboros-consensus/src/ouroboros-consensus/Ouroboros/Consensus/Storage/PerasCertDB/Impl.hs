{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Storage.PerasCertDB.Impl
  ( -- * Opening
    PerasCertDbArgs (..)
  , PerasCertDbSerialiseConstraints
  , defaultArgs
  , createDB

    -- * Trace types
  , TraceEvent (..)
  )
where

import Cardano.Binary
  ( Decoder
  , DecoderError
  , Encoding
  , FromCBOR (..)
  , ToCBOR (..)
  , decodeFullDecoder
  , decodeListLenOf
  , encodeListLen
  , serialize
  )
import Control.Monad (guard, void, when)
import Control.Tracer (Tracer, nullTracer, traceWith)
import Data.Char (isDigit)
import Data.Either (rights)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Maybe.Strict (StrictMaybe (..), maybeToStrictMaybe, strictMaybeToMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import NoThunks.Class
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.BlockchainTime (WithArrivalTime (..))
import Ouroboros.Consensus.Peras.Weight (PerasWeightSnapshot, mkPerasWeightSnapshot)
import Ouroboros.Consensus.Storage.PerasCertDB.API
import Ouroboros.Consensus.Storage.Serialisation (DecodeDisk (..), EncodeDisk (..))
import Ouroboros.Consensus.Util.Args
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Consensus.Util.STM
import System.FS.API.Lazy
import Text.Read (readMaybe)

{-------------------------------------------------------------------------------
  Database state
-------------------------------------------------------------------------------}

data PerasCertDbEnv m blk = PerasCertDbEnv
  { pcdbTracer :: !(Tracer m (TraceEvent blk))
  , pcdbState :: !(StrictTVar m (WithFingerprint (PerasCertDbState blk)))
  -- ^ The volatile state of the certificate database.
  , pcdbHasFS :: !(SomeHasFS m)
  , pcdbCodecConfig :: !(CodecConfig blk)
  }
  deriving NoThunks via OnlyCheckWhnfNamed "PerasCertDbEnv" (PerasCertDbEnv m blk)

-- | INVARIANT: See 'invariantForPerasCertDbState'.
data PerasCertDbState blk = PerasCertDbState
  { pcdsCerts :: !(Map PerasRoundNo (WithArrivalTime (ValidatedPerasCert blk)))
  -- ^ The certificates by 'PerasRoundNo'.
  , pcdsLatestCertSeen ::
      !(StrictMaybe (WithArrivalTime (ValidatedPerasCert blk)))
  -- ^ The certificate with the highest round number that has been added to the
  -- db since it has been opened, or restored from disk when opening it.
  }

deriving instance
  Show (PerasCert blk) =>
  Show (PerasCertDbState blk)

deriving instance
  NoThunks (PerasCert blk) =>
  NoThunks (PerasCertDbState blk)

deriving instance
  Generic (PerasCertDbState blk)

{-------------------------------------------------------------------------------
  Trace types
-------------------------------------------------------------------------------}

data TraceEvent blk
  = AddCert
      PerasRoundNo
      (WithArrivalTime (ValidatedPerasCert blk))
      AddPerasCertResult
  | GarbageCollected
      SlotNo

deriving instance
  Show (PerasCert blk) =>
  Show (TraceEvent blk)

deriving instance
  Eq (PerasCert blk) =>
  Eq (TraceEvent blk)

deriving instance
  Generic (TraceEvent blk)

{------------------------------------------------------------------------------
  Creating the database
------------------------------------------------------------------------------}

type PerasCertDbArgs :: (Type -> Type) -> (Type -> Type) -> Type -> Type
data PerasCertDbArgs f m blk = PerasCertDbArgs
  { pcdbaTracer :: Tracer m (TraceEvent blk)
  , pcdbaCodecConfig :: HKD f (CodecConfig blk)
  , pcdbaHasFS :: HKD f (SomeHasFS m)
  -- ^ Where certificates are persisted, so that they survive restarts.
  }

defaultArgs :: Monad m => Incomplete PerasCertDbArgs m blk
defaultArgs =
  PerasCertDbArgs
    { pcdbaTracer = nullTracer
    , pcdbaCodecConfig = noDefault
    , pcdbaHasFS = noDefault
    }

createDB ::
  forall m blk.
  ( IOLike m
  , BlockSupportsPeras blk
  , EncodeDisk blk (PerasCert blk)
  , DecodeDisk blk (PerasCert blk)
  ) =>
  Complete PerasCertDbArgs m blk ->
  m (PerasCertDB m blk)
createDB args = do
  pcds <- loadPerasCertDbState pcdbCodecConfig pcdbHasFS
  pcdbState <- newTVarIO (WithFingerprint pcds (Fingerprint 0))
  let env =
        PerasCertDbEnv
          { pcdbTracer
          , pcdbState
          , pcdbHasFS
          , pcdbCodecConfig
          }
  pure
    PerasCertDB
      { addCert = implAddCert env
      , getCertIds = implGetCertIds env
      , getCertsAfter = implGetCertsAfter env
      , getWeightSnapshot = implGetWeightSnapshot env
      , getLatestCertSeen = implGetLatestCertSeen env
      , garbageCollect = implGarbageCollect env
      }
 where
  PerasCertDbArgs
    { pcdbaTracer = pcdbTracer
    , pcdbaCodecConfig = pcdbCodecConfig
    , pcdbaHasFS = pcdbHasFS
    } = args

type PerasCertDbSerialiseConstraints blk =
  ( EncodeDisk blk (PerasCert blk)
  , DecodeDisk blk (PerasCert blk)
  )

{-------------------------------------------------------------------------------
  API implementation
-------------------------------------------------------------------------------}

-- TODO: we will need to update this method with non-trivial validation logic
-- see https://github.com/tweag/cardano-peras/issues/120
implAddCert ::
  ( IOLike m
  , IsPerasCert (PerasCert blk) blk
  , EncodeDisk blk (PerasCert blk)
  ) =>
  PerasCertDbEnv m blk ->
  WithArrivalTime (ValidatedPerasCert blk) ->
  STM m (m AddPerasCertResult)
implAddCert env@PerasCertDbEnv{pcdbTracer, pcdbState} cert = do
  let roundNo = getPerasCertRound cert
  addPerasCertRes <- do
    WithFingerprint pcds fp <- readTVar pcdbState
    if Map.member roundNo (pcdsCerts pcds)
      then pure PerasCertAlreadyInDB
      else do
        let pcdsCerts' = Map.insert roundNo cert (pcdsCerts pcds)
            pcdsLatestCertSeen' =
              case pcdsLatestCertSeen pcds of
                SNothing ->
                  SJust cert
                SJust prev
                  | getPerasCertRound cert
                      > getPerasCertRound prev ->
                      SJust cert
                  | otherwise ->
                      SJust prev
        writeTVar pcdbState $
          WithFingerprint
            PerasCertDbState
              { pcdsCerts = pcdsCerts'
              , pcdsLatestCertSeen = pcdsLatestCertSeen'
              }
            (succ fp)
        pure AddedPerasCertToDB
  pure $ do
    traceWith pcdbTracer (AddCert roundNo cert addPerasCertRes)
    when (addPerasCertRes == AddedPerasCertToDB) $
      writeCertFile env roundNo cert
    pure addPerasCertRes

implGetWeightSnapshot ::
  ( IOLike m
  , StandardHash blk
  , IsPerasCert (PerasCert blk) blk
  ) =>
  PerasCertDbEnv m blk ->
  STM m (WithFingerprint (PerasWeightSnapshot blk))
implGetWeightSnapshot PerasCertDbEnv{pcdbState} = do
  WithFingerprint pcds fp <- readTVar pcdbState
  let weights =
        mkPerasWeightSnapshot
          [ (getPerasCertPoint cert, vpcCertBoost (forgetArrivalTime cert))
          | cert <- Map.elems (pcdsCerts pcds)
          ]
  pure (WithFingerprint weights fp)

implGetCertIds ::
  IOLike m =>
  PerasCertDbEnv m blk ->
  STM m (Set PerasRoundNo)
implGetCertIds PerasCertDbEnv{pcdbState} = do
  PerasCertDbState{pcdsCerts} <-
    forgetFingerprint <$> readTVar pcdbState
  pure $ Map.keysSet pcdsCerts

implGetCertsAfter ::
  IOLike m =>
  PerasCertDbEnv m blk ->
  PerasRoundNo ->
  STM m (Map PerasRoundNo (m (WithArrivalTime (ValidatedPerasCert blk))))
implGetCertsAfter PerasCertDbEnv{pcdbState} roundNo = do
  PerasCertDbState{pcdsCerts} <-
    forgetFingerprint <$> readTVar pcdbState
  let strictlyGreater = snd $ Map.split roundNo pcdsCerts
  pure $ pure <$> strictlyGreater

implGetLatestCertSeen ::
  IOLike m =>
  PerasCertDbEnv m blk ->
  STM m (Maybe (WithArrivalTime (ValidatedPerasCert blk)))
implGetLatestCertSeen PerasCertDbEnv{pcdbState} = do
  PerasCertDbState{pcdsLatestCertSeen} <-
    forgetFingerprint <$> readTVar pcdbState
  pure $ strictMaybeToMaybe pcdsLatestCertSeen

implGarbageCollect ::
  forall m blk.
  ( IOLike m
  , IsPerasCert (PerasCert blk) blk
  ) =>
  PerasCertDbEnv m blk ->
  SlotNo ->
  STM m (m ())
implGarbageCollect PerasCertDbEnv{pcdbTracer, pcdbState} slotNo = do
  -- No need to update the 'Fingerprint' as we only remove certificates that do
  -- not matter for comparing interesting chains.
  modifyTVar pcdbState (fmap gc)
  pure $ traceWith pcdbTracer (GarbageCollected slotNo)
 where
  gc :: PerasCertDbState blk -> PerasCertDbState blk
  gc
    PerasCertDbState
      { pcdsCerts
      , pcdsLatestCertSeen
      } =
      let pcdsCerts' =
            Map.filter
              (\cert -> pointSlot (getPerasCertPoint cert) >= NotOrigin slotNo)
              pcdsCerts
       in PerasCertDbState
            { pcdsCerts = pcdsCerts'
            , pcdsLatestCertSeen = pcdsLatestCertSeen
            }

{-------------------------------------------------------------------------------
  On-disk storage
-------------------------------------------------------------------------------}

-- | Initialize the `PerasCertDbState` with the most recent `maxVolatileCertCount`
-- certificates stored on disk.
-- Certificate files that cannot be decoded (e.g. because a write was interrupted)
-- are skipped.
loadPerasCertDbState ::
  ( IOLike m
  , IsPerasCert (PerasCert blk) blk
  , DecodeDisk blk (PerasCert blk)
  ) =>
  CodecConfig blk ->
  SomeHasFS m ->
  m (PerasCertDbState blk)
loadPerasCertDbState ccfg (SomeHasFS hasFS) = do
  createDirectoryIfMissing hasFS True rootDir
  roundNos <-
    Set.fromList . mapMaybe certRoundFromFileName . Set.toList <$> listDirectory hasFS rootDir
  certs <-
    rights
      <$> traverse
        (readCertFile ccfg hasFS)
        (take maxVolatileCertCount (Set.toDescList roundNos))
  let pcdsCerts = Map.fromList [(getPerasCertRound cert, cert) | cert <- certs]
  pure
    PerasCertDbState
      { pcdsCerts
      , pcdsLatestCertSeen = maybeToStrictMaybe (snd <$> Map.lookupMax pcdsCerts)
      }

-- | The maximum number of certificates (one per round) within a common prefix
-- window of @3k/f@ slots, i.e. 1440 rounds of 90 slots for @k = 2160@ and @f = 1/20@.
-- Used as a bound on the certificates loaded in memory when opening.
maxVolatileCertCount :: Int
maxVolatileCertCount = 1440

-- | Write a certificate with its arrival time to an independent file on disk.
-- The file is ovewritten if it already exists.
writeCertFile ::
  (IOLike m, EncodeDisk blk (PerasCert blk)) =>
  PerasCertDbEnv m blk ->
  PerasRoundNo ->
  WithArrivalTime (ValidatedPerasCert blk) ->
  m ()
writeCertFile PerasCertDbEnv{pcdbHasFS = SomeHasFS hasFS, pcdbCodecConfig} roundNo cert =
  withFile hasFS (certFilePath roundNo) (AppendMode AllowExisting) $ \h -> do
    hTruncate hasFS h 0
    void $ hPutAll hasFS h $ serialize $ encodeCertFile pcdbCodecConfig cert

readCertFile ::
  (IOLike m, DecodeDisk blk (PerasCert blk)) =>
  CodecConfig blk ->
  HasFS m h ->
  PerasRoundNo ->
  m (Either DecoderError (WithArrivalTime (ValidatedPerasCert blk)))
readCertFile ccfg hasFS roundNo =
  decodeFullDecoder "PerasCertDB certificate file" (decodeCertFile ccfg)
    <$> withFile hasFS (certFilePath roundNo) ReadMode (hGetAll hasFS)

encodeCertFile ::
  EncodeDisk blk (PerasCert blk) =>
  CodecConfig blk ->
  WithArrivalTime (ValidatedPerasCert blk) ->
  Encoding
encodeCertFile ccfg (WithArrivalTime arrivalTime cert) =
  encodeListLen 2
    <> toCBOR arrivalTime
    <> encodeDisk ccfg cert

decodeCertFile ::
  DecodeDisk blk (PerasCert blk) =>
  CodecConfig blk ->
  Decoder s (WithArrivalTime (ValidatedPerasCert blk))
decodeCertFile ccfg = do
  decodeListLenOf 2
  WithArrivalTime <$> fromCBOR <*> decodeDisk ccfg

rootDir :: FsPath
rootDir = mkFsPath []

-- | Named after the round, so that the latest certificates can be selected
-- without decoding every file.
certFileName :: PerasRoundNo -> String
certFileName roundNo = show (unPerasRoundNo roundNo) <> ".cert"

certFilePath :: PerasRoundNo -> FsPath
certFilePath roundNo = mkFsPath [certFileName roundNo]

-- | Inverse of 'certFileName'. Non-canonical names (e.g. @042.cert@) are
-- rejected, as 'certFilePath' would not point to them.
certRoundFromFileName :: String -> Maybe PerasRoundNo
certRoundFromFileName name = do
  roundNo <- PerasRoundNo <$> readMaybe (takeWhile isDigit name)
  guard (certFileName roundNo == name)
  pure roundNo
