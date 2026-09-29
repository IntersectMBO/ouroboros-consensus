{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The 'IsPerasCert' projection/injection class.
module Ouroboros.Consensus.Peras.Cert.Class
  ( IsPerasCert (..)
  ) where

import Ouroboros.Consensus.Block.Abstract (Point)
import Ouroboros.Consensus.BlockchainTime.WallClock.Types (WithArrivalTime (..))
import Ouroboros.Consensus.Peras.Types
  ( BoostedBlock
  , BoostedBlockCompatibleWithPoint (..)
  , PerasRoundNo
  )

-- | Types that support being treated as Peras certificates
class
  BoostedBlockCompatibleWithPoint (BoostedBlock cert) blk =>
  IsPerasCert cert blk
    | cert -> blk
  where
  getPerasCertRound :: cert -> PerasRoundNo
  getPerasCertBlock :: cert -> BoostedBlock cert

  getPerasCertPoint :: cert -> Point blk
  getPerasCertPoint = boostedBlockToPoint . getPerasCertBlock

instance
  IsPerasCert cert blk =>
  IsPerasCert (WithArrivalTime cert) blk
  where
  getPerasCertRound = getPerasCertRound . forgetArrivalTime
  getPerasCertBlock = getPerasCertBlock . forgetArrivalTime
