{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
-- The NFData instance for 'OneEraGenTxId' below is a bench-local orphan.
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import Cardano.Protocol.Crypto (StandardCrypto)
import Control.DeepSeq (NFData (..))
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as SBS
import Data.SOP (All, Proxy (..), lengthSList)
import Data.Word (Word8)
import Ouroboros.Consensus.Cardano.Block (CardanoEras)
import Ouroboros.Consensus.Cardano.Node ()
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras (OneEraGenTxId)
import Ouroboros.Consensus.Shelley.HFEras ()
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Test.Consensus.Cardano.GenTxIdBuilders (BuildGenTxId, oneEraGenTxIds)
import Test.Tasty.Bench (Benchmark, bench, bgroup, defaultMain, env, whnf)

-- | 'OneEraGenTxId' has no 'NFData' instance, and a real one would need the
-- raw-hash walk (or a copy of it) here, which we want to avoid. A deep force is
-- not needed regardless: operands are built once and shared, and 'compare' and
-- '==' re-walk each operand on every call, so their construction never enters
-- the per-iteration figure (confirmed in the generated Core). WHNF suffices.
instance NFData (OneEraGenTxId xs) where
  rnf x = x `seq` ()

main :: IO ()
main =
  defaultMain
    [ -- 'env' builds the benchmark tree before running 'mkOperands', so the
      -- operands are not available yet at that point. The irrefutable pattern
      -- (@~@) avoids forcing the tuple while the tree is built; the operands are
      -- touched only inside the 'bench' bodies, which run after 'mkOperands'.
      env (mkOperands @(CardanoEras StandardCrypto)) $ \ ~(primaryA, primaryB, secondary) ->
        bgroup
          "txid-eq-ord"
          [ operationGroup "compare" compare primaryA primaryB secondary
          , operationGroup "==" (==) primaryA primaryB secondary
          ]
    ]

-- | One operation (@compare@ or @==@), laid out as @equal@ + @unequal@ cells
-- over the era grid.
operationGroup ::
  String ->
  (a -> a -> b) ->
  [a] ->
  [a] ->
  [a] ->
  Benchmark
operationGroup name op primaryA primaryB secondary =
  bgroup
    name
    [ bgroup
        "equal"
        [ bench (show i) $ whnf (uncurry op) (primaryA !! i, primaryB !! i)
        | i <- eras
        ]
    , bgroup
        "unequal"
        [ bench (show i ++ "-vs-" ++ show j) $ whnf (uncurry op) (primaryA !! i, secondary !! j)
        | i <- eras
        , j <- eras
        ]
    ]

-- | Number of eras to benchmark, read from the era list type. It must come from
-- the type, not the operand lists: the benchmark tree shape cannot depend on the
-- runtime operands produced by 'env'.
numEras :: Int
numEras = lengthSList (Proxy @(CardanoEras StandardCrypto))

eras :: [Int]
eras = [0 .. numEras - 1]

-- | The 32 bytes of a txid's raw hash: the role in the first byte, the rest
-- zero. That first byte alone guarantees a role-0 id never shares a hash with a
-- role-1 id.
roleBytes :: Word8 -> ShortByteString
roleBytes role = SBS.pack (role : replicate 31 0)

-- | Two role-0 operand lists and one role-1 list (see 'roleBytes'). All role-0
-- txids share one hash, all role-1 another, and the roles differ in byte 0. So a
-- role-0/role-1 pair is always unequal (the 'unequal' cells), and two same-era
-- role-0 txids are equal (the 'equal' cells).
--
-- The role-0 operands are two separate lists, not one reused twice, so the
-- 'equal' cells compare two distinct txids — as a real membership hit does —
-- making 'compare' walk and hash both. '-fno-cse' (cabal stanza) stops GHC from
-- merging the two identical lists back into one.
mkOperands ::
  All BuildGenTxId xs =>
  IO ([OneEraGenTxId xs], [OneEraGenTxId xs], [OneEraGenTxId xs])
mkOperands = pure (operandsFor 0, operandsFor 0, operandsFor 1)

-- | One operand per era (list index = era position) for the given role.
operandsFor :: All BuildGenTxId xs => Word8 -> [OneEraGenTxId xs]
operandsFor role = oneEraGenTxIds (roleBytes role)
