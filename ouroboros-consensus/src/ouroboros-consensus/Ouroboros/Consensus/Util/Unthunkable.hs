module Ouroboros.Consensus.Util.Unthunkable
  ( Unthunkable (..)
  ) where

-- | The input from which a record computes some of its values afresh every
-- time they are used.
--
-- The record quantifies existentially over @i@: it holds an @'Unthunkable' i@
-- and functions of type @'Unthunkable' i -> a@, while its type does not mention
-- @i@. So producers can hold different inputs behind one type, such as
-- different eras' transactions, and a consumer can do nothing with the input
-- but apply those functions to it.
--
-- The record never holds a thunk of their results. So, as long as each
-- function computes its result from its argument, no compiler optimization can
-- share a result between uses, and nothing retains a result after a use.
newtype Unthunkable i = MkUnthunkable i
