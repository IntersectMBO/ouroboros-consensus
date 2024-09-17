{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Utilities that avoid repetition when declaring instances for types
-- that are not amenable to @deriving@, and in particular, types with
-- existential quantification.
--
-- For existentials that only quantify one type variable, this module's
-- functionality is mostly superseded by the @some@ Hackage package. However,
-- this library involves many existentials that quantify multiple variables.
-- That can be shoehorned into @some@ with some encoding, but I believe this
-- module's weight is preferable to the overhead of using that encoding in our
-- existential data types' declarations.
module Test.Ouroboros.Consensus.ChainGenerator.Some (
    -- * 'Show'
    runShowsPrec
  , showArg
  , showCtor
  , showCtorProxy
    -- * 'Read'
  , Read.readPrec
  , readArg
  , readCtor
  , runReadPrec
    -- * 'Eq'
  , Forgotten
  , forgotten
  ) where

import           Data.Kind (Constraint, Type)
import           Data.Void (Void)
import qualified GHC.Read as Read
import           GHC.TypeLits (Symbol)
import qualified GHC.TypeLits as TE
import           Ouroboros.Consensus.Util.RedundantConstraints
import qualified Text.ParserCombinators.ReadPrec as Read
import qualified Text.Read.Lex as Read

-----

type family AbsError (s :: Symbol) (a :: Type) :: Void where
    AbsError s a = TE.TypeError (
                TE.Text "You have accidentaly applied `"
        TE.:<>: TE.Text s
        TE.:<>: TE.Text "' to a non-concrete type: "
        TE.:<>: TE.ShowType a
      )

type family NoFun (s :: Symbol) (a :: Type) (absError :: Void) :: Constraint where
    NoFun s (a -> b) abs = TE.TypeError (
                TE.Text "You have accidentaly applied `"
        TE.:<>: TE.Text s
        TE.:<>: TE.Text "' to a function type: "
        TE.:<>: TE.ShowType (a -> b)
      )
    NoFun s t        abs = ()

-----

newtype ShowBuilder a = ShowBuilder ShowS

infixl 1 `showArg`

-- | The context is satisfied by any type @a@ that is manifestly apart from @->@
runShowsPrec ::
     forall a. NoFun "runShowsPrec" a (AbsError "runShowsPrec" a)
  => Int -> ShowBuilder a -> ShowS
runShowsPrec p (ShowBuilder x) = showParen (p >= 11) x
  where
    _ = keepRedundantConstraint (Proxy @(NoFun "runShowsPrec" a (AbsError "runShowsPrec" a)))

showCtor :: a -> String -> ShowBuilder a
showCtor a s =
    showCtorProxy (toProxy a) s
  where
    toProxy :: a -> Proxy a
    toProxy = const Proxy

showCtorProxy :: proxy a -> String -> ShowBuilder a
showCtorProxy _a s = ShowBuilder $ showString s

showArg :: Show a => ShowBuilder (a -> b) -> a -> ShowBuilder b
ShowBuilder l `showArg` r = ShowBuilder $ l .  showString " " . showsPrec 11 r

-----

newtype ReadBuilder a = ReadBuilder (Read.ReadPrec a)
  deriving (Applicative, Functor)

-- | The context is satisfied by any type @a@ that is manifestly apart from @->@
runReadPrec ::
     forall a. NoFun "runReadPrec" a (AbsError "runReadPrec" a)
  => ReadBuilder a -> Read.ReadPrec a
runReadPrec (ReadBuilder x) = Read.parens $ Read.prec 10 x
  where
    _ = keepRedundantConstraint (Proxy @(NoFun "runReadPrec" a (AbsError "runReadPrec" a)))

readCtor :: a -> String -> ReadBuilder a
readCtor a s = ReadBuilder $ a <$ Read.expectP (Read.Ident s)

readArg :: Read a => ReadBuilder a
readArg = ReadBuilder $ Read.step Read.readPrec

-----

-- | An opaque type that only allows for 'Eq' and human inspection
newtype Forgotten a = Forgotten a
  deriving (Eq, Show)

forgotten :: a -> Forgotten a
forgotten = Forgotten
