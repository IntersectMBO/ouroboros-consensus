module SpecHook where

import Test.Hspec
import Cardano.Crypto.Init (cryptoInit)

hook :: Spec -> Spec
hook = before_ cryptoInit

