-- | This tool generates valid and invalid Cardano headers.
module Main (main) where

import Cardano.Crypto.Init (cryptoInit)
import Cardano.Tools.Headers (run)
import GenHeader.Parsers (parseOptions)
import Main.Utf8 (withUtf8)

main :: IO ()
main = withUtf8 $ do
    cryptoInit
    parseOptions >>= run
