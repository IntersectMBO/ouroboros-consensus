-- | The BLS signing key that the forger votes with.
module Cardano.Tools.DBSynthesizer.BlsKey
  ( readBlsSigningKey
  , renderKeyFileError
  ) where

import Cardano.Crypto.Leios (LeiosSigningKey)
import qualified Cardano.Keys as Keys
import Data.Bifunctor (bimap)
import Prettyprinter (Doc)

-- | Read a stake pool's BLS signing key from the JSON envelope that
-- @cardano-cli node key-gen-BLS@ writes. The pool registers the matching
-- verification key as its @leiosKey@, and the ledger builds the voting
-- committee from those registrations.
--
-- The envelope names its own type, and this reader checks it. If you pass a VRF
-- or KES key by mistake, the error names both types.
readBlsSigningKey :: FilePath -> IO (Either String LeiosSigningKey)
readBlsSigningKey path =
  bimap (renderKeyFileError Keys.renderTextEnvelopeError) unwrap
    <$> Keys.readFileTextEnvelope path
 where
  unwrap (Keys.BlsSigningKey sk) = sk

-- | Report one of @cardano-keys@' file errors as a message.
renderKeyFileError :: (e -> Doc ann) -> Keys.FileError e -> String
renderKeyFileError renderPayload =
  Keys.docToString . Keys.renderFileError renderPayload
