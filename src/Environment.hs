module Environment (variable) where

import qualified System.Environment
import qualified Data.ByteString as ByteString
import qualified Internal.Shortcut as IO
import qualified Interop
import qualified Task
import qualified Result
import qualified String
import qualified Maybe
import qualified Task
import qualified Prelude
import Cherry.Prelude


{-| -}
variable :: String -> Task String String
variable name =
  Interop.fromResult <| do
    var <- System.Environment.lookupEnv (String.toList name)
    Prelude.return <| case var of
      Prelude.Nothing -> Err "Could not read variable."
      Prelude.Just value -> Ok (String.fromList value)

