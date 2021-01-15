module Environment (optional, required) where

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
optional :: String -> Task String (Maybe String)
optional name =
  Interop.fromResult <| do
    var <- System.Environment.lookupEnv (String.toList name)
    Prelude.return <| case var of
      Prelude.Nothing -> Ok Nothing
      Prelude.Just value -> Ok (Just (String.fromList value))



{-| -}
required :: String -> Task String String
required name =
  Interop.fromResult <| do
    var <- System.Environment.lookupEnv (String.toList name)
    Prelude.return <| case var of
      Prelude.Nothing -> Err "Could not read variable."
      Prelude.Just value -> Ok (String.fromList value)

