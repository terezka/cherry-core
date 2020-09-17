
{-|

Module      : Interop
Description : Interop with third party libraries.
License     : BSD 3
Maintainer  : terezasokol@gmail.com
Stability   : experimental
Portability : POSIX

Interop with third party libraries.

-}

module Interop (Key, key, enter) where

import qualified Control.Exception.Safe as Control
import qualified Internal.Shortcut as Shortcut
import qualified Internal.Task as Task
import qualified Internal.Shortcut as Shortcut
import Internal.Task (Task)
import Basics
import Result (Result(..))
import Prelude (IO, return)


{-| -}
data Key
  = Key


{-| -}
key :: IO Key
key =
  return Key


{-| When working with third party libraries, you might need to
transform an `IO` into a `Task`. If that is the case, use this function.

-}
enter :: Key -> IO a -> Task k Control.SomeException a
enter _ io =
  Task.Task <| \_ ->
    io
      |> Shortcut.map Ok
      |> Control.handleAny (Err >> return)

