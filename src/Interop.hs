
{-|

Module      : Interop
Description : Interop with third party libraries.
License     : BSD 3
Maintainer  : terezasokol@gmail.com
Stability   : experimental
Portability : POSIX

Interop with third party libraries.

-}

module Interop
  ( Key, key

  , enter, exit

  -- * Exception handling
  , Task.Exception
  ) where

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

You shouldn't usually need to use this!

-}
enter :: Key -> IO a -> Task s Task.Exception a
enter _ io =
  Task.Task <| \key ->
    io
      |> Shortcut.map Ok
      |> Control.handleAny (Task.fromSomeException key >> Shortcut.map Err)


{-| When working with third party libraries, you might need to
transform a `Task` into an `IO`. If that is the case, use this function.

You shouldn't usually need to use this!

-}
exit :: Key -> Task.Config s -> Task s x a -> IO (Result x a)
exit _ =
  Task.attempt

