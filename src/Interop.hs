module Interop (Key, key, enter, exit) where


import qualified Control.Exception.Safe as Control
import qualified Internal.Shortcut as Shortcut
import qualified Internal.Task as Task
import Cherry.Prelude
import Internal.Task (Task)
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
enter :: Key -> IO a -> Task Task.Exception a
enter _ io =
  Task.Task <| \key ->
    io
      |> Shortcut.map Ok
      |> Control.handleAny (Task.fromSomeException key >> Err >> return)


{-| When working with third party libraries, you might need to
transform a `Task` into an `IO`. If that is the case, use this function.

You shouldn't usually need to use this!
-}
exit :: Key -> Task x a -> IO (Result x a)
exit _ =
  Task.attempt Nothing []