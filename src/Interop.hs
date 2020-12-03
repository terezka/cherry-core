
{-|

Module      : Interop
Description : Interop with third party libraries.
License     : BSD 3
Maintainer  : terezasokol@gmail.com
Stability   : experimental
Portability : POSIX

Interop with third party libraries.

-}

module Interop (enter, Shortcut.map, Shortcut.andThen) where

import qualified Control.Exception.Safe as Control
import qualified Internal.Shortcut as Shortcut
import qualified Internal.Task as Task
import qualified Internal.Shortcut as Shortcut
import Internal.Task (Task)
import Basics
import Result (Result(..))
import Prelude (IO, return)


{-| When working with third party libraries, you might need to
transform an `IO` into a `Task`. If that is the case, use this function.

-}
enter :: IO a -> Task Control.SomeException a
enter io =
  Task.Task <|
    Control.handleAny (Err >> return) (Shortcut.map Ok io)

