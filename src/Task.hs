
{-|

Module      : Task
Description : Tasks make it easy to describe asynchronous operations that may fail, like HTTP requests or writing to a database.
License     : BSD 3
Maintainer  : terezasokol@gmail.com
Stability   : experimental
Portability : POSIX

Use tasks to get values that can change over time, like getting the current time,
fetching data from an external server, talking to the database etc.

-}

module Task
  ( Task, Task.perform, Task.attempt, Task.Config(..)

    -- * Chains
  , andThen, Task.succeed, Task.fail, Task.sequence

    -- * Maps
  , map, map2, map3, map4, map5, map6

    -- * Errors
  , Task.onError, Task.mapError
  ) where

import qualified Internal.Task as Task
import qualified Internal.Entry as Entry
import qualified Internal.Shortcut as Shortcut
import Basics
import Internal.Task (Task)
import Prelude (IO)


-- MAPS


{-| Transform a task. Maybe you want to use [`elm/time`][time] to figure
out what time it will be in one hour:

  >  timeInOneHour :: Task s x Time.Posix
  >  timeInOneHour =
  >    Task.map addAnHour Time.now
  >
  >  addAnHour :: Time.Posix -> Time.Posix
  >  addAnHour time =
  >    Time.millisToPosix (Time.posixToMillis time + 60 * 60 * 1000)

-}
map :: (a -> b) -> Task s x a -> Task s x b
map =
  Shortcut.map


{-| Put the results of two tasks together.

  >  newsfeed :: Task s x Newsfeed
  >  newsfeed =
  >    Task.map2 combine getUser getNews

-}
map2 :: (a -> b -> result) -> Task s x a -> Task s x b -> Task s x result
map2 =
  Shortcut.map2


{-| -}
map3 :: (a -> b -> c -> result) -> Task s x a -> Task s x b -> Task s x c -> Task s x result
map3 =
  Shortcut.map3


{-| -}
map4 :: (a -> b -> c -> d -> result) -> Task s x a -> Task s x b -> Task s x c -> Task s x d -> Task s x result
map4 =
  Shortcut.map4


{-| -}
map5 :: (a -> b -> c -> d -> e -> result) -> Task s x a -> Task s x b -> Task s x c -> Task s x d -> Task s x e -> Task s x result
map5 =
  Shortcut.map5


{-| -}
map6 :: (a -> b -> c -> d -> e -> f -> result) -> Task s x a -> Task s x b -> Task s x c -> Task s x d -> Task s x e -> Task s x f -> Task s x result
map6 =
  Shortcut.map6


{-| Chain together a task and a callback. The first task will run, and if it is
successful, you give the result to the callback resulting in another task. This
task then gets run. We could use this to make a task that resolves an hour from
now:

  >  write :: Keys -> Task s x ()
  >  write keys =
  >    Http.get (http keys) "/username"
  >      |> Task.andThen Terminal.write

As an alternative, you can use this special syntax:

  >  write :: Keys -> Task s x ()
  >  write keys = do
  >    username <- Http.get (http keys) "/username"
  >    Terminal.write username

-}
andThen :: (a -> Task s x b) -> Task s x a -> Task s x b
andThen =
  Shortcut.andThen