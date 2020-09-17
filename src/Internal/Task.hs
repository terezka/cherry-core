{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Internal.Task where

import qualified Prelude as P
import qualified Data.Text
import qualified Control.Exception.Safe as Control
import qualified Data.Text.Encoding
import qualified Data.ByteString.Lazy as ByteString
import qualified GHC.Stack as Stack
import qualified Internal.Shortcut as Shortcut
import qualified Control.Concurrent.MVar as MVar
import qualified System.IO
import qualified List
import qualified Dict
import qualified String
import qualified Result
import qualified Tuple
import qualified Debug
import qualified Json.Encode as Json
import qualified Json.Encode as Json.Encode
import qualified Json.Decode as Json.Decode
import qualified System.IO
import qualified Data.Time.Clock as Clock
import qualified GHC.Stack as Stack
import qualified Internal.Utils as Utils
import Prelude (IO, Show, Functor, Monad, Applicative, FilePath, sequence_, pure, return, fmap, show)
import Control.Monad (void)
import Internal.Shortcut
import Basics
import Maybe (Maybe (..))
import Result (Result (..))
import String (String)
import Dict (Dict)
import List (List)
import Array (Array)
import Set (Set)
import Char (Char)



{-| A task is a __description__ of what you need to do. Like a todo
list. Or like a grocery list. Or like GitHub issues. So saying "the task is
to tell me the current POSIX time" does not complete the task! You need
`perform` tasks or `attempt` tasks.
-}
newtype Task k x a =
  Task { toIO :: k -> IO (Result x a) }


instance Functor (Task k a) where
  fmap func task =
    Task <| \key ->
      toIO task key
        |> fmap (Result.map func)


instance Applicative (Task k a) where
  pure a =
    succeed a

  (<*>) func task =
    Task <| \key -> do
      rFunc <- toIO func key
      rTask <- toIO task key
      let apply f t = f t
      return (Result.map2 apply rFunc rTask)


instance Monad (Task k a) where
  task >>= func =
    Task <| \key -> do
      result <- toIO task key
      case result of
        Ok ok -> toIO (func ok) key
        Err err -> return (Err err)



-- BASICS


{-| Just having a `Task` does not mean it is done. We must `perform` the task:

  >  import Cherry.Prelude
  >
  >  main =
  >    Task.perform Time.now

-}
perform :: k -> Task k Never a -> IO a
perform config task = do
  Ok a <- attempt config task
  return a


{-| Like `perform`, except for tasks which can fail.
-}
attempt :: k -> Task k x a -> IO (Result x a)
attempt key task =
  toIO task key


{-| A task that succeeds immediately when run. Often useful in the last
statement of a `do` block.

  >  import Time
  >
  >  timeAndZone :: Task x (Time.Posix, Time.Zone)
  >  timeAndZone = do
  >    time <- Time.now
  >    timezone <- Time.here
  >    Task.succeed (time, timezone)

-}
succeed :: a -> Task k x a
succeed a =
  Task <| \_ -> return (Ok a)


{-| A task that fails immediately when run. Like with `succeed`, this can be
used with `andThen` to check on the outcome of another task.

  >  type Error = NotFound
  >
  >  notFound :: Task Error a
  >  notFound =
  >    fail NotFound
-}
fail :: x -> Task k x a
fail x =
  Task <| \_ -> return (Err x)


{-| Start with a list of tasks, and turn them into a single task that returns a
list. The tasks will be run in order one-by-one and if any task fails the whole
sequence fails.

  >  sequence [ succeed 1, succeed 2 ] == succeed [ 1, 2 ]

-}
sequence :: List (Task k x a) -> Task k x (List a)
sequence tasks =
  List.foldr (map2 (:)) (succeed []) tasks


{-| Recover from a failure in a task. If the given task fails, we use the
callback to recover.

  >  fail "file not found"
  >    |> onError (\msg -> succeed 42)
  >    -- succeed 42
  >
  >  succeed 9
  >    |> onError (\msg -> succeed 42)
  >    -- succeed 9
-}
onError :: (x -> Task k y a) -> Task k x a -> Task k y a
onError func task =
  Task <| \key -> do
    result <- toIO task key
    case result of
      Ok ok -> return (Ok ok)
      Err err -> toIO (func err) key


{-| Transform the error value. This can be useful if you need a bunch of error
types to match up.

  >  data Error
  >    = Http Http.Error
  >    | WebGL WebGL.Error
  >
  >  getResources :: Task Error Resource
  >  getResources =
  >    sequence
  >      [ mapError Http serverTask
  >      , mapError WebGL textureTask
  >      ]
-}
mapError :: (x -> y) -> Task k x a -> Task k y a
mapError func task =
  onError (fail << func) task


{-| Update your key. Useful for accumulating logging data.

-}
mapKey :: (k -> l) -> Task l x a -> Task k x a
mapKey update task =
  Task <| \key ->
    toIO task (update key)


{-| Do something with your key. Useful for sending logs.

-}
getKey :: (k -> l) -> Task k x l
getKey f =
  Task <| \key ->
    return (Ok (f key))

