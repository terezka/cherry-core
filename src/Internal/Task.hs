{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports #-}

module Internal.Task where

import qualified Prelude as P
import qualified "text-utf8" Data.Text
import qualified "text-utf8" Data.Text.Encoding
import qualified Control.Exception.Safe as Control
import qualified Data.ByteString.Lazy as ByteString
import qualified GHC.Stack as Stack
import qualified Internal.Shortcut as Shortcut
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.Async as Async
import qualified System.IO
import qualified List
import qualified Dict
import qualified String
import qualified Result
import qualified Tuple
import qualified Debug
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
newtype Task x a =
  Task { toIO :: IO (Result x a) }


instance Functor (Task a) where
  fmap func task =
    Task <| do
      fmap (Result.map func) (toIO task)


instance Applicative (Task a) where
  pure a =
    succeed a

  (<*>) func task =
    Task <| do
      rFunc <- toIO func
      rTask <- toIO task
      let apply f t = f t
      return (Result.map2 apply rFunc rTask)


instance Monad (Task a) where
  task >>= func =
    Task <| do
      result <- toIO task
      case result of
        Ok ok -> toIO (func ok)
        Err err -> return (Err err)



-- BASICS


{-| Just having a `Task` does not mean it is done. We must `perform` the task:

  >  import Cherry.Prelude
  >
  >  main =
  >    Task.perform Time.now

-}
perform :: Task Never a -> IO a
perform task = do
  Ok a <- attempt task
  return a


{-| Like `perform`, except for tasks which can fail.
-}
attempt :: Task x a -> IO (Result x a)
attempt task =
  toIO task


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
succeed :: a -> Task x a
succeed a =
  Task <| return (Ok a)


{-| A task that fails immediately when run. Like with `succeed`, this can be
used with `andThen` to check on the outcome of another task.

  >  type Error = NotFound
  >
  >  notFound :: Task Error a
  >  notFound =
  >    fail NotFound
-}
fail :: x -> Task x a
fail x =
  Task <| return (Err x)


{-| Start with a list of tasks, and turn them into a single task that returns a
list. The tasks will be run in order one-by-one and if any task fails the whole
sequence fails.

  >  sequence [ succeed 1, succeed 2 ] == succeed [ 1, 2 ]

-}
sequence :: List (Task x a) -> Task x (List a)
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
onError :: (x -> Task y a) -> Task x a -> Task y a
onError func task =
  Task <| do
    result <- toIO task
    case result of
      Ok ok -> return (Ok ok)
      Err err -> toIO (func err)


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
mapError :: (x -> y) -> Task x a -> Task y a
mapError func task =
  onError (fail << func) task


{-| Run tasks in parallel.

-}
parallel :: List (Task x a) -> Task x (List a)
parallel tasks =
  Task <| Shortcut.map P.sequence (Async.forConcurrently tasks toIO)


{-| -}
fromResult :: Result x a -> Task x a
fromResult result =
  case result of
    Ok a -> succeed a
    Err x -> fail x

