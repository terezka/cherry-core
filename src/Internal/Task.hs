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
import qualified Internal.Entry as Entry
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
import qualified Internal.Queue as Queue
import qualified Internal.Utils as Utils
import Prelude (IO, Show, Functor, Monad, Applicative, FilePath, sequence_, pure, return, fmap, show)
import Control.Monad (void)
import Internal.Shortcut
import Internal.Entry (Entry)
import Internal.Queue (Queue)
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
newtype Task s x a =
  Task { toIO :: Key s -> IO (Result x a) }


instance Functor (Task s a) where
  fmap func task =
    Task <| \key ->
      toIO task key
        |> fmap (Result.map func)


instance Applicative (Task s a) where
  pure a =
    succeed a

  (<*>) func task =
    Task <| \key -> do
      rFunc <- toIO func key
      rTask <- toIO task key
      let apply f t = f t
      return (Result.map2 apply rFunc rTask)


instance Monad (Task s a) where
  task >>= func =
    Task <| \key -> do
      result <- toIO task key
      case result of
        Ok ok -> toIO (func ok) key
        Err err -> return (Err err)



-- KEY


data Key s = Key
  { key_namespace :: String
  , key_context :: s
  , key_queues :: List (Queue s)
  , key_callstack :: Stack.CallStack
  , key_tracer :: Tracer s
  , key_encoder :: s -> Json.Value
  }


{-| A traces allows you to do something before and after each of
your `segment`'s have run. This is useful for timing blocks of code.

-}
newtype Tracer s =
  Tracer { toTracer :: forall x a. String -> s -> Task s x a -> Task s x a }


{-| Create a tracer. Arguments:

  1. A task to run before each `segment`. It gets the namespace and context of the
     `segment` in question.

  2. A task to run after each `segment`. It gets the result from the task from the first
     argument.

  > main =
  >    Task.custom tracer [ Log.terminal Log.pretty ] app
  >
  > {-| A tracer which prints the start and end time of a segment. -}
  > tracer :: Log.Tracer
  > tracer =
  >   let before namespace context =
  >         Time.now
  >
  >       after start = do
  >         end <- Time.now
  >         Terminal.line <| Debug.toString start ++ " -> " ++ Debug.String end
  >  in
  >  Log.tracer before after
-}
customTracer :: (forall x a. String -> s -> Task s x a -> Task s x a) -> Tracer s
customTracer =
  Tracer


{-| No tracer.
-}
tracerless :: Tracer s
tracerless =
  Tracer (\_ _ task -> task)



-- LOG CONFIG


{-| -}
data Config s =
  Config
    { init :: s
    , tracer :: Tracer s
    , targets :: List (Target s)
    , encoder :: s -> Json.Value
    }


{-| -}
basic :: Config Entry.Basic
basic =
  let toJson = Json.dict identity identity in
  Config
    { init = Dict.empty
    , tracer = tracerless
    , targets = [ terminal (Entry.pretty toJson) ]
    , encoder = toJson
    }



-- BASICS


{-| Just having a `Task` does not mean it is done. We must `perform` the task:

  >  import Cherry.Prelude
  >
  >  main =
  >    Task.perform Time.now

-}
perform :: Config s -> Task s Never a -> IO a
perform config task = do
  Ok a <- attempt config task
  return a


{-| Like `perform`, except for tasks which can fail.
-}
attempt :: Config s -> Task s x a -> IO (Result x a)
attempt config =
  custom config


{-| Customize your logging preferences.

  >  import Cherry.Prelude
  >  import Log
  >
  >  main =
  >    Task.custom Log.tracerless [ bugsnag, Log.file Log.compact ] app

You can create a custom tracer and custom targets using `tracer` and `target`.
-}
custom :: Config s -> Task s x a -> IO (Result x a)
custom config task = do
  queues <- P.sequence (List.map toTarget (targets config))
  result <- toIO task (Key "" (init config) queues Stack.emptyCallStack (tracer config) (encoder config))
  sequence_ (List.map Queue.close queues)
  return result


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
succeed :: a -> Task s x a
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
fail :: x -> Task s x a
fail x =
  Task <| \_ -> return (Err x)


{-| Start with a list of tasks, and turn them into a single task that returns a
list. The tasks will be run in order one-by-one and if any task fails the whole
sequence fails.

  >  sequence [ succeed 1, succeed 2 ] == succeed [ 1, 2 ]

-}
sequence :: List (Task s x a) -> Task s x (List a)
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
onError :: (x -> Task s y a) -> Task s x a -> Task s y a
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
mapError :: (x -> y) -> Task s x a -> Task s y a
mapError func task =
  onError (fail << func) task



-- TARGET


{-| A target is a place where your log messages ("entries") are sent. This
could be the terminal, a file, or a custom target like New Relic, Bugsnag,
or whatever you use for logging.
-}
newtype Target s =
  Target { toTarget :: IO (Queue s) }


{-| This prints your logging entries to the terminal.

  >  main =
  >    Task.custom Log.tracerless [ Log.terminal Log.pretty ] app
  >

You can use `debug`, `error`, `info`, `warning`, and `alert` to create
logging entries of various severities.
-}
terminal :: (Entry s -> String) -> Target s
terminal write =
  Target <| do
    queue <- Queue.init
    handle <- Utils.openTerminal
    Queue.listen queue
      (Utils.writeTerminal handle << write)
      (Utils.closeTerminal handle)
    return queue


{-| This prints your logging entries to a file.

  >  main =
  >    Task.custom Log.tracerless [ Log.file "logs.txt" Log.compact ] app
  >

You can use `debug`, `error`, `info`, `warning`, and `alert` to create
logging entries of various severities.
-}
file :: FilePath -> (Entry s -> String) -> Target s
file filepath write =
  Target <| do
    queue <- Queue.init
    handle <- Utils.openFile filepath
    Queue.listen queue
      (Utils.writeFile handle << write)
      (Utils.closeFile handle)
    return queue


{-| Make your own target. Maybe you have a service like rollbar,
which you want to send your log entries to. The first argument allows
you to access any resource you might need to send the entry, the second
is actually sending it, and the last is what do do when the target is shut down.

  >  main =
  >    Task.custom Log.tracerless [bugsnag ] app
  >
  > bugsnag :: Log.Target
  > bugsnag =
  >   Log.target <| \entry ->
  >     Bugsnag.send entry

You can use `debug`, `error`, `info`, `warning`, and `alert` to create
logging entries of various severities.

Notice: You can filter what entries you send by checking the severity
using `Entry.severity`.
-}
target :: (Entry s -> Task Entry.Basic x ()) -> Target s
target write =
  Target <| do
    queue <- Queue.init
    Queue.listen queue
      (void << attempt basic << write)
      (return ())
    return queue



-- LOGGING


{-| Send a debug log entry.

  >  main =
  >    Task.perform app
  >
  >  app :: Task s x ()
  >  app = do
  >    level <- getLevel
  >    Task.debug [ value "level" level ] "Hello!"
  >
-}
debug :: Stack.HasCallStack => List (s -> s) -> String -> Task s x ()
debug =
  log Entry.Debug


{-| Same as `debug`, but sends an entry with severity `Info`.
-}
info :: Stack.HasCallStack => List (s -> s) -> String -> Task s x ()
info =
  log Entry.Info


{-| Same as `debug`, but sends an entry with severity `Warning`.
-}
warning :: Stack.HasCallStack => List (s -> s) -> String -> Task s x ()
warning =
  log Entry.Warning


{-| Same as `debug`, but sends an entry with severity `Error`.
-}
error :: Stack.HasCallStack => List (s -> s) -> String -> Task s x ()
error =
  log Entry.Error


{-| Same as `debug`, but sends an entry with severity `Alert`.
-}
alert :: Stack.HasCallStack => List (s -> s) -> String -> Task s x ()
alert =
  log Entry.Alert


{-| For logging exceptions. This shouldn't be neccessary unless you're
doing interop with regular Haskell.
-}
exception :: Stack.HasCallStack => List (s -> s) -> Exception -> Task s x ()
exception transform exception =
  log Entry.Unknown transform (exception_message exception)


log :: Entry.Severity -> List (s -> s) -> String -> Task s x ()
log severity transform message =
  Task <| \key -> do
    time <- Clock.getCurrentTime
    let context = Utils.appendContext (key_context key) transform
    let entry = Entry.Entry severity (key_namespace key) message context time (key_callstack key)
    sequence_ <| List.map (Queue.push entry) (key_queues key)
    return (Ok ())



-- SEGMENT


{-| Add context to all subsequent entries sent.

  >  login :: User.Id -> Task Entry Error User.User
  >  login id =
  >    segment "login" [ value "user_id" id ] <|
  >      actuallyLogin id
  >      debug [ value "color" Blue ] "Hello!" -- This log entry inherits the "user_id" context from the segment

Notice: You can use the `tracer` applied in `custom` to do stuff before and after
each of these segments. This can be useful if, for example, you'd like to track how long
your segment takes to finish.
-}
segment :: Stack.HasCallStack => String -> List (s -> s) -> Task s x a -> Task s x a
segment namespace transform task =
  Task <| \key ->
    let newNamespace =
          Utils.appendNamespace (key_namespace key) namespace

        new =
          key
            { key_namespace = newNamespace
            , key_context = Utils.appendContext (key_context key) transform
            , key_callstack = Stack.withFrozenCallStack Utils.appendStack newNamespace (key_callstack key)
            }

        task_ =
          toTracer (key_tracer new) (key_namespace new) (key_context new) task
    in
    Control.catches (toIO task_ new)
      [ Control.Handler (Control.throw :: Exception -> IO a)
      , Control.Handler (andThen Control.throw << fromSomeException new :: Control.SomeException -> IO a)
      ]



-- EXCEPTION


{-| An exception with extra info. Only relavant if you're doing interop with
regular Haskell.
-}
data Exception
  = Exception
      { exception_title :: String
      , exception_severity :: Entry.Severity
      , exception_message :: String
      , exception_namespace :: String
      , exception_context :: Json.Value
      , exception_time :: Clock.UTCTime
      , exception_callstack :: Stack.CallStack
      , exception_original :: Maybe Control.SomeException
      }


instance Control.Exception Exception


instance Show Exception where
  show exception =
    Data.Text.unpack <| Entry.pretty identity <|
      Entry.Entry
        { Entry.severity = exception_severity exception
        , Entry.namespace = exception_namespace exception
        , Entry.message = exception_message exception
        , Entry.context = exception_context exception
        , Entry.time = exception_time exception
        , Entry.callstack = exception_callstack exception
        }


fromSomeException :: Key s -> Control.SomeException -> IO Exception
fromSomeException key exception = do
  time <- Clock.getCurrentTime
  return Exception
    { exception_title = "Unknown error"
    , exception_severity = Entry.Unknown
    , exception_message = Data.Text.pack (Control.displayException exception)
    , exception_namespace = key_namespace key
    , exception_context = key_encoder key (key_context key)
    , exception_time = time
    , exception_callstack = key_callstack key
    , exception_original = Just exception
    }
