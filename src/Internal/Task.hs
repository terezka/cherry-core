{-# LANGUAGE GADTs #-}
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
import qualified Text
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
import Text (Text)
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
  Task { toIO :: Key -> IO (Result x a) }


instance Functor (Task a) where
  fmap func task =
    Task <| \key ->
      toIO task key
        |> fmap (Result.map func)


instance Applicative (Task a) where
  pure a =
    succeed a

  (<*>) func task =
    Task <| \key -> do
      rFunc <- toIO func key
      rTask <- toIO task key
      let apply f t = f t
      return (Result.map2 apply rFunc rTask)


instance Monad (Task a) where
  task >>= func =
    Task <| \key -> do
      result <- toIO task key
      case result of
        Ok ok -> toIO (func ok) key
        Err err -> return (Err err)



-- KEY


data Key = Key
  { key_namespace :: Text
  , key_context :: Dict Text Json.Value
  , key_queues :: List Queue
  , key_callstack :: Stack.CallStack
  , key_tracer :: Tracer
  }


{-| A traces allows you to do something before and after each of
your `segment`'s have run. This is useful for timing blocks of code.

-}
data Tracer where
  Tracer :: (Text -> Json.Value -> Task x a) -> (Result x a -> Task y b) -> Tracer


{-| Create a tracer. Arguments:

  1. A task to run before each `segment`. It gets the namespace and context of the
     `segment` in question. You can lookup stuff in the context using `Entry.lookup`.
  2. A task to run after each `segment`. It gets the result from the task from the first
     argument.

  > main =
  >    Task.customAttempt tracer [ Log.terminal Log.pretty ] app
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
tracer :: (Text -> Json.Value -> Task x a) -> (Result x a -> Task y b) -> Tracer
tracer =
  Tracer


{-| No tracer.
-}
tracerless :: Tracer
tracerless =
  Tracer (\_ _ -> succeed ()) (\_ -> succeed ())



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
attempt =
  customAttempt tracerless [ terminal Entry.pretty ]


{-| Customize your logging preferences.

  >  import Cherry.Prelude
  >  import Log
  >
  >  main =
  >    Task.customAttempt Log.tracerless [ bugsnag, Log.file Log.compact ] app

You can create a custom tracer and custom targets using `tracer` and `target`.
-}
customAttempt :: Tracer -> List Target -> Task x a -> IO (Result x a)
customAttempt tracer targets task =
  let toQueue (Target io) = io
  in do
  queues <- P.sequence (List.map toQueue targets)
  result <- toIO task (Key "" Dict.empty queues Stack.emptyCallStack tracer)
  sequence_ (List.map Queue.close queues)
  return result


{-| A task that succeeds immediately when run. It is usually used with
`andThen`. You can use it like `map` if you want:

  >  import Time
  >
  >  timeInMillis :: Task x Int
  >  timeInMillis =
  >    Time.now
  >      |> Task.andThen (\t -> succeed (Time.posixToMillis t))

-}
succeed :: a -> Task x a
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
fail :: x -> Task x a
fail x =
  Task <| \_ -> return (Err x)


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
mapError :: (x -> y) -> Task x a -> Task y a
mapError func task =
  onError (fail << func) task



-- TARGET


{-| A target is a place where your log messages ("entries") are sent. This
could be the terminal, a file, or a custom target like New Relic, Bugsnag,
or whatever you use for logging.
-}
data Target =
    Target (IO Queue)


{-| This prints your logging entries to the terminal.

  >  main =
  >    Task.customAttempt Log.tracerless [ Log.terminal Log.pretty ] app
  >

You can use `debug`, `error`, `info`, `warning`, and `alert` to create
logging entries of various severities.
-}
terminal :: (Entry -> Text) -> Target
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
  >    Task.customAttempt Log.tracerless [ Log.file "logs.txt" Log.compact ] app
  >

You can use `debug`, `error`, `info`, `warning`, and `alert` to create
logging entries of various severities.
-}
file :: FilePath -> (Entry -> Text) -> Target
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
  >    Task.customAttempt Log.tracerless [bugsnag ] app
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
target :: (Entry -> Task x ()) -> Target
target write =
  Target <| do
    queue <- Queue.init
    Queue.listen queue
      (void << attempt << write)
      (return ())
    return queue



-- LOGGING


{-| Send a debug log entry.

  >  main =
  >    Task.perform app
  >
  >  app :: Task x ()
  >  app = do
  >    level <- getLevel
  >    Log.debug [ value "level" level ] "Hello!"
  >
-}
debug :: Stack.HasCallStack => List Entry.Context -> Text -> Task x ()
debug =
  log Entry.Debug


{-| Same as `debug`, but sends an entry with severity `Info`.
-}
info :: Stack.HasCallStack => List Entry.Context -> Text -> Task x ()
info =
  log Entry.Info


{-| Same as `debug`, but sends an entry with severity `Warning`.
-}
warning :: Stack.HasCallStack => List Entry.Context -> Text -> Task x ()
warning =
  log Entry.Warning


{-| Same as `debug`, but sends an entry with severity `Error`.
-}
error :: Stack.HasCallStack => List Entry.Context -> Text -> Task x ()
error =
  log Entry.Error


{-| Same as `debug`, but sends an entry with severity `Alert`.
-}
alert :: Stack.HasCallStack => List Entry.Context -> Text -> Task x ()
alert =
  log Entry.Alert


{-| For logging exceptions. This shouldn't be neccessary unless you're
doing interop with regular Haskell.
-}
exception :: Stack.HasCallStack => List Entry.Context -> Exception -> Task x ()
exception context exception =
  log Entry.Unknown context (exception_message exception)


log :: Entry.Severity -> List Entry.Context -> Text -> Task x ()
log severity context message =
  Task <| \key -> do
    time <- fmap Debug.toString Clock.getCurrentTime
    let finalContext = ( "time", Json.string time ) : context
    let newKey = key { key_context = Utils.appendContext (key_context key) finalContext }
    let entry = Entry.Entry severity (key_namespace newKey) message (key_context newKey) (key_callstack newKey)
    sequence_ <| List.map (Queue.push entry) (key_queues newKey)
    return (Ok ())



-- SEGMENT


{-| Add context to all subsequent entries sent.

  >  login :: User.Id -> Task Entry Error User.User
  >  login id =
  >    segment "login" [ value "user_id" id ] <|
  >      actuallyLogin id
  >      debug [ value "color" Blue ] "Hello!" -- This entry inherits the "user_id" context from the segment

Notice: You can use the `tracer` applied in `customAttempt` to do stuff before and after
each of these segments. This can be useful if, for example, you'd like to track how long
your segment takes to finish.
-}
segment :: Stack.HasCallStack => Text -> List Entry.Context -> Task x a -> Task x a
segment namespace context task =
  Task <| \key ->
    let newNamespace =
          Utils.appendNamespace (key_namespace key) namespace

        new =
          key
            { key_namespace = newNamespace
            , key_context = Utils.appendContext (key_context key) context
            , key_callstack = Stack.withFrozenCallStack Utils.appendStack newNamespace (key_callstack key)
            }

        contextAsJson =
          Json.object (Dict.toList (key_context new))

        perform (Tracer before after) = do
          stuff <- toIO (before (key_namespace new) contextAsJson) new
          result <- toIO task new
          toIO (after stuff) new
          return result
    in
    Control.catches (perform <| key_tracer new)
      [ Control.Handler (Control.throw :: Exception -> IO a)
      , Control.Handler (Control.throw << fromSomeException new :: Control.SomeException -> IO a)
      ]



-- EXCEPTION


{-| An exception with extra info. Only relavant if you're doing interop with
regular Haskell.
-}
data Exception
  = Exception
      { exception_title :: Text
      , exception_severity :: Entry.Severity
      , exception_message :: Text
      , exception_namespace :: Text
      , exception_context :: List Entry.Context
      , exception_callstack :: Stack.CallStack
      , exception_original :: Maybe Control.SomeException
      }


instance Control.Exception Exception


instance Show Exception where
  show exception =
    Data.Text.unpack <| Entry.pretty <| Entry.Entry
      { Entry.severity = exception_severity exception
      , Entry.namespace = exception_namespace exception
      , Entry.message = exception_message exception
      , Entry.context = Dict.fromList (exception_context exception)
      , Entry.callstack = exception_callstack exception
      }


fromSomeException :: Key -> Control.SomeException -> Exception
fromSomeException key exception =
  Exception
    { exception_title = "Unknown error"
    , exception_severity = Entry.Unknown
    , exception_message = Data.Text.pack (Control.displayException exception)
    , exception_namespace = key_namespace key
    , exception_context = Dict.toList (key_context key)
    , exception_callstack = key_callstack key
    , exception_original = Just exception
    }
