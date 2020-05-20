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
import qualified Control.Concurrent.MVar as MVar
import qualified System.IO
import qualified Data.Time.Clock as Clock
import qualified GHC.Stack as Stack
import qualified Control.Concurrent.Async as Async
import qualified Internal.Queue as Queue
import qualified Internal.Queue as Queue
import qualified Internal.Utils as Utils
import Prelude (IO, Show, Functor, Monad, Applicative, FilePath, sequence_, pure, return, fmap, show)
import Control.Exception.Safe (bracket, bracket_)
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
      let onResult result =
            case result of
              Ok a -> Ok (func a)
              Err x -> Err x
      in
      toIO task key
        |> fmap onResult


instance Applicative (Task a) where
  pure a =
    succeed a

  (<*>) func task =
    Task <| \key -> do
      rFunc <- toIO func key
      rTask <- toIO task key
      return <|
        case (rFunc, rTask) of
          ( Ok func_, Ok task_ ) -> Ok (func_ task_)
          ( Err x, _ ) -> Err x
          ( _, Err x ) -> Err x


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
  , key_tracer :: Maybe Tracer
  }


data Tracer where
  Tracer :: (Text -> Json.Value -> Task x a) -> (Result x a -> Task y b) -> Tracer


{-| -}
tracer :: (Text -> Json.Value -> Task x a) -> (Result x a -> Task y b) -> Tracer
tracer =
  Tracer


empty :: Maybe Tracer -> List Queue -> Key
empty tracer queues =
  Key "" Dict.empty queues Stack.emptyCallStack tracer


init :: Maybe Tracer -> List Target -> IO ( Key, IO () )
init tracer targets =
  let execute settings resource queue = do
        Queue.execute (write settings resource) queue
        close settings resource

      toQueueAndQuit (Target settings) = do
        resource <- open settings
        queue <- Queue.init
        return ( queue, execute settings resource queue )

      close_ quiters = do
        quits_ <- P.sequence (List.map Async.async quiters)
        sequence_ (List.map Async.waitCatch quits_)
  in do
  queuesAndQuiters <- P.sequence (List.map toQueueAndQuit targets)
  let (queues, quiters) = List.unzip queuesAndQuiters
  return ( empty tracer queues, close_ quiters )



-- BASICS


{-| Just having a `Task` does not mean it is done. We must `perform` the task:

  >  import Cherry.Task
  >  import Cherry.Task
  >
  >  main :: IO ()
  >  main =
  >    Task.perform [] Time.now

-}
perform :: Maybe Tracer -> List Target -> Task Never a -> IO a
perform tracer targets task = do
  Ok a <- attempt tracer targets task
  return a


{-| Like `perform`, except for tasks which can fail.
-}
attempt :: Maybe Tracer -> List Target -> Task x a -> IO (Result x a)
attempt tracer targets task =
  bracket (init tracer targets) Tuple.second (Tuple.first >> toIO task)


{-| A task that succeeds immediately when run. It is usually used with
`andThen`. You can use it like `map` if you want:

  >  import Time
  >
  >  timeInMillis :: Task x Int
  >  timeInMillis =
  >    Time.now
  >      |> andThen (\t -> succeed (Time.posixToMillis t))

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


{-| A target is a place where your entries are sent. This could be the terminal, a file, or
a custom target like New Relic, Bugsnag, or whatever you use for logging.
-}
data Target where
  Target :: Settings x resource -> Target


data Settings x resource =
  Settings
    { open :: IO resource
    , write :: resource -> Entry -> IO ()
    , close :: resource -> IO ()
    }


{-| This prints your entries to the terminal.

  >  main :: Program
  >  main =
  >    Program.program decoder keys [ Log.terminal Log.pretty ] app
  >
-}
terminal :: (Entry -> Text) -> Target
terminal write =
  Target <| Settings
    { open = Utils.openTerminal
    , write = \handle entry ->
        Utils.writeTerminal handle (write entry)
    , close = Utils.closeTerminal
    }


{-| This prints the logs to a file.

  >  main :: Program
  >  main =
  >    Program.program decoder keys [ Log.file "logs.txt" Log.compact ] app

-}
file :: FilePath -> (Entry -> Text) -> Target
file filepath write =
  Target <| Settings
    { open = Utils.openFile filepath
    , write = \handle entry ->
        Utils.writeFile handle (write entry)
    , close = Utils.closeFile
    }


{-| Make your own target. Maybe you have a service like rollbar,
which you want to send your log entries to. The first argument allows
you to access any resource you might need to send the entry, the second
is actually sending it, and the last is what do do when the target is shut down.

  >   Log.custom write

-}
custom :: (Entry -> Task x ()) -> Target
custom write =
  Target <| Settings
    { open = return ()
    , write = \_ ->
        write >> attempt Nothing [ terminal Entry.pretty ] >> void
    , close = \_ -> return ()
    }



-- LOGGING


{-| Send a debug log entry.

  >  main :: Program
  >  main =
  >    Program.program decoder keys [ Log.terminal Log.pretty ] app
  >
  >  doThings :: Task x ()
  >  doThings = do
  >    Http.send request
  >    Log.debug "Hello!" [ ( "user", "terezka" ) ]
  >
-}
debug :: Stack.HasCallStack => List Entry.Context -> Text -> Task x ()
debug =
  log Entry.Debug


{-| Same as `debug`, but sends am `Info` log entry.
-}
info :: Stack.HasCallStack => List Entry.Context -> Text -> Task x ()
info =
  log Entry.Info


{-| Same as `debug`, but sends a `Warning` log entry.
-}
warning :: Stack.HasCallStack => List Entry.Context -> Text -> Task x ()
warning =
  log Entry.Warning


{-| Same as `debug`, but sends an `Error` log entry.
-}
error :: Stack.HasCallStack => List Entry.Context -> Text -> Task x ()
error =
  log Entry.Error


{-| Same as `debug`, but sends an `Alert` log entry.
-}
alert :: Stack.HasCallStack => List Entry.Context -> Text -> Task x ()
alert =
  log Entry.Alert


{-| For logging exceptions.
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
  >    context "login" [ ( "user_id", id ) ] <|
  >      actualLogin id
  >      Log.debug "Hello!" [ context "user" "terezka" ] -- Resulting entry includes "user_id" in context
  >      Log.debug "Hello!" [ context "referrals" 12, context "color" Blue ]

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

        perform =
          case key_tracer new of
            Just tracer -> withTracer tracer
            Nothing -> toIO task new

        withTracer (Tracer before after) = do
          stuff <- toIO (before (key_namespace new) contextAsJson) new
          result <- toIO task new
          toIO (after stuff) new
          return result
    in
    Control.catches perform
      [ Control.Handler (Control.throw :: Exception -> IO a)
      , Control.Handler (Control.throw << fromSomeException new :: Control.SomeException -> IO a)
      ]



-- EXCEPTION


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


valueToText :: Json.Value -> Text
valueToText =
  Data.Text.Encoding.decodeUtf8 << ByteString.toStrict << Json.toByteString


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
