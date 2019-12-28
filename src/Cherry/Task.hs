module Cherry.Task 
  ( -- * Tasks
    -- Tasks make it easy to describe asynchronous operations that may fail, like
    -- HTTP requests or writing to a database.
    Program, Task, perform

    -- * Chains
  , andThen, succeed, fail, sequence

    -- * Interop
  , enter, exit

    -- * Maps
  , map, map2, map3, map4, map5, map6

    -- * Errors
  , onError, mapError

  -- * Logging
  , Output, none, terminal, custom, multiple -- , file, compact, verbose
  , Logged(..), Entry(..), Severity(..), logged
  , debug, info, warning, error, alert
  , Context, context
  ) where

import qualified Prelude as P
import qualified Data.Text as Text
import qualified Data.List
import qualified GHC.Stack as Stack
import qualified Cherry.Internal as Internal
import qualified Cherry.List as List
import Prelude (IO, FilePath, (<>))
import Cherry.Basics
import Cherry.List (List)
import Cherry.Result (Result(..))
import Cherry.Maybe (Maybe(..))


{-| A task is a _description_ of what you need to do. Like a todo
list. Or like a grocery list. Or like GitHub issues. So saying "the task is
to tell me the current POSIX time" does not complete the task! You need
[`perform`](#perform) tasks or [`attempt`](#attempt) tasks.
-}
newtype Task x a = 
  Task { run :: Key -> P.IO (Result x a) }


data Key = Key 
  { current_namespace :: Text.Text
  , current_context :: Context
  , output :: Output
  }


instance P.Functor (Task a) where
  fmap func task =
    Task <| \key ->
      let onResult result =
            case result of
              Ok a -> Ok (func a)
              Err x -> Err x
      in
      run task key
        |> Internal.map onResult


instance P.Applicative (Task a) where
  pure a = 
    succeed a

  (<*>) func task =
    Task <| \key ->
      let onResult resultFunc resultTask = 
            case (resultFunc, resultTask) of
              ( Ok func_, Ok task_ ) ->
                Ok (func_ task_)

              ( Err x, _ ) ->
                Err x

              ( _, Err x ) ->
                Err x
      in do 
      func_ <- run func key
      task_ <- run task key
      P.return (onResult func_ task_)


instance P.Monad (Task a) where
  task >>= func =
    Task <| \key ->
      let onResult result =
            case result of
              Ok ok ->
                run (func ok) key

              Err err ->
                P.return (Err err)
      in
      run task key
        |> Internal.andThen onResult



-- BASICS


{-| A program.

  >  main :: Task.Program
  >  main =
  >    Terminal.write "Hello world!"
  >      |> Task.perform Log.none
-}
type Program = IO ()


perform :: Output -> Task x a -> Program
perform output task =
  let onResult result =
        Internal.blank

      initKey =
        Key "" [] output
  in 
  run task initKey
    |> Internal.andThen onResult


{-| A task that succeeds immediately when run. It is usually used with
[`andThen`](#andThen). You can use it like `map` if you want:

    import Time

    timeInMillis : Task x Int
    timeInMillis =
      Time.now
        |> andThen (\t -> succeed (Time.posixToMillis t))

-}
succeed :: a -> Task x a
succeed a =
  Task <| \_ -> P.return (Ok a)


{-| A task that fails immediately when run. Like with `succeed`, this can be
used with `andThen` to check on the outcome of another task.

    type Error = NotFound

    notFound : Task Error a
    notFound =
      fail NotFound
-}
fail :: x -> Task x a
fail x =
  Task <| \_ -> P.return (Err x)



-- MAPS


{-| Transform a task. Maybe you want to use [`elm/time`][time] to figure
out what time it will be in one hour:

    import Task exposing (Task)
    import Time -- elm install elm/time

    timeInOneHour : Task x Time.Posix
    timeInOneHour =
      Task.map addAnHour Time.now

    addAnHour : Time.Posix -> Time.Posix
    addAnHour time =
      Time.millisToPosix (Time.posixToMillis time + 60 * 60 * 1000)

[time]: /packages/elm/time/latest/
-}
map :: (a -> b) -> Task x a -> Task x b
map =
  Internal.map


{-| Put the results of two tasks together. For example, if we wanted to know
the current month, we could use [`elm/time`][time] to ask:

    import Task exposing (Task)
    import Time -- elm install elm/time

    getMonth : Task x Int
    getMonth =
      Task.map2 Time.toMonth Time.here Time.now

**Note:** Say we were doing HTTP requests instead. `map2` does each task in
order, so it would try the first request and only continue after it succeeds.
If it fails, the whole thing fails!

[time]: /packages/elm/time/latest/
-}
map2 :: (a -> b -> result) -> Task x a -> Task x b -> Task x result
map2 =
  Internal.map2


{-| -}
map3 :: (a -> b -> c -> result) -> Task x a -> Task x b -> Task x c -> Task x result
map3 =
  Internal.map3


{-| -}
map4 :: (a -> b -> c -> d -> result) -> Task x a -> Task x b -> Task x c -> Task x d -> Task x result
map4 =
  Internal.map4


{-| -}
map5 :: (a -> b -> c -> d -> e -> result) -> Task x a -> Task x b -> Task x c -> Task x d -> Task x e -> Task x result
map5 =
  Internal.map5


{-| -}
map6 :: (a -> b -> c -> d -> e -> f -> result) -> Task x a -> Task x b -> Task x c -> Task x d -> Task x e -> Task x f -> Task x result
map6 =
  Internal.map6


{-| Chain together a task and a callback. The first task will run, and if it is
successful, you give the result to the callback resulting in another task. This
task then gets run. We could use this to make a task that resolves an hour from
now:

    import Time -- elm install elm/time
    import Process

    timeInOneHour : Task x Time.Posix
    timeInOneHour =
      Process.sleep (60 * 60 * 1000)
        |> andThen (\_ -> Time.now)

First the process sleeps for an hour **and then** it tells us what time it is.
-}
andThen :: (a -> Task x b) -> Task x a -> Task x b
andThen =
  Internal.andThen


{-| Start with a list of tasks, and turn them into a single task that returns a
list. The tasks will be run in order one-by-one and if any task fails the whole
sequence fails.

    sequence [ succeed 1, succeed 2 ] == succeed [ 1, 2 ]

-}
sequence :: List (Task x a) -> Task x (List a)
sequence tasks =
  List.foldr (map2 (:)) (succeed []) tasks


{-| Recover from a failure in a task. If the given task fails, we use the
callback to recover.

    fail "file not found"
      |> onError (\msg -> succeed 42)
      -- succeed 42

    succeed 9
      |> onError (\msg -> succeed 42)
      -- succeed 9
-}
onError :: (x -> Task y a) -> Task x a -> Task y a
onError func task =
  Task <| \key -> 
    let onResult result =
          case result of
            Ok ok -> P.return (Ok ok)
            Err err -> run (func err) key
    in
    run task key
      |> Internal.andThen onResult


{-| Transform the error value. This can be useful if you need a bunch of error
types to match up.

    type Error
      = Http Http.Error
      | WebGL WebGL.Error

    getResources : Task Error Resource
    getResources =
      sequence
        [ mapError Http serverTask
        , mapError WebGL textureTask
        ]
-}
mapError :: (x -> y) -> Task x a -> Task y a
mapError func task =
  task |> onError (fail << func)



-- INTEROP

{-| When working with third party libraries, you might need to
transform an `IO` into a `Task`. If that is the case, use this function.

You shouldn't usually need to use this!
-}
enter :: IO (Result x a) -> Task x a
enter io =
  Task <| \_ -> io


{-| When working with third party libraries, you might need to
transform a `Task` into an `IO`. If that is the case, use this function.

You shouldn't usually need to use this!
-}
exit :: Task x a -> IO (Result x a)
exit task =
  let key = Key "" [] none in
  run task key



-- LOGGING


{-| A output channel for logging.
-}
newtype Output =
  Output { print :: Entry -> IO () }


{-| This does not store the logs anywhere.
-}
none :: Output
none =
  Output 
    { print = \_ -> Internal.blank }


{-| This prints the logs to the terminal.

  >  main :: Program
  >  main =
  >    Http.send request
  >      |> Task.perform Log.terminal
-}
terminal :: Output
terminal =
  let print entry = 
        let severity_ = severity entry
            namespace_ = namespace entry
            headerColor_ = headerColor severity_
            headerDashes_ = headerDashes severity_ namespace_
            header = headerColor_ <> "-- " <> severityText severity_ <> " " <> headerDashes_ <> " " <> namespace_ <> " \x1b[0m"
        in do
        printLine header
        printBlank
        printText (message entry)
        printBlank
        printBlank
        printLine "For context:"
        printBlank
        printContexts (contexts entry)
        printBlank

      headerColor :: Severity -> Text.Text
      headerColor severity_ =
        case severity_ of
          Debug -> "\x1b[36m"
          Info -> "\x1b[36m"
          Warning -> "\x1b[33m"
          Error -> "\x1b[35m"
          Alert -> "\x1b[31m"

      headerDashes :: Severity -> Text.Text -> Text.Text
      headerDashes severity_ namespace_ =
        let lengthSeverity = Text.length (severityText severity_) 
            lengthNamespace = Text.length namespace_
            lengthOther = 5
            lengthDashes = 80 - lengthSeverity - lengthNamespace - lengthOther
        in
        Text.pack (Data.List.replicate lengthDashes '-')

      printContexts :: Context -> IO ()
      printContexts context =
        List.map printContext context
          |> List.foldl Internal.afterwards Internal.blank

      printContext :: ( Text.Text, Text.Text ) -> IO ()
      printContext ( name, value ) = do
        printLine <| "    " <> name <> ": " <> value

      printLine :: Text.Text -> IO ()
      printLine =
        P.putStrLn << Text.unpack

      printText :: Text.Text -> IO ()
      printText =
        P.putStr << Text.unpack

      printBlank :: IO ()
      printBlank =
        P.putStrLn ""
  in
  Output { print = print }


{-| -}
file :: FilePath -> Output
file filepath =
  let print entry =
        P.error "TODO Print to file."
  in
  Output { print = print }


{-| Make your own logging outout channel! Maybe you have a service like rollbar,
which you might want to send your logs too.

-}
custom :: (Entry -> Task x a) -> Output
custom func =
  Output { print = func >> exit >> Internal.map (\_ -> ()) }


{-| Log to multiple outputs.
-}
multiple :: List Output -> Output
multiple outputs =
  let addOutput entry (Output print) io =
        io |> Internal.afterwards (print entry)

      addOutputs entry =
        List.foldr (addOutput entry) Internal.blank outputs
  in
  Output { print = addOutputs }


{-| Send a debug log entry.

  >  main :: Program
  >  main =
  >    Task.perform Log.terminal doThings
  >
  >  doThings :: Task x ()
  >  doThings = do
  >    Http.send request
  >    Log.debug "Hello!" [ ( "user", "terezka" ) ]
  >
-}
debug :: Stack.HasCallStack => Text.Text -> Context -> Task e ()
debug =
  log Debug 


{-| Same as debug, but an `Info` log entry.
-}
info :: Stack.HasCallStack => Text.Text -> Context -> Task e ()
info = 
  log Info 


{-| Same as debug, but an `Warning` log entry.
-}
warning :: Stack.HasCallStack => Text.Text -> Context -> Task e ()
warning = 
  log Warning 


{-| Same as debug, but an `Error` log entry.
-}
error :: Stack.HasCallStack => Text.Text -> Context -> Task e ()
error = 
  log Error 


{-| Same as debug, but an `Alert` log entry.
-}
alert :: Stack.HasCallStack => Text.Text -> Context -> Task e ()
alert = 
  log Alert 



data Measure
  = Compact
  | Verbose


verbose :: Measure
verbose =
  Verbose


compact :: Measure
compact =
  Compact


{-| -}
data Logged x a = Logged
  { task :: Task x a
  , success :: a -> Maybe Entry
  , failure :: x -> Maybe Entry
  }


{-| Add logging to a task.

    login :: User.Id -> Task Error User.User
    login id =
      Task.logged <| Log.Logged
        { task = actuallyLogin id
        , success = \user -> 
            Just <| Log.Entry
              { severity = Log.Info
              , namespace = "login"
              , message = "Succesfully logged in."
              , contexts = [ ( "username", username user ) ]
              }
        , failure = \error ->
            Just <| Log.Entry
              { severity = Log.Error
              , namespace = "login"
              , message = "Failed to logged in."
              , contexts = [ ( "user_id", id ) ]
              }
        }
-}
logged :: Logged x a -> Task x a
logged (Logged task success failure) =
  Task <| \key ->
    let entry result =
          case result of
            Ok ok -> success ok
            Err err -> failure err
    in do 
    result <- run task key
    print (output key) (merge key <| entry result)
    P.return result


{-| Add context to all subsequent tasks.

    
    login :: User.Id -> Task Error User.User
    login id =
      context "login" [ ( "user_id", id ) ] <|
        actualLogin id

-}
context :: Text.Text -> Context -> Task x a -> Task x a
context namespace context task =
  Task <| \key ->
    let key_ = Key
          { current_namespace = current_namespace key <> namespace
          , current_context = current_context key ++ context
          , output = output key
          }
    in
    run task key_


{-| A log entry.

-}
data Entry = Entry
  { severity :: Severity
  , namespace :: Text.Text
  , message :: Text.Text
  , contexts :: Context
  }


{-| -}
data Severity
  = Debug
  | Info
  | Warning
  | Error
  | Alert


{-| -}
type Context =
  List ( Text.Text, Text.Text )



-- INTERNAL


severityText :: Severity -> Text.Text
severityText severity =
  case severity of
    Debug -> "DEBUG"
    Info -> "INFO"
    Warning -> "WARNING"
    Error -> "ERROR"
    Alert -> "ALERT"


log :: Severity -> Text.Text -> Context -> Task x ()
log severity message context =
  Stack.withFrozenCallStack <|
    let entry_ = Entry severity "" message context in
    logged <| Logged
      { task = succeed ()
      , success = \_ -> entry_
      , failure = \_ -> entry_
      }


merge :: Key -> Entry -> Entry
merge key entry =
  Entry
    { severity = severity entry
    , namespace = current_namespace key <> namespace entry
    , message = message entry
    , contexts = current_context key ++ contexts entry
    }
