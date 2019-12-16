module Task
  ( -- Tasks make it easy to describe asynchronous operations that may fail, like HTTP requests or writing to a database.
    Task
    -- * Tasks
  , Task, perform, attempt

    -- * Chains
  , andThen, succeed, fail, sequence

    -- * Maps
  , map, map2, map3, map4, map5

    -- * Errors
  , onError, mapError
  )
where

import Basics
import Nri.Prelude.Internal (andThen, map, map2, map3, map4, map5)
import qualified Platform.Internal
import qualified Platform.Internal as Internal
import qualified Result
import Cherry.Result (Result (Err, Ok))
import Prelude (pure, sequenceA)


{-| -}
type Program
  = Prelude.IO ()


{-| -}
perform :: Task String a -> Program
perform task = do
  result <- run task
  case result of
    Result.Ok _ -> Prelude.return ()
    Result.Err msg -> Prelude.putStrLn (Prelude.show msg)


{-| -}
newtype Task x a
  = Task { run :: Key -> IO (Result x a) }
  deriving (Functor)


instance Applicative (Task x) where
  pure x = Task <| \_ -> pure (Ok x)
  (Task f) <*> (Task g) = Task <| \log ->
    map2 andMap (g log) (f log)


instance Monad (Task x) where
  (Task runTask) >>= f = Task <| \log -> do
    result <- runTask log
    case result of
      Ok y -> unTask (f y) log
      Err err -> pure (Err err)


instance MonadError e (Task x) where
  throwError err = Task <| \_ -> pure (Err err)
  catchError (Task runTask) errHandler =
    Task <| \log -> do
      result <- runTask log
      case result of
        Ok x -> pure (Ok x)
        Err err -> unTask (errHandler err) log


instance Bifunctor Task where
  bimap f g (Task runTask) =
    Task
      ( \log -> do
          result <- runTask log
          pure <| case result of
            Ok x -> Ok (g x)
            Err err -> Err (f err)
      )


-- BASICS


{-| A task that succeeds immediately when run. It is usually used with
[`andThen`](#andThen). You can use it like `map` if you want:
    import Time -- elm install elm/time
    timeInMillis :: Task x Int
    timeInMillis =
      Time.now
        |> andThen (\t -> succeed (Time.posixToMillis t))
-}
succeed :: a -> Task x a
succeed value =
  Task <| Prelude.return (Result.Ok value)


{-| A task that fails immediately when run. Like with `succeed`, this can be
used with `andThen` to check on the outcome of another task.
    type Error = NotFound
    notFound :: Task Error a
    notFound =
      fail NotFound
-}
fail :: x -> Task x a
fail error =
  Task <| Prelude.return (Result.Err error)


{-| -}
fromIO :: Prelude.IO (Result.Result x a) -> Task x a
fromIO =
  Task


{-| -}
toIO :: Task x a -> Prelude.IO (Result.Result x a)
toIO =
  run


{-| -}
fromResult :: Result.Result x a -> Task x a
fromResult =
  Task << Prelude.return



-- MAPPING


{-| Transform a task. Maybe you want to use [`elm/time`][time] to figure
out what time it will be in one hour:
    import Task exposing (Task)
    import Time -- elm install elm/time
    timeInOneHour :: Task x Time.Posix
    timeInOneHour =
      Task.map addAnHour Time.now
    addAnHour :: Time.Posix -> Time.Posix
    addAnHour time =
      Time.millisToPosix (Time.posixToMillis time + 60 * 60 * 1000)
[time]: /packages/elm/time/latest/
-}
map :: (a -> b) -> Task x a -> Task x b
map func taskA =
  taskA
    |> andThen (\a -> succeed (func a))


{-| Put the results of two tasks together. For example, if we wanted to know
the current month, we could use [`elm/time`][time] to ask:
    import Task exposing (Task)
    import Time -- elm install elm/time
    getMonth :: Task x Int
    getMonth =
      Task.map2 Time.toMonth Time.here Time.now
**Note:** Say we were doing HTTP requests instead. `map2` does each task in
order, so it would try the first request and only continue after it succeeds.
If it fails, the whole thing fails!
[time]: /packages/elm/time/latest/
-}
map2 :: (a -> b -> result) -> Task x a -> Task x b -> Task x result
map2 func taskA taskB =
  taskA
    |> andThen (\a -> taskB
    |> andThen (\b -> succeed (func a b)))


{-|-}
map3 :: (a -> b -> c -> result) -> Task x a -> Task x b -> Task x c -> Task x result
map3 func taskA taskB taskC =
  taskA
    |> andThen (\a -> taskB
    |> andThen (\b -> taskC
    |> andThen (\c -> succeed (func a b c))))


{-|-}
map4 :: (a -> b -> c -> d -> result) -> Task x a -> Task x b -> Task x c -> Task x d -> Task x result
map4 func taskA taskB taskC taskD =
  taskA
    |> andThen (\a -> taskB
    |> andThen (\b -> taskC
    |> andThen (\c -> taskD
    |> andThen (\d -> succeed (func a b c d)))))


{-|-}
map5 :: (a -> b -> c -> d -> e -> result) -> Task x a -> Task x b -> Task x c -> Task x d -> Task x e -> Task x result
map5 func taskA taskB taskC taskD taskE =
  taskA
    |> andThen (\a -> taskB
    |> andThen (\b -> taskC
    |> andThen (\c -> taskD
    |> andThen (\d -> taskE
    |> andThen (\e -> succeed (func a b c d e))))))


{-|-}
map6 :: (a -> b -> c -> d -> e -> f -> result) -> Task x a -> Task x b -> Task x c -> Task x d -> Task x e -> Task x f -> Task x result
map6 func taskA taskB taskC taskD taskE taskF =
  taskA
    |> andThen (\a -> taskB
    |> andThen (\b -> taskC
    |> andThen (\c -> taskD
    |> andThen (\d -> taskE
    |> andThen (\e -> taskF
    |> andThen (\f -> succeed (func a b c d e f)))))))


{-| Start with a list of tasks, and turn them into a single task that returns a
list. The tasks will be run in order one-by-one and if any task fails the whole
sequence fails.
    sequence [ succeed 1, succeed 2 ] == succeed [ 1, 2 ]
-}
sequence :: List (Task x a) -> Task x (List a)
sequence tasks =
  List.foldr (map2 (:)) (succeed []) tasks



-- CHAINING


{-| Chain together a task and a callback. The first task will run, and if it is
successful, you give the result to the callback resulting in another task. This
task then gets run. We could use this to make a task that resolves an hour from
now:
    import Time -- elm install elm/time
    import Process

    timeInOneHour :: Task x Time.Posix
    timeInOneHour =
      Process.sleep (60 * 60 * 1000)
        |> andThen (\_ -> Time.now)
First the process sleeps for an hour **and then** it tells us what time it is.
-}
andThen :: (a -> Task x b) -> Task x a -> Task x b
andThen func task =
  Task <| do
    result <- run task
    case result of
      Result.Ok ok -> run (func ok)
      Result.Err err -> Prelude.return (Result.Err err)


{-| -}
afterwards :: Task x b -> Task x a -> Task x b
afterwards next task =
  Task <| do
    result <- run task
    case result of
      Result.Ok _ -> run next
      Result.Err err -> Prelude.return (Result.Err err)


-- ERRORS


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
  Task <| do
    result <- run task
    case result of
      Result.Ok a -> Prelude.return (Result.Ok a)
      Result.Err x -> run <| func x


{-| Transform the error value. This can be useful if you need a bunch of error
types to match up.
    type Error
      = Http Http.Error
      | WebGL WebGL.Error
    getResources :: Task Error Resource
    getResources =
      sequence
        [ mapError Http serverTask
        , mapError WebGL textureTask
        ]
-}
mapError :: (x -> y) -> Task x a -> Task y a
mapError convert task =
  task
    |> onError (fail << convert)


{-| -}
log :: (x -> String) -> Task x a -> Task x a
log show =
  onError <| \err ->
    show err
      |> String.toChars
      |> Prelude.putStrLn
      |> IO.map (\_ -> Result.Err err)
      |> fromIO





-- |
-- Like I was saying in the `Task` documentation, just having a `Task` does not mean it is done. We must command Elm to `perform` the task:
--
--     import Time  -- elm install elm/time
--     import Task
--
--     type Msg
--       = Click
--       | Search String
--       | NewTime Time.Posix
--
--     getNewTime : Cmd Msg
--     getNewTime =
--       Task.perform NewTime Time.now
--
-- If you have worked through guide.elm-lang.org (highly recommended!) you will recognize `Cmd` from the section on The Elm Architecture. So we have changed a task like "make delicious lasagna" into a command like "Hey Elm, make delicious lasagna and give it to my update function as a `Msg` value."
perform :: (a -> msg) -> Task Never a -> Cmd msg
perform toMsg task =
  task
    |> attempt
      ( \res ->
          case res of
            Err err -> never err
            Ok x -> toMsg x
      )

-- |
-- This is very similar to `perform` except it can handle failures! So we could attempt to focus on a certain DOM node like this:
--
--     import Browser.Dom  -- elm install elm/browser
--     import Task
--
--     type Msg
--       = Click
--       | Search String
--       | Focus (Result Browser.DomError ())
--
--     focus : Cmd Msg
--     focus =
--       Task.attempt Focus (Browser.Dom.focus "my-app-search-box")
--
-- So the task is "focus on this DOM node" and we are turning it into the command "Hey Elm, attempt to focus on this DOM node and give me a Msg about whether you succeeded or failed."
attempt :: (Result x a -> msg) -> Task x a -> Cmd msg
attempt toMsg task =
  Cmd <| \logHandler ->
    Internal.unTask task logHandler
      |> map toMsg

-- |
-- A task that succeeds immediately when run. It is usually used with andThen.
-- You can use it like map if you want:
--
--    import Time -- We haven't implemented this module in Haskell yet. Wanna help?
--
--    timeInMillis : Task x Int
--    timeInMillis =
--      Time.now
--        |> andThen (\t -> succeed (Time.posixToMillis t))
succeed :: a -> Task x a
succeed x = Internal.Task (\_ -> pure (Ok x))

-- |
-- A task that succeeds immediately when run. It is usually used with `andThen`.
--
--    type Error = NotFound
--
--    notFound : Task Error a
--    notFound =
--      fail NotFound
fail :: x -> Task x a
fail x =
  Internal.Task (\_ -> pure (Err x))

-- |
-- Start with a list of tasks, and turn them into a single task that returns a list. The tasks will be run in order one-by-one and if any task fails the whole sequence fails.
--
-- sequence [ succeed 1, succeed 2 ] == succeed [ 1, 2 ]
sequence :: [Task x a] -> Task x [a]
sequence = sequenceA

-- |
-- Recover from a failure in a task. If the given task fails, we use the callback to recover.
--
--    fail "file not found"
--      |> onError (\msg -> succeed 42)
--      -- succeed 42
--
--    succeed 9
--      |> onError (\msg -> succeed 42)
--      -- succeed 9
onError :: (x -> Task y a) -> Task x a -> Task y a
onError f app =
  Internal.Task
    ( \log -> do
        res <- Internal.unTask app log
        case res of
          Ok x -> pure (Ok x)
          Err err ->
            Internal.unTask (f err) log
    )

-- |
-- Transform the error value. This can be useful if you need a bunch of error types to match up.
--
--    type Error
--      = Http Http.Error
--      | WebGL WebGL.Error
--
--    getResources : Task Error Resource
--    getResources =
--      sequence
--        [ mapError Http serverTask
--        , mapError WebGL textureTask
--        ]
mapError :: (x -> y) -> Task x a -> Task y a
mapError f app =
  Internal.Task
    ( \log ->
        Internal.unTask app log
          |> map (Result.mapError f)
    )
