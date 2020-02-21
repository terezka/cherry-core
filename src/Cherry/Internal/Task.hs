module Cherry.Internal.Task
  ( -- * Tasks
    Task(..), perform
  , andThen, succeed, fail, sequence
  , enter, exit
  , map, map2, map3, map4, map5, map6
  , onError, mapError

  -- * Logging
  , Output, none, terminal, custom, multiple, file, compact, verbose
  , Entry(..), Severity(..)
  , debug, info, warning, error, alert
  , onOk, onErr
  , Context, context
  ) where

import qualified Prelude as P
import qualified Data.Text
import qualified Data.List
import qualified Control.Exception.Safe as Exception
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TBQueue as BQ
import qualified GHC.Stack as Stack
import qualified Cherry.Internal.Shortcut as Shortcut
import qualified Cherry.Internal.Terminal as T
import qualified Cherry.List as List
import qualified Cherry.Debug as Debug
import qualified Cherry.Text as Text
import qualified Network.HostName as HostName
import qualified System.Posix.Process as Process
import qualified System.Posix.Types as Types
import Control.Monad (void)
import Control.Exception (bracket)
import Prelude (IO, FilePath, (<>))
import Cherry.Basics
import Cherry.List (List)
import Cherry.Result (Result(..))
import Cherry.Maybe (Maybe(..))


newtype Task x a =
  Task { _run :: Key -> P.IO (Result x a) }


data Key = Key
  { _currentNamespace :: Text.Text
  , _currentHost :: HostName.HostName
  , _currentPID :: Types.ProcessID
  , _currentOutput :: Output
  , _currentQueue :: BQ.TBQueue Message
  , _currentContext :: Context
  }

data Message
  = NewEntry Entry
  | Done

instance P.Functor (Task a) where
  fmap func task =
    Task <| \key ->
      let onResult result =
            case result of
              Ok a -> Ok (func a)
              Err x -> Err x
      in
      _run task key
        |> Shortcut.map onResult


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
      func_ <- _run func key
      task_ <- _run task key
      P.return (onResult func_ task_)


instance P.Monad (Task a) where
  task >>= func =
    Task <| \key ->
      let onResult result =
            case result of
              Ok ok ->
                _run (func ok) key

              Err err ->
                P.return (Err err)
      in
      _run task key
        |> Shortcut.andThen onResult



-- BASICS


perform :: Output -> Task x a -> IO (Result x a)
perform output task = do
  queue <- STM.atomically (BQ.newTBQueue 4096)
  worker <- spawnWorker output queue

  let init = do
        host <- HostName.getHostName
        pId <- Process.getProcessID
        P.return (Key "" host pId output queue [])

  let finally _ = do
        STM.atomically (BQ.writeTBQueue queue Done)
        Async.waitCatch worker |> void
        _onDone output |> void

  bracket init finally (_run task)


spawnWorker :: Output -> BQ.TBQueue Message -> IO (Async.Async ())
spawnWorker (Output write onDone) queue =
  let loop = do
        next <- STM.atomically (BQ.readTBQueue queue)
        case next of
          NewEntry entry -> do
            Exception.tryAny (write entry) |> void
            loop

          Done -> do
            P.return ()
  in do
  Async.async loop


succeed :: a -> Task x a
succeed a =
  Task <| \_ -> P.return (Ok a)


fail :: x -> Task x a
fail x =
  Task <| \_ -> P.return (Err x)



-- MAPS


map :: (a -> b) -> Task x a -> Task x b
map =
  Shortcut.map


map2 :: (a -> b -> result) -> Task x a -> Task x b -> Task x result
map2 =
  Shortcut.map2


map3 :: (a -> b -> c -> result) -> Task x a -> Task x b -> Task x c -> Task x result
map3 =
  Shortcut.map3


map4 :: (a -> b -> c -> d -> result) -> Task x a -> Task x b -> Task x c -> Task x d -> Task x result
map4 =
  Shortcut.map4


map5 :: (a -> b -> c -> d -> e -> result) -> Task x a -> Task x b -> Task x c -> Task x d -> Task x e -> Task x result
map5 =
  Shortcut.map5


map6 :: (a -> b -> c -> d -> e -> f -> result) -> Task x a -> Task x b -> Task x c -> Task x d -> Task x e -> Task x f -> Task x result
map6 =
  Shortcut.map6


andThen :: (a -> Task x b) -> Task x a -> Task x b
andThen =
  Shortcut.andThen


sequence :: List (Task x a) -> Task x (List a)
sequence tasks =
  List.foldr (map2 (:)) (succeed []) tasks


onError :: (x -> Task y a) -> Task x a -> Task y a
onError func task =
  Task <| \key ->
    let onResult result =
          case result of
            Ok ok -> P.return (Ok ok)
            Err err -> _run (func err) key
    in
    _run task key
      |> Shortcut.andThen onResult


mapError :: (x -> y) -> Task x a -> Task y a
mapError func task =
  task |> onError (fail << func)



-- INTEROP


enter :: IO (Result x a) -> Task x a
enter io =
  Task <| \_ -> io


exit :: Task x a -> IO (Result x a)
exit =
  perform none



-- LOGGING


data Output = Output
  { _write :: Entry -> IO ()
  , _onDone :: IO ()
  }


none :: Output
none =
  Output
    { _write = \_ -> P.return ()
    , _onDone = P.return ()
    }


terminal :: Output
terminal =
  let print entry =
        T.message (color entry) (title entry) (_namespace entry)
          [ _message entry
          , "For context:"
          , contexts entry
          ]

      color entry =
        case _severity entry of
          Debug -> T.cyan
          Info -> T.cyan
          Warning -> T.yellow
          Error -> T.magenta
          Alert -> T.red

      title entry =
        case _severity entry of
          Debug -> "Debug"
          Info -> "Info"
          Warning -> "Warning"
          Error -> "Error"
          Alert -> "Alert"

      contexts entry =
        List.map context (_contexts entry)
          |> Text.join T.newline

      context ( name, value ) = do
        T.indent 4 <> name <> ": " <> value
  in
  Output
    { _write = print
    , _onDone = P.return ()
    }


file :: FilePath -> Output
file filepath =
  let print entry =
        P.error "TODO Print to file."
  in
  Output
    { _write = print
    , _onDone = P.return ()
    }


custom :: Task x a -> (Entry -> Task x a) -> Output
custom onDone func =
  Output
    { _write = func >> exit >> void
    , _onDone = onDone |> exit |> void
    }


multiple :: List Output -> Output
multiple outputs =
  let addWriter entry (Output write_ _) io =
        io |> Shortcut.afterwards (write_ entry)

      write entry =
        List.foldr (addWriter entry) Shortcut.blank outputs

      addExit (Output _ onDone) io =
        io |> Shortcut.afterwards onDone

      exit =
        List.foldr addExit Shortcut.blank outputs
  in
  Output
    { _write = write
    , _onDone = exit
    }


debug :: Stack.HasCallStack => Text.Text -> Text.Text -> Context -> Task e ()
debug =
  log Debug


info :: Stack.HasCallStack => Text.Text -> Text.Text -> Context -> Task e ()
info =
  log Info


warning :: Stack.HasCallStack => Text.Text -> Text.Text -> Context -> Task e ()
warning =
  log Warning


error :: Stack.HasCallStack => Text.Text -> Text.Text -> Context -> Task e ()
error =
  log Error


alert :: Stack.HasCallStack => Text.Text -> Text.Text -> Context -> Task e ()
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
onOk :: (a -> Task () ()) -> Task x a -> Task x a
onOk log task =
  Task <| \key -> do
    result <- _run task key
    case result of
      Ok ok ->
        _run (log ok) key |> void

      Err _ ->
        P.return ()

    P.return result


{-| -}
onErr :: (x -> Task () ()) -> Task x a -> Task x a
onErr log task =
  Task <| \key -> do
    result <- _run task key
    case result of
      Ok _ ->
        P.return ()

      Err err -> do
        _run (log err) key |> void

    P.return result


context :: Text.Text -> Context -> Task x a -> Task x a
context namespace context task =
  Task <| \key ->
    let nextKey = Key
          { _currentNamespace = _currentNamespace key <> namespace
          , _currentContext = _currentContext key ++ context
          , _currentOutput = _currentOutput key
          , _currentHost = _currentHost key
          , _currentPID = _currentPID key
          , _currentQueue = _currentQueue key
          }
    in
    _run task nextKey


data Entry = Entry
  { _severity :: Severity
  , _namespace :: Text.Text
  , _message :: Text.Text
  , _contexts :: Context
  }


data Severity
  = Debug
  | Info
  | Warning
  | Error
  | Alert


type Context =
  List ( Text.Text, Text.Text )



-- INTERNAL


log :: Severity -> Text.Text -> Text.Text -> Context -> Task x ()
log severity namespace message context =
  Task <| \key -> do
    let entry = merge key (Entry severity namespace message context)
    STM.atomically <| addToQueue (_currentQueue key) entry
    P.return (Ok ())


addToQueue :: BQ.TBQueue Message -> Entry -> STM.STM Bool
addToQueue queue entry = do
  full <- BQ.isFullTBQueue queue
  if full then
    P.return (not full)
  else do
    _ <- BQ.writeTBQueue queue (NewEntry entry)
    P.return (not full)


merge :: Key -> Entry -> Entry
merge key entry =
  Entry
    { _severity = _severity entry
    , _namespace = _currentNamespace key <> _namespace entry
    , _message = _message entry
    , _contexts = _currentContext key ++ _contexts entry
    }
