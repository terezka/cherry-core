module Cherry.Internal.Task
  ( -- * Tasks
    Program, Task(..), perform
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
import qualified Control.Exception as Exception
import qualified GHC.Stack as Stack
import qualified Cherry.Internal.Shortcut as Shortcut
import qualified Cherry.Internal.Terminal as T
import qualified Cherry.List as List
import qualified Cherry.Text as Text
import Control.Exception (catch)
import Prelude (IO, FilePath, (<>))
import Cherry.Basics
import Cherry.List (List)
import Cherry.Result (Result(..))
import Cherry.Maybe (Maybe(..))


newtype Task x a =
  Task { _run :: Key -> P.IO (Result x a) }


data Key = Key
  { _current_namespace :: Text.Text
  , _current_context :: Context
  , _current_output :: Output
  }


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


type Program = IO ()


perform :: Output -> Task x a -> Program
perform output task =
  let onResult result =
        Shortcut.blank

      initKey =
        Key "" [] output
  in
  _run task initKey
    |> Shortcut.andThen onResult


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
exit task =
  let key = Key "" [] none in
  _run task key



-- LOGGING


newtype Output =
  Output { _output :: Entry -> IO () }


none :: Output
none =
  Output
    { _output = \_ -> Shortcut.blank }


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
  Output { _output = print }


file :: FilePath -> Output
file filepath =
  let print entry =
        P.error "TODO Print to file."
  in
  Output { _output = print }


custom :: (Entry -> Task x a) -> Output
custom func =
  Output { _output = func >> exit >> Shortcut.map (\_ -> ()) }


multiple :: List Output -> Output
multiple outputs =
  let addOutput entry (Output _output) io =
        io |> Shortcut.afterwards (_output entry)

      addOutputs entry =
        List.foldr (addOutput entry) Shortcut.blank outputs
  in
  Output { _output = addOutputs }


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
        _run (log ok) key
          |> P.fmap (\_ -> ())

      Err _ ->
        Shortcut.blank
    P.return result


{-| -}
onErr :: (x -> Task () ()) -> Task x a -> Task x a
onErr log task =
  Task <| \key -> do
    result <- _run task key
    case result of
      Ok _ ->
        Shortcut.blank

      Err err ->
        _run (log err) key
          |> P.fmap (\_ -> ())
    P.return result


context :: Text.Text -> Context -> Task x a -> Task x a
context namespace context task =
  Task <| \key ->
    let nextKey = Key
          { _current_namespace = _current_namespace key <> namespace
          , _current_context = _current_context key ++ context
          , _current_output = _current_output key
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
  Stack.withFrozenCallStack <|
    Task <| \key ->
      let entry_ = Entry severity namespace message context
          output = _output (_current_output key) (merge key entry_)
      in do
      output `catch` ignoreException
      P.return (Ok ())


ignoreException :: Exception.SomeException -> IO ()
ignoreException e =
  Shortcut.blank


merge :: Key -> Entry -> Entry
merge key entry =
  Entry
    { _severity = _severity entry
    , _namespace = _current_namespace key <> _namespace entry
    , _message = _message entry
    , _contexts = _current_context key ++ _contexts entry
    }