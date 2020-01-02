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
import qualified Data.Text as Text
import qualified Data.List
import qualified Control.Exception as Exception
import qualified GHC.Stack as Stack
import qualified Cherry.Internal.Shortcut as Shortcut
import qualified Cherry.List as List
import Control.Exception (catch)
import Prelude (IO, FilePath, (<>))
import Cherry.Basics
import Cherry.List (List)
import Cherry.Result (Result(..))
import Cherry.Maybe (Maybe(..))


newtype Task x a =
  Task { run :: Key -> P.IO (Result x a) }


data Key = Key
  { current_namespace :: Text.Text
  , current_context :: Context
  , current_output :: Output
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
  run task initKey
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
            Err err -> run (func err) key
    in
    run task key
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
  run task key



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
          |> List.foldl Shortcut.afterwards Shortcut.blank

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
    result <- run task key
    case result of
      Ok ok ->
        run (log ok) key
          |> P.fmap (\_ -> ())

      Err _ ->
        Shortcut.blank
    P.return result


{-| -}
onErr :: (x -> Task () ()) -> Task x a -> Task x a
onErr log task =
  Task <| \key -> do
    result <- run task key
    case result of
      Ok _ ->
        Shortcut.blank

      Err err ->
        run (log err) key
          |> P.fmap (\_ -> ())
    P.return result


context :: Text.Text -> Context -> Task x a -> Task x a
context namespace context task =
  Task <| \key ->
    let key_ = Key
          { current_namespace = current_namespace key <> namespace
          , current_context = current_context key ++ context
          , current_output = current_output key
          }
    in
    run task key_


data Entry = Entry
  { severity :: Severity
  , namespace :: Text.Text
  , message :: Text.Text
  , contexts :: Context
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


severityText :: Severity -> Text.Text
severityText severity =
  case severity of
    Debug -> "DEBUG"
    Info -> "INFO"
    Warning -> "WARNING"
    Error -> "ERROR"
    Alert -> "ALERT"


log :: Severity -> Text.Text -> Text.Text -> Context -> Task x ()
log severity namespace message context =
  Stack.withFrozenCallStack <|
    Task <| \key ->
      let entry_ = Entry severity namespace message context
          output = _output (current_output key) (merge key entry_)
      in do
      output `catch` ignoreException
      P.return (Ok ())


ignoreException :: Exception.SomeException -> IO ()
ignoreException e =
  Shortcut.blank


merge :: Key -> Entry -> Entry
merge key entry =
  Entry
    { severity = severity entry
    , namespace = current_namespace key <> namespace entry
    , message = message entry
    , contexts = current_context key ++ contexts entry
    }
