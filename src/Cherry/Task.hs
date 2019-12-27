module Cherry.Task 
  ( -- * Task
    Program, Task, perform
  , succeed, fail, logged
  , andThen, map, map2, map3, map4, map5, map6, sequence
  , onError, mapError

  -- * Interop
  , enter, exit

  -- * Logging
  , Output, none, terminal, file, compact, verbose
  , custom, multiple
  , debug, info, warning, error, alert
  , Context, context
  , Entry, Severity(..), Paragraph, note
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


{-| -}
newtype Task x a = 
  Task { run :: Key -> P.IO (Result x a) }


{-| -}
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


succeed :: a -> Task x a
succeed a =
  Task <| \_ -> P.return (Ok a)


fail :: x -> Task x a
fail x =
  Task <| \_ -> P.return (Err x)



-- NEXT


map :: (a -> b) -> Task x a -> Task x b
map =
  Internal.map

map2 :: (a -> b -> result) -> Task x a -> Task x b -> Task x result
map2 =
  Internal.map2


map3 :: (a -> b -> c -> result) -> Task x a -> Task x b -> Task x c -> Task x result
map3 =
  Internal.map3


map4 :: (a -> b -> c -> d -> result) -> Task x a -> Task x b -> Task x c -> Task x d -> Task x result
map4 =
  Internal.map4


map5 :: (a -> b -> c -> d -> e -> result) -> Task x a -> Task x b -> Task x c -> Task x d -> Task x e -> Task x result
map5 =
  Internal.map5


map6 :: (a -> b -> c -> d -> e -> f -> result) -> Task x a -> Task x b -> Task x c -> Task x d -> Task x e -> Task x f -> Task x result
map6 =
  Internal.map6


andThen :: (a -> Task x b) -> Task x a -> Task x b
andThen =
  Internal.andThen


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
      |> Internal.andThen onResult


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
  Output { print :: Entry -> IO () }


none :: Output
none =
  Output 
    { print = \_ -> Internal.blank }


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
        printParagraphs (paragraphs entry)
        printContexts (contexts entry)
        printLine ""

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

      printParagraphs :: List Paragraph -> IO ()
      printParagraphs paragraphs =
        List.map printParagraph paragraphs
          |> List.foldl Internal.afterwards Internal.blank

      printParagraph :: Paragraph -> IO ()
      printParagraph paragraph = do
        printLine ""
        printLine (text paragraph)
        printLine <| "    " <> snippet paragraph

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
  in
  Output { print = print }


file :: FilePath -> Output
file filepath =
  let print entry =
        P.error "TODO Print to file."
  in
  Output { print = print }


custom :: (Entry -> Task x a) -> Output
custom func =
  Output { print = func >> exit >> Internal.map (\_ -> ()) }


multiple :: List Output -> Output
multiple outputs =
  let addOutput entry (Output print) io =
        io |> Internal.afterwards (print entry)

      addOutputs entry =
        List.foldr (addOutput entry) Internal.blank outputs
  in
  Output { print = addOutputs }


debug :: Stack.HasCallStack => Text.Text -> Context -> Task e ()
debug =
  log Debug 


info :: Stack.HasCallStack => Text.Text -> Context -> Task e ()
info = 
  log Info 


warning :: Stack.HasCallStack => Text.Text -> Context -> Task e ()
warning = 
  log Warning 


error :: Stack.HasCallStack => Text.Text -> Context -> Task e ()
error = 
  log Error 


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


logged :: (x -> Entry) -> (a -> Entry) -> Task x a -> Task x a
logged onErr onOk task =
  Task <| \key ->
    let entry result =
          case result of
            Ok ok -> onOk ok
            Err err -> onErr err
    in do 
    result <- run task key
    print (output key) (finalEntry key <| entry result)
    P.return result


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


data Entry = Entry
  { severity :: Severity
  , namespace :: Text.Text
  , paragraphs :: List Paragraph
  , contexts :: Context
  }


data Severity
  = Debug
  | Info
  | Warning
  | Error
  | Alert


data Paragraph = Paragraph
  { text :: Text.Text
  , snippet :: Text.Text
  }


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
    let entry_ = note severity "" message context in
    logged (\_ -> entry_) (\_ -> entry_) <| succeed ()


note :: Severity -> Text.Text -> Text.Text -> Context -> Entry
note severity namespace message context =
  Entry
    { severity = severity
    , namespace = namespace
    , paragraphs = [ Paragraph message "" ]
    , contexts = context
    }


finalEntry :: Key -> Entry -> Entry
finalEntry key entry =
  Entry
    { severity = severity entry
    , namespace = current_namespace key <> namespace entry
    , paragraphs = paragraphs entry
    , contexts = current_context key ++ contexts entry
    }
