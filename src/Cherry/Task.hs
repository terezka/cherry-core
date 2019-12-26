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
  , Entry, Severity, Paragraph, entry
  ) where


import qualified Prelude as P
import qualified Data.Text as Text
import qualified GHC.Stack as Stack
import qualified Cherry.Internal as Internal
import Prelude (IO, FilePath)
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
  -- fmap :: (a -> b) -> Task x a -> Task x b
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

  -- (<*>) :: Task x (a -> b) -> Task x a -> Task x b
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
        P.pure ()

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


sequence :: List (Task x a) -> Task x a
sequence = P.error "TODO"


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
    { print = \_ -> P.pure () }


terminal :: Output
terminal =
  let print entry =
        P.putStrLn "Printing an entry!"
  in
  Output { print = print }


file :: FilePath -> Output
file filepath =
  let print entry =
        P.putStrLn "TODO print to file"
  in
  Output { print = print }

custom :: (Entry -> Task x a) -> Output
custom func =
  Output { print = func >> exit >> Internal.map (\_ -> ()) }


multiple :: List Output -> Output
multiple = P.error "TODO"

debug :: Stack.HasCallStack => Text.Text -> Context -> Task e ()
debug = P.error "TODO"

info :: Stack.HasCallStack => Text.Text -> Context -> Task e ()
info = P.error "TODO"

warning :: Stack.HasCallStack => Text.Text -> Context -> Task e ()
warning = P.error "TODO"

error :: Stack.HasCallStack => Text.Text -> Context -> Task e ()
error = P.error "TODO"

alert :: Stack.HasCallStack => Text.Text -> Context -> Task e ()
alert = P.error "TODO"


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
    print (output key) (entry result)
    P.return result


context :: Text.Text -> Context -> Task x a -> Task x a
context = P.error "TODO"


type Context =
  List ( Text.Text, Text.Text )


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


entry :: Entry
entry =
  Entry Debug "test" [] []

