{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Cherry.Internal.Task
  ( -- * Tasks
    Task(..), perform
  , andThen, succeed, fail, sequence
  , enter, exit
  , map, map2, map3, map4, map5, map6
  , onError, mapError

  -- * Logging
  , Output, none, terminal, custom, file, message, json, compact
  , Entry(..), Severity(..)
  , debug, info, warning, error, alert
  , Context, context
  ) where


import qualified Prelude as P
import qualified Data.Text
import qualified Data.List
import qualified Data.Aeson as Aeson
import qualified Data.Either as Either
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Text.Encoding
import qualified Data.Time.Clock as Clock
import qualified Control.Exception.Safe as Exception
import qualified Control.Exception
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TBQueue as BQ
import qualified Control.Concurrent.MVar as MVar
import qualified GHC.Stack as Stack
import qualified Cherry.Internal.Shortcut as Shortcut
import qualified Cherry.Internal.Terminal as T
import qualified Cherry.List as List
import qualified Cherry.Debug as Debug
import qualified Cherry.Text as Text
import qualified Cherry.Dict as Dict
import qualified Cherry.Result as Result
import qualified Network.HostName as HostName
import qualified System.Posix.Process as Process
import qualified System.IO
import Control.Monad.Except (MonadError (catchError, throwError))
import Prelude (IO, Show, Functor, Monad, Applicative, pure, return, fmap, show)
import Data.ByteString.Lazy (ByteString)
import Control.Monad (void)
import Control.Exception.Safe (bracket, bracket_)
import Control.Concurrent.STM.TBQueue (TBQueue)
import Prelude (IO, FilePath)
import Network.HostName (HostName)
import System.Posix.Types (ProcessID)
import Data.Aeson ((.=))
import Cherry.Basics
import Cherry.List (List)
import Cherry.Dict (Dict)
import Cherry.Text (Text)
import Cherry.Result (Result(..))
import Cherry.Maybe (Maybe(..))


newtype Task x a =
  Task { _run :: Key -> IO (Result x a) }


data Key = Key
  { _kNamespace :: Text
  , _kContext :: Dict Text Text
  , _kHost :: HostName
  , _kPID :: ProcessID
  , _kQueue :: List (TBQueue Message)
  , _kCallstack :: Stack.CallStack
  }

data Message
  = NewEntry Entry
  | Done

instance Functor (Task a) where
  fmap func task =
    Task <| \key ->
      let onResult result =
            case result of
              Ok a -> Ok (func a)
              Err x -> Err x
      in
      _run task key
        |> fmap onResult


instance Applicative (Task a) where
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
      return (onResult func_ task_)


instance Monad (Task a) where
  task >>= func =
    Task <| \key ->
      let onResult result =
            case result of
              Ok ok ->
                _run (func ok) key

              Err err ->
                return (Err err)
      in do
      result <- _run task key
      onResult result


instance MonadError e (Task e) where
  throwError err =
    Task <| \_ ->
      return (Err err)

  catchError task handler =
    Task <| \key -> do
      result <- _run task key
      case result of
        Ok x -> return (Ok x)
        Err err -> _run (handler err) key



-- BASICS


perform :: List Output -> Task x a -> IO (Result x a)
perform outputs task = do
  host <- HostName.getHostName
  pId <- Process.getProcessID
  let emptyKey = Key "" Dict.empty host pId [] Stack.emptyCallStack

  ( queues, quiters ) <-
      outputs
        |> List.map (toQueueAndQuit emptyKey)
        |> P.sequence
        |> fmap List.unzip

  let init :: IO Key
      init =
        return (Key "" Dict.empty host pId queues Stack.emptyCallStack)

  let exit :: Key -> IO ()
      exit _ = do
        _ <- P.sequence quiters
        return ()

  bracket init exit (_run task)


toQueueAndQuit :: Key -> Output -> IO ( TBQueue Message, IO () )
toQueueAndQuit emptyKey (Output settings) = do
  resource <- (_open settings)
  let write = (_write settings) resource
  let close = (_close settings) resource

  queue <- STM.atomically (BQ.newTBQueue 4096)
  worker <- toWorker write queue

  let quit = do
        _ <- STM.atomically (BQ.writeTBQueue queue Done)
        _ <- Async.waitCatch worker
        _ <- close
        return ()

  return ( queue, quit )


toWorker :: (Entry -> IO ()) -> TBQueue Message -> IO (Async.Async ())
toWorker write queue =
  let loop = do
        next <- STM.atomically (BQ.readTBQueue queue)
        case next of
          NewEntry entry -> do
            _ <- Exception.tryAny (write entry)
            loop

          Done ->
            return ()
  in do
  Async.async loop


succeed :: a -> Task x a
succeed a =
  Task <| \_ -> return (Ok a)


fail :: x -> Task x a
fail x =
  Task <| \_ -> return (Err x)



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
            Ok ok -> return (Ok ok)
            Err err -> _run (func err) key
    in do
    result <- _run task key
    onResult result


mapError :: (x -> y) -> Task x a -> Task y a
mapError func task =
  task |> onError (fail << func)



-- INTEROP


enter :: IO a -> Task x a
enter io =
  Task <| \_ ->
    Shortcut.map Ok io


exit :: Task x a -> IO (Result x a)
exit =
  perform [none]



-- LOGGING


data Output where
  Output :: OutputSettings x resource -> Output


data OutputSettings x resource =
  OutputSettings
    { _open :: IO resource
    , _write :: resource -> Entry -> IO ()
    , _close :: resource -> IO ()
    }


none :: Output
none =
  Output <| OutputSettings
    { _open = return ()
    , _write = \_ _ -> return ()
    , _close = \_ -> return ()
    }


terminal :: (Entry -> Text) -> Output
terminal write =
  Output <| OutputSettings
    { _open = do
        System.IO.hSetBuffering System.IO.stdout System.IO.LineBuffering
        return System.IO.stdout

    , _write = \handle entry -> do
        System.IO.hPutStr handle (Data.Text.unpack (write entry))

    , _close = \handle -> do
        System.IO.hFlush handle
        System.IO.hClose handle
    }


file :: FilePath -> (Entry -> Text) -> Output
file filepath write = -- TODO check color if terminal
  Output <| OutputSettings
    { _open = do
        handle <- System.IO.openFile filepath System.IO.AppendMode
        System.IO.hSetBuffering handle System.IO.LineBuffering
        lock <- MVar.newMVar ()
        return ( handle, lock )

    , _write = \( handle, lock ) entry -> do
        bracket_ (MVar.takeMVar lock) (MVar.putMVar lock ()) <|
          System.IO.hPutStrLn handle (Data.Text.unpack (write entry))

    , _close = \( handle, lock ) -> do
        System.IO.hFlush handle
        System.IO.hClose handle
    }


custom :: Task x r -> (r -> Entry -> Task x ()) -> (r -> Task x ()) -> Output
custom open write close =
  Output <| OutputSettings
    { _open = do
        result <- exit open
        case result of
          Ok resource -> return resource
          Err _ -> P.error "Could not initiate logger."
    , _write = \r e -> exit (write r e) |> void
    , _close = \r -> exit (close r) |> void
    }


message :: Entry -> Text
message (Entry severity namespace message time host context) =
  let ( color, title ) =
        case severity of
          Debug -> ( T.cyan, "Debug" )
          Info -> ( T.cyan, "Info" )
          Warning -> ( T.yellow, "Warning" )
          Error -> ( T.magenta, "Error" )
          Alert -> ( T.red, "Alert" )

      viewContextList =
        List.append (Dict.toList context) defaults
          |> List.map viewContext
          |> Text.join T.newline

      viewContext ( name, value ) = do
        T.indent 4 ++ name ++ ": " ++ value

      defaults =
        [ ( "host", Debug.toString host )
        , ( "time", Debug.toString time )
        ]
  in
  T.message color title namespace
    [ message
    , "For context:"
    , viewContextList
    ]

json :: Entry -> Text
json =
  Aeson.encode >> ByteString.toStrict >> Data.Text.Encoding.decodeUtf8


compact :: Entry -> Text
compact (Entry severity namespace message time host context) =
  let anything c = "[" ++ Debug.toString c ++ "]"
      string c = "[" ++ c ++ "]"
  in
  Text.concat
    [ anything severity
    , string namespace
    , string message
    , anything time
    , anything host
    , anything (Dict.toList context)
    ]



-- LOG ENTRY


debug :: Stack.HasCallStack => Text -> Text -> List Context -> Task e ()
debug =
  log Debug


info :: Stack.HasCallStack => Text -> Text -> List Context -> Task e ()
info =
  log Info


warning :: Stack.HasCallStack => Text -> Text -> List Context -> Task e ()
warning =
  log Warning


error :: Stack.HasCallStack => Text -> Text -> List Context -> Task e ()
error =
  log Error


alert :: Stack.HasCallStack => Text -> Text -> List Context -> Task e ()
alert =
  log Alert


context :: Stack.HasCallStack => Text -> List Context -> Task x a -> Task x a
context namespace context task =
  Task <| \key@(Key knamespace kcontext _ _ _ kCallstack) ->
    let new = key
          { _kNamespace = knamespace ++ "/" ++ namespace
          , _kContext = merge kcontext context
          , _kCallstack =
              case Stack.getCallStack Stack.callStack of
                ( function, location ) : _ ->
                  Stack.pushCallStack ( Data.Text.unpack (knamespace ++ "/" ++ namespace), location ) kCallstack

                _ ->
                  kCallstack
          }
    in
    Exception.catches (_run task new)
      [ Exception.Handler (Exception.throwIO :: Exception -> IO a)
      , Exception.Handler (Exception.throwIO << fromSomeException new :: Exception.SomeException -> IO a)
      ]


data Exception
  = Exception
      { eTitle :: Text
      , eSeverity :: Severity
      , eMessage :: Text
      , eNamespace :: Text
      , eContext :: List Context
      , eCallstack :: Stack.CallStack
      , eOriginal :: Maybe Control.Exception.SomeException
      }


instance Control.Exception.Exception Exception


instance Show Exception where
  show (Exception title severity message namespace context_ callstack original) =
    let color =
          case severity of
            Debug -> T.cyan
            Info -> T.cyan
            Warning -> T.yellow
            Error -> T.magenta
            Alert -> T.red

        viewContext ( name, value ) = do
          T.indent 4 ++ name ++ ": " ++ value

        viewStack ( function, location ) =
          "    context \"" ++ function ++ "\" at " ++ Stack.srcLocFile location ++ ":" ++ show (Stack.srcLocStartLine location) ++ ":" ++ show (Stack.srcLocStartCol location)
    in
    T.message color title namespace
      [ message
      , "For context:"
      , List.map viewContext context_
          |> Text.join T.newline
      , "Checkpoints:"
      , Stack.getCallStack callstack
          |> List.map (Data.Text.pack << viewStack)
          |> Text.join T.newline
      ]
      |> Text.append "\n"
      |> Data.Text.unpack


fromSomeException :: Key -> Control.Exception.SomeException -> Exception
fromSomeException key exception =
  let formatted =
        Control.Exception.displayException exception
          |> Data.Text.pack
          |> Text.lines
          |> List.map (Text.append " >    ")
          |> Text.join "\n"
  in
  Exception
    "Unknown error"
    Alert
    formatted
    (_kNamespace key)
    (_kContext key |> Dict.toList)
    (_kCallstack key)
    (Just exception)



-- ENTRY


data Entry = Entry
  { _severity :: Severity
  , _namespace :: Text
  , _message :: Text
  , _time :: Clock.UTCTime
  , _host :: HostName
  , _context :: Dict Text Text
  }


instance Aeson.ToJSON Entry where
  toJSON (Entry severity namespace message time host context) =
    Aeson.object
      [ "severity" .=
          (case severity of
              Debug -> "Debug" :: Text
              Info -> "Info"
              Warning -> "Warning"
              Error -> "Error"
              Alert -> "Alert"
          )
      , "namespace" .= namespace
      , "message" .= message
      , "time" .= time
      , "host" .= host
      , "context" .= (Dict.toList context)
      ]


data Severity
  = Debug
  | Info
  | Warning
  | Error
  | Alert
  deriving (Show)


type Context =
  ( Text, Text )



-- INTERNAL


log :: Severity -> Text -> Text -> List Context -> Task x ()
log severity namespace message context =
  Task <| \(Key knamespace kcontext host pid queue callstack) -> do
    time <- Clock.getCurrentTime
    let entry = Entry severity (knamespace ++ "/" ++ namespace) message time host (merge kcontext context)
    P.sequence <| List.map (send entry >> STM.atomically) queue
    return (Ok ())


send :: Entry -> TBQueue Message -> STM.STM Bool
send entry queue = do
  full <- BQ.isFullTBQueue queue
  if full then
    return (not full)
  else do
    BQ.writeTBQueue queue (NewEntry entry)
    return (not full)


merge :: Dict Text Text -> List Context -> Dict Text Text
merge old new =
  Dict.fromList (Dict.toList old ++ new)
