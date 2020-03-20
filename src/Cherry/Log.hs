module Cherry.Log
  ( -- * Logging
    -- Logging tools for tasks.
    Output, none, terminal, file, custom, message, json, compact
  , Task.Entry(..), Task.Severity(..)
  , debug, info, warning, error, alert
  , Context, context
  ) where

import qualified Prelude as P
import qualified Data.Text as Text
import qualified Data.List
import qualified GHC.Stack as Stack
import qualified Cherry.Internal.Task as Task
import qualified Cherry.List as List
import Prelude (IO, FilePath, (<>))
import Cherry.Basics
import Cherry.List (List)
import Cherry.Task (Task)
import Cherry.Text (Text)
import Cherry.Result (Result(..))
import Cherry.Maybe (Maybe(..))
import Prelude (IO, FilePath, (<>))


{-| A output channel for logging.
-}
type Output =
  Task.Output


{-| This does not store the logs anywhere.
-}
none :: Output
none =
  Task.none


{-| This prints the logs to the terminal.

  >  main :: Program
  >  main =
  >    Http.send request
  >      |> Task.perform Log.terminal
-}
terminal :: (Entry -> Text) -> Output
terminal =
  Task.terminal


{-| -}
message :: Entry -> Text
message =
  Task.message


{-| -}
json :: Entry -> Text
json =
  Task.json


{-| -}
compact :: Entry -> Text
compact =
  Task.compact


{-| This prints the logs to a file.

  >  main :: Program
  >  main =
  >    Http.send request
  >      |> Task.perform (Log.file "log.txt")
-}
file :: FilePath -> (Entry -> Text) -> Output
file =
  Task.file


{-| Make your own logging outout channel! Maybe you have a service like rollbar,
which you might want to send your logs too. First argument is writing, then second
is what do do when the output is show down.

    > Log.custom open write close

-}
custom :: Task x r -> (r -> Entry -> Task x ()) -> (r -> Task x ()) -> Output
custom =
  Task.custom


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
debug :: Stack.HasCallStack => Text.Text -> Text.Text -> List Context -> Task () ()
debug =
  Task.debug


{-| Same as debug, but an `Info` log entry.
-}
info :: Stack.HasCallStack => Text.Text -> Text.Text -> List Context -> Task () ()
info =
  Task.info


{-| Same as debug, but an `Warning` log entry.
-}
warning :: Stack.HasCallStack => Text.Text -> Text.Text -> List Context -> Task () ()
warning =
  Task.warning


{-| Same as debug, but an `Error` log entry.
-}
error :: Stack.HasCallStack => Text.Text -> Text.Text -> List Context -> Task () ()
error =
  Task.error


{-| Same as debug, but an `Alert` log entry.
-}
alert :: Stack.HasCallStack => Text.Text -> Text.Text -> List Context -> Task () ()
alert =
  Task.alert


{-| Add context to all subsequent tasks.

  >  login :: User.Id -> Task Error User.User
  >  login id =
  >    context "login" [ ( "user_id", id ) ] <|
  >      actualLogin id

-}
context :: Stack.HasCallStack => Text.Text -> List Context -> Task x a -> Task x a
context =
  Stack.withFrozenCallStack Task.context


{-| A log entry.

-}
type Entry =
  Task.Entry


{-| -}
type Severity
  = Task.Severity


{-| -}
type Context =
  Task.Context

