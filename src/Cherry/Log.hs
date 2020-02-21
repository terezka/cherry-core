module Cherry.Log
  ( -- * Logging
    -- Logging tools for tasks.
    Output, none, terminal, custom, multiple
  , Task.Entry(..), Task.Severity(..)
  , debug, info, warning, error, alert
  , onOk, onErr
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
import Cherry.Result (Result(..))
import Cherry.Maybe (Maybe(..))


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
terminal :: Output
terminal =
  Task.terminal


{-| Make your own logging outout channel! Maybe you have a service like rollbar,
which you might want to send your logs too.

-}
custom :: Task x a -> (Entry -> Task x a) -> Output
custom =
  Task.custom


{-| Log to multiple outputs.
-}
multiple :: List Output -> Output
multiple =
  Task.multiple


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
debug :: Stack.HasCallStack => Text.Text -> Text.Text -> Context -> Task () ()
debug =
  Task.debug


{-| Same as debug, but an `Info` log entry.
-}
info :: Stack.HasCallStack => Text.Text -> Text.Text -> Context -> Task () ()
info =
  Task.info


{-| Same as debug, but an `Warning` log entry.
-}
warning :: Stack.HasCallStack => Text.Text -> Text.Text -> Context -> Task () ()
warning =
  Task.warning


{-| Same as debug, but an `Error` log entry.
-}
error :: Stack.HasCallStack => Text.Text -> Text.Text -> Context -> Task () ()
error =
  Task.error


{-| Same as debug, but an `Alert` log entry.
-}
alert :: Stack.HasCallStack => Text.Text -> Text.Text -> Context -> Task () ()
alert =
  Task.alert


{-| Add context to all subsequent tasks.

  >  login :: User.Id -> Task Error User.User
  >  login id =
  >    context "login" [ ( "user_id", id ) ] <|
  >      actualLogin id

-}
context :: Text.Text -> Context -> Task x a -> Task x a
context =
  Task.context


{-| -}
onOk :: (a -> Task () ()) -> Task x a -> Task x a
onOk =
  Task.onOk


{-| -}
onErr :: (x -> Task () ()) -> Task x a -> Task x a
onErr =
  Task.onErr


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

