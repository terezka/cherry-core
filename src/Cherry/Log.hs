module Cherry.Log
  ( -- * Entries
    Task.Entry(..), Task.Severity(..)
    -- * Targets
  , Task.Target, terminal, file, custom
    -- * Formatting
  , pretty, compact, json
    -- * Sending an entry
  , debug, info, warning, error, alert
    -- * Adding extra context
  , context, Task.Context
  ) where

import qualified Prelude as P
import qualified Data.Text as Text
import qualified Data.List
import qualified GHC.Stack as Stack
import qualified Cherry.Internal.Task as Task
import qualified Cherry.List as List
import Prelude (IO, FilePath)
import Cherry.Basics
import Cherry.List (List)
import Cherry.Task (Task)
import Cherry.Text (Text)
import Cherry.Result (Result(..))
import Cherry.Maybe (Maybe(..))
import Prelude (IO, FilePath)


{-| This prints your entries to the terminal.

  >  main :: Program
  >  main =
  >    Program.program decoder keys [ Log.terminal Log.pretty ] app
  >
-}
terminal :: (Task.Entry -> Text) -> Task.Target
terminal =
  Task.terminal


{-| This prints the logs to a file.

  >  main :: Program
  >  main =
  >    Program.program decoder keys [ Log.file "logs.txt" Log.compact ] app

-}
file :: FilePath -> (Task.Entry -> Text) -> Task.Target
file =
  Task.file


{-| Make your own target. Maybe you have a service like rollbar,
which you want to send your log entries to. The first argument allows
you to access any resource you might need to send the entry, the second
is actually sending it, and the last is what do do when the target is shut down.

  >   let open = do
  >         resource1 <- getSomeResource
  >         resource2 <- getAnotherResource
  >         Task.succeed ( resource1, resource2 )
  >
  >       write = \( resource1, resource2 ) entry ->
  >         sendUsingResources resource1 resource2 entry
  >
  >       close = \( resource1, resource2 ) -> do
  >         closeSomeResource resource1
  >         closeAnotherResource resource2
  >   in
  >   Log.custom open write close

-}
custom :: Task x r -> (r -> Task.Entry -> Task x ()) -> (r -> Task x ()) -> Task.Target
custom =
  Task.custom


{-| Pretty formatting of the entry. Can be used with
`terminal`, `file`, or a custom output.

-}
pretty :: Task.Entry -> Text
pretty =
  Task.pretty


{-| JSON formatting of the entry. Can be used with
`terminal`, `file`, or a custom output.

-}
json :: Task.Entry -> Text
json =
  Task.json


{-| Compact one-line formatting of the entry. Can be used with
`terminal`, `file`, or a custom output.

-}
compact :: Task.Entry -> Text
compact =
  Task.compact


{-| Send a debug log entry.

  >  main :: Program
  >  main =
  >    Program.program decoder keys [ Log.terminal Log.pretty ] app
  >
  >  doThings :: Task x ()
  >  doThings = do
  >    Http.send request
  >    Log.debug "Hello!" [ ( "user", "terezka" ) ]
  >
-}
debug :: Stack.HasCallStack => Text.Text -> Text.Text -> List Task.Context -> Task () ()
debug =
  Task.debug


{-| Same as `debug`, but sends am `Info` log entry.
-}
info :: Stack.HasCallStack => Text.Text -> Text.Text -> List Task.Context -> Task () ()
info =
  Task.info


{-| Same as `debug`, but sends a `Warning` log entry.
-}
warning :: Stack.HasCallStack => Text.Text -> Text.Text -> List Task.Context -> Task () ()
warning =
  Task.warning


{-| Same as `debug`, but sends an `Error` log entry.
-}
error :: Stack.HasCallStack => Text.Text -> Text.Text -> List Task.Context -> Task () ()
error =
  Task.error


{-| Same as `debug`, but sends an `Alert` log entry.
-}
alert :: Stack.HasCallStack => Text.Text -> Text.Text -> List Task.Context -> Task () ()
alert =
  Task.alert


{-| Add context to all subsequent entries sent.

  >  login :: User.Id -> Task Error User.User
  >  login id =
  >    context "login" [ ( "user_id", id ) ] <|
  >      actualLogin id
  >      Log.debug "Hello!" [ ( "user", "terezka" ) ] -- Resulting entry includes "user_id" in context

-}
context :: Stack.HasCallStack => Text.Text -> List Task.Context -> Task x a -> Task x a
context =
  Stack.withFrozenCallStack Task.context
