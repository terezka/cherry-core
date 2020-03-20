module App (app) where

import qualified Cherry.Text as Text
import qualified Cherry.List as List
import qualified Cherry.Task as Task
import qualified Cherry.List as List
import qualified Cherry.Result as Result
import qualified Cherry.Debug as Debug
import qualified Cherry.Program as Program
import qualified Cherry.Dict as Dict
import qualified Cherry.Internal.Shortcut as Shortcut
import qualified Cherry.Maybe as Maybe
import qualified Cherry.Terminal as T
import qualified Prelude as P
import qualified Control.Concurrent
import qualified Keys
import qualified Settings
import qualified Logging
import Cherry.Basics
import Cherry.Log
import Cherry.Result (Result(..))
import Cherry.List (List(..))
import Cherry.Maybe (Maybe(..))
import Cherry.Text (Text)
import Cherry.Task (Task)
import Keys (Keys)


app :: Keys -> Task () ()
app keys =
  context "messages" [ ( "online", "true" ) ] <| do
    good "> hello first first!"
    debug "print" "Beginning the printing." [ ( "user", "tereza" ), ( "email", "terezasokol@gmail.com" ) ]
    good "> hello first!"
    bad "> hello second!"
    Task.enter (Control.Concurrent.threadDelay 1000000)
    good "> hello again!"
    debug "/namespace" "Last one." [ ( "user", "tereza" ), ( "email", "terezasokol@gmail.com" ) ]


good :: Text -> Task () ()
good string = do
  T.write (T.green ++ string ++ T.reset ++ T.newline)
    |> onOk (\_ -> info "good" "Good print succeeded." [])
    |> onErr (\_ -> info "good" "Good print errored." [])


bad :: Text -> Task () ()
bad string =
  Task.fail ()
    |> onOk (\_ -> info "bad" "Bad print succeeded." [])
    |> onErr (\_ -> error "bad" "Bad print errored." [])
