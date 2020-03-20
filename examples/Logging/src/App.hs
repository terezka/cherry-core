module App (app) where

import qualified Cherry.Text as Text
import qualified Cherry.Task as Task
import qualified Cherry.Program as Program
import qualified Cherry.Log as Log
import qualified Cherry.Terminal as T
import qualified Control.Concurrent
import qualified Keys
import qualified Settings
import qualified Logging
import qualified Prelude
import Cherry.Basics
import Cherry.Log
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
    |> Log.onOk (\_ -> info "good" "Good print succeeded." [])
    |> Log.onErr (\_ -> info "good" "Good print errored." [])


bad :: Text -> Task () ()
bad string =
  Task.fail ()
    |> Log.onOk (\_ -> info "bad" "Bad print succeeded." [])
    |> Log.onErr (\_ -> error "bad" "Bad print errored." [])
