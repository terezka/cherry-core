module Main where

import Settings
import Keys
import Cherry.Basics
import Cherry.Result (Result(..))
import Cherry.List (List(..))
import Cherry.Maybe (Maybe(..))
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
import qualified Network.HTTP as HTTP
import qualified Control.Concurrent
import Cherry.Text (Text)
import Cherry.Log


main :: Program.Program
main =
  Program.program Settings.decoder Keys.init logging app


logging :: Keys.Keys -> List Output
logging keys =
  [ bugsnag, terminal message, file "log.txt" compact ]

bugsnag :: Output
bugsnag =
  let open =
        Task.succeed ()

      write _ (Entry _ _ _ _ _ context) = do
        HTTP.simpleHTTP (HTTP.getRequest "http://hackage.haskell.org/")
          |> Task.enter
          |> Task.andThen (print context)

      print context _ =
        case Dict.get "user" context of
          Just "tereza" -> T.write "tereza"
          _ -> T.write "no user"

      close _ =
        Task.succeed ()
  in
  custom open write close


app :: Keys.Keys -> Task.Task () ()
app keys =
  context "messages" [ ( "online", "true" ) ] <| do
    good "> hello first first!"
    debug "print" "Beginning the printing." [ ( "user", "tereza" ), ( "email", "terezasokol@gmail.com" ) ]
    good "> hello first!"
    bad "> hello second!"
    Task.enter (Control.Concurrent.threadDelay 1000000)
    good "> hello again!"
    debug "/namespace" "Last one." [ ( "user", "tereza" ), ( "email", "terezasokol@gmail.com" ) ]


good :: Text.Text -> Task.Task () ()
good string = do
  T.write (T.green <> string <> T.reset <> T.newline)
    |> onOk (\_ -> info "good" "Good print succeeded." [])
    |> onErr (\_ -> info "good" "Good print errored." [])


bad :: Text.Text -> Task.Task () ()
bad string =
  Task.fail ()
    |> onOk (\_ -> info "bad" "Bad print succeeded." [])
    |> onErr (\_ -> error "bad" "Bad print errored." [])
