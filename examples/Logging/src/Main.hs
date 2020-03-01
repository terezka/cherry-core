module Main where

import Cherry.Basics
import Cherry.Result (Result(..))
import Cherry.Maybe (Maybe(..))
import qualified Cherry.Text as Text
import qualified Cherry.List as List
import qualified Cherry.Task as Task
import qualified Cherry.Result as Result
import qualified Cherry.Debug as Debug
import qualified Cherry.Internal.Shortcut as Shortcut
import qualified Cherry.Maybe as Maybe
import qualified Cherry.Terminal as T
import qualified Prelude as P
import qualified Network.HTTP as HTTP
import qualified Control.Concurrent
import Cherry.Text (Text)
import Cherry.Log


main :: P.IO (Result () ())
main =
  Task.perform [ bugsnag, terminal message, file "log.txt" compact ] messages


bugsnag :: Output
bugsnag =
  let open =
        Task.succeed ()

      write _ entry = do
        HTTP.simpleHTTP (HTTP.getRequest "http://hackage.haskell.org/")
          |> Shortcut.map Result.Ok
          |> Task.enter
          |> Task.andThen (\_ -> T.write "done")

      close _ =
        Task.succeed ()
  in
  custom open write close


messages :: Task.Task () ()
messages =
  context "messages" [ ( "online", "true" ) ] <| do
    good "> hello first first!"
    debug "print" "Beginning the printing." [ ( "user", "tereza" ), ( "email", "terezasokol@gmail.com" ) ]
    good "> hello first!"
    bad "> hello second!"
    Control.Concurrent.threadDelay 1000000
      |> Shortcut.map Ok
      |> Task.enter
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
