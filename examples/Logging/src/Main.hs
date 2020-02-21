module Main where

import Cherry.Basics
import Cherry.Result (Result(..))
import Cherry.Maybe (Maybe(..))
import qualified Cherry.Text as Text
import qualified Cherry.Task as Task
import qualified Cherry.Log as Log
import qualified Cherry.Result as Result
import qualified Cherry.Maybe as Maybe
import qualified Cherry.Terminal as T
import qualified Prelude as P


main :: P.IO (Result () ())
main =
  Task.perform Log.terminal messages


messages :: Task.Task () ()
messages =
  Log.context "messages" [ ( "online", "true" ) ] <| do
    Log.debug "/namespace" "Beginning the printing." [ ( "user", "tereza" ), ( "email", "terezasokol@gmail.com" ) ]
    printGood "> hello first!"
    printBad "> hello second!"


printGood :: Text.Text -> Task.Task () ()
printGood string =
  T.green <> string <> T.reset <> T.newline
    |> T.write
    |> Log.onOk (\_ -> Log.info "/print" "Good print succeeded." [])
    |> Log.onErr (\_ -> Log.info "/print" "Good print succeeded." [])


printBad :: Text.Text -> Task.Task () ()
printBad string =
  T.red <> "> print bad" <> T.reset <> T.newline
    |> T.write
    |> Log.onOk (\_ -> Log.info "/print" "Bad print succeeded." [])
    |> Log.onErr (\_ -> Log.error "/print" "Bad print errored." [])
