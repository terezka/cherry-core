module Main where

import Cherry.Basics
import Cherry.Result (Result(..))
import Cherry.Maybe (Maybe(..))
import qualified Cherry.Text as Text
import qualified Cherry.List as List
import qualified Cherry.Task as Task
import qualified Cherry.Log as Log
import qualified Cherry.Result as Result
import qualified Cherry.Debug as Debug
import qualified Cherry.Internal.Shortcut as Shortcut
import qualified Cherry.Maybe as Maybe
import qualified Cherry.Terminal as T
import qualified Prelude as P
import qualified Network.HTTP as HTTP


main :: P.IO (Result () ())
main =
  Task.perform [ bugsnag, Log.terminal ] messages


bugsnag :: Log.Output
bugsnag =
  let write entry = do
        HTTP.simpleHTTP (HTTP.getRequest "http://hackage.haskell.org/")
          |> Shortcut.andThen HTTP.getResponseBody
          |> Shortcut.map (List.take 2)
          |> Shortcut.map Result.Ok
          |> Task.enter
          |> Task.map (List.map Debug.toString >> Text.concat)
          |> Task.andThen T.write

      close =
        Task.succeed ()
  in
  Log.custom write close


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
    |> Log.onErr (\_ -> Log.info "/print" "Good print errored." [])


printBad :: Text.Text -> Task.Task () ()
printBad string =
  T.red <> "> print bad" <> T.reset <> T.newline
    |> T.write
    |> Log.onOk (\_ -> Log.info "/print" "Bad print succeeded." [])
    |> Log.onErr (\_ -> Log.error "/print" "Bad print errored." [])
