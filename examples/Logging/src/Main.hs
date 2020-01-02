module Main where

import Cherry.Basics
import Cherry.Result (Result(..))
import Cherry.Maybe (Maybe(..))
import qualified Data.Text as Text
import qualified Cherry.Task as Task
import qualified Cherry.Log as Log
import qualified Cherry.Result as Result
import qualified Cherry.Maybe as Maybe
import qualified Prelude as P


main :: Task.Program
main =
  Task.perform rollbar messages


rollbar :: Log.Output
rollbar =
  Log.custom <| \_ ->
    Task.succeed (10 // 0)
    |> Task.andThen (\n -> print Ok (P.show n))


messages :: Task.Task () ()
messages =
  Log.context "messages" [ ( "online", "true" ) ] <| do
    Log.debug "" "Beginning the printing." [ ( "user", "tereza" ), ( "email", "terezasokol@gmail.com" ) ]
    printGood "> hello first!"
    printBad "> hello second!"


printGood :: P.String -> Task.Task () ()
printGood note =
  print Ok note
    |> Log.onOk (\_ -> Log.info "/print" "Good print succeeded." [])
    |> Log.onErr (\_ -> Log.info "/print" "Good print succeeded." [])


printBad :: P.String -> Task.Task () ()
printBad note =
  print Err "> Not working"
    |> Log.onOk (\_ -> Log.info "/print" "Bad print succeeded." [])
    |> Log.onErr (\_ -> Log.error "/print" "Bad print errored." [])


print :: (() -> Result () ()) -> P.String -> Task.Task () ()
print toResult text =
  P.fmap toResult (P.putStrLn text)
    |> Task.enter