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
  Task.perform Log.terminal messages


messages :: Task.Task () ()
messages = 
  Log.context "messages" [ ( "online", "true" ) ] <| do 
    Log.debug "" "Beginning the printing." [ ( "user", "tereza" ), ( "email", "terezasokol@gmail.com" ) ]
    print "> hello first!"
    printBad "> hello second!"


print :: P.String -> Task.Task () ()
print note =
  P.fmap Err (P.putStrLn note)
    |> Task.enter
    |> Task.andThen (\_ -> Log.info "/print" "Good print succeeded." [])
    |> Task.onError (\_ -> Log.info "/print" "Good print succeeded." [])


printBad :: P.String -> Task.Task () ()
printBad note =
  P.fmap Err (P.putStrLn "> Not working")
    |> Task.enter
    |> Task.andThen (\_ -> Log.info "/print" "Bad print succeeded." [])
    |> Task.onError (\_ -> Log.error "/print" "Bad print errored." [])

