module Main where

import Cherry.Basics
import Cherry.Result (Result(..))
import qualified Data.Text as Text
import qualified Cherry.Task as Task
import qualified Cherry.Result as Result
import qualified Prelude as P


main :: Task.Program
main =
  Task.perform Task.terminal messages


messages :: Task.Task () ()
messages = 
  Task.context "messages" [ ( "online", "true" ) ] <| do 
    Task.debug "Beginning the printing." [ ( "user", "tereza" ), ( "email", "terezasokol@gmail.com" ) ]
    print "> hello first!"
    printBad "> hello second!"


print :: P.String -> Task.Task () ()
print note =
  Task.logged (\_ -> Task.note Task.Error "/print" "Good print errored." []) (\_ -> Task.note Task.Info "/print" "Good print succeeded." []) <|
    Task.enter <| P.fmap Ok (P.putStrLn note)


printBad :: P.String -> Task.Task () ()
printBad note =
  Task.logged (\_ -> Task.note Task.Error "/print" "Bad print errored." []) (\_ -> Task.note Task.Info "/print" "Bad print succeeded." []) <|
    Task.enter <| P.fmap Err (P.putStrLn "> Not working")