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
    Log.debug "Beginning the printing." [ ( "user", "tereza" ), ( "email", "terezasokol@gmail.com" ) ]
    print "> hello first!"
    printBad "> hello second!"


print :: P.String -> Task.Task () ()
print note =
  Log.logged <| Log.Logged
    { Log.task = Task.enter <| P.fmap Ok (P.putStrLn note)
    , Log.success = \_ -> Just <| Log.Entry Log.Info "/print" "Good print succeeded." []
    , Log.failure = \_ -> Just <| Log.Entry Log.Error "/print" "Good print errored." []
    }


printBad :: P.String -> Task.Task () ()
printBad note =
  Log.logged <| Log.Logged
    { Log.task = Task.enter <| P.fmap Err (P.putStrLn "> Not working")
    , Log.success = \_ -> Just <| Log.Entry Log.Info "/print" "Bad print succeeded." []
    , Log.failure = \_ -> Just <| Log.Entry Log.Error "/print" "Bad print errored." []
    }
