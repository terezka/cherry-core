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
messages = do 
  Task.debug "debug something" [ ( "user", "tereza" ), ( "email", "terezasokol@gmail.com" ) ]
  print "hello first!"
  printBad "hello second!"


print :: P.String -> Task.Task () ()
print note =
  Task.logged (\_ -> Task.entry Task.Error "here" "print err!" []) (\_ -> Task.entry Task.Info "here" "print ok!" []) <|
    Task.enter <| P.fmap Ok (P.putStrLn note)


printBad :: P.String -> Task.Task () ()
printBad note =
  Task.logged (\_ -> Task.entry Task.Error "here" "printBad err!" []) (\_ -> Task.entry Task.Info "here" "printBad ok!" []) <|
    Task.enter <| P.fmap Err (P.putStrLn "Not working")