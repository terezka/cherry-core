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
  print "hello first!"
  printBad "hello second!"


print :: P.String -> Task.Task () ()
print note =
  Task.logged (\_ -> Task.entry "print err!") (\_ -> Task.entry "print ok!") <|
    Task.enter <| P.fmap Ok (P.putStrLn note)


printBad :: P.String -> Task.Task () ()
printBad note =
  Task.logged (\_ -> Task.entry "printBad err!") (\_ -> Task.entry "printBad ok!") <|
    Task.enter <| P.fmap Err (P.putStrLn "Not working")