module Main where

import Cherry.Basics
import Cherry.Result (Result(..))
import qualified Data.Text as Text
import qualified Cherry.Task as Task
import qualified Cherry.Result as Result
import qualified Prelude as P


main :: Task.Program
main =
  print "hello!"
    |> Task.perform Task.terminal


print :: P.String -> Task.Task () ()
print note =
  Task.logged (\_ -> Task.entry) (\_ -> Task.entry) <|
    Task.enter <| P.fmap Ok (P.putStrLn note)