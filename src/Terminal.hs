
{-|

Module      : Terminal
Description : Read and write to the terminal.
License     : BSD 3
Maintainer  : terezasokol@gmail.com
Stability   : experimental
Portability : POSIX

Read and write to the terminal.

-}

module Terminal (line, lines, read) where

import qualified List
import qualified String
import qualified Internal.Task as Task
import qualified Data.Text.IO as IO
import Prelude (return, putStrLn, putStr, getContents)
import Basics
import Maybe (Maybe (..))
import Result (Result (..))
import String (String)
import Dict (Dict)
import List (List)
import Array (Array)
import Task (Task)
import Set (Set)
import Char (Char)


{-| -}
line :: String -> Task x ()
line string =
  Task.Task <| \_ -> do
    IO.putStrLn string
    return (Ok ())


{-| -}
lines :: List String -> Task x ()
lines strings =
  Task.Task <| \_ -> do
    IO.putStr (String.join "\n" strings)
    return (Ok ())


{-| -}
read :: Task x String
read =
  Task.Task <| \_ -> do
    contents <- IO.getContents
    return (Ok contents)
