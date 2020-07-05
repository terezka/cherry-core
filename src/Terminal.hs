
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
import qualified Internal.Utils as U
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
line :: String -> Task s x ()
line string =
  Task.Task <| \_ -> do
    handle <- U.openTerminal
    U.writeTerminal handle (string ++ "\n")
    U.closeTerminal handle
    return (Ok ())


{-| -}
lines :: List String -> Task s x ()
lines strings =
  Task.Task <| \_ -> do
    handle <- U.openTerminal
    U.writeTerminal handle (String.join "\n" strings ++ "\n")
    U.closeTerminal handle
    return (Ok ())


{-| -}
read :: Task s x String
read =
  Task.Task <| \_ -> do
    contents <- IO.getContents
    return (Ok contents)
