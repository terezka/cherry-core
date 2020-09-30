
{-|

Module      : File
Description : Read and write to a file.
License     : BSD 3
Maintainer  : terezasokol@gmail.com
Stability   : experimental
Portability : POSIX

Read and write to a file.

-}

module File (read, write) where

import qualified List
import qualified String
import qualified Internal.Task as Task
import qualified Internal.Utils as U
import qualified Data.Text.IO as IO
import Prelude (return, getContents)
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
write :: String -> String -> Task x ()
write filename string =
  Task.Task <| do
    IO.writeFile (String.toList filename) (String.toTextUtf8 string)
    return (Ok ())


{-| -}
read :: String -> Task x String
read filename =
  Task.Task <| do
    contents <- IO.readFile (String.toList filename)
    return (Ok (String.fromTextUtf8 contents))
