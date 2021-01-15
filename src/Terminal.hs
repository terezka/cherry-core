{-# LANGUAGE PackageImports #-}

{-|

Module      : Terminal
Description : Read and write to the terminal.
License     : BSD 3
Maintainer  : terezasokol@gmail.com
Stability   : experimental
Portability : POSIX

Read and write to the terminal.

-}

module Terminal (write, read) where

import qualified List
import qualified String
import qualified Internal.Task as Task
import qualified Internal.Utils as U
import qualified "text-utf8" Data.Text.IO as IO
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
write :: String -> Task x ()
write string =
  Task.Task <| do
    IO.putStrLn (String.toTextUtf8 string)
    return (Ok ())


{-| -}
read :: Task x String
read =
  Task.Task <| do
    contents <- IO.getLine
    return (Ok (String.fromTextUtf8 contents))
