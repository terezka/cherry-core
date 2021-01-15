{-# LANGUAGE PackageImports #-}

{-|

Module      : File
Description : Read and write to a file.
License     : BSD 3
Maintainer  : terezasokol@gmail.com
Stability   : experimental
Portability : POSIX

Read and write to a file.

-}

module File (Path, read, write, doesExist, list) where

import qualified List
import qualified String
import qualified Internal.Task as Task
import qualified Internal.Utils as U
import qualified "text-utf8" Data.Text.IO as IO
import qualified System.Directory as Directory
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
type Path
  = String


{-| -}
write :: Path -> String -> Task x ()
write filename string =
  Task.Task <| do
    let dir = String.split "/" filename |> List.reverse |> List.drop 1 |> List.reverse |> String.join "/"
    Directory.createDirectoryIfMissing True (String.toList dir)
    IO.writeFile (String.toList filename) (String.toTextUtf8 string)
    return (Ok ())


{-| -}
doesExist :: Path -> Task x Bool
doesExist path =
  Task.Task <| do
    bool <- Directory.doesFileExist (String.toList path)
    return (Ok bool)


{-| -}
read :: Path -> Task x String
read filename =
  Task.Task <| do
    contents <- IO.readFile (String.toList filename)
    return (Ok (String.fromTextUtf8 contents))


{-| -}
list :: Path -> Task x (List Path)
list path =
  Task.Task <| do
    files <- Directory.listDirectory (String.toList path)
    return (Ok (List.map String.fromList files))
