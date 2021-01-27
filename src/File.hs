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

module File (Path, read, rename, write, delete, doesExist, list) where

import qualified List
import qualified String
import qualified Data.Maybe
import qualified System.IO.Error
import qualified Control.Exception
import qualified Internal.Task as Task
import qualified Internal.Utils as U
import qualified "text-utf8" Data.Text.IO as IO
import qualified System.Directory as Directory
import qualified Prelude as P
import Prelude (return, getContents, sequence)
import Data.List (stripPrefix)
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
write path string =
  Task.Task <| do
    let dir = String.split "/" path |> List.reverse |> List.drop 1 |> List.reverse |> String.join "/"
    Directory.createDirectoryIfMissing True (String.toList dir)
    IO.writeFile (String.toList path) (String.toTextUtf8 string)
    return (Ok ())


{-| -}
rename :: Path -> Path -> Task x ()
rename old new =
  Task.Task <|
    let handleError err =
          if System.IO.Error.isDoesNotExistError err
            then return ()
            else Control.Exception.throwIO err
    in
    P.fmap Ok (Directory.renameFile (String.toList old) (String.toList new) `Control.Exception.catch` handleError)


{-| -}
delete :: Path -> Task x ()
delete path =
  Task.Task <|
    let handleError err =
          if System.IO.Error.isDoesNotExistError err
            then return ()
            else Control.Exception.throwIO err
    in
    P.fmap Ok (Directory.removeFile (String.toList path) `Control.Exception.catch` handleError)


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
list :: Path -> Task String (List Path)
list path =
  Task.Task <|
    let
        findFiles :: P.String -> List P.String -> P.IO (List P.String)
        findFiles folder acc = do
          items <- Directory.listDirectory folder
          newfiles <- List.foldl checkOne (return []) (List.map (\i -> folder ++ "/" ++ i) items)
          return (acc ++ newfiles)

        checkOne :: P.String -> P.IO (List P.String) -> P.IO (List P.String)
        checkOne name acc = do
          exists <- Directory.doesDirectoryExist name
          if exists then do
            files <- acc
            findFiles name files
          else
            P.fmap (\fs -> name : fs) acc
    in do
    let directory = String.toList path
    exists <- Directory.doesDirectoryExist directory
    if exists then do
      files <- findFiles directory []
      let final = Data.Maybe.mapMaybe (stripPrefix (directory ++ "/")) files
      return (Ok (List.map String.fromList final))
    else
      return (Err "Given path is not a directory.")
