{-# LANGUAGE PackageImports #-}

module Internal.Utils where

import qualified String
import qualified List
import qualified Dict
import qualified "text-utf8" Data.Text
import qualified GHC.Stack as Stack
import qualified System.IO
import qualified Control.Concurrent.MVar as MVar
import qualified Prelude as P
import Prelude (IO, FilePath, return, fmap, putStr, getLine)
import Basics
import Maybe (Maybe (..))
import Result (Result (..))
import String (String)
import Dict (Dict)
import List (List)
import Array (Array)
import Set (Set)
import Char (Char)


-- TEXT HELPERS


red :: String
red =
  "\x1b[31m"


blue :: String
blue =
  "\x1b[34m"


magenta :: String
magenta =
  "\x1b[35m"

green :: String
green =
  "\x1b[32m"


yellow :: String
yellow =
  "\x1b[33m"


cyan :: String
cyan =
  "\x1b[36m"


gray :: String
gray =
  "\x1b[90m"


white :: String
white =
  "\x1b[37m"


reset :: String
reset =
  "\x1b[0m"


break :: String
break =
  "\n"


blank :: String
blank =
  "\n\n"


underline :: String
underline =
  "\x1b[4m"


italic :: String
italic =
  "\x1b[3m"


indent :: Int -> String
indent number =
  String.repeat number " "



-- MESSAGE


header :: String -> String -> String -> String
header color title location =
  color ++ "-- " ++ String.toUpper title ++ " " ++ dashes title location ++ " " ++ location ++ " " ++ reset


dashes :: String -> String -> String
dashes title location =
  let number = 75 - String.length title - String.length location in
  String.repeat number "-"


breakAt80 :: String -> String
breakAt80 text =
  let
      fold :: String -> ( List String, List String ) -> ( List String, List String )
      fold word ( lines, words ) =
        let next = String.join " " (word : words) in
        if word == "\n" then
          ( lines ++ [ String.join " " words ], [] )
        else if String.length next > 80 then
          ( lines ++ [ String.join " " words ], [ word ] )
        else
          ( lines, words ++ [ word ] )

      concat :: ( List String, List String ) -> String
      concat ( lines, words ) =
        String.join break (lines ++ [ String.join " " words ])
  in
  text
    |> String.replace "\n" " \n"
    |> String.split " "
    |> List.foldl fold ([], [])
    |> concat


--

toString :: P.String -> String
toString =
  Data.Text.pack >> String.fromTextUtf8
