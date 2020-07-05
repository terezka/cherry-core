module Internal.Utils where

import qualified String
import qualified List
import qualified Dict
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



-- KEY HELPERS


appendNamespace :: String -> String -> String
appendNamespace old new =
  if String.isEmpty old then new else old ++ "/" ++ new


appendContext :: s -> List (s -> s) -> s
appendContext old transformers =
  List.foldl (\t s -> t s) old transformers


appendStack :: Stack.HasCallStack => String -> Stack.CallStack -> Stack.CallStack
appendStack namespace old =
  case Stack.getCallStack Stack.callStack of
    ( function, location ) : _ ->
      Stack.pushCallStack ( String.toList namespace, location ) old

    _ ->
      old



-- FILE HELPERS


openFile :: FilePath -> IO (MVar.MVar System.IO.Handle)
openFile filepath =
  do  handle <- System.IO.openFile filepath System.IO.AppendMode
      System.IO.hSetBuffering handle System.IO.LineBuffering
      MVar.newMVar handle


writeFile :: MVar.MVar System.IO.Handle -> String -> IO ()
writeFile lock string =
  MVar.withMVar lock <| \handle ->
    System.IO.hPutStrLn handle (String.toList string)
    -- TODO use hPutBuf to skip lots of allocations
    -- See the following implementation for an example
    -- https://hackage.haskell.org/package/bytestring-0.10.10.0/docs/Data-ByteString.html#v:hPut


closeFile :: MVar.MVar System.IO.Handle -> IO ()
closeFile lock =
  do  handle <- MVar.takeMVar lock
      System.IO.hFlush handle
      System.IO.hClose handle



-- TERMINAL HELPERS


openTerminal :: IO System.IO.Handle
openTerminal = do
  System.IO.hSetBuffering System.IO.stdout (System.IO.BlockBuffering P.Nothing)
  return System.IO.stdout


writeTerminal :: System.IO.Handle -> String -> IO ()
writeTerminal handle text =
  System.IO.hPutStr handle (String.toList text)


closeTerminal :: System.IO.Handle -> IO ()
closeTerminal handle = do
  System.IO.hFlush handle
  return ()


{-| -}
write :: String -> IO ()
write string =
  putStr (String.toList string)


{-| -}
read :: IO String
read =
  fmap String.fromList getLine



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
