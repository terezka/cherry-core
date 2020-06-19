module Internal.Utils where

import qualified Text
import qualified List
import qualified Dict
import qualified Data.Text
import qualified GHC.Stack as Stack
import qualified System.IO
import qualified Control.Concurrent.MVar as MVar
import qualified Prelude as P
import Control.Exception.Safe (bracket_)
import Prelude (IO, FilePath, return, fmap, putStr, getLine)
import Basics
import Maybe (Maybe (..))
import Result (Result (..))
import Text (Text)
import Dict (Dict)
import List (List)
import Array (Array)
import Set (Set)
import Char (Char)



-- KEY HELPERS


appendNamespace :: Text -> Text -> Text
appendNamespace old new =
  if Text.isEmpty old then new else old ++ "/" ++ new


appendContext :: Dict Text a -> List ( Text, a ) -> Dict Text a
appendContext old new =
  Dict.union (Dict.fromList new) old


appendStack :: Stack.HasCallStack => Text -> Stack.CallStack -> Stack.CallStack
appendStack namespace old =
  case Stack.getCallStack Stack.callStack of
    ( function, location ) : _ ->
      Stack.pushCallStack ( Data.Text.unpack namespace, location ) old

    _ ->
      old



-- FILE HELPERS


openFile :: FilePath -> IO ( System.IO.Handle, MVar.MVar () )
openFile filepath = do
  handle <- System.IO.openFile filepath System.IO.AppendMode
  System.IO.hSetBuffering handle (System.IO.BlockBuffering P.Nothing)
  lock <- MVar.newMVar ()
  return ( handle, lock )


writeFile :: ( System.IO.Handle, MVar.MVar () ) -> Text -> IO ()
writeFile ( handle, lock ) text =
  bracket_ (MVar.takeMVar lock) (MVar.putMVar lock ()) <|
    System.IO.hPutStrLn handle (Data.Text.unpack text)


closeFile :: ( System.IO.Handle, MVar.MVar () ) -> IO ()
closeFile ( handle, _ ) = do
  System.IO.hFlush handle
  System.IO.hClose handle
  return ()


-- TERMINAL HELPERS


openTerminal :: IO System.IO.Handle
openTerminal = do
  System.IO.hSetBuffering System.IO.stdout (System.IO.BlockBuffering P.Nothing)
  return System.IO.stdout


writeTerminal :: System.IO.Handle -> Text -> IO ()
writeTerminal handle text =
  System.IO.hPutStr handle (Data.Text.unpack text)


closeTerminal :: System.IO.Handle -> IO ()
closeTerminal handle = do
  System.IO.hFlush handle
  return ()


{-| -}
write :: Text -> IO ()
write string =
  putStr (Data.Text.unpack string)


{-| -}
read :: IO Text
read =
  getLine
    |> fmap Data.Text.pack



-- TEXT HELPERS


red :: Text
red =
  "\x1b[31m"


blue :: Text
blue =
  "\x1b[34m"


magenta :: Text
magenta =
  "\x1b[35m"

green :: Text
green =
  "\x1b[32m"


yellow :: Text
yellow =
  "\x1b[33m"


cyan :: Text
cyan =
  "\x1b[36m"


gray :: Text
gray =
  "\x1b[90m"


white :: Text
white =
  "\x1b[37m"


reset :: Text
reset =
  "\x1b[0m"


newline :: Text
newline =
  "\n"


underline :: Text
underline =
  "\x1b[4m"


italic :: Text
italic =
  "\x1b[3m"


indent :: Int -> Text
indent number =
  Text.repeat number " "



-- MESSAGE


message :: Text -> Text -> Text -> List Text -> Text
message color title location content =
  Text.concat
    [ header color title location
    , newline
    , newline
    , (paragraphs content)
    , newline
    , newline
    ]


header :: Text -> Text -> Text -> Text
header color title location =
  color ++ "-- " ++ Text.toUpper title ++ " " ++ dashes title location ++ " " ++ location ++ " " ++ reset


dashes :: Text -> Text -> Text
dashes title location =
  let number = 75 - Text.length title - Text.length location in
  Text.repeat number "-"


breakAt80 :: Text -> Text
breakAt80 text =
  let
      fold :: Text -> ( List Text, List Text ) -> ( List Text, List Text )
      fold word ( lines, words ) =
        let next = Text.join " " (word : words) in
        if word == "\n" then
          ( lines ++ [ Text.join " " words ], [] )
        else if Text.length next > 80 then
          ( lines ++ [ Text.join " " words ], [ word ] )
        else
          ( lines, words ++ [ word ] )

      concat :: ( List Text, List Text ) -> Text
      concat ( lines, words ) =
        Text.join newline (lines ++ [ Text.join " " words ])
  in
  text
    |> Text.replace "\n" " \n"
    |> Text.split " "
    |> List.foldl fold ([], [])
    |> concat


paragraphs :: List Text -> Text
paragraphs =
  Text.join (newline ++ newline)
