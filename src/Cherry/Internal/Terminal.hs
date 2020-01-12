module Cherry.Internal.Terminal (write, read, red, blue, green, magenta, yellow, cyan, black, white, reset, underline, italic, newline, indent, message, paragraphs) where

import qualified Prelude as P
import qualified Cherry.List as List
import qualified Cherry.Text as Text
import qualified Data.Text
import Cherry.List (List)
import Cherry.Basics


{-| -}
write :: Text.Text -> P.IO ()
write string =
  P.putStr (Data.Text.unpack string)


{-| -}
read :: P.IO Text.Text
read =
  P.getLine
    |> P.fmap Data.Text.pack



-- CHARACTERS


red :: Text.Text
red =
  "\x1b[31m"


blue :: Text.Text
blue =
  "\x1b[34m"


magenta :: Text.Text
magenta =
  "\x1b[35m"

green :: Text.Text
green =
  "\x1b[32m"


yellow :: Text.Text
yellow =
  "\x1b[33m"


cyan :: Text.Text
cyan =
  "\x1b[36m"


black :: Text.Text
black =
  "\x1b[30m"


white :: Text.Text
white =
  "\x1b[37m"


reset :: Text.Text
reset =
  "\x1b[0m"


newline :: Text.Text
newline =
  "\n"


underline :: Text.Text
underline =
  "\x1b[4m"


italic :: Text.Text
italic =
  "\x1b[3m"


indent :: Int -> Text.Text
indent number =
  Text.repeat number " "



-- MESSAGE


message :: Text.Text -> Text.Text -> Text.Text -> List Text.Text -> P.IO ()
message color title location content = do
  write (header color title location)
  write newline
  write newline
  write (paragraphs content)
  write newline
  write newline


header :: Text.Text -> Text.Text -> Text.Text -> Text.Text
header color title location =
  color <> "-- " <> Text.toUpper title <> " " <> dashes title location <> " " <> location <> " " <> reset


dashes :: Text.Text -> Text.Text -> Text.Text
dashes title location =
  let number = 75 - Text.length title - Text.length location in
  Text.repeat number "-"



-- PARAGRAPHS


{-| TODO break at 80 characters.
-}
paragraphs :: List Text.Text -> Text.Text
paragraphs =
  Text.join (newline <> newline)
