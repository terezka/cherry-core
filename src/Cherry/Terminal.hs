module Cherry.Terminal (write, read, message, red, blue, green, magenta, yellow, cyan, black, white, reset, underline, italic, newline) where

import qualified Prelude as P
import qualified Cherry.Task as Task
import qualified Cherry.List as List
import qualified Cherry.Text as Text
import qualified Cherry.Internal.Terminal as T
import Cherry.Task (Task)
import Cherry.List (List)
import Cherry.Basics
import Cherry.Result (Result(..))


{-| Write some text to the terminal. Remember to add newlines where
  appropriate, or use `message`.

  > import qualified Cherry.Terminal as T
  >
  > T.write <| T.green ++ "What is your name?" ++ T.reset ++ T.newline
  >

-}
write :: Text.Text -> Task e ()
write =
  T.write >> Task.enter


{-| -}
read :: Task e Text.Text
read =
  T.read |> Task.enter


{-| Write a message with a title, a location, and some paragraphs.

  > message "My message" "Main.hs"
  >    [ "This is a message from my program."
  >    , "This message is on the next line."
  >    ]

The above code results in:

  > -- MY MESSAGE ---------------------------------------------------------- Main.hs
  >
  > This is a message from my program.
  >
  > This message is on the next line.
  >

-}
message :: Text.Text -> Text.Text -> List Text.Text -> Task e ()
message title location content =
  T.message cyan title location content
    |> write



-- CHARACTERS


{-| -}
red :: Text.Text
red =
  T.red


{-| -}
blue :: Text.Text
blue =
  T.blue


{-| -}
magenta :: Text.Text
magenta =
  T.magenta


{-| -}
green :: Text.Text
green =
  T.green


{-| -}
yellow :: Text.Text
yellow =
  T.yellow


{-| -}
cyan :: Text.Text
cyan =
  T.cyan


{-| -}
black :: Text.Text
black =
  T.black


{-| -}
white :: Text.Text
white =
  T.white


{-| -}
reset :: Text.Text
reset =
  T.reset


{-| -}
newline :: Text.Text
newline =
  T.newline


{-| -}
underline :: Text.Text
underline =
  T.underline


{-| -}
italic :: Text.Text
italic =
  T.italic


