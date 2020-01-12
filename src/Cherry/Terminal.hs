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


{-| -}
write :: Text.Text -> Task e ()
write =
  T.write >> P.fmap (\_ -> Ok ()) >> Task.enter


{-| -}
read :: Task e Text.Text
read =
  T.read
    |> P.fmap Ok
    |> Task.enter



-- CHARACTERS


red :: Text.Text
red =
  T.red


blue :: Text.Text
blue =
  T.blue


magenta :: Text.Text
magenta =
  T.magenta

green :: Text.Text
green =
  T.green


yellow :: Text.Text
yellow =
  T.yellow


cyan :: Text.Text
cyan =
  T.cyan


black :: Text.Text
black =
  T.black


white :: Text.Text
white =
  T.white


reset :: Text.Text
reset =
  T.reset


newline :: Text.Text
newline =
  T.newline


underline :: Text.Text
underline =
  T.underline


italic :: Text.Text
italic =
  T.italic



-- MESSAGE


message :: Text.Text -> Text.Text -> List Text.Text -> Task e ()
message title location content =
  T.message cyan title location content
    |> P.fmap (\_ -> Ok ())
    |> Task.enter
