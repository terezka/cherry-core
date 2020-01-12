module Cherry.Terminal (write, read, red, blue, green, magenta, yellow, cyan, black, white, reset, underline, italic, newline) where

import qualified Prelude as P
import qualified Cherry.Task as Task
import qualified Cherry.List as List
import Cherry.Task (Task)
import Cherry.List (List)
import Cherry.Basics
import Cherry.Result (Result(..))


message :: P.String -> Task e ()
message string =
  P.fmap (\_ -> Ok ()) (P.putStrLn string)
    |> Task.enter


write :: P.String -> Task e ()
write string =
  P.fmap (\_ -> Ok ()) (P.putStr string)
    |> Task.enter


read :: Task e P.String
read =
  P.fmap Ok P.getLine
    |> Task.enter



-- CHARACTERS


red :: P.String
red =
  "\x1b[31m"


blue :: P.String
blue =
  "\x1b[34m"


magenta :: P.String
magenta =
  "\x1b[35m"

green :: P.String
green =
  "\x1b[32m"


yellow :: P.String
yellow =
  "\x1b[33m"


cyan :: P.String
cyan =
  "\x1b[36m"


black :: P.String
black =
  "\x1b[30m"


white :: P.String
white =
  "\x1b[37m"


reset :: P.String
reset =
  "\x1b[0m"


newline :: P.String
newline =
  "\n"


underline :: P.String
underline =
  "\x1b[4m"


italic :: P.String
italic =
  "\x1b[3m"


