module Postgres (Connection, connect, execute, executeOne, Variable, int, text, bool, real, enum, null) where

import qualified Control.Exception
import qualified Database.PostgreSQL.LibPQ as PG
import qualified Data.Either
import qualified Data.Maybe
import qualified Maybe
import qualified String
import qualified List
import qualified Result
import qualified Task
import qualified Terminal
import qualified Internal.Task
import qualified Internal.Utils as U
import qualified Internal.Shortcut as IO
import qualified Postgres.Decoder as Decoder
import Cherry.Prelude


{-|

TODO:
  - async connect

-}


{-| -}
newtype Connection
  = Connection PG.Connection


{-| -}
connect :: String -> Task String Connection
connect details =
  PG.connectdb (String.toByteString details)
    |> IO.map (Result.Ok << Connection)
    |> Internal.Task.Task


{-| -}
execute :: Connection -> String -> List Variable -> Decoder.Table a -> Task String (List a)
execute (Connection connection) query variables decoder = do
  let replaceOne ( i, v ) = String.replace ("$" ++ String.fromInt (i + 1)) (toValue v)
  let final = List.foldl replaceOne query (List.indexedMap (,) variables)
  Terminal.write (U.blue ++ "Executed query: " ++ U.reset ++ final)
  result <- Internal.Task.Task <| do
    PG.exec connection (String.toByteString final)
      |> IO.map (Maybe.fromHMaybe >> Result.fromMaybe "Bad query.")
  Decoder.run decoder result


{-| -}
executeOne :: Connection -> String -> List Variable -> Decoder.Table a -> Task String a
executeOne connection query variables decoder =
  let getOne all =
        case all of
          [] -> Task.fail "Query rendered no results!"
          first : [] -> Task.succeed first
          rest -> Task.fail "Query rendered more than one results!"
  in do
  rows <- execute connection query variables decoder
  getOne rows


{-| -}
data Variable
  = Int Int
  | Text String
  | Boolean Bool
  | Real Float
  | Enum String String
  | NULL


{-| -}
int :: Int -> Variable
int =
  Int


{-| -}
text :: String -> Variable
text =
  Text


{-| -}
bool :: Bool -> Variable
bool =
  Boolean


{-| -}
real :: Float -> Variable
real =
  Real


{-| -}
enum :: String -> String -> Variable
enum =
  Enum


{-| -}
null :: Variable
null =
  NULL


toValue :: Variable -> String
toValue variable =
  case variable of
    Int int -> String.fromInt int
    Text string -> "\'" ++ string ++ "\'"
    Boolean bool -> if bool then "TRUE" else "FALSE"
    Real float -> String.fromFloat float
    Enum _ value -> "\'" ++ value ++ "\'"
    NULL -> "NULL"
