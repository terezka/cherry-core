{-# LANGUAGE OverloadedStrings #-}

module Postgres.Decoder
  ( run
  , Table, table, table2, table3, table4, table5, table6
  , Column, column
  , Value, map, nullable, string, int, bool
  ) where

import qualified Database.PostgreSQL.LibPQ as PG
import qualified Prelude
import qualified Maybe
import qualified List
import qualified Debug
import qualified Dict
import qualified String
import qualified Task
import qualified Result
import qualified Http
import qualified Internal.Task
import qualified Internal.Shortcut as Shortcut
import qualified Prelude as P
import Cherry.Prelude


{-| -}
newtype Table a
  = Table (PG.Result -> Task String (List a))


{-| -}
newtype Column a
  = Column (PG.Result -> PG.Row -> Task String a)


{-| -}
newtype Value a
  = Value (Maybe String -> Result String a)



-- RUN


{-| -}
run :: Table a -> PG.Result -> Task String (List a)
run (Table decoder) result =
  decoder result



-- TABLES


{-| -}
table :: (a -> b) -> Column a -> Table b
table func (Column column1) =
  Table <| \result ->
    readRows result <| \row ->
      Task.map func
        (column1 result row)


{-| -}
table2 :: (a -> b -> c) -> Column a -> Column b -> Table c
table2 func (Column column1) (Column column2) =
  Table <| \result ->
    readRows result <| \row ->
      Task.map2 func
        (column1 result row)
        (column2 result row)


{-| -}
table3 :: (a -> b -> c -> d) -> Column a -> Column b -> Column c -> Table d
table3 func (Column column1) (Column column2) (Column column3) =
  Table <| \result ->
    readRows result <| \row ->
      Task.map3 func
        (column1 result row)
        (column2 result row)
        (column3 result row)


{-| -}
table4 :: (a -> b -> c -> d -> e) -> Column a -> Column b -> Column c -> Column d -> Table e
table4 func (Column column1) (Column column2) (Column column3) (Column column4) =
  Table <| \result ->
    readRows result <| \row ->
      Task.map4 func
        (column1 result row)
        (column2 result row)
        (column3 result row)
        (column4 result row)


{-| -}
table5 :: (a -> b -> c -> d -> e -> f) -> Column a -> Column b -> Column c -> Column d -> Column e -> Table f
table5 func (Column column1) (Column column2) (Column column3) (Column column4) (Column column5) =
  Table <| \result ->
    readRows result <| \row ->
      Task.map5 func
        (column1 result row)
        (column2 result row)
        (column3 result row)
        (column4 result row)
        (column5 result row)


{-| -}
table6 :: (a -> b -> c -> d -> e -> f -> g) -> Column a -> Column b -> Column c -> Column d -> Column e -> Column f -> Table g
table6 func (Column column1) (Column column2) (Column column3) (Column column4) (Column column5) (Column column6) =
  Table <| \result ->
    readRows result <| \row ->
      Task.map6 func
        (column1 result row)
        (column2 result row)
        (column3 result row)
        (column4 result row)
        (column5 result row)
        (column6 result row)



-- COLUMN


{-| -}
column :: String -> Value a -> Column a
column name (Value decoder) =
  Column <| \result row ->
    columnNumber result name
      |> Task.andThen (getValue result row)
      |> Task.andThen (decoder >> Task.fromResult)



-- VALUE


{-| -}
map :: (a -> b) -> Value a -> Value b
map f (Value decoder) =
  Value <| \string ->
    Result.map f (decoder string)


{-| -}
nullable :: Value a -> Value (Maybe a)
nullable (Value decoder) =
  Value <| \maybeValue ->
    case maybeValue of
      Nothing -> Result.Ok Nothing
      Just _ -> Result.map Just (decoder maybeValue)


{-| -}
string :: Value String
string =
  Value <| \maybeString ->
    withNotNull maybeString <| \string ->
      Result.Ok string


{-| -}
int :: Value Int
int =
  Value <| \maybeString ->
    withNotNull maybeString <| \string ->
      String.toInt string
        |> Result.fromMaybe "Value is not an int."


{-| -}
bool :: Value Bool
bool =
  Value <| \maybeString ->
    withNotNull maybeString <| \string ->
      case String.toUpper string of
        "T" -> Result.Ok True
        "F" -> Result.Ok False
        _   -> Result.Err "Value is not an boolean."



-- HELPERS


withNotNull :: Maybe String -> (String -> Result String a) -> Result String a
withNotNull maybeString func =
  case maybeString of
    Nothing ->
      Result.Err "Illegal value: NULL"

    Just string ->
      func string


readRows :: PG.Result -> (PG.Row -> Task String a) -> Task String (List a)
readRows result decode =
  let read done index total =
        if PG.toRow index == total then
          Task.sequence done
        else
          read (decode (PG.toRow index) : done) (index + 1) total
  in
  Task.andThen (read [] 0) (numberOfRows result)


numberOfRows :: PG.Result -> Task String PG.Row
numberOfRows result =
  PG.ntuples result
    |> Shortcut.map Result.Ok
    |> Internal.Task.Task


columnNumber :: PG.Result -> String -> Task String PG.Column
columnNumber result name =
  PG.fnumber result (String.toByteString name)
    |> Shortcut.map (Maybe.fromHMaybe >> Result.fromMaybe "Could not find column.")
    |> Internal.Task.Task


getValue :: PG.Result -> PG.Row -> PG.Column -> Task String (Maybe String)
getValue result row column =
  Internal.Task.Task <| do
    value <- PG.getvalue result row column
    value
      |> Maybe.fromHMaybe
      |> Maybe.map String.fromByteString
      |> Result.Ok
      |> P.return

