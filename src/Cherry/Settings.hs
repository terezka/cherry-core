module Cherry.Settings
  ( -- * Decoder
    Decoder
  , succeed
  , required
  , optional
  , decode

    -- * Parser
  , Parser
  , text
  , int
  , float
  , boolean
  , custom
  )
where


import qualified Data.Text
import qualified Cherry.Maybe as Maybe
import qualified Cherry.Result as Result
import qualified Cherry.Dict as Dict
import qualified Cherry.Task as Task
import qualified Cherry.Text as Text
import qualified Cherry.List as List
import qualified System.Environment
import qualified Prelude
import Cherry.Maybe (Maybe(..))
import Cherry.Result (Result(..))
import Cherry.Dict (Dict)
import Cherry.Task (Task)
import Cherry.List (List)
import Cherry.Text (Text)
import Cherry.Basics


-- DECODER


{-| -}
newtype Decoder settings =
  Decoder (Dict Text Text -> Result Text settings)


{-| -}
succeed :: a -> Decoder a
succeed value =
  Decoder (\_ -> Ok value)


{-| -}
required :: Text -> Parser a -> Decoder (a -> b) -> Decoder b
required key (Parser parser) (Decoder parse) =
  Decoder <| \env ->
    Result.map2 (\f v -> f v) (parse env) <|
      case Dict.get key env |> Maybe.map parser of
        Just result -> result
        Nothing -> Err ("Could not find variable: " ++ key)


{-| -}
optional :: Text -> Parser a -> a -> Decoder (a -> b) -> Decoder b
optional key (Parser parser) fallback (Decoder parse) =
  Decoder <| \env ->
    Result.map2 (\f v -> f v) (parse env) <|
      case Dict.get key env |> Maybe.map parser of
        Just value -> value
        Nothing -> Ok fallback


{-| -}
decode :: Decoder a -> Task Text a
decode (Decoder decoder) =
  let environment =
        List.map (\(k, v) -> ( Data.Text.pack k, Data.Text.pack v)) >> Dict.fromList

      read env =
        case decoder env of
          Ok settings -> Task.succeed settings
          Err err -> Task.fail err
  in do
  Task.enter System.Environment.getEnvironment
    |> Task.map environment
    |> Task.andThen read



-- PARSER


{-| -}
newtype Parser a
  = Parser (Text -> Result Text a)


{-| -}
text :: Parser Text
text =
  Parser Ok


{-| -}
int :: Parser Int
int =
  Parser <| \text ->
    case Text.toInt text of
      Nothing -> Err ("Could not parse as integer: " ++ text)
      Just n -> Ok n


{-| -}
float :: Parser Float
float =
  Parser <| \text ->
    case Text.toFloat text of
      Nothing -> Err ("Could not parse as float: " ++ text)
      Just n -> Ok n


{-| -}
boolean :: Parser Bool
boolean =
  Parser <| \text ->
    case Text.toUpper text of
      "TRUE" -> Ok True
      "FALSE" -> Ok False
      _ -> Err ("Could not parse as boolean: " ++ text)


{-| -}
custom :: Parser a -> (a -> Result Text b) -> Parser b
custom (Parser base) f =
  Parser (base >> Result.andThen f)
