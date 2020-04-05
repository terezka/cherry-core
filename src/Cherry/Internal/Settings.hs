module Cherry.Internal.Settings
  ( -- * Decoder
    Decoder
  , decode
  , required
  , optional
  , run

    -- * Parser
  , Parser
  , map
  , andThen
  , text
  , int
  , float
  , boolean
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
import Cherry.Result (Result(..))
import Cherry.Maybe (Maybe(..))
import Cherry.Dict (Dict)
import Cherry.Task (Task)
import Cherry.List (List)
import Cherry.Text (Text)
import Cherry.Basics


-- DECODER


{-| A decoder for environment variables.
-}
newtype Decoder settings =
  Decoder (Dict Text Text -> Result Text settings)


{-| Create a constant decoder. Useful when decoding several settings.

> data Settings =
>   Settings
>     { debugMode :: Bool
>     , port :: Int
>     }
>
> decoder :: Settings.Decoder Settings
> decoder =
>   Settings.decode Settings
>     |> Settings.optional "DEBUG_MODE" Settings.boolean False
>     |> Settings.optional "PORT" Settings.int 9000


-}
decode :: a -> Decoder a
decode value =
  Decoder (\_ -> Ok value)


{-| Decode a required setting. Arguments:

  1. Name of environment variable
  2. How to parse variable.

> data Settings =
>   Settings
>     { port :: Int
>     }
>
> decoder :: Settings.Decoder Settings
> decoder =
>   Settings.decode Settings
>     |> Settings.required "PORT" Settings.int

-}
required :: Text -> Parser a -> Decoder (a -> b) -> Decoder b
required key (Parser parser) (Decoder parse) =
  Decoder <| \env ->
    Result.map2 (\f v -> f v) (parse env) <|
      case Dict.get key env |> Maybe.map parser of
        Just result -> result
        Nothing -> Err ("Could not find variable: " ++ key)


{-| Decode an optional setting. Arguments:

  1. Name of environment variable
  2. How to parse variable.
  3. A default value if variable is not set.

> data Settings =
>   Settings
>     { port :: Int
>     }
>
> decoder :: Settings.Decoder Settings
> decoder =
>   Settings.decode Settings
>     |> Settings.optional "PORT" Settings.int 9000

-}
optional :: Text -> Parser a -> a -> Decoder (a -> b) -> Decoder b
optional key (Parser parser) fallback (Decoder parse) =
  Decoder <| \env ->
    Result.map2 (\f v -> f v) (parse env) <|
      case Dict.get key env |> Maybe.map parser of
        Just value -> value
        Nothing -> Ok fallback



-- DECODER / INTERNAL


{-| -}
run :: Decoder a -> Task Text a
run (Decoder decoder) =
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


{-| An environment variable parser. -}
newtype Parser a
  = Parser (Text -> Result Text a)


{-| Ignore the variable and produce a certain value.
-}
succeed :: a -> Parser a
succeed a =
  Parser (\_ -> Ok a)


{-| Ignore the variable and produce a certain error message.
-}
fail :: Text -> Parser a
fail x =
  Parser (\_ -> Err x)


{-| -}
map :: (a -> b) -> Parser a -> Parser b
map f (Parser a) =
  Parser <| \text ->
    case a text of
      Ok ok -> Ok (f ok)
      Err err -> Err err


{-| -}
andThen :: (a -> Result Text b) -> Parser a -> Parser b
andThen f (Parser a) =
  Parser <| \text ->
    case a text of
      Ok ok -> f ok
      Err err -> Err err


{-| Decode an environment variable into a text.
-}
text :: Parser Text
text =
  Parser Ok


{-| Decode an environment variable into an integer.
-}
int :: Parser Int
int =
  Parser <| \text ->
    case Text.toInt text of
      Nothing -> Err ("Could not parse as integer: " ++ text)
      Just n -> Ok n


{-| Decode an environment variable into a float.
-}
float :: Parser Float
float =
  Parser <| \text ->
    case Text.toFloat text of
      Nothing -> Err ("Could not parse as float: " ++ text)
      Just n -> Ok n


{-| Decode an environment variable into a boolean.
-}
boolean :: Parser Bool
boolean =
  Parser <| \text ->
    case Text.toUpper text of
      "TRUE" -> Ok True
      "FALSE" -> Ok False
      _ -> Err ("Could not parse as boolean: " ++ text)

