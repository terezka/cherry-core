module Internal.Entry
  ( Entry(..), Severity(..), Context
  , toColor, toTitle
  , pretty, compact, json
  , value, int, float, text, lookup
  ) where


import qualified GHC.Stack as Stack
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Data.ByteString.Lazy as ByteString
import qualified Debug
import qualified Dict
import qualified Text
import qualified Result
import qualified List
import qualified Json.Encode as Json
import qualified Json.Encode as Json.Encode
import qualified Json.Decode as Json.Decode
import qualified Internal.Utils as U
import Basics
import Maybe (Maybe (..))
import Result (Result (..))
import Text (Text)
import Dict (Dict)
import List (List)
import Array (Array)
import Set (Set)
import Char (Char)


{-| An entry is a single log item. It has a bunch of standard info as well as details you have defined.

-}
data Entry = Entry
  { severity :: Severity
  , namespace :: Text
  , message :: Text
  , context :: Dict Text Json.Value
  , callstack :: Stack.CallStack
  }


instance Json.Encodable Entry where
  encoder (Entry severity namespace message context _) =
    Json.object
      [ ( "severity", Json.string (toTitle severity) )
      , ( "namespace", Json.string namespace )
      , ( "message", Json.string message )
      , ( "context", Json.object (Dict.toList context) )
      ]


{-| A key value pair comprising a piece of context.
-}
type Context =
  ( Text, Json.Value )



-- SEVERITY


data Severity
  = Debug
  | Info
  | Warning
  | Error
  | Alert
  | Unknown


toColor :: Severity -> Text
toColor severity =
  case severity of
    Debug -> U.cyan
    Info -> U.cyan
    Warning -> U.yellow
    Error -> U.magenta
    Alert -> U.red
    Unknown -> U.red


toTitle :: Severity -> Text
toTitle severity =
  case severity of
    Debug -> "Debug"
    Info -> "Info"
    Warning -> "Warning"
    Error -> "Error"
    Alert -> "Alert"
    Unknown -> "Unknown Error"



-- TO STRING


{-| Pretty formatting of the entry. Can be used with
`terminal`, `file`, or a custom output.

-}
pretty :: Entry -> Text
pretty (Entry severity namespace message context callstack) =
  let viewExtra =
        case severity of
          Unknown ->
            [ "Context:"
            , viewContextList
            , "Segments:"
            , viewSegments
            ]

          _ ->
            [ "Context:"
            , viewContextList
            ]

      viewContextList =
        Dict.toList context
          |> List.map (\(a, b) -> ( a, toText b))
          |> List.map viewContext
          |> Text.join U.newline

      viewContext ( name, value ) = do
        U.indent 2 ++ name ++ ": " ++ value

      viewSegments =
        Stack.getCallStack callstack
          |> List.map viewStack
          |> Text.join U.newline

      viewStack ( function, location ) =
        U.indent 2 ++ "\"" ++ Data.Text.pack function ++ "\" at " ++ viewLocation location

      viewLocation location =
        Text.join ":"
          [ Data.Text.pack (Stack.srcLocFile location)
          , Debug.toString (Stack.srcLocStartLine location)
          , Debug.toString (Stack.srcLocStartCol location)
          ]
  in
  U.message (toColor severity) (toTitle severity) namespace
    [ U.breakAt80 message
    , U.gray ++ U.paragraphs viewExtra ++ U.reset
    ]


{-| Compact one-line formatting of the entry. Can be used with
`terminal`, `file`, or a custom output.

-}
compact :: Entry -> Text
compact (Entry severity namespace message context _) =
  let anything c = "[" ++ Debug.toString c ++ "]"
      string c = "[" ++ c ++ "]"
  in
  Text.concat
    [ string (toTitle severity)
    , string namespace
    , string message
    , Dict.toList context
          |> List.map (\(a, b) -> ( a, toText b ))
          |> anything
    ]


toText :: Json.Value -> Text
toText =
  Data.Text.Encoding.decodeUtf8 << ByteString.toStrict << Json.toByteString


{-| JSON formatting of the entry. Can be used with
`terminal`, `file`, or a custom output.

-}
json :: Entry -> Text
json =
  Json.encoder >> Json.toByteString >> ByteString.toStrict >> Data.Text.Encoding.decodeUtf8



-- CONTEXT HELPERS


{-| -}
value :: Json.Encode.Encodable a => Text -> a -> Context
value key value =
  ( key, Json.encoder value )


{-| -}
int :: Text -> Int -> Context
int key value =
  ( key, Json.int value )


{-| -}
float :: Text -> Float -> Context
float key value =
  ( key, Json.float value )


{-| -}
text :: Text -> Text -> Context
text key value =
  ( key, Json.string value )


{-| -}
lookup :: Json.Decode.Decodable a => Text -> Dict Text Json.Value -> Result Text a
lookup key contexts =
  Dict.get key contexts
    |> Result.fromMaybe ("Could not find key: " ++ key)
    |> Result.andThen (Json.Decode.fromValue Json.Decode.decoder >> Result.mapError Debug.toString)
