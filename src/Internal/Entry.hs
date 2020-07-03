module Internal.Entry
  ( Entry(..), Severity(..), Context
  , toColor, toTitle
  , pretty, compact, json
  , bool, string, int, float, value
  ) where


import qualified GHC.Stack as Stack
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Data.ByteString.Lazy as ByteString
import qualified Debug
import qualified Dict
import qualified String
import qualified Result
import qualified List
import qualified Json.Encode as Json
import qualified Json.Encode
import qualified Json.Decode
import qualified Internal.Utils as U
import Basics
import Maybe (Maybe (..))
import Result (Result (..))
import String (String)
import Dict (Dict)
import List (List)
import Array (Array)
import Set (Set)
import Char (Char)


{-| An entry is a single log message. You send an entry to your
logging targets every time your program executes one of the following
functions: `debug`, `info`, `warning`, `error`, `alert`, or `exception`.

  > error [ value "is_premium" isPremium ] "Could not find user wishlist."

-}
data Entry = Entry
  { severity :: Severity
  , namespace :: String
  , message :: String
  , context :: Dict String Json.Value
  , callstack :: Stack.CallStack
  }


encode :: Entry -> Json.Value
encode (Entry severity namespace message context _) =
  Json.object
    [ ( "severity", Json.string (toTitle severity) )
    , ( "namespace", Json.string namespace )
    , ( "message", Json.string message )
    , ( "context", Json.object (Dict.toList context) )
    ]


{-| A key value pair comprising a piece of context for your entry or `segment`.
-}
type Context =
  ( String, Json.Value )



-- SEVERITY


data Severity
  = Debug
  | Info
  | Warning
  | Error
  | Alert
  | Unknown


toColor :: Severity -> String
toColor severity =
  case severity of
    Debug -> U.cyan
    Info -> U.cyan
    Warning -> U.yellow
    Error -> U.magenta
    Alert -> U.red
    Unknown -> U.red


toTitle :: Severity -> String
toTitle severity =
  case severity of
    Debug -> "Debug"
    Info -> "Info"
    Warning -> "Warning"
    Error -> "Error"
    Alert -> "Alert"
    Unknown -> "Unknown Error"



-- TO STRING


{-| "Pretty" formatting of an entry. Can be used with `terminal`,
`file`, or inside a custom target.

-}
pretty :: Entry -> String
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
          |> List.map viewContext
          |> String.join U.newline

      viewContext ( name, value ) = do
        U.indent 2 ++ name ++ ": " ++ toText value

      viewSegments =
        Stack.getCallStack callstack
          |> List.map viewStack
          |> String.join U.newline

      viewStack ( function, location ) =
        U.indent 2 ++ "\"" ++ Data.Text.pack function ++ "\" at " ++ viewLocation location

      viewLocation location =
        String.join ":"
          [ Data.Text.pack (Stack.srcLocFile location)
          , Debug.toString (Stack.srcLocStartLine location)
          , Debug.toString (Stack.srcLocStartCol location)
          ]
  in
  U.message (toColor severity) (toTitle severity) namespace
    [ U.breakAt80 message
    , U.gray ++ U.paragraphs viewExtra ++ U.reset
    ]


{-| Compact one-line formatting of the entry. Can be used with `terminal`,
`file`, or inside a custom target.

-}
compact :: Entry -> String
compact (Entry severity namespace message context _) =
  let string c = "[" ++ c ++ "]"
  in
  String.concat
    [ string (toTitle severity)
    , string namespace
    , string message
    , Dict.toList context
          |> List.map (\(a, b) -> ( a, toText b ))
          |> Debug.toString
          |> string
    ]


toText :: Json.Value -> String
toText =
  Data.Text.Encoding.decodeUtf8 << ByteString.toStrict << Json.toByteString


{-| JSON formatting of the entry. Can be used with `terminal`, `file`,
or inside a custom target.
-}
json :: Entry -> String
json =
  encode >> Json.toByteString >> ByteString.toStrict >> Data.Text.Encoding.decodeUtf8



-- CONTEXT HELPERS


bool :: String -> Bool -> Context
bool key value =
  ( key, Json.bool value )


string :: String -> String -> Context
string key value =
  ( key, Json.string value )


int :: String -> Int -> Context
int key value =
  ( key, Json.int value )


float :: String -> Float -> Context
float key value =
  ( key, Json.float value )


{-| TODO Use to create a piece of context for your entry or `segment`.

    > info [ value "user" (encode user) ] "User visited the referrals page."

Warning: Watch out for adding the same key twice!

    > info [ value "name" (string "evan"), value "name" (string "tereza") ] "User logged in"
    > -- only the last "name" value will survive, in this case "tereza".

-}
value :: String -> Json.Value -> Context
value key value =
  ( key, value )

