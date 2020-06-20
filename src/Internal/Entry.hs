module Internal.Entry
  ( Entry(..), Severity(..), Context
  , toColor, toTitle
  , pretty, compact, json
  , value, lookup
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
import Json.Encode (Encodable)
import Json.Decode (Decodable)


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


instance Json.Encodable Entry where
  encoder (Entry severity namespace message context _) =
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
  Json.encoder >> Json.toByteString >> ByteString.toStrict >> Data.Text.Encoding.decodeUtf8



-- CONTEXT HELPERS


{-| Use to create a piece of context for your entry or `segment`.

    > info [ value "user" user ] "User visited the referrals page."

To use this function, the second argument but be `Encodable`. This means
it must define an encoder like this:

    > import qualified Json.Encode as Json
    >
    > instance Json.Encodable User where
    >   encoder (User name age) =
    >     Json.object
    >       [ ( "name", Json.string name )
    >       , ( "age", Json.int age )
    >       ]

This lets me know how to encode your data!

Note: If you are trying to log a constant, say `12`, the compiler might not
be able to quess it's type (is it an integer or float?). To fix this,
add a type signature.

    > info [ value "attempts" (12 :: Int) ] "User visited the referrals page."

This may happend with strings too. The remedy is the same!

    > info [ value "name" ("tereza" :: Int) ] "The user's name."

Warning: Watch out for adding the same key twice!

    > info [ value "name" "tereza", value "name" "evan" ] "User logged in"
    > -- only the last "name" value will survive!

-}
value :: Encodable a => String -> a -> Context
value key value =
  ( key, Json.encoder value )


{-| Inside your custom target, you can access the enitre `Entry`, including the
context you attached using `value`. With this function, you can find a certain
piece of context that you added.

  > bugsnag :: Log.Target
  > bugsnag =
  >   Log.custom <| entry ->
  >     case Log.lookup "user" entry of
  >       Ok name -> Bugsnag.send name
  >       Err _ -> Task.succeed ()

To use this, the value you expect to retrieve must be `Decodable`. This means
it must add a decoder like this:

    > import qualified Json.Decode as Json
    >
    > instance Json.Decodable User where
    >   decoder =
    >     Json.map2 User
    >       (Json.field "name" Json.string)
    >       (Json.field "age" Json.int)

This lets me know how to decode your data! REMEMBER: The decoder must match the
encoder used with `value`.

-}
lookup :: Decodable a => String -> Entry -> Result String a
lookup key Entry{context = cx} =
  Dict.get key cx
    |> Result.fromMaybe ("Could not find key: " ++ key)
    |> Result.andThen (Json.Decode.fromValue Json.Decode.decoder >> Result.mapError Debug.toString)
