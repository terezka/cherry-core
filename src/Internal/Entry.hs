{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Internal.Entry
  ( Entry(..), Severity(..)
  , toColor, toTitle
  , pretty, compact, json
  , WithMisc(..), bool, string, int, float, value
  , Basic
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
import qualified Data.Time.Clock as Clock
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
data Entry s =
  Entry
    { severity :: Severity
    , namespace :: String
    , message :: String
    , context :: s
    , time :: Clock.UTCTime
    , callstack :: Stack.CallStack
    }


encode :: (s -> Json.Value) -> Entry s -> Json.Value
encode encodeContext (Entry severity namespace message context time _) =
  Json.object
    [ ( "severity", Json.string (toTitle severity) )
    , ( "namespace", Json.string namespace )
    , ( "message", Json.string message )
    , ( "context", encodeContext context )
    , ( "time", Json.string (Debug.toString time) )
    ]



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
pretty :: (s -> Json.Value) -> Entry s -> String
pretty encodeContext (Entry severity namespace message context time callstack) =
  let viewExtra =
        case severity of
          Unknown ->
            [ "Context:"
            , toText (encodeContext context)
            , "Segments:"
            , viewSegments
            ]

          _ ->
            [ "Context:"
            , toText (encodeContext context)
            ]

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
compact :: (s -> Json.Value) -> Entry s -> String
compact encodeContext (Entry severity namespace message context _ _) =
  let string c = "[" ++ c ++ "]"
  in
  String.concat
    [ string (toTitle severity)
    , string namespace
    , string message
    , string (toText (encodeContext context))
    ]


toText :: Json.Value -> String
toText =
  Data.Text.Encoding.decodeUtf8 << ByteString.toStrict << Json.toByteString


{-| JSON formatting of the entry. Can be used with `terminal`, `file`,
or inside a custom target.
-}
json :: (s -> Json.Value) -> Entry s -> String
json encodeContext =
  encode encodeContext >> Json.toByteString >> ByteString.toStrict >> Data.Text.Encoding.decodeUtf8



-- CONTEXT HELPERS


{-| -}
class WithMisc a where
  setMisc :: String -> Json.Value -> a -> a


{-| -}
bool :: WithMisc s => String -> Bool -> s -> s
bool key value =
  setMisc key (Json.bool value)


{-| -}
string :: WithMisc s => String -> String -> s -> s
string key value =
  setMisc key (Json.string value)


{-| -}
int :: WithMisc s => String -> Int -> s -> s
int key value =
  setMisc key (Json.int value)


{-| -}
float :: WithMisc s => String -> Float -> s -> s
float key value =
  setMisc key (Json.float value)


{-| -}
value :: WithMisc s => String -> Json.Value -> s -> s
value key value =
  setMisc key value


-- CONTEXT / DEFAULT


type Basic
  = Dict String Json.Value


instance WithMisc Basic where
  setMisc =
    Dict.insert
