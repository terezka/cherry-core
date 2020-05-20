{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, TypeSynonymInstances #-}

{-|

Module      : Json.Encode
Description : Encode JSON.
License     : BSD 3
Maintainer  : terezasokol@gmail.com
Stability   : experimental
Portability : POSIX

-}

module Json.Encode
  ( -- * Encoding
    toByteString, Value, Encodable(..)
    -- * Primitives
  , string, chars, int, float, bool, null
    -- * Arrays
  , list, array
    -- * Objects
  , object, dict
  )
  where


import qualified Data.ByteString.Internal as ByteString
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as Text
import qualified Data.Text.Encoding
import qualified List
import qualified Dict
import Basics (Float)
import Prelude hiding (null, Float)
import Data.Monoid ((<>))
import Json.Ast (AST(..))


{-| -}
class Encodable a where
  encoder :: a -> Value


instance Encodable Text.Text where
  encoder = string


instance Encodable Bool where
  encoder = bool


instance Encodable Int where
  encoder = int

instance Encodable Float where
  encoder = float



-- VALUES


{-| Represents a JSON value.
-}
type Value
  = AST


{-| Turn a `String` into a JSON string.

 > import Json.Encode exposing (encode, string)
 >
 > -- encode 0 (string "")      == "\"\""
 > -- encode 0 (string "abc")   == "\"abc\""
 > -- encode 0 (string "hello") == "\"hello\""
-}
string :: Text.Text -> Value
string str =
  String (Data.Text.Encoding.encodeUtf8 str)


{-| Turn a `Bool` into a JSON boolean.

 > import Json.Encode exposing (encode, bool)
 >
 > -- encode 0 (bool True)  == "true"
 > -- encode 0 (bool False) == "false"
-}
bool :: Bool -> Value
bool =
  Boolean


{-| Turn an `Int` into a JSON number.

 > import Json.Encode exposing (encode, int)
 >
 > -- encode 0 (int 42) == "42"
 > -- encode 0 (int -7) == "-7"
 > -- encode 0 (int 0)  == "0"
-}
int :: Int -> Value
int =
  Int


{-| Turn a `Float` into a JSON number.

 > import Json.Encode exposing (encode, float)
 >
 > -- encode 0 (float 3.14)     == "3.14"
 > -- encode 0 (float 1.618)    == "1.618"
 > -- encode 0 (float -42)      == "-42"
 > -- encode 0 (float NaN)      == "null"
 > -- encode 0 (float Infinity) == "null"

**Note:** Floating point numbers are defined in the [IEEE 754 standard](https://en.wikipedia.org/wiki/IEEE_754)
which is hardcoded into almost all CPUs. This standard allows `Infinity` and
`NaN`. [The JSON spec](https://www.json.org/) does not include these values, so we encode them
both as `null`.
-}
float :: Float -> Value
float =
  Float


{-| Create a JSON `null` value.

 > import Json.Encode exposing (encode, null)
 >
 > -- encode 0 null == "null"
-}
null :: Value
null =
  NULL


{-| Turn a `List` into a JSON array.

 > import Json.Encode as Encode exposing (bool, encode, int, list, string)
 >
 > -- encode 0 (list int [1,3,4])       == "[1,3,4]"
 > -- encode 0 (list bool [True,False]) == "[true,false]"
 > -- encode 0 (list string ["a","b"])  == """["a","b"]"""
-}
list :: (a -> Value) -> [a] -> Value
list encodeEntry entries =
  Array $ map encodeEntry entries


{-| Turn an `Array` into a JSON array.
-}
array :: [Value] -> Value
array =
  Array


{-| Create a JSON object.

 > import Json.Encode as Encode
 >
 > tom : Encode.Value
 > tom =
 >   Encode.object
 >     [ ( "name", Encode.string "Tom" )
 >     , ( "age", Encode.int 42 )
 >     ]
 >
 > -- Encode.encode 0 tom == """{"name":"Tom","age":42}"""

-}
object :: [(Text.Text, Value)] -> Value
object pairs =
  let toBts ( key, value ) =
        ( Data.Text.Encoding.encodeUtf8 key, value )
  in
  Object (List.map toBts pairs)


{-| Turn a `Dict` into a JSON object.

 > import Dict exposing (Dict)
 > import Json.Encode as Encode
 >
 > people : Dict String Int
 > people =
 >   Dict.fromList [ ("Tom",42), ("Sue", 38) ]
 >
 > -- Encode.encode 0 (Encode.dict identity Encode.int people)
 > --   == """{"Tom":42,"Sue":38}"""
-}
dict :: (k -> Text.Text) -> (v -> Value) -> Dict.Dict k v -> Value
dict encodeKey encodeValue pairs =
  let toBts ( key, value ) =
        ( Data.Text.Encoding.encodeUtf8 (encodeKey key), encodeValue value )
  in
  Object $ List.map toBts (Dict.toList pairs)



-- CHARS


chars :: [Char] -> Value
chars chrs =
  String (BSU.fromString (escape chrs))


escape :: [Char] -> [Char]
escape chrs =
  case chrs of
    [] ->
      []

    c:cs
      | c == '\r' -> '\\' : 'r'  : escape cs
      | c == '\n' -> '\\' : 'n'  : escape cs
      | c == '\"' -> '\\' : '"'  : escape cs
      | c == '\\' -> '\\' : '\\' : escape cs
      | otherwise -> c : escape cs



-- ENCODE


{-| Convert a `Value` into a bytestring.

 > import Json.Encode as Encode
 >
 > tom : Encode.Value
 > tom =
 >   Encode.object
 >     [ ( "name", Encode.string "Tom" )
 >     , ( "age", Encode.int 42 )
 >     ]
 >
 > compact =
 >   Encode.toByteString tom
 >   -- {"name":"Tom","age":42}
-}
toByteString :: Value -> BL.ByteString
toByteString value =
  B.toLazyByteString $ encodeHelp "" value


encodeHelp :: BSC.ByteString -> Value -> B.Builder
encodeHelp indent value =
  case value of
    Array [] ->
      B.string7 "[]"

    Array (first : rest) ->
      encodeArray indent first rest

    Object [] ->
      B.string7 "{}"

    Object (first : rest) ->
      encodeObject indent first rest

    String str ->
      B.char7 '"' <> B.byteString str <> B.char7 '"'

    Boolean boolean ->
      B.string7 (if boolean then "true" else "false")

    Int n ->
      B.intDec n

    Float float_ ->
      B.doubleDec float_

    NULL ->
      "null"



-- ENCODE ARRAY


encodeArray :: BSC.ByteString -> Value -> [Value] -> B.Builder
encodeArray =
  encodeSequence arrayOpen arrayClose encodeHelp


arrayOpen :: B.Builder
arrayOpen =
  B.string7 "["


arrayClose :: B.Builder
arrayClose =
  B.char7 ']'



-- ENCODE OBJECT


encodeObject :: BSC.ByteString -> (ByteString.ByteString, Value) -> [(ByteString.ByteString, Value)] -> B.Builder
encodeObject =
  encodeSequence objectOpen objectClose encodeField


objectOpen :: B.Builder
objectOpen =
  B.string7 "{"


objectClose :: B.Builder
objectClose =
  B.char7 '}'


encodeField :: BSC.ByteString -> (ByteString.ByteString, Value) -> B.Builder
encodeField indent (key, value) =
  B.char7 '"' <> B.byteString key <> B.string7 "\":" <> encodeHelp indent value



-- ENCODE SEQUENCE


encodeSequence :: B.Builder -> B.Builder -> (BSC.ByteString -> a -> B.Builder) -> BSC.ByteString -> a -> [a] -> B.Builder
encodeSequence open close encodeEntry indent first rest =
  let
    newIndent =
      indent

    newIndentBuilder =
      B.byteString newIndent

    closer =
      B.byteString indent <> close

    addValue field builder =
      commaNewline
      <> newIndentBuilder
      <> encodeEntry newIndent field
      <> builder
  in
    open
    <> newIndentBuilder
    <> encodeEntry newIndent first
    <> foldr addValue closer rest


commaNewline :: B.Builder
commaNewline =
  B.string7 ","
