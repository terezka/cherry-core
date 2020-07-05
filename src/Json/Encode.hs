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
    Value, toBuilder
    -- * Primitives
  , string, chars, int, float, bool, null
    -- * Arrays
  , list
    -- * Objects
  , object, dict
  )
  where

-- TODO: add INLINE to all the functions that produce a Value and
-- see the impact in production code. I suspect it will be valuable.
-- I think using the composition operator in their implementations
-- is better for this, but I am not 100% sure.

import qualified Data.ByteString.Builder.Prim as P
import Data.ByteString.Builder.Prim ((>$<), (>*<))
import qualified Data.ByteString.Builder as B
import qualified Data.Char as Char
import qualified Data.Text.Encoding as TE
import Data.Monoid ((<>))
import Prelude hiding (String, Float, null)
import Data.Word (Word8)

import Basics (Float)
import qualified Dict
import qualified List
import qualified String
import String (String)


-- ENCODER


{-| Representation of a JSON value that can be turned into a string, written
to a file, or turned into a `Data.ByteString.Builder` value.
-}
newtype Value =
  Value { _toBuilder :: B.Builder }



-- TO BUILDER


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
 >   Encode.toBuilder tom
 >   -- {"name":"Tom","age":42}
-}
toBuilder :: Value -> B.Builder
toBuilder (Value builder) =
  builder



-- STRING


{-| Turn a `String` into a JSON string.

 > import Json.Encode exposing (encode, string)
 >
 > -- encode 0 (string "")      == "\"\""
 > -- encode 0 (string "abc")   == "\"abc\""
 > -- encode 0 (string "hello") == "\"hello\""
-}
string :: String -> Value
string str =
  Value $ B.char7 '"' <> escapeString str <> B.char7 '"'


escapeString :: String -> B.Builder
escapeString str =
  TE.encodeUtf8BuilderEscaped escapeWord8 (String.toTextUtf8 str)


{-# INLINE escapeWord8 #-}
escapeWord8 :: P.BoundedPrim Word8
escapeWord8 =
  P.condB (>  0x5C {-\-} ) (P.liftFixedToBounded P.word8) $
  P.condB (== 0x5C {-\-} ) (P.liftFixedToBounded (const ('\\','\\') >$< P.char7 >*< P.char7)) $
  P.condB (== 0x22 {-"-} ) (P.liftFixedToBounded (const ('\\','\"') >$< P.char7 >*< P.char7)) $
  P.condB (== 0x2F {-/-} ) (P.liftFixedToBounded (const ('\\','/') >$< P.char7 >*< P.char7)) $
  P.condB (>= 0x20 {- -} ) (P.liftFixedToBounded P.word8) $
  P.condB (== 0x08 {-\b-}) (P.liftFixedToBounded (const ('\\','b') >$< P.char7 >*< P.char7)) $
  P.condB (== 0x09 {-\t-}) (P.liftFixedToBounded (const ('\\','t') >$< P.char7 >*< P.char7)) $
  P.condB (== 0x0A {-\n-}) (P.liftFixedToBounded (const ('\\','n') >$< P.char7 >*< P.char7)) $
  P.condB (== 0x0C {-\f-}) (P.liftFixedToBounded (const ('\\','f') >$< P.char7 >*< P.char7)) $
  P.condB (== 0x0D {-\r-}) (P.liftFixedToBounded (const ('\\','r') >$< P.char7 >*< P.char7)) $
  P.liftFixedToBounded (toLowCode >$< P.word8 >*< P.word8 >*< P.word8 >*< P.word8 >*< P.word8 >*< P.word8)


{-# INLINE toLowCode #-}
toLowCode :: Word8 -> (Word8,(Word8,(Word8,(Word8,(Word8,Word8)))))
toLowCode code =
  let
    (tens, ones) = divMod code 16
  in
  (0x5C {-\-}, (0x75 {-u-}, (0x30 {-0-}, (0x30 {-0-}, (tens, ones)))))



-- CHARS


chars :: [Char] -> Value
chars chrs =
  Value $ B.char7 '"' <> P.primMapListBounded escapeChar chrs <> B.char7 '"'


{-# INLINE escapeChar #-}
escapeChar :: P.BoundedPrim Char
escapeChar =
  P.condB (> '\\') P.charUtf8 (fromIntegral . Char.ord >$< escapeWord8)



-- BOOL


{-| Turn a `Bool` into a JSON boolean.

 > import Json.Encode exposing (encode, bool)
 >
 > -- encode 0 (bool True)  == "true"
 > -- encode 0 (bool False) == "false"
-}
bool :: Bool -> Value
bool b =
  if b
  then Value "true"
  else Value "false"



-- INT


{-| Turn an `Int` into a JSON number.

 > import Json.Encode exposing (encode, int)
 >
 > -- encode 0 (int 42) == "42"
 > -- encode 0 (int -7) == "-7"
 > -- encode 0 (int 0)  == "0"
-}
int :: Int -> Value
int =
  Value . B.intDec



-- FLOAT


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
  Value . B.doubleDec



-- NULL


{-| Create a JSON `null` value.

 > import Json.Encode exposing (encode, null)
 >
 > -- encode 0 null == "null"
-}
null :: Value
null =
  Value "null"



-- ARRAYS


{-| Turn a `List` into a JSON array.

 > import Json.Encode as Encode exposing (bool, encode, int, list, string)
 >
 > -- encode 0 (list int [1,3,4])       == "[1,3,4]"
 > -- encode 0 (list bool [True,False]) == "[true,false]"
 > -- encode 0 (list string ["a","b"])  == """["a","b"]"""
-}
list :: (a -> Value) -> [a] -> Value
list encodeEntry entries =
  case entries of
    []   -> Value (B.string7 "[]")
    x:xs -> Value (encodeSequence arrayOpen arrayClose (_toBuilder . encodeEntry) x xs)


arrayOpen :: B.Builder
arrayOpen =
  B.string7 "["


arrayClose :: B.Builder
arrayClose =
  B.char7 ']'



-- OBJECTS


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
object :: [(String, Value)] -> Value
object fields =
  case fields of
    []   -> Value (B.string7 "{}")
    f:fs -> Value (encodeSequence objectOpen objectClose encodeField f fs)


encodeField :: (String, Value) -> B.Builder
encodeField (key, Value builder) =
  B.char7 '"' <> escapeString key <> B.string7 "\":" <> builder


objectOpen :: B.Builder
objectOpen =
  B.string7 "{"


objectClose :: B.Builder
objectClose =
  B.char7 '}'



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
dict :: (k -> String) -> (v -> Value) -> Dict.Dict k v -> Value
dict encodeKey encodeValue pairs =
  let
    toPair (key, value) =
      (encodeKey key, encodeValue value)
  in
  object $ List.map toPair (Dict.toList pairs)



-- ENCODE SEQUENCE


encodeSequence :: B.Builder -> B.Builder -> (a -> B.Builder) -> a -> [a] -> B.Builder
encodeSequence open close encodeEntry x xs =
  let
    addEntry entry builder =
      comma
      <> encodeEntry entry
      <> builder
  in
  open
  <> encodeEntry x
  <> foldr addEntry close xs


comma :: B.Builder
comma =
  B.string7 ","
