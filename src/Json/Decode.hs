{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind -fno-warn-name-shadowing #-}
{-# LANGUAGE BangPatterns, Rank2Types, MagicHash, OverloadedStrings, UnboxedTuples, TypeSynonymInstances #-}

{-|

Module      : Json.Decode
Description : Decode JSON.
License     : BSD 3
Maintainer  : terezasokol@gmail.com
Stability   : experimental
Portability : POSIX

-}

module Json.Decode
  ( -- Turn JSON values into Haskell values.
    -- * Primitives
    Decoder, Decodable(..), string, chars, bool, int, float, null, succeed, fail
    -- * Data Structures
  , nullable, list, oneOrMore, dict, pair, field, at
    -- * Inconsistent Data Structure
  , maybe, oneOf
    -- * Run Decoders
  , fromByteString, fromValue
  , Error(..), Problem(..), DecodeExpectation(..), ParseError(..), errorToString
    -- * Transforming
  , map, map2, map3, map4, map5, map6, map7, map8, andThen
  )
  where

import qualified Data.Either as Either
import qualified Data.List as List hiding (map)
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.ByteString.Internal as ByteString
import qualified Data.ByteString.Char8 as Char8

import qualified Json.Encode as Json
import qualified Parser.Keyword as K
import qualified Parser.Primitives as P
import qualified Parser.Variable as V
import qualified Dict
import Parser.Primitives (Row, Col)
import Json.Ast (AST(..))

import Foreign.Ptr (Ptr, plusPtr, minusPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import GHC.Word (Word8)
import Data.Word (Word16)
import Prelude hiding (maybe, map, fail, null, Float, String)
import String (String)
import Basics (Float)
import Result (Result(..))
import Dict (Dict)




-- RUNNERS


{-| Parse the given string into a JSON value and then run the `Decoder` on it.
This will fail if the string is not well-formed JSON or if the `Decoder`
fails for some reason.

 > fromByteString int "4"     == Ok 4
 > fromByteString int "1 + 2" == Err ...

-}
fromByteString :: Decoder a -> ByteString.ByteString -> Result Error a
fromByteString (Decoder decode) src =
  case P.fromByteString pFile BadEnd src of
    Either.Right ast ->
      decode ast Ok (Err . DecodeProblem)

    Either.Left problem ->
      Err (ParseProblem problem)


{-| Parse the given string into a JSON value and then run the `Decoder` on it.
This will fail if the string is not well-formed JSON or if the `Decoder`
fails for some reason.

 > fromValue int (Encode.int 4)     == Ok 4
 > fromValue int (Encode.bool True) == Err ...

-}
fromValue :: Decoder a -> Json.Value -> Result Error a
fromValue (Decoder decode) ast =
  decode ast Ok (Err . DecodeProblem)



-- DECODERS


{-| A value that knows how to decode JSON values.
-}
newtype Decoder a =
  Decoder (forall b. AST -> (a -> b) -> (Problem -> b) -> b)


{-| -}
class Decodable a where
  decoder :: Decoder a


instance Decodable String where
  decoder = string


instance Decodable Bool where
  decoder = bool


instance Decodable Int where
  decoder = int


instance Decodable Float where
  decoder = float



-- ERRORS


{-| A structured error describing exactly how the decoder failed. You can use
this to create more elaborate visualizations of a decoder problem. For example,
you could show the entire JSON object and show the part causing the failure in
red.
-}
data Error
  = DecodeProblem Problem
  | ParseProblem ParseError
  deriving (Eq, Show)



-- DECODE PROBLEMS


data Problem
  = Field ByteString.ByteString Problem
  | Index Int Problem
  | OneOf Problem [Problem]
  | Failure String
  | Expecting DecodeExpectation
  deriving (Eq, Show)


{-| -}
data DecodeExpectation
  = TObject
  | TArray
  | TString
  | TBool
  | TInt
  | TFloat
  | TObjectWith ByteString.ByteString
  | TArrayPair Int
  | TNull
  deriving (Eq, Show)



-- ERROR TO STRING


{-| Convert a decoding error into a `String` that is nice for debugging.

This function is WORK IN PROGRESS and frankly not very good yet.
-}
errorToString :: Error -> String
errorToString error =
  case error of
    DecodeProblem problem ->
      case problem of
        Field name _ ->
          "Could not decode field " <> Text.decodeUtf8 name <> "."

        Index i _ ->
          "Errored at array index " <> Text.pack (show i) <> "."

        OneOf _ _ ->
          "Could not find any solutions in oneOf"

        Failure msg ->
          msg

        Expecting expecting ->
          case expecting of
            TObject ->
              "Expected an object."

            TArray ->
              "Expected an array."

            TString ->
              "Expected a string."

            TBool ->
              "Expected a boolean."

            TInt ->
              "Expected an int."

            TFloat ->
              "Expected a float."

            TObjectWith field_ ->
              "Expected an object with a property \"" <> Text.decodeUtf8 field_ <> "\"."

            TArrayPair _ ->
              "Expected an array of two elements."

            TNull ->
              "Expected a null."

    ParseProblem _ ->
      "Parser problem TODO"



-- INSTANCES


instance Functor Decoder where
  {-# INLINE fmap #-}
  fmap func (Decoder decodeA) =
    Decoder $ \ast ok err ->
      let
        ok' a = ok (func a)
      in
      decodeA ast ok' err


instance Applicative Decoder where
  {-# INLINE pure #-}
  pure = return

  {-# INLINE (<*>) #-}
  (<*>) (Decoder decodeFunc) (Decoder decodeArg) =
    Decoder $ \ast ok err ->
      let
        okF func =
          let
            okA arg = ok (func arg)
          in
          decodeArg ast okA err
      in
      decodeFunc ast okF err


instance Monad Decoder where
  {-# INLINE return #-}
  return a =
    Decoder $ \_ ok _ ->
      ok a

  {-# INLINE (>>=) #-}
  (>>=) (Decoder decodeA) callback =
    Decoder $ \ast ok err ->
      let
        ok' a =
          case callback a of
            Decoder decodeB -> decodeB ast ok err
      in
      decodeA ast ok' err



-- STRINGS


{-| Decode a JSON string into a list of `Char`'s.

 > fromByteString string "true"              == Err ...
 > fromByteString string "42"                == Err ...
 > fromByteString string "3.14"              == Err ...
 > fromByteString string "\"hello\""         == Ok "hello"
 > fromByteString string "{ \"hello\": 42 }" == Err ...

-}
chars :: Decoder [Char]
chars =
  Decoder $ \ast ok err ->
    case ast of
      String snippet ->
        ok (Char8.unpack snippet)

      _ ->
        err (Expecting TString)


{-| Decode a JSON string into a `Text`.

 > fromByteString string "true"              == Err ...
 > fromByteString string "42"                == Err ...
 > fromByteString string "3.14"              == Err ...
 > fromByteString string "\"hello\""         == Ok "hello"
 > fromByteString string "{ \"hello\": 42 }" == Err ...

-}
string :: Decoder String
string =
  Decoder $ \ast ok err ->
    case ast of
      String snippet ->
        ok (Text.decodeUtf8 snippet)

      _ ->
        err (Expecting TString)



-- BOOL


{-| Decode a JSON boolean into a `Prelude.Bool`.

 > fromByteString bool "true"              == Ok True
 > fromByteString bool "42"                == Err ...
 > fromByteString bool "3.14"              == Err ...
 > fromByteString bool "\"hello\""         == Err ...
 > fromByteString bool "{ \"hello\": 42 }" == Err ...

-}
bool :: Decoder Bool
bool =
  Decoder $ \ast ok err ->
    case ast of
      Boolean boolean ->
        ok boolean

      _ ->
        err (Expecting TBool)



-- INT


{-| Decode a JSON number into an `Prelude.Int`.

 > fromByteString int "true"              == Err ...
 > fromByteString int "42"                == Ok 42
 > fromByteString int "3.14"              == Err ...
 > fromByteString int "\"hello\""         == Err ...
 > fromByteString int "{ \"hello\": 42 }" == Err ...

-}
int :: Decoder Int
int =
  Decoder $ \ast ok err ->
    case ast of
      Int n ->
        ok n

      _ ->
        err (Expecting TInt)



-- FLOAT


{-| Decode a JSON number into a `Prelude.Float`.

 > fromByteString float "true"              == Err ..
 > fromByteString float "42"                == Ok 42
 > fromByteString float "3.14"              == Ok 3.14
 > fromByteString float "\"hello\""         == Err ...
 > fromByteString float "{ \"hello\": 42 }" == Err ...

-}
float :: Decoder Float
float =
  Decoder $ \ast ok err ->
    case ast of
      Float n ->
        ok n

      _ ->
        err (Expecting TFloat)



-- NULL


{-| Decode a nullable JSON value into a value.

 > fromByteString (nullable int) "13"    == Ok (Just 13)
 > fromByteString (nullable int) "42"    == Ok (Just 42)
 > fromByteString (nullable int) "null"  == Ok Nothing
 > fromByteString (nullable int) "true"  == Err ..

-}
nullable :: Decoder a -> Decoder (Maybe.Maybe a)
nullable decoder =
  oneOf
    [ fmap Maybe.Just decoder
    , null_
    ]


null_ :: Decoder (Maybe.Maybe a)
null_ =
  Decoder $ \ast ok err ->
    case ast of
      NULL ->
        ok Maybe.Nothing

      _ ->
        err (Expecting TNull)


{-| Decode a `null` value into some value.

 > fromByteString (null False) "null" == Ok False
 > fromByteString (null 42) "null"    == Ok 42
 > fromByteString (null 42) "42"      == Err ..
 > fromByteString (null 42) "false"   == Err ..

So if you ever see a `null`, this will return whatever value you specified.
-}
null :: a -> Decoder a
null value =
  Decoder $ \ast ok err ->
    case ast of
      NULL ->
        ok value

      _ ->
        err (Expecting TNull)



-- MAYBE


{-| Helpful for dealing with optional fields. Here are a few slightly different
examples:

 > json = """{ "name": "tom", "age": 42 }"""
 > fromByteString (maybe (field "age"    int  )) json == Ok (Just 42)
 > fromByteString (maybe (field "name"   int  )) json == Ok Nothing
 > fromByteString (maybe (field "height" float)) json == Ok Nothing
 > fromByteString (field "age"    (maybe int  )) json == Ok (Just 42)
 > fromByteString (field "name"   (maybe int  )) json == Ok Nothing
 > fromByteString (field "height" (maybe float)) json == Err ...

Notice the last example! It is saying we *must* have a field named `height` and
the content *may* be a float. There is no `height` field, so the decoder fails.
Point is, `maybe` will make exactly what it contains conditional. For optional
fields, this means you probably want it *outside* a use of `field` or `at`.
-}
maybe :: Decoder a -> Decoder (Maybe.Maybe a)
maybe decoder_ =
  oneOf
    [ fmap Maybe.Just decoder_
    , return Maybe.Nothing
    ]



-- LISTS


{-| Decode a JSON array into a `List`.

 > fromByteString (list int) "[1,2,3]"       == Ok [1,2,3]
 > fromByteString (list bool) "[true,false]" == Ok [True,False]

-}
list :: Decoder a -> Decoder [a]
list decoder =
  Decoder $ \ast ok err ->
    case ast of
      Array asts ->
        listHelp decoder ok err 0 asts []

      _ ->
        err (Expecting TArray)


listHelp :: Decoder a -> ([a] -> b) -> (Problem -> b) -> Int -> [AST] -> [a] -> b
listHelp decoder@(Decoder decodeA) ok err !i asts revs =
  case asts of
    [] ->
      ok (List.reverse revs)

    ast:asts ->
      let
        ok' value = listHelp decoder ok err (i+1) asts (value:revs)
        err' prob = err (Index i prob)
      in
      decodeA ast ok' err'



-- PAIR


{-| Decode a JSON array of exactly two elements into a `Tuple`.

 > fromByteString (pair int book) "[1, false]"    == Ok (1, false)
 > fromByteString (pair int bool) "[1, false, 3]" == Err ..

-}
pair :: Decoder a -> Decoder b -> Decoder ( a, b )
pair (Decoder decodeA) (Decoder decodeB) =
  Decoder $ \ast ok err ->
    case ast of
      Array vs ->
        case vs of
          [astA,astB] ->
            let
              err0 e = err (Index 0 e)
              ok0 a =
                let
                  err1 e = err (Index 1 e)
                  ok1 b = ok (a,b)
                in
                decodeB astB ok1 err1
            in
            decodeA astA ok0 err0

          _ ->
            err (Expecting (TArrayPair (List.length vs)))

      _ ->
        err (Expecting TArray)



-- OBJECTS


{-| Decode a JSON object into an `Dict`.

 > fromByteString (dict int) "{ \"alice\": 42, \"bob\": 99 }"
 >   == Ok (Dict.fromList [("alice", 42), ("bob", 99)])

If you need the keys (like `alice` and `bob`) available in the `Dict`
values as well, I recommend using a (private) intermediate data structure like
`Info` in this example:

 > module User exposing (User, decoder)
 >
 > import Dict
 > import Json.Decode exposing (..)
 >
 > type alias User =
 >   { name : String
 >   , height : Float
 >   , age : Int
 >   }
 >
 > decoder : Decoder (Dict.Dict String User)
 > decoder =
 >   map (Dict.map infoToUser) (dict infoDecoder)
 >
 > type alias Info =
 >   { height : Float
 >   , age : Int
 >   }
 >
 > infoDecoder : Decoder Info
 > infoDecoder =
 >   map2 Info
 >     (field "height" float)
 >     (field "age" int)
 >
 > infoToUser : String -> Info -> User
 > infoToUser name { height, age } =
 >   User name height age

So now JSON like

 > { "alice": { height: 1.6, age: 33 }}

are turned into dictionary values like

> Dict.singleton "alice" (User "alice" 1.6 33)

if you need that.
-}
dict :: Decoder a -> Decoder (Dict String a)
dict valueDecoder =
  map Dict.fromList (pairs valueDecoder)


pairs :: Decoder a -> Decoder [( String, a )]
pairs valueDecoder =
  Decoder $ \ast ok err ->
    case ast of
      Object kvs ->
        pairsHelp valueDecoder ok err kvs []

      _ ->
        err (Expecting TObject)


pairsHelp :: Decoder a -> ([( String, a )] -> b) -> (Problem -> b) -> [( ByteString.ByteString, AST )] -> [( String, a )] -> b
pairsHelp valueDecoder@(Decoder decodeA) ok err kvs revs =
  case kvs of
    [] ->
      ok (List.reverse revs)

    ( string_, ast ) : kvs ->
      let
        key = Text.decodeUtf8 string_
        ok' value = pairsHelp valueDecoder ok err kvs (( key, value ) : revs)
        err' prob = err (Field string_ prob)
      in
      decodeA ast ok' err'


{-| Decode a JSON array that has one or more elements.
-}
oneOrMore :: (a -> [a] -> value) -> Decoder a -> Decoder value
oneOrMore toValue decoder =
  list decoder >>= oneOrMoreHelp toValue


oneOrMoreHelp :: (a -> [a] -> value) -> [a] -> Decoder value
oneOrMoreHelp toValue xs =
  case xs of
    [] ->
      fail "a ARRAY with at least ONE element"

    y : ys ->
      succeed (toValue y ys)



-- FIELDS


{-| Decode a JSON object, requiring a particular field.

 > fromByteString (field "x" int) "{ \"x\": 3 }"            == Ok 3
 > fromByteString (field "x" int) "{ \"x\": 3, \"y\": 4 }"  == Ok 3
 > fromByteString (field "x" int) "{ \"x\": true }"         == Err ...
 > fromByteString (field "x" int) "{ \"y\": 4 }"            == Err ...
 > fromByteString (field "name" string) "{ \"name\": \"tom\" }" == Ok "tom"

The object *can* have other fields. Lots of them! The only thing this decoder
cares about is if `x` is present and that the value there is an `Int`.
Check out [`map2`](#map2) to see how to decode multiple fields!
-}
field :: ByteString.ByteString -> Decoder a -> Decoder a
field key (Decoder decodeA) =
  Decoder $ \ast ok err ->
    case ast of
      Object kvs ->
        case findField key kvs of
          Maybe.Just value ->
            let
              err' prob =
                err (Field key prob)
            in
            decodeA value ok err'

          Maybe.Nothing ->
            err (Expecting (TObjectWith key))

      _ ->
        err (Expecting TObject)


findField :: ByteString.ByteString -> [( ByteString.ByteString, AST )] -> Maybe.Maybe AST
findField key pairs =
  case pairs of
    [] ->
      Maybe.Nothing

    (bts, value) : remainingPairs ->
      if key == bts
      then Just value
      else findField key remainingPairs


{-| Decode a nested JSON object, requiring certain fields.

 > json = """{ "person": { "name": "tom", "age": 42 } }"""
 > fromByteString (at ["person", "name"] string) json  == Ok "tom"
 > fromByteString (at ["person", "age" ] int   ) json  == Ok "42

This is really just a shorthand for saying things like:

 > field "person" (field "name" string) == at ["person","name"] string

-}
at :: [ByteString.ByteString] -> Decoder a -> Decoder a
at fields decoder =
    List.foldr field decoder fields



-- ONE OF


{-| Try a bunch of different decoders. This can be useful if the JSON may come
in a couple different formats. For example, say you want to read an array of
numbers, but some of them are `null`.

 > import String
 >
 > badInt : Decoder Int
 > badInt =
 >   oneOf [ int, null 0 ]
 >
 > -- fromByteString (list badInt) "[1,2,null,4]" == Ok [1,2,0,4]

Why would someone generate JSON like this? Questions like this are not good
for your health. The point is that you can use `oneOf` to handle situations
like this!

You could also use `oneOf` to help version your data. Try the latest format,
then a few older ones that you still support. You could use `andThen` to be
even more particular if you wanted.
-}
oneOf :: [Decoder a] -> Decoder a
oneOf decoders =
  Decoder $ \ast ok err ->
    case decoders of
      Decoder decodeA : decoders ->
        let
          err' e =
            oneOfHelp ast ok err decoders e []
        in
        decodeA ast ok err'

      [] ->
        error "Ran into (Json.Decode.oneOf [])"


oneOfHelp :: AST -> (a -> b) -> (Problem -> b) -> [Decoder a] -> Problem -> [Problem] -> b
oneOfHelp ast ok err decoders p ps =
  case decoders of
    Decoder decodeA : decoders ->
      let
        err' p' =
          oneOfHelp ast ok err decoders p' (p:ps)
      in
      decodeA ast ok err'

    [] ->
      err (oneOfError [] p ps)


oneOfError :: [Problem] -> Problem -> [Problem] -> Problem
oneOfError problems prob ps =
  case ps of
    [] ->
      OneOf prob problems

    p:ps ->
      oneOfError (prob:problems) p ps



-- PRIMITIVES


{-| Ignore the JSON and make the decoder fail. This is handy when used with
`oneOf` or `andThen` where you want to give a custom error message in some
case.

See the [`andThen`](#andThen) docs for an example.
-}
fail :: String -> Decoder a
fail x =
  Decoder $ \_ _ err ->
    err (Failure x)


{-| Ignore the JSON and produce a certain value.

 > fromByteString (succeed 42) "true"    == Ok 42
 > fromByteString (succeed 42) "[1,2,3]" == Ok 42
 > fromByteString (succeed 42) "hello"   == Err ... -- this is not a valid JSON string

This is handy when used with `oneOf` or `andThen`.
-}
succeed :: a -> Decoder a
succeed a =
  Decoder $ \_ ok _ ->
    ok a



-- MAPS


{-| Transform a decoder. Maybe you just want to know the length of a string:

 > import String
 >
 > stringLength : Decoder Int
 > stringLength =
 >   map String.length string

It is often helpful to use `map` with `oneOf`, like when defining `nullable`:

    nullable : Decoder a -> Decoder (Maybe a)
    nullable decoder =
      oneOf
        [ null Nothing
        , map Just decoder
        ]

-}
map :: (a -> value) -> Decoder a -> Decoder value
map f a =
  return f <*> a


{-| Try two decoders and then combine the result. We can use this to decode
objects with many fields:

 > type alias Point = { x : Float, y : Float }
 >
 > point : Decoder Point
 > point =
 >   map2 Point
 >     (field "x" float)
 >     (field "y" float)
 >
 > -- fromByteString point """{ "x": 3, "y": 4 }""" == Ok { x = 3, y = 4 }

It tries each individual decoder and puts the result together with the `Point`
constructor.
-}
map2 :: (a -> b -> value) -> Decoder a -> Decoder b -> Decoder value
map2 func a b =
  return func <*> a <*> b


{-| Try three decoders and then combine the result. We can use this to decode
objects with many fields:

 > type alias Person = { name : String, age : Int, height : Float }
 >
 > person : Decoder Person
 > person =
 >   map3 Person
 >     (at ["name"] string)
 >     (at ["info","age"] int)
 >     (at ["info","height"] float)
 >
 > -- json = """{ "name": "tom", "info": { "age": 42, "height": 1.8 } }"""
 > -- fromByteString person json == Ok { name = "tom", age = 42, height = 1.8 }

Like `map2` it tries each decoder in order and then give the results to the
`Person` constructor. That can be any function though!
-}
map3 :: (a -> b -> c -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder value
map3 func a b c =
  return func <*> a <*> b <*> c


{-| -}
map4 :: (a -> b -> c -> d -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder value
map4 func a b c d =
  return func <*> a <*> b <*> c <*> d


{-| -}
map5 :: (a -> b -> c -> d -> e -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder value
map5 func a b c d e =
  return func <*> a <*> b <*> c <*> d <*> e


{-| -}
map6 :: (a -> b -> c -> d -> e -> f -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f -> Decoder value
map6 func a b c d e f =
  return func <*> a <*> b <*> c <*> d <*> e <*> f


{-| -}
map7 :: (a -> b -> c -> d -> e -> f -> g -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f -> Decoder g -> Decoder value
map7 func a b c d e f g =
  return func <*> a <*> b <*> c <*> d <*> e <*> f <*> g


{-| -}
map8 :: (a -> b -> c -> d -> e -> f -> g -> h -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f -> Decoder g -> Decoder h -> Decoder value
map8 func a b c d e f g h =
  return func <*> a <*> b <*> c <*> d <*> e <*> f <*> g <*> h


{-| Create decoders that depend on previous results. If you are creating
versioned data, you might do something like this:

 > info : Decoder Info
 > info =
 >   field "version" int
 >     |> andThen infoHelp
 >
 > infoHelp : Int -> Decoder Info
 > infoHelp version =
 >   case version of
 >     4 ->
 >       infoDecoder4
 >
 >     3 ->
 >       infoDecoder3
 >
 >     _ ->
 >       fail <|
 >         "Trying to decode info, but version "
 >         ++ toString version ++ " is not supported."
 >
 > -- infoDecoder4 : Decoder Info
 > -- infoDecoder3 : Decoder Info

-}
andThen :: (a -> Decoder b) -> Decoder a -> Decoder b
andThen callback (Decoder decodeA) =
  Decoder $ \ast ok err ->
    let
      ok' a =
        case callback a of
          Decoder decodeB -> decodeB ast ok err
    in
    decodeA ast ok' err



-- PARSE


type Parser a =
  P.Parser ParseError a


data ParseError
  = ObjectStart Row Col
  | ObjectMore Row Col
  | ObjectEnd Row Col
  | ArrayStart Row Col
  | ArrayMore Row Col
  | ArrayEnd Row Col
  | StringStart Row Col
  | StringProblem StringProblem Row Col
  | NumberStart Row Col
  | NumberProblem NumberProblem Row Col
  | Bool Row Col
  | Null Row Col
  | Value Row Col
  | Colon Row Col
  | BadEnd Row Col
  deriving (Eq, Show)


data StringProblem
  = BadStringEnd
  | BadStringControlChar
  | BadStringEscapeChar
  | BadStringEscapeHex
  deriving (Eq, Show)


data NumberProblem
  = NumberEnd
  | NumberDot Int
  | NumberNoLeadingZero
  deriving (Eq, Show)



-- PARSE AST


pFile :: Parser AST
pFile =
  do  spaces
      value <- pValue
      spaces
      return value


pValue :: Parser AST
pValue =
  P.oneOf Value
    [ String <$> pString
    , pObject
    , pArray
    , K.symbol 0x2B {- + -} NumberStart >> pNumber id id
    , K.symbol 0x2D {- - -} NumberStart >> pNumber negate negate
    , pNumber id id
    , K.k4 0x74 0x72 0x75 0x65      Bool >> return (Boolean True)
    , K.k5 0x66 0x61 0x6C 0x73 0x65 Bool >> return (Boolean False)
    , K.k4 0x6E 0x75 0x6C 0x6C      Null >> return NULL
    ]



-- OBJECT


pObject :: Parser AST
pObject =
  do  P.word1 0x7B {- { -} ObjectStart
      spaces
      P.oneOf ObjectMore
        [ do  entry <- pField
              spaces
              pObjectHelp [entry]
        , do  P.word1 0x7D {-}-} ObjectEnd
              return (Object [])
        ]


pObjectHelp :: [( ByteString.ByteString, AST )] -> Parser AST
pObjectHelp revEntries =
  P.oneOf ObjectMore
    [ do  P.word1 0x2C {-,-} ObjectMore
          spaces
          entry <- pField
          spaces
          pObjectHelp (entry : revEntries)
    ,
      do  P.word1 0x7D {-}-} ObjectEnd
          return (Object (List.reverse revEntries))
    ]


pField :: Parser (ByteString.ByteString, AST)
pField =
  do  key <- pString
      spaces
      P.word1 0x3A {-:-} Colon
      spaces
      value <- pValue
      return (key, value)



-- ARRAY


pArray :: Parser AST
pArray =
  do  P.word1 0x5B {-[-} ArrayStart
      spaces
      P.oneOf ArrayMore
        [ do  entry <- pValue
              spaces
              pArrayHelp [entry]
        , do  P.word1 0x5D {-]-} ArrayEnd
              return (Array [])
        ]


pArrayHelp :: [AST] -> Parser AST
pArrayHelp revEntries =
  P.oneOf ArrayMore
    [ do  P.word1 0x2C {-,-} ArrayMore
          spaces
          entry <- pValue
          spaces
          pArrayHelp (entry:revEntries)
    ,
      do  P.word1 0x5D {-]-} ArrayEnd
          return (Array (List.reverse revEntries))
    ]



-- STRING


pString :: Parser ByteString.ByteString
pString =
  P.Parser $ \(P.State src pos end indent row col) cok _ cerr eerr ->
    if pos < end && P.unsafeIndex pos == 0x22 {-"-} then
      let
        !pos1 = plusPtr pos 1
        !col1 = col + 1

        (# status, newPos, newRow, newCol #) =
          pStringHelp pos1 end row col1
      in
      case status of
        GoodString ->
          let !off = minusPtr pos1 (unsafeForeignPtrToPtr src)
              !len = minusPtr newPos pos1 - 1
              !bts = ByteString.PS src off len
              !newState = P.State src newPos end indent newRow newCol
          in
          cok bts newState

        BadString problem ->
          cerr newRow newCol (StringProblem problem)

    else
      eerr row col StringStart


data StringStatus
  = GoodString
  | BadString StringProblem


pStringHelp :: Ptr Word8 -> Ptr Word8 -> Word16 -> Word16 -> (# StringStatus, Ptr Word8, Word16, Word16 #)
pStringHelp pos end row col =
  if pos >= end then
    (# BadString BadStringEnd, pos, row, col #)
  else
    case P.unsafeIndex pos of
      0x22 {-"-} ->
        (# GoodString, plusPtr pos 1, row, col + 1 #)

      0x0A {-\n-} ->
        (# BadString BadStringEnd, pos, row, col #)

      0x5C {-\-} ->
        let !pos1 = plusPtr pos 1 in
        if pos1 >= end then
          (# BadString BadStringEnd, pos1, row + 1, col #)

        else
          case P.unsafeIndex pos1 of
            0x22 {-"-} -> pStringHelp (plusPtr pos 2) end row (col + 2)
            0x5C {-\-} -> pStringHelp (plusPtr pos 2) end row (col + 2)
            0x2F {-/-} -> pStringHelp (plusPtr pos 2) end row (col + 2)
            0x62 {-b-} -> pStringHelp (plusPtr pos 2) end row (col + 2)
            0x66 {-f-} -> pStringHelp (plusPtr pos 2) end row (col + 2)
            0x6E {-n-} -> pStringHelp (plusPtr pos 2) end row (col + 2)
            0x72 {-r-} -> pStringHelp (plusPtr pos 2) end row (col + 2)
            0x74 {-t-} -> pStringHelp (plusPtr pos 2) end row (col + 2)
            0x75 {-u-} ->
              let !pos6 = plusPtr pos 6 in
              if pos6 <= end
                && isHex (P.unsafeIndex (plusPtr pos 2))
                && isHex (P.unsafeIndex (plusPtr pos 3))
                && isHex (P.unsafeIndex (plusPtr pos 4))
                && isHex (P.unsafeIndex (plusPtr pos 5))
              then
                pStringHelp pos6 end row (col + 6)
              else
                (# BadString BadStringEscapeHex, pos, row, col #)

            _ ->
              (# BadString BadStringEscapeChar, pos, row, col #)

      word ->
        if word < 0x20 then
          (# BadString BadStringControlChar, pos, row, col #)

        else
          let !newPos = plusPtr pos (P.getCharWidth pos end word) in
          pStringHelp newPos end row (col + 1)


isHex :: Word8 -> Bool
isHex word =
     0x30 {-0-} <= word && word <= 0x39 {-9-}
  || 0x61 {-a-} <= word && word <= 0x66 {-f-}
  || 0x41 {-A-} <= word && word <= 0x46 {-F-}



-- SPACES


spaces :: Parser ()
spaces =
  P.Parser $ \state@(P.State src pos end indent row col) cok eok _ _ ->
    let (# newPos, newRow, newCol #) = eatSpaces pos end row col in
    if pos == newPos then
      eok () state

    else
      let !newState = P.State src newPos end indent newRow newCol in
      cok () newState


eatSpaces :: Ptr Word8 -> Ptr Word8 -> Word16 -> Word16 -> (# Ptr Word8, Word16, Word16 #)
eatSpaces pos end row col =
  if pos >= end then
    (# pos, row, col #)

  else
    case P.unsafeIndex pos of
      0x20 {-  -} -> eatSpaces (plusPtr pos 1) end row (col + 1)
      0x09 {-\t-} -> eatSpaces (plusPtr pos 1) end row (col + 1)
      0x0A {-\n-} -> eatSpaces (plusPtr pos 1) end (row + 1) 1
      0x0D {-\r-} -> eatSpaces (plusPtr pos 1) end row col
      _ ->
        (# pos, row, col #)



-- NUMBERS


pNumber :: (Int -> Int) -> (Float -> Float) -> Parser AST
pNumber signInt signFloat =
  P.Parser $ \(P.State src pos end indent row col) cok _ cerr eerr ->
    if pos >= end then
      eerr row col NumberStart

    else
      let !word = P.unsafeIndex pos in
      if not (isDecimalDigit word) then
        eerr row col NumberStart

      else
        let
          outcome =
            let !pos1 = plusPtr pos 1 in
            if word == 0x30 {-0-} then
              chompZero pos1 end
            else
              chompInt pos1 end (toInt word)
        in
        case outcome of
          BadOutcome newPos problem ->
            let !newCol = col + fromIntegral (minusPtr newPos pos) in
            cerr row newCol (NumberProblem problem)

          OkInt newPos n ->
            let !newCol = col + fromIntegral (minusPtr newPos pos)
                !integer = Int (signInt n)
                !newState = P.State src newPos end indent row newCol
            in
            cok integer newState

          OkFloat newPos n ->
            let !newCol = col + fromIntegral (minusPtr newPos pos)
                !float = Float (signFloat n)
                !newState = P.State src newPos end indent row newCol
            in
            cok float newState



-- CHOMP OUTCOME


data Outcome
  = BadOutcome (Ptr Word8) NumberProblem
  | OkInt (Ptr Word8) Int
  | OkFloat (Ptr Word8) Float



-- CHOMP INT


chompInt :: Ptr Word8 -> Ptr Word8 -> Int -> Outcome
chompInt !pos end !n =
  if pos >= end then
    OkInt pos n

  else
    let !word = P.unsafeIndex pos in
    if isDecimalDigit word then
      let !pos1 = plusPtr pos 1 in
      chompInt pos1 end (10 * n + toInt word)

    else if word == 0x2E {-.-} then
      let !pos1 = plusPtr pos 1 in
      chompFraction pos1 end n

    else if word == 0x65 {-e-} || word == 0x45 {-E-} then
      chompExponent (plusPtr pos 1) end (fromIntegral n)

    else if isDirtyEnd pos end word then
      BadOutcome pos NumberEnd

    else
      OkInt pos n



-- CHOMP FRACTION


chompFraction :: Ptr Word8 -> Ptr Word8 -> Int -> Outcome
chompFraction pos end !n =
  if pos >= end then
    BadOutcome pos (NumberDot n)

  else
    let !word1 = P.unsafeIndex pos in
    if isDecimalDigit word1 then
      let !fraction = 1 / 10 * toFloat word1
          !n' = fromIntegral n + fraction
          !pos1 = plusPtr pos 1
      in
      chompFractionHelp pos1 end (-2) n'

  else
    BadOutcome pos (NumberDot n)


chompFractionHelp :: Ptr Word8 -> Ptr Word8 -> Float -> Float -> Outcome
chompFractionHelp pos end !power !n =
  if pos >= end then
    OkFloat pos n

  else
    let !word = P.unsafeIndex pos in
    if isDecimalDigit word then
      let !fraction = (10 ** power) * toFloat word
          !n' = n + fraction
      in
      chompFractionHelp (plusPtr pos 1) end (power - 1) n'

    else if word == 0x65 {-e-} || word == 0x45 {-E-} then
      chompExponent (plusPtr pos 1) end n

    else if isDirtyEnd pos end word then
      BadOutcome pos NumberEnd

    else
      OkFloat pos n



-- CHOMP EXPONENT


chompExponent :: Ptr Word8 -> Ptr Word8 -> Float -> Outcome
chompExponent pos end n =
  if pos >= end then
    BadOutcome pos NumberEnd

  else
    let !word = P.unsafeIndex pos in
    if isDecimalDigit word then
      let !exponent = toInt word in
      chompExponentHelp (plusPtr pos 1) end exponent n

    else if word == 0x2B {- + -} then
      let !pos1 = plusPtr pos 1
          !word1 = P.unsafeIndex pos1
      in
      if pos1 < end && isDecimalDigit word1 then
        let !exponent = toInt word1
            !pos2 = plusPtr pos 2
        in
        chompExponentHelp pos2 end exponent n

      else
        BadOutcome pos NumberEnd

    else if word == 0x2D {- - -} then
      let !pos1 = plusPtr pos 1
          !word1 = P.unsafeIndex pos1
      in
      if pos1 < end && isDecimalDigit word1 then
        let !exponent = toInt word1
            !pos2 = plusPtr pos 2
        in
        chompExponentHelp pos2 end (negate exponent) n

      else
        BadOutcome pos NumberEnd

    else
      BadOutcome pos NumberEnd


chompExponentHelp :: Ptr Word8 -> Ptr Word8 -> Int -> Float -> Outcome
chompExponentHelp pos end exponent n =
  if pos >= end then
    OkFloat pos (n * 10^exponent)

  else
    let !word = P.unsafeIndex pos in
    if isDecimalDigit word then
      let !exponent' = 10 * exponent + toInt word
          !pos1 = plusPtr pos 1
      in
      chompExponentHelp pos1 end exponent' n

    else
      OkFloat pos (n * 10^exponent)



-- CHOMP ZERO


chompZero :: Ptr Word8 -> Ptr Word8 -> Outcome
chompZero pos end =
  if pos >= end then
    OkInt pos 0
  else
    let !word = P.unsafeIndex pos in
    if word == 0x2E {-.-} then
      let !pos1 = plusPtr pos 1 in
      chompFraction pos1 end 0

    else if isDecimalDigit word then
      BadOutcome pos NumberNoLeadingZero

    else if isDirtyEnd pos end word then
      BadOutcome pos NumberEnd

    else
      OkInt pos 0



-- HELPERS


{-# INLINE isDecimalDigit #-}
isDecimalDigit :: Word8 -> Bool
isDecimalDigit word =
  word <= 0x39 {-9-} && word >= 0x30 {-0-}


isDirtyEnd :: Ptr Word8 -> Ptr Word8 -> Word8 -> Bool
isDirtyEnd pos end word =
  V.getInnerWidthHelp pos end word > 0


toInt :: Word8 -> Int
toInt word =
  fromIntegral (word - 0x30 {-0-})


toFloat :: Word8 -> Float
toFloat word =
  fromIntegral (word - 0x30 {-0-})

