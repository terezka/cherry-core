
{-|

Module      : String
Description : A built-in representation for efficient string manipulation. `String` values are *not* lists of characters.
License     : BSD 3
Maintainer  : terezasokol@gmail.com
Stability   : experimental
Portability : POSIX

-}

module String
  ( -- * String
    String, isEmpty, length, reverse, repeat, replace

    -- * Building and Splitting
  , append, concat, split, join, words, lines

    -- * Get Substrings
  , slice, left, right, dropLeft, dropRight

    -- * Check for Substrings
  , contains, startsWith, endsWith, indexes, indices

    -- * Int Conversions
  , toInt, fromInt

    -- * Float Conversions
  , toFloat, fromFloat

    -- * Char Conversions
  , fromChar, cons, uncons

    -- * List Conversions
  , toList, fromList

    -- * Formatting
    -- Cosmetic operations such as padding with extra characters or trimming whitespace.
  , toUpper, toLower, pad, padLeft, padRight, trim, trimLeft, trimRight

    -- * Higher-Order Functions
  , map, filter, foldl, foldr, any, all

    -- * Conversions to Haskell Types
  , toBuilder, toTextUtf8, fromTextUtf8
  , fromByteString, toByteString, fromLazyByteString, toLazyByteString
  )
where

import Prelude (Bool, Float, Int, (+), (<), Show, show)
import Char (Char)
import List (List)
import Maybe (Maybe(..))
import qualified Prelude
import qualified Data.ByteString.Builder as HB
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.String as HS
import qualified Data.Text as HT
import qualified Data.Text.Encoding as HTE
import qualified Data.Text.Internal.Search as HTIS
import qualified Data.Maybe as HM
import qualified Text.Read as HTR
import qualified List as List



{-| A `String` is a chunk of text:

  >  "Hello!"
  >  "How are you?"
  >  "🙈🙉🙊"
  >
  >  -- strings with escape characters
  >  "this\n\t\"that\""
  >  "\u{1F648}\u{1F649}\u{1F64A}" -- "🙈🙉🙊"
  >
  >  -- multiline strings
  >  """Triple double quotes let you
  >  create "multiline strings" which
  >  can have unescaped quotes and newlines.
  >  """

A `String` can represent any sequence of [unicode characters](https://en.wikipedia.org/wiki/Unicode). You can use
the unicode escapes from `\u{0000}` to `\u{10FFFF}` to represent characters
by their code point. You can also include the unicode characters directly.
Using the escapes can be better if you need one of the many whitespace
characters with different widths.

-}

newtype String =
  String HT.Text
  deriving (Prelude.Eq, Prelude.Ord)


instance HS.IsString String where
  fromString = fromList

instance Show String where
  show (String s) = show s


{-| Determine if a string is empty.

  >  isEmpty "" == True
  >  isEmpty "the world" == False
-}
isEmpty :: String -> Bool
isEmpty (String s) =
  HT.null s


{-|  Get the length of a string.

  >  length "innumerable" == 11
  >  length "" == 0
-}
length :: String -> Int
length (String s) =
  HT.length s


{-| Reverse a string.

  >  reverse "stressed" == "desserts"
-}
reverse :: String -> String
reverse (String s) =
  String (HT.reverse s)


{-| Repeat a string *n* times.

  >  repeat 3 "ha" == "hahaha"
-}
repeat :: Int -> String -> String
repeat n (String s) =
  String (HT.replicate n s)


{-| Replace all occurrences of some substring.

  >  replace "." "-" "Json.Decode.succeed" == "Json-Decode-succeed"
  >  replace "," "/" "a,b,c,d,e"           == "a/b/c/d/e"
-}
replace :: String -> String -> String -> String
replace (String before) (String after) (String string) =
  String (HT.replace before after string)



-- BUILDING AND SPLITTING


{-| Append two strings. You can also use [the `(++)` operator](Basics#++) to do this.

  >  append "butter" "fly" == "butterfly"
-}
append :: String -> String -> String
append (String a) (String b) =
  String (HT.append a b)


{-| Concatenate many strings into one.

  >  concat ["never","the","less"] == "nevertheless"
-}
concat :: List String -> String
concat strings =
  String (HT.concat (List.map (\(String s) -> s) strings))


{-| Split a string using a given separator.

  >  split "," "cat,dog,cow"        == ["cat","dog","cow"]
  >  split "/" "home/evan/Desktop/" == ["home","evan","Desktop", ""]
-}
split :: String -> String -> List String
split (String sep) (String string) =
  if HT.null sep
  then List.map fromChar (HT.unpack string)
  else List.map String (HT.splitOn sep string)
  -- docs say that HT.splitOn will crash on empty strings
  -- https://hackage.haskell.org/package/text-utf8-1.2.3.0/docs/Data-Text.html#v:splitOn


{-| Put many strings together with a given separator.

  >  join "a" ["H","w","ii","n"]        == "Hawaiian"
  >  join " " ["cat","dog","cow"]       == "cat dog cow"
  >  join "/" ["home","evan","Desktop"] == "home/evan/Desktop"
-}
join :: String -> List String -> String
join (String sep) strings =
  String (HT.intercalate sep (List.map (\(String s) -> s) strings))


{-| Break a string into words, splitting on chunks of whitespace.

  >  words "How are \t you? \n Good?" == ["How","are","you?","Good?"]
-}
words :: String -> List String
words (String s) =
  List.map String (HT.words s)


{-| Break a string into lines, splitting on newlines.

  >  lines "How are you?\nGood?" == ["How are you?", "Good?"]
-}
lines :: String -> List String
lines (String s) =
  List.map String (HT.lines s)



-- SUBSTRINGS


{-| Take a substring given a start and end index. Negative indexes
 are taken starting from the *end* of the list.

  >  slice  7  9 "snakes on a plane!" == "on"
  >  slice  0  6 "snakes on a plane!" == "snakes"
  >  slice  0 -7 "snakes on a plane!" == "snakes on a"
  >  slice -6 -1 "snakes on a plane!" == "plane"
-}
slice :: Int -> Int -> String -> String
slice start end (String str) =
  let
    len = HT.length str

    normalize value =
      clamp 0 len (if value < 0 then len + value else value)

    lo = normalize start
    hi = normalize end
  in
  if lo < hi
  then String (HT.drop lo (HT.take hi str))
  else String HT.empty


clamp :: Int -> Int -> Int -> Int
clamp lo hi n =
  if n < lo then
    lo
  else if hi < n then
    hi
  else
    n


{-| Take *n* characters from the left side of a string.

  >  left 2 "Mulder" == "Mu"
-}
left :: Int -> String -> String
left n (String s) =
  String (HT.take n s)


{-| Take *n* characters from the right side of a string.

  >  right 2 "Scully" == "ly"
-}
right :: Int -> String -> String
right n (String s) =
  String (HT.takeEnd n s)


{-| Drop *n* characters from the left side of a string.

  >  dropLeft 2 "The Lone Gunmen" == "e Lone Gunmen"
-}
dropLeft :: Int -> String -> String
dropLeft n (String s) =
  String (HT.drop n s)


{-| Drop *n* characters from the right side of a string.

  >  dropRight 2 "Cigarette Smoking Man" == "Cigarette Smoking M"
-}
dropRight :: Int -> String -> String
dropRight n (String s) =
  String (HT.dropEnd n s)



-- DETECT SUBSTRINGS


{-| See if the second string contains the first one.

  >  contains "the" "theory" == True
  >  contains "hat" "theory" == False
  >  contains "THE" "theory" == False
-}
contains :: String -> String -> Bool
contains (String sub) (String string) =
  HT.isInfixOf sub string


{-| See if the second string starts with the first one.

  >  startsWith "the" "theory" == True
  >  startsWith "ory" "theory" == False
-}
startsWith :: String -> String -> Bool
startsWith (String start) (String string) =
  HT.isPrefixOf start string


{-| See if the second string ends with the first one.

  >  endsWith "the" "theory" == False
  >  endsWith "ory" "theory" == True
-}
endsWith :: String -> String -> Bool
endsWith (String end) (String string) =
  HT.isSuffixOf end string


{-| Get all of the indexes for a substring in another string.

  >  indexes "i" "Mississippi"   == [1,4,7,10]
  >  indexes "ss" "Mississippi"  == [2,5]
  >  indexes "needle" "haystack" == []
-}
indexes :: String -> String -> List Int
indexes (String sub) (String str) =
  HTIS.indices sub str


{-| Alias for `indexes`.
-}
indices :: String -> String -> List Int
indices =
  indexes



-- FORMATTING


{-| Convert a string to all upper case. Useful for case-insensitive comparisons
 and VIRTUAL YELLING.

  >  toUpper "skinner" == "SKINNER"
-}
toUpper :: String -> String
toUpper (String s) =
  String (HT.toUpper s)


{-| Convert a string to all lower case. Useful for case-insensitive comparisons.

  >  toLower "X-FILES" == "x-files"
-}
toLower :: String -> String
toLower (String s) =
  String (HT.toLower s)


{-| Pad a string on both sides until it has a given length.

  >  pad 5 ' ' "1"   == "  1  "
  >  pad 5 ' ' "11"  == "  11 "
  >  pad 5 ' ' "121" == " 121 "
-}
pad :: Int -> Char -> String -> String
pad n char (String str) =
  String (HT.center n char str)


{-| Pad a string on the left until it has a given length.

  >  padLeft 5 '.' "1"   == "....1"
  >  padLeft 5 '.' "11"  == "...11"
  >  padLeft 5 '.' "121" == "..121"
-}
padLeft :: Int -> Char -> String -> String
padLeft n char (String str) =
  String (HT.justifyRight n char str)


{-| Pad a string on the right until it has a given length.

  >  padRight 5 '.' "1"   == "1...."
  >  padRight 5 '.' "11"  == "11..."
  >  padRight 5 '.' "121" == "121.."
-}
padRight :: Int -> Char -> String -> String
padRight n char (String str) =
  String (HT.justifyLeft n char str)


{-| Get rid of whitespace on both sides of a string.

  >  trim "  hats  \n" == "hats"
-}
trim :: String -> String
trim (String str) =
  String (HT.strip str)


{-| Get rid of whitespace on the left of a string.

  >  trimLeft "  hats  \n" == "hats  \n"
-}
trimLeft :: String -> String
trimLeft (String str) =
  String (HT.stripStart str)


{-| Get rid of whitespace on the right of a string.

  >  trimRight "  hats  \n" == "  hats"
-}
trimRight :: String -> String
trimRight (String str) =
  String (HT.stripEnd str)



-- INT CONVERSIONS


{-| Try to convert a string into an int, failing on improperly formatted strings.

  >  String.toInt "123" == Just 123
  >  String.toInt "-42" == Just -42
  >  String.toInt "3.1" == Nothing
  >  String.toInt "31a" == Nothing

If you are extracting a number from some raw user input, you will typically
want to use [`Maybe.withDefault`](Maybe#withDefault) to handle bad data:

  >  Maybe.withDefault 0 (String.toInt "42") == 42
  >  Maybe.withDefault 0 (String.toInt "ab") == 0
-}
toInt :: String -> Maybe Int
toInt str =
  case toList str of
    '+':chars -> safeRead chars
    chars     -> safeRead chars


safeRead :: (Prelude.Read a) => List Char -> Maybe a
safeRead chars =
  case HTR.readMaybe chars of
    HM.Just a  -> Just a
    HM.Nothing -> Nothing


{-| Convert an `Int` to a `String`.

  >  String.fromInt 123 == "123"
  >  String.fromInt -42 == "-42"

-}
fromInt :: Int -> String
fromInt n =
  fromList (Prelude.show n)



-- FLOAT CONVERSIONS


{-| Try to convert a string into a float, failing on improperly formatted strings.

  >  String.toFloat "123" == Just 123.0
  >  String.toFloat "-42" == Just -42.0
  >  String.toFloat "3.1" == Just 3.1
  >  String.toFloat "31a" == Nothing

If you are extracting a number from some raw user input, you will typically
want to use [`Maybe.withDefault`](Maybe#withDefault) to handle bad data:

  >  Maybe.withDefault 0 (String.toFloat "42.5") == 42.5
  >  Maybe.withDefault 0 (String.toFloat "cats") == 0
-}
toFloat :: String -> Maybe Float
toFloat str =
  case toList str of
    '+':chars -> safeRead chars
    '.':chars -> safeRead ('0':'.':chars)
    chars     -> safeRead chars


{-| Convert a `Float` to a `String`.

  >  String.fromFloat 123 == "123"
  >  String.fromFloat -42 == "-42"
  >  String.fromFloat 3.9 == "3.9"
-}
fromFloat :: Float -> String
fromFloat n =
  fromList (Prelude.show n)



-- LIST CONVERSIONS


{-| Convert a String to a list of characters.

  >  toList "abc" == ['a','b','c']
  >  toList "🙈🙉🙊" == ['🙈','🙉','🙊']
-}
toList :: String -> List Char
toList (String str) =
  HT.unpack str


{-| Convert a list of characters into a String. Can be useful if you
 want to create a string primarily by consing, perhaps for decoding
 something.

  >  fromList ['a','b','c'] == "abc"
  >  fromList ['🙈','🙉','🙊'] == "🙈🙉🙊"
-}
fromList :: List Char -> String
fromList chars =
  String (HT.pack chars)



-- CHAR CONVERSIONS


{-| Create a String from a given character.

  >  fromChar 'a' == "a"
-}
fromChar :: Char -> String
fromChar char =
  String (HT.singleton char)


{-| Add a character to the beginning of a String.

  >  cons 'T' "he truth is out there" == "The truth is out there"
-}
cons :: Char -> String -> String
cons char (String str) =
  String (HT.cons char str)


{-| Split a non-empty String into its head and tail. This lets you
pattern match on strings exactly as you would with lists.

  >  uncons "abc" == Just ('a',"bc")
  >  uncons ""    == Nothing
-}
uncons :: String -> Maybe (Char, String)
uncons (String str) =
  case HT.uncons str of
    HM.Just (c,s) -> Just (c, String s)
    HM.Nothing    -> Nothing



-- HIGHER-ORDER FUNCTIONS


{-| Transform every character in a String

  >  map (\c -> if c == '/' then '.' else c) "a/b/c" == "a.b.c"
-}
map :: (Char -> Char) -> String -> String
map func (String str) =
  String (HT.map func str)


{-| Keep only the characters that pass the test.

  >  filter isDigit "R2-D2" == "22"
-}
filter :: (Char -> Bool) -> String -> String
filter isGood (String str) =
  String (HT.filter isGood str)


{-| Reduce a String from the left.

  >  foldl cons "" "time" == "emit"
-}
foldl :: (Char -> b -> b) -> b -> String -> b
foldl step state (String str) =
  HT.foldl' (Prelude.flip step) state str


{-| Reduce a String from the right.

  >  foldr cons "" "time" == "time"
-}
foldr :: (Char -> b -> b) -> b -> String -> b
foldr step state (String str) =
  HT.foldr step state str


{-| Determine whether *any* characters pass the test.

  >  any isDigit "90210" == True
  >  any isDigit "R2-D2" == True
  >  any isDigit "heart" == False
-}
any :: (Char -> Bool) -> String -> Bool
any isGood (String str) =
  HT.any isGood str


{-| Determine whether *all* characters pass the test.

  >  all isDigit "90210" == True
  >  all isDigit "R2-D2" == False
  >  all isDigit "heart" == False
-}
all :: (Char -> Bool) -> String -> Bool
all isGood (String str) =
  HT.all isGood str



-- CONVERSION TO HASKELL TYPES


{-| It is pretty common to use `Data.ByteString.Builder` when generating output
so this function is compatible with that system, and fast!
-}
toBuilder :: String -> HB.Builder
toBuilder (String str) =
  HTE.encodeUtf8Builder str


{-| -}
fromByteString :: B.ByteString -> String
fromByteString bs =
  fromList (B.unpack bs)


{-| -}
toByteString :: String -> B.ByteString
toByteString s =
  B.pack (toList s)


{-| -}
fromLazyByteString :: BL.ByteString -> String
fromLazyByteString bs =
  fromByteString (BL.toStrict bs)


{-| -}
toLazyByteString :: String -> BL.ByteString
toLazyByteString s =
  BL.fromStrict (B.pack (toList s))



{-| Convert to a `Text` value as defined in the `text-utf8` package.

You can do more conversions from there if needed.
-}
toTextUtf8 :: String -> HT.Text
toTextUtf8 (String str) =
  str


{-| Convert from a `Text` value as defined in the `text-utf8` package.
-}
fromTextUtf8 :: HT.Text -> String
fromTextUtf8 =
  String

