{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind -fno-warn-name-shadowing #-}
{-# LANGUAGE BangPatterns, MagicHash, Rank2Types, UnboxedTuples #-}
module Parser.Primitives
  ( fromString
  , Parser(..), State(..), Pos, End, Row, Col
  , oneOf, oneOfWithFallback
  , inContext, specialize
  , getPosition, getCol, addLocation, addEnd
  , word1, word2
  , symbol, k4, k5
  , unsafeIndex, isWord, getCharWidth
  , chompInnerChars
  , getUpperWidth
  , getInnerWidth
  , getInnerWidthHelp
  )
  where


import qualified Data.Char as Char
import qualified Data.Text.Internal as T
import qualified Data.Text.Array as T
import GHC.Exts (Char(C#), Int#, (+#), (-#), chr#, uncheckedIShiftL#, word2Int#)
import GHC.Prim (ByteArray#, indexWord8Array#)
import GHC.Types (Int(I#))
import GHC.Word (Word8(W8#), Word16)
import qualified Parser.Reporting as R
import Prelude hiding (length)

import qualified Result as R
import qualified String



-- PARSER


newtype Parser x a =
  Parser (
    forall b.
      State
      -> (a -> State -> b)                       -- consumed ok
      -> (a -> State -> b)                       -- empty ok
      -> (Row -> Col -> (Row -> Col -> x) -> b)  -- consumed err
      -> (Row -> Col -> (Row -> Col -> x) -> b)  -- empty err
      -> b
  )


data State = -- TODO try taking some out to avoid allocation?
  State
    { _src :: ByteArray#
    , _pos :: {-# UNPACK #-} !Pos
    , _end :: {-# UNPACK #-} !End
    , _row :: {-# UNPACK #-} !Row
    , _col :: {-# UNPACK #-} !Col
    }


type Pos = Int
type End = Int

type Row = Word16
type Col = Word16



-- FUNCTOR


instance Functor (Parser x) where
  {-# INLINE fmap #-}
  fmap f (Parser parser) =
    Parser $ \state cok eok cerr eerr ->
      let
        cok' a s = cok (f a) s
        eok' a s = eok (f a) s
      in
      parser state cok' eok' cerr eerr



-- APPLICATIVE


instance Applicative (Parser x) where
  {-# INLINE pure #-}
  pure = return

  {-# INLINE (<*>) #-}
  (<*>) (Parser parserFunc) (Parser parserArg) =
    Parser $ \state cok eok cerr eerr ->
      let
        cokF func s1 =
          let
            cokA arg s2 = cok (func arg) s2
          in
          parserArg s1 cokA cokA cerr cerr

        eokF func s1 =
          let
            cokA arg s2 = cok (func arg) s2
            eokA arg s2 = eok (func arg) s2
          in
          parserArg s1 cokA eokA cerr eerr
      in
      parserFunc state cokF eokF cerr eerr



-- ONE OF


{-# INLINE oneOf #-}
oneOf :: (Row -> Col -> x) -> [Parser x a] -> Parser x a
oneOf toError parsers =
  Parser $ \state cok eok cerr eerr ->
    oneOfHelp state cok eok cerr eerr toError parsers


oneOfHelp
  :: State
  -> (a -> State -> b)
  -> (a -> State -> b)
  -> (Row -> Col -> (Row -> Col -> x) -> b)
  -> (Row -> Col -> (Row -> Col -> x) -> b)
  -> (Row -> Col -> x)
  -> [Parser x a]
  -> b
oneOfHelp state cok eok cerr eerr toError parsers =
  case parsers of
    Parser parser : parsers ->
      let
        eerr' _ _ _ =
          oneOfHelp state cok eok cerr eerr toError parsers
      in
      parser state cok eok cerr eerr'

    [] ->
      let
        (State _ _ _ row col) = state
      in
      eerr row col toError



-- ONE OF WITH FALLBACK


{-# INLINE oneOfWithFallback #-}
oneOfWithFallback :: [Parser x a] -> a -> Parser x a -- TODO is this function okay? Worried about allocation/laziness with fallback values.
oneOfWithFallback parsers fallback =
  Parser $ \state cok eok cerr _ ->
    oowfHelp state cok eok cerr parsers fallback


oowfHelp
  :: State
  -> (a -> State -> b)
  -> (a -> State -> b)
  -> (Row -> Col -> (Row -> Col -> x) -> b)
  -> [Parser x a]
  -> a
  -> b
oowfHelp state cok eok cerr parsers fallback =
  case parsers of
    [] ->
      eok fallback state

    Parser parser : parsers ->
      let
        eerr' _ _ _ =
          oowfHelp state cok eok cerr parsers fallback
      in
      parser state cok eok cerr eerr'



-- MONAD


instance Monad (Parser x) where
  {-# INLINE return #-}
  return value =
    Parser $ \state _ eok _ _ ->
      eok value state

  {-# INLINE (>>=) #-}
  (Parser parserA) >>= callback =
    Parser $ \state cok eok cerr eerr ->
      let
        cok' a s =
          case callback a of
            Parser parserB -> parserB s cok cok cerr cerr

        eok' a s =
          case callback a of
            Parser parserB -> parserB s cok eok cerr eerr
      in
      parserA state cok' eok' cerr eerr



-- FROM STRING


fromString :: Parser x a -> (Row -> Col -> x) -> String.String -> R.Result x a
fromString (Parser parser) toBadEnd string =
  let
    !(T.Text (T.Array src) pos length) = String.toTextUtf8 string
    toOk' = toOk toBadEnd
  in
  parser (State src pos (pos + length) 1 1) toOk' toOk' toErr toErr


toOk :: (Row -> Col -> x) -> a -> State -> R.Result x a
toOk toBadEnd !a (State _ pos end row col) =
  if pos == end
  then R.Ok a
  else R.Err (toBadEnd row col)


toErr :: Row -> Col -> (Row -> Col -> x) -> R.Result x a
toErr row col toError =
  R.Err (toError row col)



-- POSITION


getCol :: Parser x Word16
getCol =
  Parser $ \state@(State _ _ _ _ col) _ eok _ _ ->
    eok col state


{-# INLINE getPosition #-}
getPosition :: Parser x R.Position
getPosition =
  Parser $ \state@(State _ _ _ row col) _ eok _ _ ->
    eok (R.Position row col) state


addLocation :: Parser x a -> Parser x (R.Located a)
addLocation (Parser parser) =
  Parser $ \state@(State _ _ _ sr sc) cok eok cerr eerr ->
    let
      cok' a s@(State _ _ _ er ec) = cok (R.At (R.Region (R.Position sr sc) (R.Position er ec)) a) s
      eok' a s@(State _ _ _ er ec) = eok (R.At (R.Region (R.Position sr sc) (R.Position er ec)) a) s
    in
    parser state cok' eok' cerr eerr


addEnd :: R.Position -> a -> Parser x (R.Located a)
addEnd start value =
  Parser $ \state@(State _ _ _ row col) _ eok _ _ ->
    eok (R.at start (R.Position row col) value) state



-- CONTEXT


inContext :: (x -> Row -> Col -> y) -> Parser y start -> Parser x a -> Parser y a
inContext addContext (Parser parserStart) (Parser parserA) =
  Parser $ \state@(State _ _ _ row col) cok eok cerr eerr ->
    let
      cerrA r c tx = cerr row col (addContext (tx r c))
      eerrA r c tx = eerr row col (addContext (tx r c))

      cokS _ s = parserA s cok cok cerrA cerrA
      eokS _ s = parserA s cok eok cerrA eerrA
    in
    parserStart state cokS eokS cerr eerr


specialize :: (x -> Row -> Col -> y) -> Parser x a -> Parser y a
specialize addContext (Parser parser) =
  Parser $ \state@(State _ _ _ row col) cok eok cerr eerr ->
    let
      cerr' r c tx = cerr row col (addContext (tx r c))
      eerr' r c tx = eerr row col (addContext (tx r c))
    in
    parser state cok eok cerr' eerr'



-- SYMBOLS


word1 :: Word8 -> (Row -> Col -> x) -> Parser x ()
word1 word toError =
  Parser $ \(State src pos end row col) cok _ _ eerr ->
    if pos < end && unsafeIndex src pos == word then
      let !newState = State src (pos + 1) end row (col + 1) in
      cok () newState
    else
      eerr row col toError


word2 :: Word8 -> Word8 -> (Row -> Col -> x) -> Parser x ()
word2 w1 w2 toError =
  Parser $ \(State src pos end row col) cok _ _ eerr ->
    let
      !pos1 = pos + 1
    in
    if pos1 < end && unsafeIndex src pos == w1 && unsafeIndex src pos1 == w2 then
      let !newState = State src (pos + 2) end row (col + 2) in
      cok () newState
    else
      eerr row col toError


symbol :: Word8 -> (Row -> Col -> x) -> Parser x ()
symbol w1 toError =
  Parser $ \(State src pos end row col) cok _ _ eerr ->
    let !pos1 = pos + 1 in
    if pos1 <= end && unsafeIndex src pos == w1
    then
      let !s = State src pos1 end row (col + 1) in cok () s
    else
      eerr row col toError


k4 :: Word8 -> Word8 -> Word8 -> Word8 -> (Row -> Col -> x) -> Parser x ()
k4 w1 w2 w3 w4 toError =
  Parser $ \(State src pos end row col) cok _ _ eerr ->
    let !pos4 = pos + 4 in
    if pos4 <= end
      && unsafeIndex src (pos    ) == w1
      && unsafeIndex src (pos + 1) == w2
      && unsafeIndex src (pos + 2) == w3
      && unsafeIndex src (pos + 3) == w4
      && getInnerWidth src pos4 end == 0
    then
      let !s = State src pos4 end row (col + 4) in cok () s
    else
      eerr row col toError


k5 :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> (Row -> Col -> x) -> Parser x ()
k5 w1 w2 w3 w4 w5 toError =
  Parser $ \(State src pos end row col) cok _ _ eerr ->
    let !pos5 = pos + 5 in
    if pos5 <= end
      && unsafeIndex src (pos    ) == w1
      && unsafeIndex src (pos + 1) == w2
      && unsafeIndex src (pos + 2) == w3
      && unsafeIndex src (pos + 3) == w4
      && unsafeIndex src (pos + 4) == w5
      && getInnerWidth src pos5 end == 0
    then
      let !s = State src pos5 end row (col + 5) in cok () s
    else
      eerr row col toError



-- LOW-LEVEL CHECKS


{-# INLINE unsafeIndex #-}
unsafeIndex :: ByteArray# -> Pos -> Word8
unsafeIndex src (I# pos) =
  W8# (indexWord8Array# src pos)


{-# INLINE isWord #-}
isWord :: ByteArray# -> Pos -> End -> Word8 -> Bool
isWord src pos end word =
  pos < end && unsafeIndex src pos == word


getCharWidth :: Word8 -> Int
getCharWidth word
  | word < 0x80 = 1
  | word < 0xc0 = error "Need UTF-8 encoded input. Ran into unrecognized bits."
  | word < 0xe0 = 2
  | word < 0xf0 = 3
  | word < 0xf8 = 4
  | True        = error "Need UTF-8 encoded input. Ran into unrecognized bits."



-- UPPER CHARS


{-# INLINE getUpperWidth #-}
getUpperWidth :: ByteArray# -> Pos -> End -> Int
getUpperWidth src pos end =
  if pos < end then
    getUpperWidthHelp src pos end (unsafeIndex src pos)
  else
    0


{-# INLINE getUpperWidthHelp #-}
getUpperWidthHelp :: ByteArray# -> Pos -> End -> Word8 -> Int
getUpperWidthHelp src pos _ word
  | 0x41 {- A -} <= word && word <= 0x5A {- Z -} = 1
  | word < 0xc0 = 0
  | word < 0xe0 = if Char.isUpper (chr2 src pos word) then 2 else 0
  | word < 0xf0 = if Char.isUpper (chr3 src pos word) then 3 else 0
  | word < 0xf8 = if Char.isUpper (chr4 src pos word) then 4 else 0
  | True        = 0



-- INNER CHARS


chompInnerChars :: ByteArray# -> Pos -> End -> Col -> (# Pos, Col #)
chompInnerChars src !pos end !col =
  let !width = getInnerWidth src pos end in
  if width == 0 then
    (# pos, col #)
  else
    chompInnerChars src (pos + width) end (col + 1)


getInnerWidth :: ByteArray# -> Pos -> End -> Int
getInnerWidth src pos end =
  if pos < end then
    getInnerWidthHelp src pos end (unsafeIndex src pos)
  else
    0


{-# INLINE getInnerWidthHelp #-}
getInnerWidthHelp :: ByteArray# -> Pos -> End -> Word8 -> Int
getInnerWidthHelp src pos _ word
  | 0x61 {- a -} <= word && word <= 0x7A {- z -} = 1
  | 0x41 {- A -} <= word && word <= 0x5A {- Z -} = 1
  | 0x30 {- 0 -} <= word && word <= 0x39 {- 9 -} = 1
  | word == 0x5F {- _ -} = 1
  | word < 0xc0 = 0
  | word < 0xe0 = if Char.isAlpha (chr2 src pos word) then 2 else 0
  | word < 0xf0 = if Char.isAlpha (chr3 src pos word) then 3 else 0
  | word < 0xf8 = if Char.isAlpha (chr4 src pos word) then 4 else 0
  | True        = 0



-- EXTRACT CHARACTERS


{-# INLINE chr2 #-}
chr2 :: ByteArray# -> Pos -> Word8 -> Char
chr2 src pos firstWord =
  let
    !i1# = unpack firstWord
    !i2# = unpack (unsafeIndex src (pos + 1))
    !c1# = uncheckedIShiftL# (i1# -# 0xC0#) 6#
    !c2# = i2# -# 0x80#
  in
  C# (chr# (c1# +# c2#))


{-# INLINE chr3 #-}
chr3 :: ByteArray# -> Pos -> Word8 -> Char
chr3 src pos firstWord =
  let
    !i1# = unpack firstWord
    !i2# = unpack (unsafeIndex src (pos + 1))
    !i3# = unpack (unsafeIndex src (pos + 2))
    !c1# = uncheckedIShiftL# (i1# -# 0xE0#) 12#
    !c2# = uncheckedIShiftL# (i2# -# 0x80#) 6#
    !c3# = i3# -# 0x80#
  in
  C# (chr# (c1# +# c2# +# c3#))


{-# INLINE chr4 #-}
chr4 :: ByteArray# -> Pos -> Word8 -> Char
chr4 src pos firstWord =
  let
    !i1# = unpack firstWord
    !i2# = unpack (unsafeIndex src (pos + 1))
    !i3# = unpack (unsafeIndex src (pos + 2))
    !i4# = unpack (unsafeIndex src (pos + 3))
    !c1# = uncheckedIShiftL# (i1# -# 0xF0#) 18#
    !c2# = uncheckedIShiftL# (i2# -# 0x80#) 12#
    !c3# = uncheckedIShiftL# (i3# -# 0x80#) 6#
    !c4# = i4# -# 0x80#
  in
  C# (chr# (c1# +# c2# +# c3# +# c4#))


unpack :: Word8 -> Int#
unpack (W8# word#) =
  word2Int# word#

