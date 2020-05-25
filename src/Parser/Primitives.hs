{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind -fno-warn-name-shadowing #-}
{-# LANGUAGE BangPatterns, MagicHash, Rank2Types, UnboxedTuples #-}
module Parser.Primitives
  ( fromString
  , Parser(..), State(..), Pos, End, Row, Col
  , oneOf, oneOfWithFallback
  , inContext, specialize
  , getPosition, getCol, addLocation, addEnd
  , getIndent, setIndent, withIndent, withBacksetIndent
  , word1, word2
  , unsafeIndex, isWord, getCharWidth
  )
  where


import qualified Data.Text.Internal as T
import qualified Data.Text.Array as T
import GHC.Prim (ByteArray#, indexWord8Array#)
import GHC.Types (Int(I#))
import GHC.Word (Word8(W8#), Word16)
import qualified Parser.Reporting as R
import Prelude hiding (length)

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
    , _indent :: {-# UNPACK #-} !Word16
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
        (State _ _ _ _ row col) = state
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


fromString :: Parser x a -> (Row -> Col -> x) -> String.String -> Either x a
fromString (Parser parser) toBadEnd string =
  let
    !(T.Text (T.Array src) pos length) = String.toTextUtf8 string
    toOk' = toOk toBadEnd
  in
  parser (State src pos (pos + length) 0 1 1) toOk' toOk' toErr toErr


toOk :: (Row -> Col -> x) -> a -> State -> Either x a
toOk toBadEnd !a (State _ pos end _ row col) =
  if pos == end
  then Right a
  else Left (toBadEnd row col)


toErr :: Row -> Col -> (Row -> Col -> x) -> Either x a
toErr row col toError =
  Left (toError row col)



-- POSITION


getCol :: Parser x Word16
getCol =
  Parser $ \state@(State _ _ _ _ _ col) _ eok _ _ ->
    eok col state


{-# INLINE getPosition #-}
getPosition :: Parser x R.Position
getPosition =
  Parser $ \state@(State _ _ _ _ row col) _ eok _ _ ->
    eok (R.Position row col) state


addLocation :: Parser x a -> Parser x (R.Located a)
addLocation (Parser parser) =
  Parser $ \state@(State _ _ _ _ sr sc) cok eok cerr eerr ->
    let
      cok' a s@(State _ _ _ _ er ec) = cok (R.At (R.Region (R.Position sr sc) (R.Position er ec)) a) s
      eok' a s@(State _ _ _ _ er ec) = eok (R.At (R.Region (R.Position sr sc) (R.Position er ec)) a) s
    in
    parser state cok' eok' cerr eerr


addEnd :: R.Position -> a -> Parser x (R.Located a)
addEnd start value =
  Parser $ \state@(State _ _ _ _ row col) _ eok _ _ ->
    eok (R.at start (R.Position row col) value) state



-- INDENT


getIndent :: Parser x Word16
getIndent =
  Parser $ \state@(State _ _ _ indent _ _) _ eok _ _ ->
    eok indent state


setIndent :: Word16 -> Parser x ()
setIndent indent =
  Parser $ \(State src pos end _ row col) _ eok _ _ ->
    let
      !newState = State src pos end indent row col
    in
    eok () newState


withIndent :: Parser x a -> Parser x a
withIndent (Parser parser) =
  Parser $ \(State src pos end oldIndent row col) cok eok cerr eerr ->
    let
      cok' a (State s p e _ r c) = cok a (State s p e oldIndent r c)
      eok' a (State s p e _ r c) = eok a (State s p e oldIndent r c)
    in
    parser (State src pos end col row col) cok' eok' cerr eerr


withBacksetIndent :: Word16 -> Parser x a -> Parser x a
withBacksetIndent backset (Parser parser) =
  Parser $ \(State src pos end oldIndent row col) cok eok cerr eerr ->
    let
      cok' a (State s p e _ r c) = cok a (State s p e oldIndent r c)
      eok' a (State s p e _ r c) = eok a (State s p e oldIndent r c)
    in
    parser (State src pos end (col - backset) row col) cok' eok' cerr eerr



-- CONTEXT


inContext :: (x -> Row -> Col -> y) -> Parser y start -> Parser x a -> Parser y a
inContext addContext (Parser parserStart) (Parser parserA) =
  Parser $ \state@(State _ _ _ _ row col) cok eok cerr eerr ->
    let
      cerrA r c tx = cerr row col (addContext (tx r c))
      eerrA r c tx = eerr row col (addContext (tx r c))

      cokS _ s = parserA s cok cok cerrA cerrA
      eokS _ s = parserA s cok eok cerrA eerrA
    in
    parserStart state cokS eokS cerr eerr


specialize :: (x -> Row -> Col -> y) -> Parser x a -> Parser y a
specialize addContext (Parser parser) =
  Parser $ \state@(State _ _ _ _ row col) cok eok cerr eerr ->
    let
      cerr' r c tx = cerr row col (addContext (tx r c))
      eerr' r c tx = eerr row col (addContext (tx r c))
    in
    parser state cok eok cerr' eerr'



-- SYMBOLS


word1 :: Word8 -> (Row -> Col -> x) -> Parser x ()
word1 word toError =
  Parser $ \(State src pos end indent row col) cok _ _ eerr ->
    if pos < end && unsafeIndex src pos == word then
      let !newState = State src (pos + 1) end indent row (col + 1) in
      cok () newState
    else
      eerr row col toError


word2 :: Word8 -> Word8 -> (Row -> Col -> x) -> Parser x ()
word2 w1 w2 toError =
  Parser $ \(State src pos end indent row col) cok _ _ eerr ->
    let
      !pos1 = pos + 1
    in
    if pos1 < end && unsafeIndex src pos == w1 && unsafeIndex src pos1 == w2 then
      let !newState = State src (pos + 2) end indent row (col + 2) in
      cok () newState
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

