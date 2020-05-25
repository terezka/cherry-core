{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns #-}
module Parser.Keyword
  ( symbol
  , k4
  , k5
  )
  where


import Prelude
import Data.Word (Word8)

import qualified Parser.Variable as Var
import qualified Parser.Primitives as P
import Parser.Primitives (Parser, Row, Col)


symbol :: Word8 -> (Row -> Col -> x) -> Parser x ()
symbol w1 toError =
  P.Parser $ \(P.State src pos end indent row col) cok _ _ eerr ->
    let !pos1 = pos + 1 in
    if pos1 <= end && P.unsafeIndex src pos == w1
    then
      let !s = P.State src pos1 end indent row (col + 1) in cok () s
    else
      eerr row col toError


k4 :: Word8 -> Word8 -> Word8 -> Word8 -> (Row -> Col -> x) -> Parser x ()
k4 w1 w2 w3 w4 toError =
  P.Parser $ \(P.State src pos end indent row col) cok _ _ eerr ->
    let !pos4 = pos + 4 in
    if pos4 <= end
      && P.unsafeIndex src (pos    ) == w1
      && P.unsafeIndex src (pos + 1) == w2
      && P.unsafeIndex src (pos + 2) == w3
      && P.unsafeIndex src (pos + 3) == w4
      && Var.getInnerWidth src pos4 end == 0
    then
      let !s = P.State src pos4 end indent row (col + 4) in cok () s
    else
      eerr row col toError


k5 :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> (Row -> Col -> x) -> Parser x ()
k5 w1 w2 w3 w4 w5 toError =
  P.Parser $ \(P.State src pos end indent row col) cok _ _ eerr ->
    let !pos5 = pos + 5 in
    if pos5 <= end
      && P.unsafeIndex src (pos    ) == w1
      && P.unsafeIndex src (pos + 1) == w2
      && P.unsafeIndex src (pos + 2) == w3
      && P.unsafeIndex src (pos + 3) == w4
      && P.unsafeIndex src (pos + 4) == w5
      && Var.getInnerWidth src pos5 end == 0
    then
      let !s = P.State src pos5 end indent row (col + 5) in cok () s
    else
      eerr row col toError

