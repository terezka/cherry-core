
{-|

Module      : Bitwise
Description : Work with bits.
License     : BSD 3
Maintainer  : terezasokol@gmail.com
Stability   : experimental
Portability : POSIX

-}

module Bitwise
  ( and,
    or,
    xor,
    complement,
    shiftLeftBy,
    shiftRightBy,
    shiftRightZfBy,
  )
where

import Data.Bits ((.&.), (.|.))
import Basics (Int)
import qualified Prelude
import qualified Data.Bits


{-| Bitwise AND
-}
and :: Int -> Int -> Int
and =
  (.&.)


{-| Bitwise OR
-}
or :: Int -> Int -> Int
or =
  (.|.)


{-| Bitwise XOR
-}
xor :: Int -> Int -> Int
xor = Data.Bits.xor


{-| Flip each bit individually, often called bitwise NOT
-}
complement :: Int -> Int
complement = Data.Bits.complement


{-| Shift bits to the left by a given offset, filling new bits with zeros.
This can be used to multiply numbers by powers of two.

  >  shiftLeftBy 1 5 == 10
  >  shiftLeftBy 5 1 == 32
-}
shiftLeftBy :: Int -> Int -> Int
shiftLeftBy offset value =
  Data.Bits.shift value (Prelude.fromIntegral offset)


{-| Shift bits to the right by a given offset, filling new bits with
whatever is the topmost bit. This can be used to divide numbers by powers of two.

  >  shiftRightBy 1  32 == 16
  >  shiftRightBy 2  32 == 8
  >  shiftRightBy 1 -32 == -16

This is called an [arithmetic right shift](https://en.wikipedia.org/wiki/Bitwise_operation#Arithmetic_shift), often written `>>`, and
sometimes called a sign-propagating right shift because it fills empty spots
with copies of the highest bit.

-}
shiftRightBy :: Int -> Int -> Int
shiftRightBy offset value =
  Data.Bits.shiftR value (Prelude.fromIntegral offset)


{-| Shift bits to the right by a given offset, filling new bits with zeros.

  >  shiftRightZfBy 1  32 == 16
  >  shiftRightZfBy 2  32 == 8
  >  shiftRightZfBy 1 -32 == 2147483632

This is called an [logical right shift](https://en.wikipedia.org/wiki/Bitwise_operation#Logical_shift), often written `>>>`, and
sometimes called a zero-fill right shift because it fills empty spots with
zeros.

-}
shiftRightZfBy :: Int -> Int -> Int
shiftRightZfBy offset value =
  -- For some reason Data.Bits does not implement this function. The general idea is:
  -- 1. Generate a mask that will clear the leftmost n bits when ANDed with the result.
  -- 2. Shift right by n bits.
  -- 3. Bitwise AND the mask.
  let n = Prelude.fromIntegral offset
      oneBits = Data.Bits.complement Data.Bits.zeroBits :: Int
      shiftedValue = Data.Bits.shiftR value n
      shiftedMask = Data.Bits.rotateR (Data.Bits.shiftL oneBits n) n
   in and shiftedValue shiftedMask
