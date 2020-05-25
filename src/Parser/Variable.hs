{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns, MagicHash, OverloadedStrings, UnboxedTuples #-}
module Parser.Variable
  ( chompInnerChars
  , getUpperWidth
  , getInnerWidth
  , getInnerWidthHelp
  )
  where


import qualified Data.Char as Char
import Data.Word (Word8)
import GHC.Prim (ByteArray#)
import GHC.Exts (Char(C#), Int#, (+#), (-#), chr#, uncheckedIShiftL#, word2Int#)
import GHC.Word (Word8(W8#))
import Prelude

import Parser.Primitives (Pos, End, Col, unsafeIndex)




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

