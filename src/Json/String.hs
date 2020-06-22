{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE MagicHash, UnboxedTuples #-}
module Json.String
  ( Chunk(..)
  , toTextUtf8
  )
  where


import Prelude (($), (+), (<), (=<<), fromIntegral, map, otherwise, sum)
import Data.Bits ((.&.), shiftR)
import qualified Data.Text.Internal as T
import qualified Data.Text.Array as T (Array(Array))
import GHC.Exts
  ( Int(I#)
  , ByteArray#
  , MutableByteArray#
  , newByteArray#
  , unsafeFreezeByteArray#
  , copyByteArray#
  , writeWord8Array#
  )
import GHC.ST (ST(ST), runST)
import GHC.Word (Word8(W8#))



-- CHUNKS


data Chunk
  = Slice Int Int
  | Escape Word8
  | CodePoint Int



-- TO TEXT


toTextUtf8 :: ByteArray# -> [Chunk] -> T.Text
toTextUtf8 src chunks =
  case chunks of
    [] ->
      T.empty

    [Slice off len] ->
      T.text (T.Array src) off len

    _ ->
      let
        len = sum (map getChunkSize chunks)
        arr = runST (writeChunks src chunks =<< newByteArray len)
      in
      T.text arr 0 len


writeChunks :: ByteArray# -> [Chunk] -> MBA s -> ST s T.Array
writeChunks src chunks mba =
    go 0 chunks
  where
    go offset chunks =
      case chunks of
        [] ->
          freeze mba

        c:cs ->
          case c of
            Slice off len ->
              do  copy src off mba offset len
                  go (offset + len) cs

            Escape escape ->
              do  writeWord8 mba offset escape
                  go (offset + 1) cs

            CodePoint n
              | n < 0x80 ->
                  do  writeWord8 mba (offset    ) (fromIntegral n)
                      go             (offset + 1) cs

              | n < 0x800 ->
                  do  writeWord8 mba (offset    ) (fromIntegral ((shiftR n 6         ) + 0xC0))
                      writeWord8 mba (offset + 1) (fromIntegral ((       n   .&. 0x3F) + 0x80))
                      go             (offset + 2) cs

              | n < 0x10000 ->
                  do  writeWord8 mba (offset    ) (fromIntegral ((shiftR n 12         ) + 0xE0))
                      writeWord8 mba (offset + 1) (fromIntegral ((shiftR n  6 .&. 0x3F) + 0x80))
                      writeWord8 mba (offset + 2) (fromIntegral ((       n    .&. 0x3F) + 0x80))
                      go             (offset + 3) cs

              | otherwise ->
                  do  writeWord8 mba (offset    ) (fromIntegral ((shiftR n 18         ) + 0xF0))
                      writeWord8 mba (offset + 1) (fromIntegral ((shiftR n 12 .&. 0x3F) + 0x80))
                      writeWord8 mba (offset + 2) (fromIntegral ((shiftR n  6 .&. 0x3F) + 0x80))
                      writeWord8 mba (offset + 3) (fromIntegral ((       n    .&. 0x3F) + 0x80))
                      go             (offset + 4) cs



-- GET CHUNK SIZE


getChunkSize :: Chunk -> Int
getChunkSize chunk =
  case chunk of
    Slice _ len    -> len
    Escape _       -> 1
    CodePoint code
      | code < 0x80    -> 1
      | code < 0x800   -> 2
      | code < 0x10000 -> 3
      | otherwise      -> 4



-- HELPERS


data MBA s =
  MBA# (MutableByteArray# s)


newByteArray :: Int -> ST s (MBA s) -- PERF see if newPinnedByteArray for len > 256 is positive
newByteArray (I# len#) =
  ST $ \s ->
    case newByteArray# len# s of
      (# s, mba# #) -> (# s, MBA# mba# #)


freeze :: MBA s -> ST s T.Array
freeze (MBA# mba#) =
  ST $ \s ->
    case unsafeFreezeByteArray# mba# s of
      (# s, ba# #) -> (# s, T.Array ba# #)


copy :: ByteArray# -> Int -> MBA s -> Int -> Int -> ST s ()
copy ba# (I# offset#) (MBA# mba#) (I# i#) (I# len#) =
  ST $ \s ->
    case copyByteArray# ba# offset# mba# i# len# s of
      s -> (# s, () #)


writeWord8 :: MBA s -> Int -> Word8 -> ST s ()
writeWord8 (MBA# mba#) (I# offset#) (W8# w#) =
  ST $ \s ->
    case writeWord8Array# mba# offset# w# s of
      s -> (# s, () #)
