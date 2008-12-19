{-# OPTIONS_GHC -Wall -fwarn-tabs #-}

----------------------------------------------------------------
--                                                  ~ 2008.12.19
-- |
-- Module      :  Data.Trie.ByteStringInternal
-- Copyright   :  Copyright (c) 2008--2009 wren ng thornton
-- License     :  BSD3
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Helper functions on 'ByteString's for "Data.Trie".
----------------------------------------------------------------


module Data.Trie.ByteStringInternal
    ( ByteString, ByteStringElem
    , wordHead
    , splitMaximalPrefix
    ) where

import qualified Data.ByteString as S
import Data.ByteString.Internal (ByteString(..), inlinePerformIO)
import Data.Word

import Control.Monad

import Foreign.ForeignPtr    (ForeignPtr, withForeignPtr)
import Foreign.Ptr           (Ptr, plusPtr, castPtr)
import Foreign.Storable      (Storable(..))
import Foreign.Marshal.Alloc (alloca)
import System.IO.Unsafe      (unsafePerformIO)
import Data.Bits             ((.&.))
----------------------------------------------------------------

type ByteStringElem = Word8 -- Associated type of ByteString


----------------------------------------------------------------
-- | Return the first natural 'Word' worth of string, padding by
-- zeros as necessary. Number of elements per word, and position
-- of elements within the word varies by architecture.
wordHead :: ByteString -> Word
wordHead (PS s o l) = inlinePerformIO $
                          withForeignPtr s $ \p ->
                              liftM (maskBytes l .&.)
                                  (peek (p `plusPtr` o :: Ptr Word))

-- The 0x are automatically truncated when too large, don't need
-- to worry about 'max'ing with sizeOf Word.
maskBytes :: Int -> Word
maskBytes i
    | isLittleEndian = case 0 `max` i of
        0 -> 0x0000000000000000
        1 -> 0x00000000000000FF
        2 -> 0x000000000000FFFF
        3 -> 0x0000000000FFFFFF
        4 -> 0x00000000FFFFFFFF
        5 -> 0x000000FFFFFFFFFF
        6 -> 0x0000FFFFFFFFFFFF
        7 -> 0x00FFFFFFFFFFFFFF
        _ -> 0xFFFFFFFFFFFFFFFF
    | otherwise = case 0 `max` i of
        0 -> 0x0000000000000000
        1 -> 0xFF00000000000000
        2 -> 0xFFFF000000000000
        3 -> 0xFFFFFF0000000000
        4 -> 0xFFFFFFFF00000000
        5 -> 0xFFFFFFFFFF000000
        6 -> 0xFFFFFFFFFFFF0000
        7 -> 0xFFFFFFFFFFFFFF00
        _ -> 0xFFFFFFFFFFFFFFFF

-- TODO: How to get this to execute statically?...
-- BUG? is 'alloca' safe in 'inlinePerformIO' (used in 'wordHead')?
{-# NOINLINE isLittleEndian #-}
isLittleEndian :: Bool
isLittleEndian = unsafePerformIO $ alloca $ \p -> do
    poke p    (0x04030201 :: Word32)
    b <- peek (castPtr p  :: Ptr Word8)
    case b of
        0x01 -> return True
        0x04 -> return False
        _    -> error "non-standard endianness! Contact the maintainer."

----------------------------------------------------------------
-- | Returns the longest shared prefix and the two remaining suffixes
-- for a pair of strings.
splitMaximalPrefix :: ByteString -> ByteString
                   -> (ByteString, ByteString, ByteString)
splitMaximalPrefix
    str1@(PS s1 off1 len1)
    str2@(PS s2 off2 len2)
    | len1 == 0 = (S.empty, S.empty, str2)
    | len2 == 0 = (S.empty, str1, S.empty)
    | otherwise = inlinePerformIO $
        withForeignPtr s1 $ \p1 ->
        withForeignPtr s2 $ \p2 -> do
            i <- indexOfDifference
                    (p1 `ptrElemOff` off1)
                    (p2 `ptrElemOff` off2)
                    (min len1 len2)
            let pre = if off1 + len1 < off2 + len2  -- share the smaller one
                      then newPS s1 off1 i
                      else newPS s2 off2 i
            let s1' = newPS s1 (off1 + i) (len1 - i)
            let s2' = newPS s2 (off2 + i) (len2 - i)
            
            return $! (,,) !$ pre !$ s1' !$ s2'

-- | C-style pointer addition, without the liberal type of 'plusPtr'.
{-# INLINE ptrElemOff #-}
ptrElemOff :: Storable a => Ptr a -> Int -> Ptr a
ptrElemOff p i = p `plusPtr` (i * sizeOf (unsafePerformIO (peek p)))

{-# INLINE newPS #-}
newPS :: ForeignPtr ByteStringElem -> Int -> Int -> ByteString
newPS s o l = if l <= 0 then S.empty else PS s o l

{-# INLINE (!$) #-}
(!$) :: (a -> b) -> a -> b
(!$)  = ($!) -- fix associativity bug


-- Calculates the first index where values differ.
-- BUG: There has to be a smarter algorithm for this
{-# INLINE indexOfDifference #-}
indexOfDifference :: Ptr ByteStringElem -> Ptr ByteStringElem -> Int -> IO Int
indexOfDifference p1 p2 limit = go 0
    where
    go n = if   n >= limit
           then return limit
           else do c1 <- peekElemOff p1 n
                   c2 <- peekElemOff p2 n
                   if c1 == c2
                       then go $! n+1
                       else return n

----------------------------------------------------------------
----------------------------------------------------------- fin.
