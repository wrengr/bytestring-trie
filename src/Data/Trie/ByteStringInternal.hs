{-# OPTIONS_GHC -Wall -fwarn-tabs #-}

----------------------------------------------------------------
--                                                  ~ 2008.12.24
-- |
-- Module      :  Data.Trie.ByteStringInternal
-- Copyright   :  Copyright (c) 2008--2009 wren ng thornton
-- License     :  BSD3
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Helper functions on 'ByteString's for "Data.Trie.Internal".
----------------------------------------------------------------


module Data.Trie.ByteStringInternal
    ( ByteString, ByteStringElem
    {-, unsafeWordHead-}
    , breakMaximalPrefix
    ) where

import qualified Data.ByteString as S
import Data.ByteString.Internal (ByteString(..), inlinePerformIO)
import Data.Word

import Control.Monad

import Foreign.ForeignPtr    (ForeignPtr, withForeignPtr)
import Foreign.Ptr           (Ptr, plusPtr{-, castPtr-})
import Foreign.Storable      (Storable(..))
import System.IO.Unsafe      (unsafePerformIO)
{-
import Foreign.Marshal.Alloc (alloca)
import Data.Bits
-}
----------------------------------------------------------------

type ByteStringElem = Word8 -- Associated type of ByteString

{- -- Not used at present
-- | The size of 'Word' in bytes.
{-# INLINE sizeOfWord #-}
sizeOfWord :: Int
sizeOfWord  = sizeOf (undefined :: Word)

----------------------------------------------------------------

-- | Return the first natural 'Word' worth of string, padding by
-- zeros as necessary. The position of elements within the word
-- varies by architecture, hence this function is quasi-unsafe to
-- use for trieing on the bit-vector representation. A safer version
-- may be forthcoming, though trieing on multiple bytes at once
-- appears impractical at the moment.
unsafeWordHead :: ByteString -> Word
unsafeWordHead (PS s o l) = inlinePerformIO $
                                withForeignPtr s $ \p ->
                                    liftM (maskInitialBytes l .&.)
                                        (peek (p `plusPtr` o :: Ptr Word))

maskInitialBytes :: Int -> Word
maskInitialBytes byteCount
    | isLittleEndian = case effectiveByteCount of
                       0 -> 0x0000000000000000
                       1 -> 0x00000000000000FF
                       2 -> 0x000000000000FFFF
                       3 -> 0x0000000000FFFFFF
                       4 -> 0x00000000FFFFFFFF
                       5 -> 0x000000FFFFFFFFFF
                       6 -> 0x0000FFFFFFFFFFFF
                       7 -> 0x00FFFFFFFFFFFFFF
                       _ -> 0xFFFFFFFFFFFFFFFF
    | otherwise      = case effectiveByteCount of
                       0 -> 0x0000000000000000
                       1 -> 0xFF00000000000000
                       2 -> 0xFFFF000000000000
                       3 -> 0xFFFFFF0000000000
                       4 -> 0xFFFFFFFF00000000
                       5 -> 0xFFFFFFFFFF000000
                       6 -> 0xFFFFFFFFFFFF0000
                       7 -> 0xFFFFFFFFFFFFFF00
                       _ -> 0xFFFFFFFFFFFFFFFF
    where
    effectiveByteCount = 0 `max` (byteCount `min` sizeOfWord)


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
        _    -> error ("non-standard endianness detected! "
                       ++ "Contact the Data.Trie maintainer.")

-- End unused code -}

----------------------------------------------------------------
-- | Returns the longest shared prefix and the two remaining suffixes
-- for a pair of strings.
breakMaximalPrefix :: ByteString -> ByteString
                   -> (ByteString, ByteString, ByteString)
breakMaximalPrefix
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

-- | fix associativity bug
{-# INLINE (!$) #-}
(!$) :: (a -> b) -> a -> b
(!$)  = ($!)


-- | Calculates the first index where values differ.
--
-- BUG: There has to be a smarter algorithm for this. In particular,
-- it'd be nice if there was a function like C's @memcmp@, but which
-- returns the index rather than the 'Ordering'. If it were agnostic
-- ot architecture, that'd be even nicer.
--
-- TODO: rather than revert to @goByte@, we should use xor- and
-- mask-munging so @goWord@ can reuse the information it already
-- has to discover the byte of difference. The trick is making that
-- faster than the current version. And ensuring alignment.
{-# INLINE indexOfDifference #-}
indexOfDifference :: Ptr ByteStringElem -> Ptr ByteStringElem -> Int -> IO Int
indexOfDifference p1 p2 limit = goByte 0
    where
    {- BUG: using this assumes ByteStrings are Word-aligned
    goWord n = if   n + sizeOfWord >= limit
               then goByte n
               else do w1 <- peek (p1 `plusPtr` n :: Ptr Word)
                       w2 <- peek (p2 `plusPtr` n :: Ptr Word)
                       if w1 == w2
                           then goWord $! n + sizeOfWord
                           else goByte n
    -}
    
    goByte n = if   n >= limit
               then return limit
               else do c1 <- peekElemOff p1 n
                       c2 <- peekElemOff p2 n
                       if c1 == c2
                           then goByte $! n+1
                           else return n

----------------------------------------------------------------
----------------------------------------------------------- fin.
