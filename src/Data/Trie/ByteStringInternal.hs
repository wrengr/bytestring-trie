{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 701
-- Neither 'unsafeDupablePerformIO' nor 'Data.ByteString.Internal' is safe.
{-# LANGUAGE Trustworthy #-}
#endif
------------------------------------------------------------
--                                              ~ 2021.11.27
-- |
-- Module      :  Data.Trie.ByteStringInternal
-- Copyright   :  Copyright (c) 2008--2021 wren gayle romano
-- License     :  BSD3
-- Maintainer  :  wren@cpan.org
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Helper functions on 'ByteString's for "Data.Trie.Internal".
------------------------------------------------------------

module Data.Trie.ByteStringInternal
    ( ByteString, ByteStringElem
    , breakMaximalPrefix
    ) where

import qualified Data.ByteString as S
import Data.ByteString.Internal (ByteString(PS))
import Data.Word
import Foreign.ForeignPtr       (ForeignPtr, withForeignPtr)
import Foreign.Ptr              (Ptr, plusPtr)
import Foreign.Storable         (Storable(..))
-- This module name is since @__GLASGOW_HASKELL__ >= 611@.
import GHC.IO                   (unsafeDupablePerformIO)

------------------------------------------------------------
-- | Associated type of 'ByteString'
type ByteStringElem = Word8


------------------------------------------------------------
-- The @since annotation is for when this got re-exported from
-- "Data.Trie.Internal".
--
-- | Returns the longest shared prefix and the two remaining suffixes
-- for a pair of strings.
--
-- * @s == (\\(pre,s',z') -> pre '<>' s') ('breakMaximalPrefix' s z)@
-- * @z == (\\(pre,s',z') -> pre '<>' z') ('breakMaximalPrefix' s z)@
--
-- @since 0.2.2
breakMaximalPrefix
    :: ByteString
    -> ByteString
    -> (ByteString, ByteString, ByteString)
breakMaximalPrefix
    str1@(PS s1 off1 len1)
    str2@(PS s2 off2 len2)
    | len1 == 0 = (S.empty, S.empty, str2)
    | len2 == 0 = (S.empty, str1, S.empty)
    | otherwise = unsafeDupablePerformIO $
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

-- | Get the 'sizeOf' type @a@, without requiring @-XScopedTypeVariables@
-- nor making a spurious call to 'System.IO.Unsafe.unsafePerformIO' or similar.
sizeOfPtr :: Storable a => Ptr a -> Int
sizeOfPtr = sizeOf . (undefined :: Ptr a -> a)
{-# INLINE sizeOfPtr #-}

-- | C-style pointer addition, without the excessively liberal type
-- of 'plusPtr'.
ptrElemOff :: Storable a => Ptr a -> Int -> Ptr a
ptrElemOff p i = p `plusPtr` (i * sizeOfPtr p)
{-# INLINE ptrElemOff #-}

-- | Smart-constructor to share 'S.empty' as appropriate.
newPS :: ForeignPtr ByteStringElem -> Int -> Int -> ByteString
newPS s o l = if l <= 0 then S.empty else PS s o l
{-# INLINE newPS #-}

-- | fix associativity bug
(!$) :: (a -> b) -> a -> b
(!$)  = ($!)
{-# INLINE (!$) #-}


------------------------------------------------------------
-- This naive algorithm doesn't depend on architecture details.  We
-- could speed things up (in theory) by checking a natural word at
-- a time and then falling back to checking each byte once the
-- mismatched word is found.  But in practice that doesn't seem to
-- actually speed things up.
--
-- | Calculates the first index where values differ.
indexOfDifference
    :: Ptr ByteStringElem
    -> Ptr ByteStringElem
    -> Int
    -> IO Int
indexOfDifference p1 p2 limit = goByte 0
    where
    goByte n =
        if   n >= limit
        then return limit
        else do c1 <- peekElemOff p1 n
                c2 <- peekElemOff p2 n
                if c1 == c2
                    then goByte $! n+1
                    else return n

------------------------------------------------------------
------------------------------------------------------- fin.
