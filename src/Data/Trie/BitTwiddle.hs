{-# OPTIONS_GHC -Wall -fwarn-tabs -fno-warn-name-shadowing #-}
{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE CPP #-}
#ifdef __GLASGOW_HASKELL__
{-# LANGUAGE MagicHash #-}
#endif
#if __GLASGOW_HASKELL__ >= 701
-- Naturally, the MagicHash stuff from "GHC.Exts" isn't considered safe.
{-# LANGUAGE Trustworthy #-}
#endif
----------------------------------------------------------------
--                                                  ~ 2021.12.11
-- |
-- Module      :  Data.Trie.BitTwiddle
-- Copyright   :  2012 Clark Gaebel, 2012 Johan Tibel, 2002 Daan Leijen
-- License     :  BSD3
-- Maintainer  :  libraries@haskell.org, wren@cpan.org
-- Stability   :  stable
-- Portability :  portable (with CPP)
--
-- Functions to treat 'Word' as a bit-vector for big-endian patricia
-- trees. This code is duplicated from "Data.IntMap" (or
-- "Utils.Containers.Internal.BitUtil" these days). The only
-- differences are that some of the conversion functions are
-- specialized to 'Data.Word.Word8' for bytestrings, instead of
-- being specialized to 'Int'.
----------------------------------------------------------------

module Data.Trie.BitTwiddle
    (
    -- * Type aliases
      KeyElem, Prefix, Mask
    -- * Predicates
    , zero, nomatch, shorter
    -- * Constructors
    , applyMask, getMask
    ) where

import Data.Trie.ByteStringInternal (ByteStringElem)

-- It's too much noise to fully restrict this import, so just note
-- the requirements:
--      base 4.8.0 / GHC 7.10.1 -- 'countLeadingZeros', 'countTrailingZeros'
--      base 4.7.0 / GHC 7.8.2  -- 'FiniteBits', 'finiteBitSize'
--      base 4.5.0 / GHC 7.4.1  -- 'popCount'
import Data.Bits

-- To make it clearer what we're really testing for.
-- TODO: make this into a Cabal flag; for easier testing if nothing else.
#define USE_CLZ_IMPLEMENTATION MIN_VERSION_base(4,8,0)

#if __GLASGOW_HASKELL__ >= 503
-- Before GHC 5.3 these were in "GlaExts" instead.
import GHC.Exts
    ( Word(W#)
    , Int(I#)
#   if USE_CLZ_IMPLEMENTATION
    , shiftL#
#   else
    , shiftRL#
#   endif
    )
#else
import Data.Word (Word)
#endif

----------------------------------------------------------------

-- | 'KeyElem' is what we actually use for 'Prefix' and 'Mask'.
-- For now we're using 'ByteStringElem' ('Data.Word.Word8') for
-- simplicity, but in the future we might switch to a larger word
-- size.
type KeyElem = ByteStringElem

-- | Some prefix of the 'KeyElem', as constructed by 'mask'.
type Prefix  = KeyElem

-- | A single bit, signifying a mask (of all the bits preceding the
-- masking bit).
type Mask    = KeyElem

elemToNat :: KeyElem -> Word
{-# INLINE elemToNat #-}
elemToNat = fromIntegral

natToElem :: Word -> KeyElem
{-# INLINE natToElem #-}
natToElem = fromIntegral

-- TODO: newer versions of the containers library just use
-- 'unsafeShift{R,L}' unilaterally. So, what is the difference
-- (i.e., these days) between using the 'uncheckedShift{L,RL}#' of
-- 'unsafeShift{L,R}' vs using the 'shift{L,RL}#' of 'shift{L,R}'?
-- Also, do we no longer need to trick GHC into actually unboxing
-- and inlining these?
#if USE_CLZ_IMPLEMENTATION
shiftLL :: Word -> Int -> Word
{-# INLINE shiftLL #-}
#   if __GLASGOW_HASKELL__
-- Use unboxing to get @shiftLL@ inlined.
shiftLL (W# x) (I# i) = W# (shiftL# x i)
#   else
shiftLL x i = unsafeShiftL x i
#   endif
#else
shiftRL :: Word -> Int -> Word
{-# INLINE shiftRL #-}
#   if __GLASGOW_HASKELL__
-- Use unboxing to get @shiftRL@ inlined.
shiftRL (W# x) (I# i) = W# (shiftRL# x i)
#   else
shiftRL x i = unsafeShiftR x i
#   endif
#endif


{---------------------------------------------------------------
-- Endian independent bit twiddling (Trie endianness, not architecture)
---------------------------------------------------------------}

-- TODO: should we use the (Bits Word8) instance instead of 'elemToNat'
-- and (Bits Nat)? We need to compare Core, C--, or ASM in order
-- to decide this. The choice will apply to 'zero', 'mask', 'maskW',...
-- If we shouldn't, then we should probably send a patch upstream
-- to fix the (Bits Word8) instance.

-- | Is the key zero under the masking bit?  If true then whatever
-- is associated with that key should go to the left, otherwise it
-- should go to the right.
zero :: KeyElem -> Mask -> Bool
{-# INLINE zero #-}
zero i m = (elemToNat i) .&. (elemToNat m) == 0

-- | Does the 'mask'ed key /not/ match the prefix?  (Hence a subtree
-- matching the value doesn't exist.)
nomatch :: KeyElem -> Prefix -> Mask -> Bool
{-# INLINE nomatch #-}
nomatch i p m = applyMask i m /= p

-- | Convert a masking bit to the full mask it represents, and then
-- return the prefix of the key under that mask (i.e., all the bits
-- preceding the masking bit).
applyMask :: KeyElem -> Mask -> Prefix
{-# INLINE applyMask #-}
applyMask i m = maskW (elemToNat i) (elemToNat m)


{---------------------------------------------------------------
-- Big endian operations (Trie endianness, not architecture)
---------------------------------------------------------------}

-- | Get mask by setting all bits higher than the smallest bit in
-- @m@. Then apply that mask to @i@.
maskW :: Word -> Word -> Prefix
{-# INLINE maskW #-}
maskW i m = natToElem (i .&. (complement (m-1) `xor` m))
-- TODO: try the alternatives mentioned in the Containers paper:
-- \i m -> natToElem (i .&. (negate m - m))
-- \i m -> natToElem (i .&. (m * complement 1))
-- N.B. these return /all/ the low bits, and therefore they are not
-- equal functions for all m. They are, however, equal when only
-- one bit of m is set.

-- | Determine whether the first mask denotes a shorter prefix than
-- the second.
shorter :: Mask -> Mask -> Bool
{-# INLINE shorter #-}
shorter m1 m2 = elemToNat m1 > elemToNat m2

-- | Determine first differing bit of two prefixes.
getMask :: Prefix -> Prefix -> Mask
{-# INLINE getMask #-}
getMask p1 p2 = natToElem (highestBitMask (elemToNat p1 `xor` elemToNat p2))

{---------------------------------------------------------------
  Finding the highest bit (mask) in a word [x] can be done efficiently
  in three ways:
  * convert to a floating point value and the mantissa tells us the
    [log2(x)] that corresponds with the highest bit position. The
    mantissa is retrieved either via the standard C function [frexp]
    or by some bit twiddling on IEEE compatible numbers (float).
    Note that one needs to use at least [double] precision for an
    accurate mantissa of 32 bit numbers.
  * use bit twiddling, a logarithmic sequence of bitwise or's and
    shifts (bit).
  * use processor specific assembler instruction (asm).

  The most portable way would be [bit], but is it efficient enough?
  I have measured the cycle counts of the different methods on an
  AMD Athlon-XP 1800 (~ Pentium III 1.8Ghz) using the RDTSC
  instruction:

  highestBitMask: method  cycles
                  --------------
                   frexp   200
                   float    33
                   bit      11
                   asm      12

  highestBit:     method  cycles
                  --------------
                   frexp   195
                   float    33
                   bit      11
                   asm      11

  Wow, the bit twiddling is on today's RISC like machines even
  faster than a single CISC instruction (BSR)!
---------------------------------------------------------------}

{---------------------------------------------------------------
  [highestBitMask] returns a word where only the highest bit is
  set. It is found by first setting all bits in lower positions
  than the highest bit and than taking an exclusive or with the
  original value. Allthough the function may look expensive, GHC
  compiles this into excellent C code that subsequently compiled
  into highly efficient machine code. The algorithm is derived from
  Jorg Arndt's FXT library.
---------------------------------------------------------------}

highestBitMask :: Word -> Word
{-# INLINE highestBitMask #-}
#if USE_CLZ_IMPLEMENTATION
-- This is the implementation used in newer versions of the containers library.
-- Added this implementation here in version 0.2.7.
highestBitMask w = shiftLL 1 (wordSize - 1 - countLeadingZeros w)
#else
-- This is the classic one we used up to bytestring-trie-0.2.6.1
-- And it's still what containers falls back to for older versions of base.
--
-- N.B., because this is not exported and is only used by 'branchMask'
-- which operates on 'Word8' inputs, we can safely restrict the
-- algorithm to only doing the first few steps, rather than doing
-- all the steps needed for 'Word64'.
highestBitMask x
    = case (x .|. shiftRL x 1) of
       x -> case (x .|. shiftRL x 2) of
        x -> case (x .|. shiftRL x 4) of        -- for 8-bit input range.
        {-
         x -> case (x .|. shiftRL x 8) of       -- for 16-bit
          x -> case (x .|. shiftRL x 16) of     -- for 32-bit
           x -> case (x .|. shiftRL x 32) of    -- for 64-bit platforms
        -}
            x -> (x `xor` shiftRL x 1)
#endif

#if USE_CLZ_IMPLEMENTATION
wordSize :: Int
{-# INLINE wordSize #-}
#   if MIN_VERSION_base(4,7,0)
wordSize = finiteBitSize (0 :: Word)
#   else
wordSize = bitSize (0 :: Word)
#   endif
#endif

----------------------------------------------------------------
----------------------------------------------------------- fin.
