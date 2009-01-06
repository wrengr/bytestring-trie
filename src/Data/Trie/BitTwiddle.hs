{-# OPTIONS_GHC -Wall -fwarn-tabs -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -cpp -fglasgow-exts #-}

----------------------------------------------------------------
--                                                  ~ 2009.01.05
-- |
-- Module      :  Data.Trie.BitTwiddle
-- Copyright   :  Copyright (c) Daan Leijen 2002
-- License     :  BSD3
-- Maintainer  :  libraries@haskell.org, wren@community.haskell.org
-- Stability   :  provisional
-- Portability :  portable (with CPP)
--
-- Functions to treat 'Word' as a bit-vector for big-endian patricia
-- trees. This code is duplicated from "Data.IntMap". The only
-- differences are that some of the conversion functions are
-- specialized to 'Word8' for bytestrings, instead of being specialized
-- to 'Int'.
----------------------------------------------------------------

module Data.Trie.BitTwiddle
    ( Prefix, Mask
    , elemToNat
    , zero, nomatch
    , mask, shorter, branchMask
    ) where

import Data.Bits
import Data.Trie.ByteStringInternal (ByteStringElem)

#if __GLASGOW_HASKELL__ >= 503
import GHC.Exts  ( Word(..), Int(..), shiftRL# )
#elif __GLASGOW_HASKELL__
import GlaExts   ( Word(..), Int(..), shiftRL# )
#else
import Data.Word (Word)
#endif

----------------------------------------------------------------

-- TODO: Natural word size, is 4*Word8 on my machine. Which means
-- it'll be more efficient to Branch by the first 4 bytes instead
-- of just one...
type KeyElem = ByteStringElem 
type Prefix  = KeyElem 
type Mask    = KeyElem 

{-# INLINE elemToNat #-}
elemToNat :: KeyElem -> Word
elemToNat i = fromIntegral i

{-# INLINE natToElem #-}
natToElem :: Word -> KeyElem
natToElem w = fromIntegral w

{-# INLINE shiftRL #-}
shiftRL :: Word -> Int -> Word
#if __GLASGOW_HASKELL__
-- GHC: use unboxing to get @shiftRL@ inlined.
shiftRL (W# x) (I# i) = W# (shiftRL# x i)
#else
shiftRL x i = shiftR x i
#endif


{---------------------------------------------------------------
-- Endian independent bit twiddling (Trie endianness, not architecture)
---------------------------------------------------------------}

-- | Is the value under the mask zero?
{-# INLINE zero #-}
zero :: KeyElem -> Mask -> Bool
zero i m = (elemToNat i) .&. (elemToNat m) == 0

-- | Does a value /not/ match some prefix, for all the bits preceding
-- a masking bit? (Hence a subtree matching the value doesn't exist.)
{-# INLINE nomatch #-}
nomatch :: KeyElem -> Prefix -> Mask -> Bool
nomatch i p m = mask i m /= p

{-# INLINE mask #-}
mask :: KeyElem -> Mask -> Prefix
mask i m = maskW (elemToNat i) (elemToNat m)


{---------------------------------------------------------------
-- Big endian operations (Trie endianness, not architecture)
---------------------------------------------------------------}

-- | Get mask by setting all bits higher than the smallest bit in
-- @m@. Then apply that mask to @i@.
{-# INLINE maskW #-}
maskW :: Word -> Word -> Prefix
maskW i m = natToElem (i .&. (complement (m-1) `xor` m))

-- | Determine whether the first mask denotes a shorter prefix than
-- the second.
{-# INLINE shorter #-}
shorter :: Mask -> Mask -> Bool
shorter m1 m2 = elemToNat m1 > elemToNat m2

-- | Determine first differing bit of two prefixes.
{-# INLINE branchMask #-}
branchMask :: Prefix -> Prefix -> Mask
branchMask p1 p2
    = natToElem (highestBitMask (elemToNat p1 `xor` elemToNat p2))

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
{-# INLINE highestBitMask #-}
highestBitMask :: Word -> Word
highestBitMask x
    = case (x .|. shiftRL x 1) of 
       x -> case (x .|. shiftRL x 2) of 
        x -> case (x .|. shiftRL x 4) of 
         x -> case (x .|. shiftRL x 8) of 
          x -> case (x .|. shiftRL x 16) of 
           x -> case (x .|. shiftRL x 32) of   -- for 64 bit platforms
            x -> (x `xor` shiftRL x 1)

----------------------------------------------------------------
----------------------------------------------------------- fin.
