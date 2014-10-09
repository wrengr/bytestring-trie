-- This file copied without modifications from unordered-containers-0.2.4.0

{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Data.Trie.ArrayMapped.PopCount
    ( popCount
    ) where

#if __GLASGOW_HASKELL__ >= 704
import Data.Bits (popCount)
#else
import Data.Word (Word)
import Foreign.C (CUInt)
#endif

#if __GLASGOW_HASKELL__ < 704
foreign import ccall unsafe "popc.h popcount" c_popcount :: CUInt -> CUInt

popCount :: Word -> Int
popCount w = fromIntegral (c_popcount (fromIntegral w))
#endif