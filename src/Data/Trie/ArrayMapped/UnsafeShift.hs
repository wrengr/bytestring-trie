-- This file copied without modifications from unordered-containers-0.2.4.0

{-# LANGUAGE MagicHash #-}

module Data.Trie.ArrayMapped.UnsafeShift
    ( unsafeShiftL
    , unsafeShiftR
    ) where

import GHC.Exts (Word(W#), Int(I#), uncheckedShiftL#, uncheckedShiftRL#)

unsafeShiftL :: Word -> Int -> Word
unsafeShiftL (W# x#) (I# i#) = W# (x# `uncheckedShiftL#` i#)
{-# INLINE unsafeShiftL #-}

unsafeShiftR :: Word -> Int -> Word
unsafeShiftR (W# x#) (I# i#) = W# (x# `uncheckedShiftRL#` i#)
{-# INLINE unsafeShiftR #-}
