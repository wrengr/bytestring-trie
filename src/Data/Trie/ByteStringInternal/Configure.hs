{-# OPTIONS_GHC -Wall -fwarn-tabs #-}

----------------------------------------------------------------
--                                                  ~ 2009.01.04
-- |
-- Module      :  Data.Trie.ByteStringInternal.Configure
-- Copyright   :  Copyright (c) 2008--2009 wren ng thornton
-- License     :  BSD3
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  beta
-- Portability :  portable
--
-- Configuration program for C parameters in indexOfDifference.h
----------------------------------------------------------------


module Main (main) where

import Data.Word             (Word8, Word32)
import Foreign.Ptr           (Ptr, castPtr)
import Foreign.Storable      (Storable(..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.C.Types       (CInt)

import qualified Data.ByteString as S
import qualified Data.ByteString.Internal as S (c2w)
import System.IO
----------------------------------------------------------------

errorContactMaintainer  :: String -> Bool
errorContactMaintainer s = error (s ++ " Contact the Data.Trie maintainer.")

-- cf also the new byteorder package
-- <http://hackage.haskell.org/cgi-bin/hackage-scripts/package/byteorder>
-- It's less efficient, but deals with mixed endianness better.
-- It only does detection though, not the bit-munging we need later.
isLittleEndian :: IO Bool
isLittleEndian = alloca $ \p -> do
    poke p    (0x04030201 :: Word32)
    b <- peek (castPtr p  :: Ptr Word8)
    return $! case b of
        0x01 -> True
        0x04 -> False
        _    -> errorContactMaintainer "non-standard endianness detected!"

is32Bit :: Bool
is32Bit = case sizeOf (undefined :: CInt) of -- BUG: Not the best test...
              4 -> True
              8 -> False
              _ -> errorContactMaintainer "non-standard word size detected!"

main :: IO ()
main = do
    isLE <- isLittleEndian
    is32 <- return $! is32Bit
    withFile "indexOfDifference.h" WriteMode $ \h -> do
        let printLn = S.hPutStrLn h . S.pack . map S.c2w
        printLn "#ifndef __hDataTrie__"
        printLn "#define __hDataTrie__"
        printLn ""
        if isLE
            then printLn "#define __hDataTrie_isLittleEndian__"
            else return ()
        if is32
            then printLn "#define __hDataTrie_Nat32__"
            else printLn "#define __hDataTrie_Nat64__"
        printLn ""
        printLn "#ifdef __cplusplus"
        printLn "extern \"C\"{"
        printLn "#endif"
        printLn "int indexOfDifference(const void* p1, const void* p2, const int limit);"
        printLn "#ifdef __cplusplus"
        printLn "}"
        printLn "#endif"
        printLn ""
        printLn "#endif /* __hDataTrie__ */"

----------------------------------------------------------------
----------------------------------------------------------- fin.
