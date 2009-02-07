{-# OPTIONS_GHC -Wall -fwarn-tabs #-}

----------------------------------------------------------------
--                                                  ~ 2009.02.06
-- |
-- Module      :  Bench.TrieFile
-- Copyright   :  Copyright (c) 2008--2009 wren ng thornton
-- License     :  BSD3
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Example program to read each line from a file (e.g. @/usr/dict@)
-- into a trie. Used to benchmark the naive algorithm against the
-- optimized C. Also used to verify correctness of C implementation,
-- and Trie implementation in general.
----------------------------------------------------------------

module Main (main) where

import qualified Data.Trie             as T
import qualified Data.ByteString       as S
import qualified Data.ByteString.Char8 as S (unlines)

import System.IO          (withFile, IOMode(..), hIsEOF)
import System.Environment (getArgs)
----------------------------------------------------------------

main :: IO ()
main  = do
    [file] <- getArgs
    withFile file ReadMode $ \h -> do
        let go t = do b <- hIsEOF h
                      if b
                          then return t
                          else do
                              line <- S.hGetLine h
                              go $! T.insert line () t
        t <- go T.empty
        S.putStrLn . S.unlines . T.keys $ t
        -- putStrLn . show . T.size $ t

----------------------------------------------------------------
----------------------------------------------------------- fin.
