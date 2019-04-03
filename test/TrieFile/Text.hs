{-# OPTIONS_GHC -Wall -fwarn-tabs #-}

----------------------------------------------------------------
--                                                  ~ 2009.02.12
-- |
-- Module      :  Bench.TrieFile
-- Copyright   :  Copyright (c) 2008--2009 wren gayle romano
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

module TrieFile.Text (testText, readTrieTextFromFile) where

import qualified Data.Trie.Text          as Tr
import qualified Data.Trie.Text.Internal as Tr
import qualified Data.Text.IO            as T

import qualified Data.Foldable         as F

{-
import Microbench
import Control.Exception (evaluate)
-- -}

import System.IO          (withFile, IOMode(..), hIsEOF)
import System.Environment (getArgs)
----------------------------------------------------------------

-- Broken out for manual use in GHCi
readTrieTextFromFile :: FilePath -> IO (Tr.TrieText Int)
readTrieTextFromFile file = withFile file ReadMode $ \h ->
    let go i t = do { b <- hIsEOF h
                    ; if b
                      then return t
                      else do { line <- T.hGetLine h
                              ; (go $! i+1) $! Tr.insertText line i t
                              }
                    }
    in go 0 Tr.emptyText

testText :: IO ()
testText = do
    args <- getArgs
    case args of
      [] -> putStrLn "No TrieText file to benchmark"
      file:_ -> do
        t <- readTrieTextFromFile file -- >>= evaluate

        -- `sort`
        -- S.putStrLn . S.unlines . Tr.keys $ t

        -- `wc -l`
        -- putStrLn . show . Tr.size $ t


        {- -- Tests for comparing inferred foldl/foldr vs hand-written version
        microbench "List.foldr elems" $ do
            vs <- return $! Tr.elems t
            n  <- return $! foldr (\v r -> v `seq` (1+) $! r) (0::Int) vs
            n `seq` (return () :: IO ())

        microbench "Trie.foldr @elems" $ do
            t' <- return $! t
            n  <- return $! F.foldr (\v r -> v `seq` (1+) $! r) (0::Int) t'
            n `seq` (return () :: IO ())

        microbench "Trie.foldl @elems" $ do
            t' <- return $! t
            n  <- return $! F.foldl (\r v -> v `seq` (1+) $! r) (0::Int) t'
            n `seq` (return () :: IO ())
        -- -}

        -- {- -- verify associativity of folds
        putStrLn . show . take 20 . F.foldr (:) [] $ t
        putStrLn . show . take 20 . Tr.foldrWithKeyText (const (:)) [] $ t

        putStrLn . show . take 20 . F.foldl (flip (:)) [] $ t
        -- -}


----------------------------------------------------------------
----------------------------------------------------------- fin.

