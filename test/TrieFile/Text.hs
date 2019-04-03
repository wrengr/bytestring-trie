{-# OPTIONS_GHC -Wall -fwarn-tabs #-}

----------------------------------------------------------------
--                                                  ~ 2019.04.03
-- |
-- Module      :  TrieFile.Text
-- Copyright   :  Copyright (c) 2008--2009 wren gayle romano, 2019 michael j. klein
-- License     :  BSD3
-- Maintainer  :  lambdamichael@gmail.com
-- Stability   :  experimental
--
-- Example program to read each line from a file (e.g. @/usr/dict@)
-- into a trie. Used to benchmark the naive algorithm against the
-- optimized C. Also used to verify correctness of C implementation,
-- and Trie implementation in general.
----------------------------------------------------------------

module TrieFile.Text (test, readTrieFromFile) where

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
readTrieFromFile :: FilePath -> IO (Tr.Trie Int)
readTrieFromFile file = withFile file ReadMode $ \h ->
    let go i t = do { b <- hIsEOF h
                    ; if b
                      then return t
                      else do { line <- T.hGetLine h
                              ; (go $! i+1) $! Tr.insert line i t
                              }
                    }
    in go 0 Tr.empty

test :: IO ()
test = do
    args <- getArgs
    case args of
      [] -> putStrLn "No Trie file to benchmark"
      file:_ -> do
        t <- readTrieFromFile file -- >>= evaluate

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
        putStrLn . show . take 20 . Tr.foldrWithKey (const (:)) [] $ t

        putStrLn . show . take 20 . F.foldl (flip (:)) [] $ t
        -- -}


----------------------------------------------------------------
----------------------------------------------------------- fin.

