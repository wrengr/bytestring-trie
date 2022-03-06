{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
{-# LANGUAGE BangPatterns #-}

----------------------------------------------------------------
--                                                  ~ 2022.03.04
-- |
-- Module      :  Bench.Main
-- Copyright   :  2008--2022 wren romano
-- License     :  BSD-3-Clause
-- Maintainer  :  wren@cpan.org
-- Stability   :  provisional
-- Portability :  portable (with bang-patterns)
--
----------------------------------------------------------------

module Bench.Main (main, readTrieFromFile) where
-- Exporting 'readTrieFromFile' for reuse in GHCi while debugging.

import Bench.Foldable (realTrie_to_benchTrie, bgroup_Foldable)

import qualified Data.Trie          as T
-- import qualified Data.Trie.Internal as TI
import qualified Data.ByteString    as S

import qualified Criterion.Main     as C
import qualified System.IO          as Sys (withFile, IOMode(..), hIsEOF)
import qualified System.Environment as Sys (getProgName, getArgs, withArgs)
import qualified System.Exit        as Sys (exitFailure)
----------------------------------------------------------------

-- | Read a file and insert each line into a trie with its (base-0)
-- line number.
readTrieFromFile :: FilePath -> IO (T.Trie Int)
readTrieFromFile file = Sys.withFile file Sys.ReadMode (go 0 T.empty)
    where
    go !i !t !h = do
        b <- Sys.hIsEOF h
        if b
          then return t
          else do
            line <- S.hGetLine h
            go (i+1) (T.insert line i t) h

-- TODO: maybe add some more CLI structure, for things like choosing
-- to use random inputs instead of file, or parameters of randomness,
-- etc.
main :: IO ()
main  = do
    args <- Sys.getArgs
    case args of
      [] -> do
        prog <- Sys.getProgName
        putStrLn "ERROR: Missing file argument"
        putStrLn $ "Usage: " ++ prog ++ " FILE [criterionFlags]"
        Sys.exitFailure
      file:rest ->
        Sys.withArgs rest $ C.defaultMain
          [ C.env (realTrie_to_benchTrie <$> readTrieFromFile file) $ \ t ->
            bgroup_Foldable [t]
            -- TODO: the other benchmark suites here.
          ]


----------------------------------------------------------------
----------------------------------------------------------- fin.
