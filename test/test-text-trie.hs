{-# OPTIONS_GHC -Wall -fwarn-tabs #-}

----------------------------------------------------------------
--                                                  ~ 2019.04.03
-- |
-- Copyright   :  Copyright (c) 2019 michael j. klein
-- License     :  BSD3
-- Maintainer  :  lambdamichael@gmail.com
-- Stability   :  experimental
----------------------------------------------------------------

import qualified Data.Trie.TextInternal.Test
import qualified Data.Trie.Text.Test
import qualified FromListBench
import qualified FromListBench.Text
import qualified TrieFile.Text

main :: IO ()
main = do
  putStrLn "Data.Trie.TextInternal.Test"
  Data.Trie.TextInternal.Test.test
  putStrLn "End: Data.Trie.TextInternal.Test"
  putStrLn ""

  putStrLn "Data.Trie.Text.Test"
  Data.Trie.Text.Test.test
  putStrLn "End: Data.Trie.Text.Test"
  putStrLn ""

  putStrLn "FromListBench"
  FromListBench.test
  putStrLn "End: FromListBench"
  putStrLn ""

  putStrLn "FromListBench.Text"
  FromListBench.Text.test
  putStrLn "End: FromListBench.Text"
  putStrLn ""

  putStrLn "TrieFile.Text"
  TrieFile.Text.test
  putStrLn "End: TrieFile.Text"
  putStrLn ""

