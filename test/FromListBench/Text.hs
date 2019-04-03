{-# OPTIONS_GHC -Wall -fwarn-tabs #-}

----------------------------------------------------------------
--                                                  ~ 2009.01.04
-- |
-- Module      :  Bench.FromList
-- Copyright   :  Copyright (c) 2008--2009 wren gayle romano
-- License     :  BSD3
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Benchmarking for left- vs right-fold for @fromList@.
----------------------------------------------------------------

module FromListBench.Text (testText) where

import qualified Data.Trie.Text as Tr
import Data.Trie.Text.Convenience (insertIfAbsentText)
import Data.List             (foldl', sort)
import qualified Data.Text as T

import Microbench
import Control.Exception     (evaluate)
----------------------------------------------------------------

fromListRText, fromListLText :: [(T.Text, a)] -> Tr.TrieText a
fromListRText = foldr  (uncurry Tr.insertText) Tr.emptyText
fromListLText = foldl' (flip $ uncurry $ insertIfAbsentText) Tr.emptyText

getListText, getListText'  :: T.Text -> Int -> [(T.Text, Int)]
getListText  xs n = map (\k -> (k,0)) . T.inits . T.take n $ xs
getListText' xs n = map (\k -> (k,0)) . T.tails . T.take n $ xs

testText :: IO ()
testText = do
    -- 100000 is large enough to trigger Microbench's stop condition,
    -- and small enough to not lock up the system in trying to create it.
    xs <- evaluate $ T.replicate 100000 (T.singleton 'a')

    microbench "fromListRText obverse" (Tr.nullText . fromListRText . getListText xs)
    microbench "fromListLText obverse" (Tr.nullText . fromListLText . getListText xs)

    putStrLn ""
    microbench "fromListRText reverse" (Tr.nullText . fromListRText . getListText' xs)
    microbench "fromListLText reverse" (Tr.nullText . fromListLText . getListText' xs)

    -- Sorting forces it into the obverse order at O(n log n) cost
    putStrLn ""
    putStrLn ""
    microbench "fromListRText obverse sorted" (Tr.nullText . fromListRText . sort . getListText xs)
    microbench "fromListLText obverse sorted" (Tr.nullText . fromListLText . sort . getListText xs)
    putStrLn ""
    microbench "fromListRText reverse sorted" (Tr.nullText . fromListRText . sort . getListText' xs)
    microbench "fromListLText reverse sorted" (Tr.nullText . fromListLText . sort . getListText' xs)

----------------------------------------------------------------
----------------------------------------------------------- fin.

