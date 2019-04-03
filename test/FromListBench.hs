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

module FromListBench (test) where

import qualified Data.Trie as Tr
import Data.Trie.Convenience (insertIfAbsent)
import Data.List             (foldl', sort)
import qualified Data.ByteString as S
-- import qualified Data.Text as T

import Data.ByteString.Internal (c2w)

import Microbench
import Control.Exception     (evaluate)
----------------------------------------------------------------

fromListR, fromListL :: [(S.ByteString, a)] -> Tr.Trie a
fromListR = foldr  (uncurry Tr.insert) Tr.empty
fromListL = foldl' (flip $ uncurry $ insertIfAbsent) Tr.empty

getList, getList'  :: S.ByteString -> Int -> [(S.ByteString, Int)]
getList  xs n = map (\k -> (k,0)) . S.inits . S.take n $ xs
getList' xs n = map (\k -> (k,0)) . S.tails . S.take n $ xs

-- fromListRText, fromListLText :: [(T.Text, a)] -> Tr.TrieText a
-- fromListRText = foldr  (uncurry Tr.insertText) Tr.emptyText
-- fromListLText = foldl' (flip $ uncurry $ insertIfAbsentText) Tr.emptyText

-- getListText, getListText'  :: T.Text -> Int -> [(T.Text, Int)]
-- getListText  xs n = map (\k -> (k,0)) . T.inits . T.take n $ xs
-- getListText' xs n = map (\k -> (k,0)) . T.tails . T.take n $ xs


test :: IO ()
test  = do
    -- 100000 is large enough to trigger Microbench's stop condition,
    -- and small enough to not lock up the system in trying to create it.
    xs <- evaluate $ S.replicate 100000 (c2w 'a')

    microbench "fromListR obverse" (Tr.null . fromListR . getList xs)
    microbench "fromListL obverse" (Tr.null . fromListL . getList xs)

    putStrLn ""
    microbench "fromListR reverse" (Tr.null . fromListR . getList' xs)
    microbench "fromListL reverse" (Tr.null . fromListL . getList' xs)

    -- Sorting forces it into the obverse order at O(n log n) cost
    putStrLn ""
    putStrLn ""
    microbench "fromListR obverse sorted" (Tr.null . fromListR . sort . getList xs)
    microbench "fromListL obverse sorted" (Tr.null . fromListL . sort . getList xs)
    putStrLn ""
    microbench "fromListR reverse sorted" (Tr.null . fromListR . sort . getList' xs)
    microbench "fromListL reverse sorted" (Tr.null . fromListL . sort . getList' xs)

-- testText :: IO ()
-- testText = do
--     -- 100000 is large enough to trigger Microbench's stop condition,
--     -- and small enough to not lock up the system in trying to create it.
--     xs <- evaluate $ T.replicate 100000 (T.singleton 'a')

--     microbench "fromListRText obverse" (Tr.nullText . fromListRText . getListText xs)
--     microbench "fromListLText obverse" (Tr.nullText . fromListLText . getListText xs)

--     putStrLn ""
--     microbench "fromListRText reverse" (Tr.nullText . fromListRText . getListText' xs)
--     microbench "fromListLText reverse" (Tr.nullText . fromListLText . getListText' xs)

--     -- Sorting forces it into the obverse order at O(n log n) cost
--     putStrLn ""
--     putStrLn ""
--     microbench "fromListRText obverse sorted" (Tr.nullText . fromListRText . sort . getListText xs)
--     microbench "fromListLText obverse sorted" (Tr.nullText . fromListLText . sort . getListText xs)
--     putStrLn ""
--     microbench "fromListRText reverse sorted" (Tr.nullText . fromListRText . sort . getListText' xs)
--     microbench "fromListLText reverse sorted" (Tr.nullText . fromListLText . sort . getListText' xs)

----------------------------------------------------------------
----------------------------------------------------------- fin.
