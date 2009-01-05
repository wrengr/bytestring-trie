{-# OPTIONS_GHC -Wall -fwarn-tabs #-}

----------------------------------------------------------------
--                                                  ~ 2009.01.04
-- |
-- Module      :  Data.Trie.Test
-- Copyright   :  Copyright (c) 2008--2009 wren ng thornton
-- License     :  BSD3
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Testing 'Trie's.
----------------------------------------------------------------
module Data.Trie.Test where

import qualified Data.Trie as T
import qualified Data.ByteString as S
import qualified Data.ByteString.Internal as S (c2w)

import qualified Test.HUnit as HU
-- import qualified Test.QuickCheck     as QC
-- import qualified Test.SmallCheck     as SC
-- import qualified Test.LazySmallCheck as LSC
-- import qualified Test.SparseCheck    as PC
----------------------------------------------------------------
----------------------------------------------------------------

packC2W :: String -> S.ByteString
packC2W  = S.pack . map S.c2w

vocab2trie :: [String] -> T.Trie Int
vocab2trie  = T.fromList . flip zip [0..] . map packC2W

----------------------------------------------------------------
main :: IO ()
main  = do HU.runTestTT $ HU.TestList
                        [ test_Submap
                        , test_Insert
                        ]
           return ()

testEqual ::  (Show a, Eq a) => String -> a -> a -> HU.Test
testEqual s a b =
    HU.TestLabel s $ HU.TestCase $ HU.assertEqual "" a b

----------------------------------------------------------------
test_Submap :: HU.Test
test_Submap = HU.TestLabel "submap"
    $ HU.TestList
    [ nullSubmap "split on arc fails"    fi   True
    , nullSubmap "prefix of arc matches" fo   False
    , nullSubmap "suffix of empty fails" food True
    , nullSubmap "missing branch fails"  bag  True
    , nullSubmap "at a branch matches"   ba   False
    ]
    where
    t    = vocab2trie ["foo", "bar", "baz"]
    fi   = packC2W "fi"
    fo   = packC2W "fo"
    food = packC2W "food"
    ba   = packC2W "ba"
    bag  = packC2W "bag"
    
    nullSubmap s q b = testEqual s (T.null $ T.submap q t) b

----------------------------------------------------------------
-- requires Eq (Trie a) and, in case it fails, Show (Trie a)
test_Insert :: HU.Test
test_Insert = HU.TestLabel "insert"
    $ HU.TestList
    [ testEqual "insertion is commutative for prefix/superfix"
        (T.insert aba o $ T.insert abaissed i $ T.empty)
        (T.insert abaissed i $ T.insert aba o $ T.empty)
    ]
    where
    aba      = packC2W "aba"
    abaissed = packC2W "abaissed"
    
    o = 0::Int
    i = 1::Int

instance Show a => Show (T.Trie a) where
    show = T.showTrie

----------------------------------------------------------------
----------------------------------------------------------- fin.
