{-# OPTIONS_GHC -Wall -fwarn-tabs #-}

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
                        [ tests_Submap
                        ]
           return ()

testEqual ::  (Show a, Eq a) => String -> a -> a -> HU.Test
testEqual s a b =
    HU.TestLabel s $ HU.TestCase $ HU.assertEqual "" a b

----------------------------------------------------------------
tests_Submap :: HU.Test
tests_Submap = HU.TestLabel "submap"
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
----------------------------------------------------------- fin.
