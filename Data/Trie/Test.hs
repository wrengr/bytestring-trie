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
testEqual ::  (Show a, Eq a) => String -> a -> a -> HU.Test
testEqual s a b =
    HU.TestLabel s $ HU.TestCase $ HU.assertEqual "" a b

----------------------------------------------------------------
tests_Submap :: HU.Test
tests_Submap = HU.TestLabel "submap"
    $ HU.TestList
    [ testEqual "prefix of arc matches" (T.null $ T.submap fo t) False
    , testEqual "split of arc fails"    (T.null $ T.submap fi t) True
    ]
    where
    t  = vocab2trie ["foo", "bar", "baz"]
    fo = packC2W "fo"
    fi = packC2W "fi"

----------------------------------------------------------------
----------------------------------------------------------- fin.
