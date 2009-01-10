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
module Data.Trie.Test (packC2W, main) where

import qualified Data.Trie                as T
import qualified Data.Trie.Internal       as T (showTrie)
import qualified Data.ByteString          as S
import qualified Data.ByteString.Internal as S (c2w)

import qualified Test.HUnit          as HU
import qualified Test.QuickCheck     as QC
import qualified Test.SmallCheck     as SC
-- import qualified Test.LazySmallCheck as LSC
-- import qualified Test.SparseCheck    as PC

import Control.Monad (liftM)
import Data.List     (nubBy, sortBy)
import Data.Ord      (comparing)
----------------------------------------------------------------
----------------------------------------------------------------

packC2W :: String -> S.ByteString
packC2W  = S.pack . map S.c2w

vocab2trie :: [String] -> T.Trie Int
vocab2trie  = T.fromList . flip zip [0..] . map packC2W

----------------------------------------------------------------
main :: IO ()
main  = do 
    putStrLn ""
    putStrLn (replicate 80 '~')
    
    
    putStrLn "hunit:"
    HU.runTestTT $ HU.TestList
                 [ test_Union
                 , test_Submap
                 , test_Insert
                 ]
    putStrLn ""
    
    {-
    putStrLn "smallcheck @ ():"
    checkSmall (prop_insert  :: Word -> () -> T.Trie () -> Bool)
    checkSmall (prop_submap1 :: Word -> T.Trie () -> Bool)
    checkSmall (prop_submap2 :: Word -> T.Trie () -> Bool)
    checkSmall (prop_toList  :: T.Trie () -> Bool)
    checkSmall (prop_fromList_toList :: [(Word, ())] -> Bool)
    putStrLn ""
    -}
    
    putStrLn "quickcheck @ Int:"
    checkQuick (prop_insert  :: Word -> Int -> T.Trie Int -> Bool)
    checkQuick (prop_submap1 :: Word -> T.Trie Int -> Bool)
    checkQuick (prop_submap2 :: Word -> T.Trie Int -> Bool)
    checkQuick (prop_toList  :: T.Trie Int -> Bool)
    checkQuick (prop_fromList_toList :: [(Word, Int)] -> Bool)
    putStrLn ""
    where
    checkQuick = QC.check (QC.defaultConfig { QC.configMaxTest = 500 })
    checkSmall = SC.smallCheck 20

testEqual ::  (Show a, Eq a) => String -> a -> a -> HU.Test
testEqual s a b =
    HU.TestLabel s $ HU.TestCase $ HU.assertEqual "" a b

----------------------------------------------------------------
-- Because we avoid epsilons everywhere else, need to make sure 'mergeBy' gets it right
test_Union :: HU.Test
test_Union = HU.TestLabel "epsilon union"
    $ HU.TestList
    [ testEqual "left"  (e1 `T.unionL` e2) e1
    , testEqual "right" (e1 `T.unionR` e2) e2 -- meh, why not
    ]
    where
    e1 = T.singleton S.empty (4::Int)
    e2 = T.singleton S.empty (2::Int)

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

-- Needed by tests to show elements that fail.
instance Show a => Show (T.Trie a) where
    show = T.showTrie

----------------------------------------------------------------
-- TODO: we need a better instance of Arbitrary for lists to make them longer than our smallcheck depth.
-- 
-- I use strings with characters picked from a very restricted subset
-- in order to have more labels with shared prefixes.
newtype Letter = Letter { unLetter :: Char }
    deriving (Eq, Ord, Show)
letters :: [Char]
letters = ['a'..'m']

instance QC.Arbitrary Letter where
    arbitrary = Letter `fmap` QC.elements letters
    -- coarbitrary

newtype Word = Word { unWord :: S.ByteString }
    deriving (Eq, Ord)

instance Show Word where
    show (Word s) = "Word {unWord = packC2W " ++ show s ++ " }"

instance QC.Arbitrary Word where
    arbitrary = QC.sized $ \n -> do
        k <- QC.choose (0,n)
        s <- QC.vector k
        c <- QC.arbitrary -- We only want non-empty strings.
        return . Word . packC2W $ map unLetter (c:s)
    -- coarbitrary

instance (QC.Arbitrary a) => QC.Arbitrary (T.Trie a) where
    arbitrary = QC.sized $ \n -> do
        k      <- QC.choose (0,n)
        labels <- map unWord `fmap` QC.vector k
        elems  <- QC.vector k
        return . T.fromList $ zip labels elems
    -- coarbitrary


-- type Series a = Int -> [a]

instance SC.Serial Letter where
    series      d = take (d+1) $ map Letter letters
    coseries rs d = do f <- SC.coseries rs d
                       return $ \c -> f (fromEnum (unLetter c) - fromEnum 'a')
    
instance SC.Serial Word where
    series d = liftM (Word . packC2W . map unLetter)(SC.series d :: [[Letter]])
    -- coseries :: Series b -> Series (a -> b)
    
{-
instance (SC.Serial a) => SC.Serial (Trie a) where
    series :: Series a
    coseries :: Series b -> Series (a -> b)
-}


----------------------------------------------------------------
prop_insert :: (Eq a) => Word -> a -> T.Trie a -> Bool
prop_insert (Word k) v t =
    (T.lookup k . T.insert k v $ t) == Just v

prop_submap1 :: Word -> T.Trie a -> Bool
prop_submap1 (Word k) t =
    all (`T.member` t) . T.keys . T.submap k $ t

prop_submap2 :: Word -> T.Trie a -> Bool
prop_submap2 (Word k) t =
    all (S.isPrefixOf k) . T.keys . T.submap k $ t

prop_toList :: T.Trie a -> Bool
prop_toList t = ordered (T.keys t)
    where ordered xs = and (zipWith (<=) xs (drop 1 xs))

prop_fromList_toList :: (Eq a) => [(Word, a)] -> Bool
prop_fromList_toList assocs =
    (T.toList . T.fromList) === (nubBy (apFst (==)) . sortBy (comparing fst))
    $ map (first unWord) assocs

----------------------------------------------------------------
-- | Lift a function to apply to the first of pairs, retaining the second.
first :: (a -> b) -> (a,c) -> (b,c)
first f (x,y) = (f x, y)

-- | Lift a binary function to apply to the first of pairs, discarding seconds.
apFst :: (a -> b -> c) -> ((a,d) -> (b,e) -> c)
apFst f (x,_) (y,_) = f x y

-- | Function equality
(===) :: (Eq b) => (a -> b) -> (a -> b) -> (a -> Bool)
(===) f g x = (==) (f x) (g x)
----------------------------------------------------------------
----------------------------------------------------------- fin.
