{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
{-# LANGUAGE CPP
           , MultiParamTypeClasses
           , FlexibleInstances
           , FlexibleContexts
           , TypeSynonymInstances
           #-}

----------------------------------------------------------------
--                                                  ~ 2021.11.07
-- |
-- Module      :  Data.Trie.Test
-- Copyright   :  Copyright (c) 2008--2021 wren gayle romano
-- License     :  BSD3
-- Maintainer  :  wren@cpan.org
-- Stability   :  provisional
-- Portability :  semi-portable (MPTC,...)
--
-- Testing 'Trie's.
----------------------------------------------------------------
module Data.Trie.Test (packC2W, main) where

import qualified Data.Trie                as T
import qualified Data.Trie.Internal       as TI
import qualified Data.Trie.Convenience    as TC
import qualified Data.ByteString          as S
import qualified Data.ByteString.Internal as S (c2w, w2c)

import qualified Test.HUnit          as HU
import qualified Test.QuickCheck     as QC
import qualified Test.SmallCheck     as SC
import qualified Test.SmallCheck.Series as SC
-- import qualified Test.LazySmallCheck as LSC
-- import qualified Test.SparseCheck    as PC

import Data.List (nubBy, sortBy)
import Data.Ord  (comparing)
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
    _ <- HU.runTestTT $ HU.TestList
                 [ test_Union
                 , test_Submap
                 , test_Insert
                 , test_Delete
                 ]
    putStrLn ""

    putStrLn "quickcheck @ Int:"
    checkQuick 500  (prop_insert        :: Str -> Int -> T.Trie Int -> Bool)
    checkQuick 5000 (prop_singleton     :: Str -> Int -> Bool)
    checkQuick 500  (prop_size_insert   :: Str -> Int -> T.Trie Int -> QC.Property)
    checkQuick 500  (prop_size_delete   :: Str -> Int -> T.Trie Int -> QC.Property)
    checkQuick 500  (prop_insert_delete :: Str -> Int -> T.Trie Int -> QC.Property)
    checkQuick 500  (prop_delete_lookup :: Str -> T.Trie Int -> QC.Property)
    checkQuick 500  (prop_submap1       :: Str -> T.Trie Int -> Bool)
    checkQuick 500  (prop_submap2       :: Str -> T.Trie Int -> Bool)
    checkQuick 500  (prop_submap3       :: Str -> T.Trie Int -> Bool)
    checkQuick 500  (prop_toList        :: T.Trie Int -> Bool)
    checkQuick 500  (prop_isect         :: T.Trie Int -> T.Trie Int -> Bool)
    checkQuick 500  (prop_fromList_takes_first :: [(Str, Int)] -> Bool)
    checkQuick 500  (prop_fromListR_takes_first :: [(Str, Int)] -> Bool)
    checkQuick 500  (prop_fromListL_takes_first :: [(Str, Int)] -> Bool)
    checkQuick 500  (prop_fromListS_takes_first :: [(Str, Int)] -> Bool)
    checkQuick 500  (prop_fromListWithConst_takes_first :: [(Str, Int)] -> Bool)
    checkQuick 500  (prop_fromListWithLConst_takes_first :: [(Str, Int)] -> Bool)
    putStrLn ""

    putStrLn "smallcheck @ ():" -- Beware the exponential!
    checkSmall 3 (prop_insert        :: Str -> () -> T.Trie () -> Bool)
    checkSmall 7 (prop_singleton     :: Str -> () -> Bool)
    checkSmall 3 (prop_size_insert   :: Str -> () -> T.Trie () -> SC.Property IO)
    checkSmall 3 (prop_size_delete   :: Str -> () -> T.Trie () -> SC.Property IO)
    checkSmall 3 (prop_insert_delete :: Str -> () -> T.Trie () -> SC.Property IO)
    checkSmall 3 (prop_delete_lookup :: Str -> T.Trie () -> SC.Property IO)
    checkSmall 3 (prop_submap1       :: Str -> T.Trie () -> Bool)
    checkSmall 3 (prop_submap2       :: Str -> T.Trie () -> Bool)
    -- checkSmall 3 (prop_submap3 :: Str -> T.Trie () -> Bool)
    {- -- BUG: Needs both instances of Monoid and SC.Serial...
    putStrLn "smallcheck @ Letter:"
    checkSmall 4 (prop_toList        :: T.Trie Letter -> Bool)
    checkSmall 5 (prop_fromList_takes_first :: [(Str, Letter)] -> Bool)
    checkSmall 5 (prop_fromListR_takes_first :: [(Str, Letter)] -> Bool)
    checkSmall 5 (prop_fromListL_takes_first :: [(Str, Letter)] -> Bool)
    checkSmall 5 (prop_fromListS_takes_first :: [(Str, Letter)] -> Bool)
    checkSmall 5 (prop_fromListWithConst_takes_first :: [(Str, Letter)] -> Bool)
    checkSmall 5 (prop_fromListWithLConst_takes_first :: [(Str, Letter)] -> Bool)
    -}
    putStrLn ""
    where
    -- QuickCheck >=2.1.0 && <2.5.0 used 'maxDiscard' instead, which
    -- has a different semantics and which we set to @max 1000 (10*n)@.
    -- But since the cabal file lists QuickCheck-2.10 as the minimum
    -- version, must switch to the new 'maxDiscardRatio' instead.
    checkQuick n =
        QC.quickCheckWith (QC.stdArgs
            { QC.maxSize         = n
            , QC.maxSuccess      = n
            , QC.maxDiscardRatio = 10 `min` round (1000 / fromIntegral n :: Double)
            })
    checkSmall d f = SC.smallCheck d f >> putStrLn ""

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
    , testEqual "unionR regression" (tLeft `T.unionR` tRight) tRightResult
    , testEqual "unionL regression" (tLeft `T.unionL` tRight) tLeftResult
    ]
    where
    e1 = T.singleton S.empty (4::Int)
    e2 = T.singleton S.empty (2::Int)

    -- Regression test against bug filed by Gregory Crosswhite on 2010.06.10 against version 0.2.1.1.
    a, b :: S.ByteString
    a = read "\"\231^\179\160Y\134Gr\158<)&\222\217#\156\""
    b = read "\"\172\193\GSp\222\174GE\186\151\DC1#P\213\147\SI\""
    tLeft   = T.fromList [(a,1::Int),(b,0::Int)]
    tRight  = T.fromList [(a,2::Int)]
    tRightResult = T.fromList [(a,2::Int),(b,0::Int)]
    tLeftResult  = T.fromList [(a,1::Int),(b,0::Int)]


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


test_Delete :: HU.Test
test_Delete = HU.TestLabel "delete"
    $ HU.TestList
    [ testEqual "deleting epsilon from empty trie is empty"
        (T.delete epsilon T.empty) (T.empty :: T.Trie Int)
    ]
    where
    epsilon = packC2W ""

----------------------------------------------------------------
-- TODO: we need a better instance of Arbitrary for lists to make
-- them longer than our smallcheck depth.
--
-- I use strings with characters picked from a very restricted subset
-- in order to have more labels with shared prefixes.
newtype Letter = Letter Char
    deriving (Eq, Ord, Show)

-- Separated from the newtype definition so as not to pollute the
-- derived 'Show' instance.
unLetter :: Letter -> Char
unLetter (Letter c) = c

letters :: [Letter]
letters = Letter <$> ['a'..'m']

instance QC.Arbitrary Letter where
    arbitrary = QC.elements letters

-- TODO: instance QC.CoArbitrary Letter

instance Monad m => SC.Serial m Letter where
    series = SC.generate $ \d -> take d letters

instance Monad m => SC.CoSerial m Letter where
    coseries rs =
        SC.coseries rs SC.>>- \f ->
        return $ \c -> f (unLetter c)

----------------------------------------------------------------
newtype Str = Str { unStr :: S.ByteString }
    deriving (Eq, Ord)

instance Show Str where
    show (Str s) = "Str " ++ show s

packLetters :: [Letter] -> Str
packLetters = Str . packC2W . map unLetter

instance QC.Arbitrary Str where
    arbitrary = QC.sized $ \n -> do
        k <- QC.choose (0,n)
        s <- QC.vector k
        c <- QC.arbitrary -- We only want non-empty strings.
        return $ packLetters (c:s)

-- TODO: instance QC.CoArbitrary Str

instance Monad m => SC.Serial m Str where
    series = packLetters <$> SC.series

instance Monad m => SC.CoSerial m Str where
    coseries rs =
        SC.alts0 rs SC.>>- \z ->
        SC.alts2 rs SC.>>- \f ->
        return $ \(Str xs) ->
            if S.null xs
            then z
            else f (Letter . S.w2c $ S.head xs) (Str $ S.tail xs)

----------------------------------------------------------------
instance (QC.Arbitrary a) => QC.Arbitrary (T.Trie a) where
    arbitrary = QC.sized $ \n -> do
        k      <- QC.choose (0,n)
        labels <- map unStr <$> QC.vector k
        elems  <- QC.vector k
        return . T.fromList $ zip labels elems

-- TODO: instance QC.CoArbitrary (Trie a)

-- TODO: This instance really needs some work. The smart constructures
-- ensure only valid values are generated, but there are redundancies
-- and inefficiencies.
instance (Monad m, Monoid a, SC.Serial m a) => SC.Serial m (T.Trie a) where
    series =   SC.cons0 T.empty
        SC.\/  SC.cons3 arcHACK
        SC.\/  SC.cons2 mappend
        where
        arcHACK (Str k) Nothing  t = T.singleton k () >> t
        arcHACK (Str k) (Just v) t = T.singleton k v
                                        >>= T.unionR t . T.singleton S.empty

-- TODO: instance Monad m => SC.CoSerial m (T.Trie a)

----------------------------------------------------------------
----------------------------------------------------------------
-- | If you insert a value, you can look it up
prop_insert :: (Eq a) => Str -> a -> T.Trie a -> Bool
prop_insert (Str k) v t =
    (T.lookup k . T.insert k v $ t) == Just v

-- | A singleton, is.
prop_singleton :: (Eq a) => Str -> a -> Bool
prop_singleton (Str k) v =
    T.insert k v T.empty == T.singleton k v

-- | Deal with QC/SC polymorphism issues because of (==>)
-- Fundeps would be nice here, but |b->a is undecidable, and |a->b is wrong
class CheckGuard a b where
    (==>) :: Bool -> a -> b

instance (QC.Testable a) => CheckGuard a QC.Property where
    (==>) = (QC.==>)

instance (Monad m, SC.Testable m a) => CheckGuard a (SC.Property m) where
    (==>) = (SC.==>)

prop_size_insert :: (Eq a, CheckGuard Bool b) => Str -> a -> T.Trie a -> b
prop_size_insert (Str k) v t = not (k `T.member` t) ==> (
    (T.size . T.insert k v) === ((1+) . T.size)
    $ t)

prop_size_delete :: (Eq a, CheckGuard Bool b) => Str -> a -> T.Trie a -> b
prop_size_delete (Str k) v t = not (k `T.member` t) ==> (
    (T.size . T.delete k . T.insert k v) === T.size
    $ t)

prop_insert_delete :: (Eq a, CheckGuard Bool b) => Str -> a -> T.Trie a -> b
prop_insert_delete (Str k) v t = not (k `T.member` t) ==> (
    (T.delete k . T.insert k v) === id
    $ t)

prop_delete_lookup :: (Eq a, CheckGuard Bool b) => Str -> T.Trie a -> b
prop_delete_lookup (Str k) t = not (k `T.member` t) ==> (
    (T.lookup k . T.delete k) === const Nothing
    $ t)

-- | All keys in a submap are keys in the supermap
prop_submap1 :: Str -> T.Trie a -> Bool
prop_submap1 (Str k) t =
    all (`T.member` t) . T.keys . T.submap k $ t

-- | All keys in a submap have the query as a prefix
prop_submap2 :: Str -> T.Trie a -> Bool
prop_submap2 (Str k) t =
    all (S.isPrefixOf k) . T.keys . T.submap k $ t

-- | All values in a submap are the same in the supermap
prop_submap3 :: (Eq a) => Str -> T.Trie a -> Bool
prop_submap3 (Str k) t =
    (\q -> T.lookup q t' == T.lookup q t) `all` T.keys t'
    where t' = T.submap k t

prop_isect :: (Eq a) => T.Trie a -> T.Trie a -> Bool
prop_isect t0 t1 =
  TI.intersectBy (\a _ -> Just a) t0 t1 == TC.disunion (T.unionL t0 t1) (TC.disunion t0 t1)

-- | Keys are ordered when converting to a list
prop_toList :: T.Trie a -> Bool
prop_toList t = ordered (T.keys t)
    where ordered xs = and (zipWith (<=) xs (drop 1 xs))

_takes_first :: (Eq c) => ([(S.ByteString, c)] -> T.Trie c) -> [(Str, c)] -> Bool
_takes_first f assocs =
    (T.toList . f) === (nubBy (apFst (==)) . sortBy (comparing fst))
    $ map (first unStr) assocs

-- | 'fromList' takes the first value for a given key
prop_fromList_takes_first :: (Eq a) => [(Str, a)] -> Bool
prop_fromList_takes_first = _takes_first T.fromList

-- | 'fromListR' takes the first value for a given key
prop_fromListR_takes_first :: (Eq a) => [(Str, a)] -> Bool
prop_fromListR_takes_first = _takes_first TC.fromListR

-- | 'fromListL' takes the first value for a given key
prop_fromListL_takes_first :: (Eq a) => [(Str, a)] -> Bool
prop_fromListL_takes_first = _takes_first TC.fromListL

-- | 'fromListS' takes the first value for a given key
prop_fromListS_takes_first :: (Eq a) => [(Str, a)] -> Bool
prop_fromListS_takes_first = _takes_first TC.fromListS

-- | @('fromListWith' const)@ takes the first value for a given key
prop_fromListWithConst_takes_first :: (Eq a) => [(Str, a)] -> Bool
prop_fromListWithConst_takes_first = _takes_first (TC.fromListWith const)

-- | @('fromListWithL' const)@ takes the first value for a given key
prop_fromListWithLConst_takes_first :: (Eq a) => [(Str, a)] -> Bool
prop_fromListWithLConst_takes_first = _takes_first (TC.fromListWithL const)

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
