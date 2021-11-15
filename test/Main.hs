{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
{-# LANGUAGE CPP
           , MultiParamTypeClasses
           , FlexibleInstances
           , FlexibleContexts
           , TypeSynonymInstances
           #-}

----------------------------------------------------------------
--                                                  ~ 2021.11.15
-- |
-- Module      :  test/Main
-- Copyright   :  Copyright (c) 2008--2021 wren gayle romano
-- License     :  BSD3
-- Maintainer  :  wren@cpan.org
-- Stability   :  provisional
-- Portability :  semi-portable (MPTC,...)
--
-- Testing 'Trie's.
----------------------------------------------------------------
module Main (main) where

import Utils

import qualified Data.Trie              as T
import qualified Data.Trie.Convenience  as TC
import qualified Data.ByteString        as S

import qualified Test.Tasty             as Tasty
import qualified Test.Tasty.SmallCheck  as SC
import qualified Test.Tasty.QuickCheck  as QC

import Data.List (nubBy, sortBy)
import Data.Ord  (comparing)
----------------------------------------------------------------
----------------------------------------------------------------

main :: IO ()
main = Tasty.defaultMain tests

tests :: Tasty.TestTree
tests =
  Tasty.testGroup "All Tests"
  [ hunitTests
  , Tasty.testGroup "Properties"
    [ quickcheckTests
    , smallcheckTests
    ]
  ]

hunitTests :: Tasty.TestTree
hunitTests =
  Tasty.testGroup "HUnit"
  [ test_Union
  , test_Intersect
  , test_Submap
  , test_Insert
  , test_Delete
  ]

{-
-- TODO: make this work
data Prop :: * -> * where
    Prop :: (QC.Testable a, SC.Testable IO a) => String -> a -> Prop a

data PropChecker = QuickCheck | SmallCheck
    deriving (Eq, Show)

testProp :: PropChecker -> Prop a -> Tasty.TestTree
testProp QuickCheck (Prop name a) = QC.testProperty name a
testProp SmallCheck (Prop name a) = SC.testProperty name a
-}

quickcheckTests :: Tasty.TestTree
quickcheckTests
  = Tasty.localOption (QC.QuickCheckTests    500) -- QC.Args.maxSuccess
  . Tasty.localOption (QC.QuickCheckMaxSize  500) -- QC.Args.maxSize
  . Tasty.localOption (QC.QuickCheckMaxRatio 10)  -- QC.Args.maxDiscardRatio
  $ Tasty.testGroup "QuickCheck"
  [ Tasty.testGroup "@Int"
    [ QC.testProperty
        "prop_insert"
        (prop_insert        :: Str -> Int -> T.Trie Int -> Bool)
    , QC.testProperty
        -- TODO: this one needs a bigger budget to be effective.
        "prop_singleton"
        (prop_singleton     :: Str -> Int -> Bool)
    , QC.testProperty
        "prop_size_insert"
        (prop_size_insert   :: Str -> Int -> T.Trie Int -> QC.Property)
    , QC.testProperty
        "prop_size_delete"
        (prop_size_delete   :: Str -> Int -> T.Trie Int -> QC.Property)
    , QC.testProperty
        "prop_insert_delete"
        (prop_insert_delete :: Str -> Int -> T.Trie Int -> QC.Property)
    , QC.testProperty
        "prop_delete_lookup"
        (prop_delete_lookup :: Str -> T.Trie Int -> QC.Property)
    , QC.testProperty
        "prop_submap1"
        (prop_submap1       :: Str -> T.Trie Int -> Bool)
    , QC.testProperty
        "prop_submap2"
        (prop_submap2       :: Str -> T.Trie Int -> Bool)
    , QC.testProperty
        "prop_submap3"
        (prop_submap3       :: Str -> T.Trie Int -> Bool)
    --
    , QC.testProperty
        "prop_intersectL"
        (prop_intersectL    :: T.Trie Int -> T.Trie Int -> Bool)
    , QC.testProperty
        "prop_intersectR"
        (prop_intersectR    :: T.Trie Int -> T.Trie Int -> Bool)
    , QC.testProperty
        "prop_intersectPlus"
        (prop_intersectPlus :: T.Trie Int -> T.Trie Int -> Bool)
    --
    , QC.testProperty
        "prop_toList"
        (prop_toList        :: T.Trie Int -> Bool)
    , QC.testProperty
        "prop_fromList_takes_first"
        (prop_fromList_takes_first :: [(Str, Int)] -> Bool)
    , QC.testProperty
        "prop_fromListR_takes_first"
        (prop_fromListR_takes_first :: [(Str, Int)] -> Bool)
    , QC.testProperty
        "prop_fromListL_takes_first"
        (prop_fromListL_takes_first :: [(Str, Int)] -> Bool)
    , QC.testProperty
        "prop_fromListS_takes_first"
        (prop_fromListS_takes_first :: [(Str, Int)] -> Bool)
    , QC.testProperty
        "prop_fromListWithConst_takes_first"
        (prop_fromListWithConst_takes_first :: [(Str, Int)] -> Bool)
    , QC.testProperty
        "prop_fromListWithLConst_takes_first"
        (prop_fromListWithLConst_takes_first :: [(Str, Int)] -> Bool)
    ]
  ]

smallcheckTests :: Tasty.TestTree
smallcheckTests
  = Tasty.localOption (SC.SmallCheckDepth 3)
  $ Tasty.testGroup "SmallCheck"
  [ Tasty.testGroup "@()"
    -- These use @()@ to reduce the problem of exponential growth.
    [ SC.testProperty
        "prop_insert"
        (prop_insert        :: Str -> () -> T.Trie () -> Bool)
    , Tasty.localOption (SC.SmallCheckDepth 7)
      $ SC.testProperty
        "prop_singleton"
        (prop_singleton     :: Str -> () -> Bool)
    , SC.testProperty
        "prop_size_insert"
        (prop_size_insert   :: Str -> () -> T.Trie () -> SC.Property IO)
    , SC.testProperty
        "prop_size_delete"
        (prop_size_delete   :: Str -> () -> T.Trie () -> SC.Property IO)
    , SC.testProperty
        "prop_insert_delete"
        (prop_insert_delete :: Str -> () -> T.Trie () -> SC.Property IO)
    , SC.testProperty
        "prop_delete_lookup"
        (prop_delete_lookup :: Str -> T.Trie () -> SC.Property IO)
    , SC.testProperty
        "prop_submap1"
        (prop_submap1       :: Str -> T.Trie () -> Bool)
    , SC.testProperty
        "prop_submap2"
        (prop_submap2       :: Str -> T.Trie () -> Bool)
    {- -- Was commented out for some reason...
    , SC.testProperty
        "prop_submap3"
        (prop_submap3       :: Str -> T.Trie () -> Bool)
    -}
    ]
  {- -- BUG: requires (Monoid Int) because of our (SC.Serial m (T.Trie a)) instance.
  , Tasty.testGroup "@Int"
    [ SC.testProperty
        "prop_intersectL"
        (prop_intersectL    :: T.Trie Int -> T.Trie Int -> Bool)
    , SC.testProperty
        "prop_intersectR"
        (prop_intersectR    :: T.Trie Int -> T.Trie Int -> Bool)
    , SC.testProperty
        "prop_intersectPlus"
        (prop_intersectPlus :: T.Trie Int -> T.Trie Int -> Bool)
    ]
  -}
  {- -- BUG: requires (Monoid Letter) because of our (SC.Serial m (T.Trie a)) instance.
  , Tasty.testGroup "@Letter"
    [ SC.testProperty
        "prop_toList"
        (prop_toList        :: T.Trie Letter -> Bool)
    , SC.testProperty
        "prop_fromList_takes_first"
        (prop_fromList_takes_first :: [(Str, Letter)] -> Bool)
    , SC.testProperty
        "prop_fromListR_takes_first"
        (prop_fromListR_takes_first :: [(Str, Letter)] -> Bool)
    , SC.testProperty
        "prop_fromListL_takes_first"
        (prop_fromListL_takes_first :: [(Str, Letter)] -> Bool)
    , SC.testProperty
        "prop_fromListS_takes_first"
        (prop_fromListS_takes_first :: [(Str, Letter)] -> Bool)
    , SC.testProperty
        "prop_fromListWithConst_takes_first"
        (prop_fromListWithConst_takes_first :: [(Str, Letter)] -> Bool)
    , SC.testProperty
        "prop_fromListWithLConst_takes_first"
        (prop_fromListWithLConst_takes_first :: [(Str, Letter)] -> Bool)
    ]
    -}
  ]


----------------------------------------------------------------
----------------------------------------------------------------
-- Because we avoid epsilons everywhere else, need to make sure
-- 'mergeBy' gets it right
test_Union :: Tasty.TestTree
test_Union =
    Tasty.testGroup "mergeBy"
    [ testEqual "unionL epsilon" (e1 `T.unionL` e2) e1
    , testEqual "unionR epsilon" (e1 `T.unionR` e2) e2
    , testEqual "unionL regression" (tLeft `T.unionL` tRight) tLeftResult
    , testEqual "unionR regression" (tLeft `T.unionR` tRight) tRightResult
    ]
    where
    e1 = T.singleton S.empty (4::Int)
    e2 = T.singleton S.empty (2::Int)

    -- Regression test against bug filed by Gregory Crosswhite on
    -- 2010.06.10 against version 0.2.1.1.
    a, b :: S.ByteString
    a = read "\"\231^\179\160Y\134Gr\158<)&\222\217#\156\""
    b = read "\"\172\193\GSp\222\174GE\186\151\DC1#P\213\147\SI\""
    tLeft, tRight, tRightResult, tLeftResult :: T.Trie Int
    tLeft        = T.fromList [(a,1), (b,0)]
    tRight       = T.fromList [(a,2)]
    tRightResult = T.fromList [(a,2), (b,0)]
    tLeftResult  = T.fromList [(a,1), (b,0)]

test_Intersect :: Tasty.TestTree
test_Intersect =
    Tasty.testGroup "intersectBy"
    [ testEqual "intersectL" (tLeft `T.intersectL` tRight) tLeftResult
    , testEqual "intersectR" (tLeft `T.intersectR` tRight) tRightResult
    ]
    where
    -- Trivial regression example
    -- BUG: why doesn't this catch that same minimal error that QuickCheck originally gave us?
    a, b, c :: S.ByteString
    a = packC2W "a"
    b = packC2W "b"
    c = packC2W "c"
    tLeft, tRight, tRightResult, tLeftResult :: T.Trie Int
    tLeft        = T.fromList [(a,0), (b,2)]
    tRight       = T.fromList [(a,1), (c,3)]
    tLeftResult  = T.fromList [(a,0)]
    tRightResult = T.fromList [(a,1)]
    -- TODO: better unit test for when one string is prefix of another.

----------------------------------------------------------------
test_Submap :: Tasty.TestTree
test_Submap =
    Tasty.testGroup "submap"
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
test_Insert :: Tasty.TestTree
test_Insert =
    Tasty.testGroup "insert"
    [ testEqual "insertion is commutative for prefix/superfix"
        (T.insert aba o $ T.insert abaissed i $ T.empty)
        (T.insert abaissed i $ T.insert aba o $ T.empty)
    ]
    where
    aba      = packC2W "aba"
    abaissed = packC2W "abaissed"

    o = 0::Int
    i = 1::Int


test_Delete :: Tasty.TestTree
test_Delete =
    Tasty.testGroup "delete"
    [ testEqual "deleting epsilon from empty trie is empty"
        (T.delete epsilon T.empty) (T.empty :: T.Trie Int)
    ]
    where
    -- TODO: why not 'S.empty'?
    epsilon = packC2W ""

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

-- | Left-biased @x ∩ y == (x ∪ y) ⋈ (x ⋈ y)@.
prop_intersectL :: (Eq a) => T.Trie a -> T.Trie a -> Bool
prop_intersectL x y =
    T.intersectL x y == (T.unionL x y `TC.disunion` TC.disunion x y)

-- | Right-biased @x ∩ y == (x ∪ y) ⋈ (x ⋈ y)@.
prop_intersectR :: (Eq a) => T.Trie a -> T.Trie a -> Bool
prop_intersectR x y =
    T.intersectR x y == (T.unionR x y `TC.disunion` TC.disunion x y)

-- | Additive @x ∩ y == (x ∪ y) ⋈ (x ⋈ y)@.
prop_intersectPlus :: (Eq a, Num a) => T.Trie a -> T.Trie a -> Bool
prop_intersectPlus x y =
    T.intersectBy plus x y
    == (T.mergeBy plus x y `TC.disunion` TC.disunion x y)
    where
    plus a b = Just (a + b)

-- TODO: actually use this somewhere, or give GHC a pragma to silence
-- the unused-warning; so that out CI summary is clean enough to
-- find real warnings.
-- TODO: Can we effectively generate interesting enough functions
-- to make this worth using?
--
-- | Arbitrary @x ∩ y == (x ∪ y) ⋈ (x ⋈ y)@.
prop_intersectBy :: (Eq a) => (a -> a -> Maybe a) -> T.Trie a -> T.Trie a -> Bool
prop_intersectBy f x y =
    T.intersectBy f x y == (T.mergeBy f x y `TC.disunion` TC.disunion x y)


-- | Keys are ordered when converting to a list
prop_toList :: T.Trie a -> Bool
prop_toList t = ordered (T.keys t)
    where ordered xs = and (zipWith (<=) xs (drop 1 xs))

_takes_first :: (Eq c) => ([(S.ByteString, c)] -> T.Trie c) -> [(Str, c)] -> Bool
_takes_first f assocs =
    (T.toList . f) === (nubBy (apFst (==)) . sortBy (comparing fst))
    $ map (first unStr) assocs

-- | Lift a function to apply to the first of pairs, retaining the second.
first :: (a -> b) -> (a,c) -> (b,c)
first f (x,y) = (f x, y)

-- | Lift a binary function to apply to the first of pairs, discarding seconds.
apFst :: (a -> b -> c) -> ((a,d) -> (b,e) -> c)
apFst f (x,_) (y,_) = f x y

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
----------------------------------------------------------- fin.
