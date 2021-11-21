{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
{-# LANGUAGE CPP
           , MultiParamTypeClasses
           , FlexibleInstances
           , FlexibleContexts
           , TypeSynonymInstances
           #-}

----------------------------------------------------------------
--                                                  ~ 2021.11.20
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

import qualified System.Exit            as System (exitSuccess, exitFailure)
import qualified System.IO              as System (hPutStrLn, stderr)
import qualified Test.Tasty.Ingredients as Tasty (tryIngredients)
import qualified Test.Tasty.Options     as Tasty (singleOption, OptionSet)
import qualified Test.Tasty.Runners     as Tasty (installSignalHandlers, parseOptions)
import qualified Test.Tasty             as Tasty
import qualified Test.Tasty.SmallCheck  as SC
import qualified Test.Tasty.QuickCheck  as QC

import Data.List (nubBy, sortBy)
import Data.Ord  (comparing)
----------------------------------------------------------------
----------------------------------------------------------------

-- We can't use 'Tasty.defaultMain' together with 'Tasty.localOption',
-- because what we want to do is to set new defaults but still allow
-- the commandline to override those defaults, and I don't see any
-- way to do that with 'Tasty.defaultMain'.
--
-- TODO: Still need some way to remove the few remaining
-- 'Tasty.localOption' calls that aren't global...
main :: IO ()
main = do
  let ins = Tasty.defaultIngredients
  Tasty.installSignalHandlers
  opts <- Tasty.parseOptions ins tests
  case Tasty.tryIngredients ins (globalOptions <> opts) tests of
    Nothing -> do
      System.hPutStrLn System.stderr
        "No ingredients agreed to run. Something is wrong."
      System.exitFailure
    Just act -> do
      ok <- act
      if ok
        then System.exitSuccess
        else System.exitFailure

-- We add the timeout for the sake of GithubActions CI, so we don't
-- accidentally blow our budget.  Since 'tests' actually runs in
-- around 7~8sec normally (or 10sec with HPC enabled), allowing
-- 30sec is more than generous.
globalOptions :: Tasty.OptionSet
globalOptions = mconcat
    [ Tasty.singleOption (Tasty.mkTimeout 30000000)  -- 30sec; in microsecs
    , Tasty.singleOption (QC.QuickCheckTests    500) -- QC.Args.maxSuccess
    , Tasty.singleOption (QC.QuickCheckMaxSize  400) -- QC.Args.maxSize
    , Tasty.singleOption (QC.QuickCheckMaxRatio 10)  -- QC.Args.maxDiscardRatio
    , Tasty.singleOption (SC.SmallCheckDepth    3)
    ]

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
TODO: see if we can't figure out some way of wrapping our properties
so that we can just write this list once and then pass in a token
for which checker to resolve to; something like:

    data Prop a = Prop String a

    data PropChecker = QuickCheck | SmallCheck
        deriving (Eq, Show)

    testProp :: (QC.Testable a, SC.Testable IO a) => PropChecker -> Prop a -> Tasty.TestTree
    testProp QuickCheck (Prop name a) = QC.testProperty name a
    testProp SmallCheck (Prop name a) = SC.testProperty name a

Of course, the problem with that implementation is that we need to
have the Prop remain polymorphic in the CheckGuard type, and have
testProp resole it depending on the PropChecker.  So is there a way
to do that without GADTs or impredicativity?
-}

quickcheckTests :: Tasty.TestTree
quickcheckTests
  = Tasty.testGroup "QuickCheck"
  [ Tasty.testGroup "Trivial properties (@Int)"
    [ QC.testProperty
        "prop_insert"
        (prop_insert        :: WS -> Int -> WTrie Int -> Bool)
    , QC.testProperty
        "prop_singleton"
        (prop_singleton     :: WS -> Int -> Bool)
    , QC.testProperty
        "prop_size_insert"
        (prop_size_insert   :: WS -> Int -> WTrie Int -> QC.Property)
    , QC.testProperty
        "prop_size_delete"
        (prop_size_delete   :: WS -> Int -> WTrie Int -> QC.Property)
    , QC.testProperty
        "prop_insert_delete"
        (prop_insert_delete :: WS -> Int -> WTrie Int -> QC.Property)
    , QC.testProperty
        "prop_delete_lookup"
        (prop_delete_lookup :: WS -> WTrie Int -> QC.Property)
    ]
  , Tasty.testGroup "Submap properties (@Int)"
    [ QC.testProperty
        "prop_submap_keysAreMembers"
        (prop_submap_keysAreMembers :: WS -> WTrie Int -> Bool)
    , QC.testProperty
        "prop_submap_keysHavePrefix"
        (prop_submap_keysHavePrefix :: WS -> WTrie Int -> Bool)
    , QC.testProperty
        "prop_submap_valuesEq"
        (prop_submap_valuesEq       :: WS -> WTrie Int -> Bool)
    , QC.testProperty
        "prop_deleteSubmap_keysAreMembers"
        (prop_deleteSubmap_keysAreMembers :: WS -> WTrie Int -> Bool)
    , QC.testProperty
        "prop_deleteSubmap_keysLackPrefix"
        (prop_deleteSubmap_keysLackPrefix :: WS -> WTrie Int -> Bool)
    , QC.testProperty
        "prop_deleteSubmap_disunion"
        (prop_deleteSubmap_disunion :: WS -> WTrie Int -> Bool)
    ]
  , Tasty.localOption (QC.QuickCheckMaxSize 300)
    -- BUG: fix that 'Tasty.localOption'
  $ Tasty.testGroup "Intersection properties (@Int)"
    [ QC.testProperty
        "prop_intersectL"
        (prop_intersectL    :: WTrie Int -> WTrie Int -> Bool)
    , QC.testProperty
        "prop_intersectR"
        (prop_intersectR    :: WTrie Int -> WTrie Int -> Bool)
    , QC.testProperty
        "prop_intersectPlus"
        (prop_intersectPlus :: WTrie Int -> WTrie Int -> Bool)
    ]
  , Tasty.testGroup "List-conversion properties (@Int)"
    [ QC.testProperty
        "prop_toList"
        (prop_toList        :: WTrie Int -> Bool)
    , QC.testProperty
        "prop_fromList_takes_first"
        (prop_fromList_takes_first :: [(WS, Int)] -> Bool)
    , QC.testProperty
        "prop_fromListR_takes_first"
        (prop_fromListR_takes_first :: [(WS, Int)] -> Bool)
    , QC.testProperty
        "prop_fromListL_takes_first"
        (prop_fromListL_takes_first :: [(WS, Int)] -> Bool)
    , QC.testProperty
        "prop_fromListS_takes_first"
        (prop_fromListS_takes_first :: [(WS, Int)] -> Bool)
    , QC.testProperty
        "prop_fromListWithConst_takes_first"
        (prop_fromListWithConst_takes_first :: [(WS, Int)] -> Bool)
    , QC.testProperty
        "prop_fromListWithLConst_takes_first"
        (prop_fromListWithLConst_takes_first :: [(WS, Int)] -> Bool)
    ]
  ]

smallcheckTests :: Tasty.TestTree
smallcheckTests
  = Tasty.testGroup "SmallCheck"
  [ Tasty.testGroup "Trivial properties (@W)"
    -- These use @()@ to reduce the problem of exponential growth.
    -- Depth=4 is marginal here (takes about 2sec each)
    [ SC.testProperty
        "prop_insert"
        (prop_insert        :: WS -> W -> WTrie W -> Bool)
    , SC.testProperty
        -- This one can easily handle depth=7 (takes about 0.15sec)
        "prop_singleton"
        (prop_singleton     :: WS -> W -> Bool)
    , SC.testProperty
        "prop_size_insert"
        (prop_size_insert   :: WS -> W -> WTrie W -> SC.Property IO)
    , SC.testProperty
        "prop_size_delete"
        (prop_size_delete   :: WS -> W -> WTrie W -> SC.Property IO)
    , SC.testProperty
        "prop_insert_delete"
        (prop_insert_delete :: WS -> W -> WTrie W -> SC.Property IO)
    , SC.testProperty
        "prop_delete_lookup"
        (prop_delete_lookup :: WS -> WTrie W -> SC.Property IO)
    ]
  , Tasty.testGroup "Submap properties (@W)"
    -- Depth=4 is okay here (takes about 0.5sec each)
    [ SC.testProperty
        "prop_submap_keysAreMembers"
        (prop_submap_keysAreMembers :: WS -> WTrie W -> Bool)
    , SC.testProperty
        "prop_submap_keysHavePrefix"
        (prop_submap_keysHavePrefix :: WS -> WTrie W -> Bool)
    , SC.testProperty
        "prop_submap_valuesEq"
        (prop_submap_valuesEq       :: WS -> WTrie W -> Bool)
    , SC.testProperty
        "prop_deleteSubmap_keysAreMembers"
        (prop_deleteSubmap_keysAreMembers :: WS -> WTrie W -> Bool)
    , SC.testProperty
        "prop_deleteSubmap_keysLackPrefix"
        (prop_deleteSubmap_keysLackPrefix :: WS -> WTrie W -> Bool)
    , SC.testProperty
        "prop_deleteSubmap_disunion"
        (prop_deleteSubmap_disunion :: WS -> WTrie W -> Bool)
    ]
  , Tasty.testGroup "Intersection properties (@W/@Int)"
    -- Warning: Using depth=4 here is bad (the first two take about
    -- 26.43sec; the last one much longer).
    [ SC.testProperty
        "prop_intersectL"
        (prop_intersectL    :: WTrie W -> WTrie W -> Bool)
    , SC.testProperty
        "prop_intersectR"
        (prop_intersectR    :: WTrie W -> WTrie W -> Bool)
    , SC.testProperty
        "prop_intersectPlus"
        (prop_intersectPlus :: WTrie Int -> WTrie Int -> Bool)
    ]
  , Tasty.testGroup "List-conversion properties (@W)"
    [ SC.testProperty
        "prop_toList"
        (prop_toList        :: WTrie W -> Bool)
    , SC.testProperty
        "prop_fromList_takes_first"
        (prop_fromList_takes_first :: [(WS, W)] -> Bool)
    , SC.testProperty
        "prop_fromListR_takes_first"
        (prop_fromListR_takes_first :: [(WS, W)] -> Bool)
    , SC.testProperty
        "prop_fromListL_takes_first"
        (prop_fromListL_takes_first :: [(WS, W)] -> Bool)
    , SC.testProperty
        "prop_fromListS_takes_first"
        (prop_fromListS_takes_first :: [(WS, W)] -> Bool)
    , SC.testProperty
        "prop_fromListWithConst_takes_first"
        (prop_fromListWithConst_takes_first :: [(WS, W)] -> Bool)
    , SC.testProperty
        "prop_fromListWithLConst_takes_first"
        (prop_fromListWithLConst_takes_first :: [(WS, W)] -> Bool)
    ]
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
    a, b, c :: S.ByteString
    a = packC2W "a"
    b = packC2W "b"
    c = packC2W "c"
    tLeft, tRight, tRightResult, tLeftResult :: T.Trie Int
    tLeft        = T.fromList [(S.empty,0), (a,1), (b,3)]
    tRight       = T.fromList [             (a,2), (c,4)]
    tLeftResult  = T.fromList [(a,1)]
    tRightResult = T.fromList [(a,2)]
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
prop_insert :: (Eq a) => WS -> a -> WTrie a -> Bool
prop_insert (WS k) v (WT t) =
    (T.lookup k . T.insert k v $ t) == Just v

-- | A singleton, is.
prop_singleton :: (Eq a) => WS -> a -> Bool
prop_singleton (WS k) v =
    T.insert k v T.empty == T.singleton k v

prop_size_insert :: (Eq a, CheckGuard Bool b) => WS -> a -> WTrie a -> b
prop_size_insert (WS k) v =
    ((not . T.member k) .==>. (T.size . T.insert k v) .==. ((1+) . T.size)) . unWT

prop_size_delete :: (Eq a, CheckGuard Bool b) => WS -> a -> WTrie a -> b
prop_size_delete (WS k) v =
    ((not . T.member k) .==>. (T.size . T.delete k . T.insert k v) .==. T.size) . unWT

prop_insert_delete :: (Eq a, CheckGuard Bool b) => WS -> a -> WTrie a -> b
prop_insert_delete (WS k) v =
    ((not . T.member k) .==>. (T.delete k . T.insert k v) .==. id) . unWT

prop_delete_lookup :: (Eq a, CheckGuard Bool b) => WS -> WTrie a -> b
prop_delete_lookup (WS k) =
    ((not . T.member k) .==>. (T.lookup k . T.delete k) .==. const Nothing) . unWT

-- | All keys in a submap are keys in the supermap
prop_submap_keysAreMembers :: WS -> WTrie a -> Bool
prop_submap_keysAreMembers (WS k) (WT t) =
    all (`T.member` t) . T.keys . T.submap k $ t

-- | All keys in a submap have the query as a prefix
prop_submap_keysHavePrefix :: WS -> WTrie a -> Bool
prop_submap_keysHavePrefix (WS k) (WT t) =
    all (S.isPrefixOf k) . T.keys . T.submap k $ t

-- | All values in a submap are the same in the supermap
prop_submap_valuesEq :: (Eq a) => WS -> WTrie a -> Bool
prop_submap_valuesEq (WS k) (WT t) =
    ((`T.lookup` t') .==. (`T.lookup` t)) `all` T.keys t'
    where t' = T.submap k t

-- | All keys in the result are keys in the supermap
prop_deleteSubmap_keysAreMembers :: WS -> WTrie a -> Bool
prop_deleteSubmap_keysAreMembers (WS k) (WT t) =
    all (`T.member` t) . T.keys . T.deleteSubmap k $ t

-- | All keys in a submap lack the query as a prefix
prop_deleteSubmap_keysLackPrefix :: WS -> WTrie a -> Bool
prop_deleteSubmap_keysLackPrefix (WS k) (WT t) =
    all (not . S.isPrefixOf k) . T.keys . T.deleteSubmap k $ t

-- | `T.submap` and `T.deleteSubmap` partition every trie for every key.
prop_deleteSubmap_disunion :: (Eq a) => WS -> WTrie a -> Bool
prop_deleteSubmap_disunion (WS k) (WT t) =
    t == (T.submap k t `TC.disunion` T.deleteSubmap k t)

-- | Left-biased @x ∩ y == (x ∪ y) ⋈ (x ⋈ y)@.
prop_intersectL :: (Eq a) => WTrie a -> WTrie a -> Bool
prop_intersectL (WT x) (WT y) =
    T.intersectL x y == (T.unionL x y `TC.disunion` TC.disunion x y)

-- | Right-biased @x ∩ y == (x ∪ y) ⋈ (x ⋈ y)@.
prop_intersectR :: (Eq a) => WTrie a -> WTrie a -> Bool
prop_intersectR (WT x) (WT y) =
    T.intersectR x y == (T.unionR x y `TC.disunion` TC.disunion x y)

-- | Additive @x ∩ y == (x ∪ y) ⋈ (x ⋈ y)@.
prop_intersectPlus :: (Eq a, Num a) => WTrie a -> WTrie a -> Bool
prop_intersectPlus (WT x) (WT y) =
    T.intersectBy plus x y
    == (T.mergeBy plus x y `TC.disunion` TC.disunion x y)
    where
    plus a b = Just (a + b)

{-
-- TODO: actually use this. Until then, commented out to silence
-- the warnings from out CI summary.
-- TODO: Can we effectively generate interesting enough functions
-- to make this worth using?
--
-- | Arbitrary @x ∩ y == (x ∪ y) ⋈ (x ⋈ y)@.
prop_intersectBy :: (Eq a) => (a -> a -> Maybe a) -> WTrie a -> WTrie a -> Bool
prop_intersectBy f (WT x) (WT y) =
    T.intersectBy f x y == (T.mergeBy f x y `TC.disunion` TC.disunion x y)
-}


-- | Keys are ordered when converting to a list
prop_toList :: WTrie a -> Bool
prop_toList = ordered . T.keys . unWT
    where ordered xs = and (zipWith (<=) xs (drop 1 xs))

-- | If there are duplicate keys in the @assocs@, then @f@ will
-- take the first value.
_takes_first :: (Eq c) => ([(S.ByteString, c)] -> T.Trie c) -> [(WS, c)] -> Bool
_takes_first f assocs =
    (T.toList . f) .==. (nubBy (apFst (==)) . sortBy (comparing fst))
    $ map (first unWS) assocs

-- | Lift a function to apply to the first of pairs, retaining the second.
first :: (a -> b) -> (a,c) -> (b,c)
first f (x,y) = (f x, y)

-- | Lift a binary function to apply to the first of pairs, discarding seconds.
apFst :: (a -> b -> c) -> ((a,d) -> (b,e) -> c)
apFst f (x,_) (y,_) = f x y

-- | 'T.fromList' takes the first value for a given key.
prop_fromList_takes_first :: (Eq a) => [(WS, a)] -> Bool
prop_fromList_takes_first = _takes_first T.fromList

-- | 'T.fromListR' takes the first value for a given key.
prop_fromListR_takes_first :: (Eq a) => [(WS, a)] -> Bool
prop_fromListR_takes_first = _takes_first TC.fromListR

-- | 'T.fromListL' takes the first value for a given key.
prop_fromListL_takes_first :: (Eq a) => [(WS, a)] -> Bool
prop_fromListL_takes_first = _takes_first TC.fromListL

-- | 'T.fromListS' takes the first value for a given key.
prop_fromListS_takes_first :: (Eq a) => [(WS, a)] -> Bool
prop_fromListS_takes_first = _takes_first TC.fromListS

-- | @('TC.fromListWith' const)@ takes the first value for a given key.
prop_fromListWithConst_takes_first :: (Eq a) => [(WS, a)] -> Bool
prop_fromListWithConst_takes_first = _takes_first (TC.fromListWith const)

-- | @('TC.fromListWithL' const)@ takes the first value for a given key.
prop_fromListWithLConst_takes_first :: (Eq a) => [(WS, a)] -> Bool
prop_fromListWithLConst_takes_first = _takes_first (TC.fromListWithL const)

----------------------------------------------------------------
----------------------------------------------------------- fin.
