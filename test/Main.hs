{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
{-# LANGUAGE CPP
           , MultiParamTypeClasses
           , FlexibleContexts
           #-}

----------------------------------------------------------------
--                                                  ~ 2021.11.21
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
import qualified Data.Trie.Internal     as TI
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
#if MIN_VERSION_base(4,13,0)
-- [aka GHC 8.8.1] @(<>)@ is re-exported from the Prelude.
#elif MIN_VERSION_base(4,9,0)
import Data.Semigroup      ((<>))
#elif MIN_VERSION_base(4,5,0)
import Data.Monoid         ((<>))
#endif
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
  , Tasty.testGroup "match/matches properties (@Int)"
    [ QC.testProperty
        "prop_matches_keysOrdered"
        (prop_matches_keysOrdered   :: WS -> WTrie Int -> Bool)
    , QC.testProperty
        "prop_matches_keysArePrefix"
        (prop_matches_keysArePrefix :: WS -> WTrie Int -> Bool)
    , QC.testProperty
        "prop_minMatch_is_first_matches"
        (prop_minMatch_is_first_matches :: WS -> WTrie Int -> Bool)
    , QC.testProperty
        "prop_match_is_last_matches"
        (prop_match_is_last_matches :: WS -> WTrie Int -> Bool)
    ]
  , Tasty.testGroup "Priority-queue functions (@Int)"
    [ QC.testProperty
        "prop_minAssoc_is_first_toList"
        (prop_minAssoc_is_first_toList :: WTrie Int -> Bool)
    , QC.testProperty
        "prop_maxAssoc_is_last_toList"
        (prop_maxAssoc_is_last_toList :: WTrie Int -> Bool)
    , QC.testProperty
        "prop_updateMinViewBy_ident"
        (prop_updateMinViewBy_ident :: WTrie Int -> Bool)
    , QC.testProperty
        "prop_updateMaxViewBy_ident"
        (prop_updateMaxViewBy_ident :: WTrie Int -> Bool)
    , QC.testProperty
        "prop_updateMinViewBy_gives_minAssoc"
        (prop_updateMinViewBy_gives_minAssoc :: (WS -> Int -> Maybe Int) -> WTrie Int -> Bool)
    , QC.testProperty
        "prop_updateMaxViewBy_gives_maxAssoc"
        (prop_updateMaxViewBy_gives_maxAssoc :: (WS -> Int -> Maybe Int) -> WTrie Int -> Bool)
    ]
  , Tasty.testGroup "List-conversion properties (@Int)"
    [ QC.testProperty
        "prop_toListBy_keysOrdered"
        (prop_toListBy_keysOrdered  :: WTrie Int -> Bool)
    , QC.testProperty
        "prop_fromList_takes_first"
        (prop_fromList_takes_first  :: [(WS, Int)] -> Bool)
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

-- Throughout, we try to use 'W' or @()@ whenever we can, to reduce
-- the exponential growth problem.
smallcheckTests :: Tasty.TestTree
smallcheckTests
  = Tasty.testGroup "SmallCheck"
  [ Tasty.testGroup "Trivial properties (@W)"
    -- Depth=4 is death here (>30sec) ever since changing the instance.
    [ SC.testProperty
        "prop_insert"
        (prop_insert        :: WS -> W -> WTrie W -> Bool)
    , SC.testProperty
        -- This one can easily handle depth=6 fine (~0.1sec), but d=7 (~4sec)
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
  , Tasty.testGroup "Submap properties (@()/@W)"
    -- Depth=4 is at best very marginal here...
    [ SC.testProperty
        "prop_submap_keysAreMembers"
        (prop_submap_keysAreMembers :: WS -> WTrie () -> Bool)
    , SC.testProperty
        "prop_submap_keysHavePrefix"
        (prop_submap_keysHavePrefix :: WS -> WTrie () -> Bool)
    , SC.testProperty
        "prop_submap_valuesEq"
        (prop_submap_valuesEq       :: WS -> WTrie W -> Bool)
    , SC.testProperty
        "prop_deleteSubmap_keysAreMembers"
        (prop_deleteSubmap_keysAreMembers :: WS -> WTrie () -> Bool)
    , SC.testProperty
        "prop_deleteSubmap_keysLackPrefix"
        (prop_deleteSubmap_keysLackPrefix :: WS -> WTrie () -> Bool)
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
  , Tasty.testGroup "match/matches properties (@()/@W)"
    [ SC.testProperty
        "prop_matches_keysOrdered"
        (prop_matches_keysOrdered   :: WS -> WTrie () -> Bool)
    , SC.testProperty
        "prop_matches_keysArePrefix"
        (prop_matches_keysArePrefix :: WS -> WTrie () -> Bool)
    , SC.testProperty
        "prop_minMatch_is_first_matches"
        (prop_minMatch_is_first_matches :: WS -> WTrie W -> Bool)
    , SC.testProperty
        "prop_match_is_last_matches"
        (prop_match_is_last_matches :: WS -> WTrie W -> Bool)
    ]
  , Tasty.testGroup "Priority-queue functions (@W)"
    -- Depth=4 takes about 1sec each
    [ SC.testProperty
        "prop_minAssoc_is_first_toList"
        (prop_minAssoc_is_first_toList :: WTrie W -> Bool)
    , SC.testProperty
        "prop_maxAssoc_is_last_toList"
        (prop_maxAssoc_is_last_toList :: WTrie W -> Bool)
    , SC.testProperty
        "prop_updateMinViewBy_ident"
        (prop_updateMinViewBy_ident :: WTrie W -> Bool)
    , SC.testProperty
        "prop_updateMaxViewBy_ident"
        (prop_updateMaxViewBy_ident :: WTrie W -> Bool)
    -- HACK: must explicitly pass functions for these two, else they're too slow
    , SC.testProperty
        "prop_updateMinViewBy_gives_minAssoc"
        (prop_updateMinViewBy_gives_minAssoc undefined :: WTrie W -> Bool)
    , SC.testProperty
        "prop_updateMaxViewBy_gives_maxAssoc"
        (prop_updateMaxViewBy_gives_maxAssoc undefined :: WTrie W -> Bool)
    ]
  , Tasty.adjustOption (+ (1::SC.SmallCheckDepth))
  $ Tasty.testGroup "List-conversion properties (@()/@W)"
    [ SC.testProperty
        "prop_toListBy_keysOrdered"
        (prop_toListBy_keysOrdered  :: WTrie () -> Bool)
    , SC.testProperty
        "prop_fromList_takes_first"
        (prop_fromList_takes_first  :: [(WS, W)] -> Bool)
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
prop_submap_keysAreMembers (WS q) (WT t) =
    all (`T.member` t) . T.keys . T.submap q $ t
    -- TODO: should we use 'QC.conjoin' (assuming another class to overload it) in lieu of 'all'? What are the actual benefits of doing so? Ditto for all the uses below.

-- | All keys in a submap have the query as a prefix
prop_submap_keysHavePrefix :: WS -> WTrie a -> Bool
prop_submap_keysHavePrefix (WS q) =
    all (q `S.isPrefixOf`) . T.keys . T.submap q . unWT

-- | All values in a submap are the same in the supermap
prop_submap_valuesEq :: (Eq a) => WS -> WTrie a -> Bool
prop_submap_valuesEq (WS q) (WT t) =
    ((`T.lookup` t') .==. (`T.lookup` t)) `all` T.keys t'
    where t' = T.submap q t

-- | All keys in the result are keys in the supermap
prop_deleteSubmap_keysAreMembers :: WS -> WTrie a -> Bool
prop_deleteSubmap_keysAreMembers (WS q) (WT t) =
    all (`T.member` t) . T.keys . T.deleteSubmap q $ t

-- | All keys in a submap lack the query as a prefix
prop_deleteSubmap_keysLackPrefix :: WS -> WTrie a -> Bool
prop_deleteSubmap_keysLackPrefix (WS q) =
    all (not . S.isPrefixOf q) . T.keys . T.deleteSubmap q . unWT

-- | `T.submap` and `T.deleteSubmap` partition every trie for every key.
prop_deleteSubmap_disunion :: (Eq a) => WS -> WTrie a -> Bool
prop_deleteSubmap_disunion (WS q) (WT t) =
    t == (T.submap q t `TC.disunion` T.deleteSubmap q t)

-- TODO: other than as a helper like below, could we actually
-- generate interesting enough functions to make this worth testing
-- directly?
--
-- | Arbitrary @x ∩ y == (x ∪ y) ⋈ (x ⋈ y)@.
prop_intersectBy :: (Eq a) => (a -> a -> Maybe a) -> WTrie a -> WTrie a -> Bool
prop_intersectBy f (WT x) (WT y) =
    T.intersectBy f x y == (T.mergeBy f x y `TC.disunion` TC.disunion x y)

-- | Left-biased @x ∩ y == (x ∪ y) ⋈ (x ⋈ y)@.
prop_intersectL :: (Eq a) => WTrie a -> WTrie a -> Bool
prop_intersectL = prop_intersectBy (\x _ -> Just x)

-- | Right-biased @x ∩ y == (x ∪ y) ⋈ (x ⋈ y)@.
prop_intersectR :: (Eq a) => WTrie a -> WTrie a -> Bool
prop_intersectR = prop_intersectBy (\_ y -> Just y)

-- | Additive @x ∩ y == (x ∪ y) ⋈ (x ⋈ y)@.
prop_intersectPlus :: (Eq a, Num a) => WTrie a -> WTrie a -> Bool
prop_intersectPlus = prop_intersectBy (\x y -> Just (x + y))

isOrdered :: (Ord a) => [a] -> Bool
isOrdered xs = and (zipWith (<=) xs (drop 1 xs))

-- | 'T.toListBy', 'T.toList', and 'T.keys' are ordered by keys.
prop_toListBy_keysOrdered :: WTrie a -> Bool
prop_toListBy_keysOrdered = isOrdered . T.keys . unWT

fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a

-- | 'T.matches' is ordered by keys.
prop_matches_keysOrdered :: WS -> WTrie a -> Bool
prop_matches_keysOrdered (WS q) (WT t) =
    isOrdered . map fst3 $ T.matches t q

-- | Matching keys are a prefix of the query.
prop_matches_keysArePrefix :: WS -> WTrie a -> Bool
prop_matches_keysArePrefix (WS q) (WT t) =
    all (`S.isPrefixOf` q) . map fst3 $ T.matches t q

_eqHead :: (Eq a) => Maybe a -> [a] -> Bool
_eqHead Nothing  []    = True
_eqHead (Just x) (y:_) = x == y
_eqHead _        _     = False

_eqLast :: (Eq a) => Maybe a -> [a] -> Bool
_eqLast Nothing  []       = True
_eqLast (Just x) ys@(_:_) = x == last ys
_eqLast _        _        = False

prop_minMatch_is_first_matches :: Eq a => WS -> WTrie a -> Bool
prop_minMatch_is_first_matches (WS q) (WT t) =
    _eqHead (T.minMatch t q) (T.matches t q)

prop_match_is_last_matches :: Eq a => WS -> WTrie a -> Bool
prop_match_is_last_matches (WS q) (WT t) =
    _eqLast (T.match t q) (T.matches t q)

prop_minAssoc_is_first_toList :: Eq a => WTrie a -> Bool
prop_minAssoc_is_first_toList (WT t) =
    _eqHead (TI.minAssoc t) (T.toList t)

prop_maxAssoc_is_last_toList :: Eq a => WTrie a -> Bool
prop_maxAssoc_is_last_toList (WT t) =
    _eqLast (TI.maxAssoc t) (T.toList t)

view2assoc :: Maybe (S.ByteString, a, T.Trie a) -> Maybe (S.ByteString, a)
view2assoc Nothing        = Nothing
view2assoc (Just (k,v,_)) = Just (k,v)

-- TODO: again, can we actually generate any interesting functions here?
prop_updateMinViewBy_gives_minAssoc :: Eq a => (WS -> a -> Maybe a) -> WTrie a -> Bool
prop_updateMinViewBy_gives_minAssoc f =
    ((view2assoc . TI.updateMinViewBy (f . WS)) .==. TI.minAssoc) . unWT

prop_updateMaxViewBy_gives_maxAssoc :: Eq a => (WS -> a -> Maybe a) -> WTrie a -> Bool
prop_updateMaxViewBy_gives_maxAssoc f =
    ((view2assoc . TI.updateMaxViewBy (f . WS)) .==. TI.maxAssoc) . unWT

view2trie :: Maybe (S.ByteString, a, T.Trie a) -> T.Trie a
view2trie Nothing        = T.empty
view2trie (Just (_,_,t)) = t

prop_updateMinViewBy_ident :: Eq a => WTrie a -> Bool
prop_updateMinViewBy_ident =
    ((view2trie . TI.updateMinViewBy (\_ v -> Just v)) .==. id) . unWT

prop_updateMaxViewBy_ident :: Eq a => WTrie a -> Bool
prop_updateMaxViewBy_ident =
    ((view2trie . TI.updateMaxViewBy (\_ v -> Just v)) .==. id) .unWT


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
