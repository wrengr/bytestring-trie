{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
{-# LANGUAGE CPP
           , MultiParamTypeClasses
           , FlexibleContexts
           #-}

----------------------------------------------------------------
--                                                  ~ 2021.12.04
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
-- [aka GHC 8.8.1]: Prelude re-exports 'Semigroup'.
#elif MIN_VERSION_base(4,9,0)
-- [aka GHC 8.0.1]: "Data.Semigroup" added to base.
import Data.Semigroup      (Semigroup(..))
#elif MIN_VERSION_base(4,5,0)
-- [aka GHC 7.4.1]: @(<>)@ added to "Data.Monoid".
import Data.Monoid         ((<>))
#endif

#if MIN_VERSION_base(4,9,0)
import Data.Semigroup      (Sum)
#else
data Sum a = Sum a
    deriving (Eq, Ord, Read, Show, Bounded, Num)
instance Num a => Monoid (Sum a) where
    mempty = Sum 0
    mappend (Sum x) (Sum y) = Sum (x + y)
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
-- accidentally blow our budget.  So far, even with HPC enabled,
-- each test takes less than 1sec (almost all of them taking less
-- than 0.5sec); so allowing 10s per test is more than generous.
-- Since the full suite takes around 20sec, that means the timeout
-- will cap the suite to take no more than 200sec / 3.3min.
globalOptions :: Tasty.OptionSet
globalOptions = mconcat
    [ Tasty.singleOption (Tasty.mkTimeout 10000000)  -- 10sec; in microsecs
    , Tasty.singleOption (QC.QuickCheckTests    500) -- QC.Args.maxSuccess
    , Tasty.singleOption (QC.QuickCheckMaxSize  400) -- QC.Args.maxSize
    , Tasty.singleOption (QC.QuickCheckMaxRatio 10)  -- QC.Args.maxDiscardRatio
    , Tasty.singleOption (SC.SmallCheckDepth    3)
    ]

-- We run the HUnit and smallcheck tests first, since they're the
-- fastest.  Thus, we only pay for the QuickCheck tests if we have to.
tests :: Tasty.TestTree
tests =
  Tasty.testGroup "All Tests"
  [ hunitTests
  , Tasty.testGroup "Properties"
    [ smallcheckTests
    , quickcheckTests
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
  [ Tasty.testGroup "Data.Trie.ByteStringInternal"
    [ QC.testProperty
        "prop_breakMaximalPrefix"
        (prop_breakMaximalPrefix :: WS -> WS -> Bool)
    ]
  , Tasty.testGroup "Trivialities (@Int)"
    [ QC.testProperty
        "prop_singleton"
        (prop_singleton     :: WS -> Int -> Bool)
    , QC.testProperty
        "prop_insert_lookup"
        (prop_insert_lookup :: WS -> Int -> WTrie Int -> Bool)
    , QC.testProperty
        "prop_delete_lookup"
        (prop_delete_lookup :: WS -> WTrie Int -> Bool)
    , QC.testProperty
        "prop_insert_size"
        (prop_insert_size   :: WS -> Int -> WTrie Int -> Bool)
    , QC.testProperty
        "prop_delete_size"
        (prop_delete_size   :: WS -> WTrie Int -> Bool)
    , QC.testProperty
        "prop_insert_delete"
        (prop_insert_delete :: WS -> Int -> WTrie Int -> QC.Property)
    ]
  , Tasty.testGroup "Submap (@Int)"
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
  $ Tasty.testGroup "Intersection (@Int)"
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
  , Tasty.testGroup "Matching (@Int)"
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
  , Tasty.testGroup "Priority-queue (@Int)"
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
  , Tasty.testGroup "List-conversion (@Int)"
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
  , Tasty.testGroup "Type classes"
    [ Tasty.testGroup "Functor (@Int)"
      [ QC.testProperty
          "prop_FunctorIdentity"
          (prop_FunctorIdentity   :: WTrie Int -> Bool)
      -- TODO: prop_FunctorCompose
      {-
      -- TODO: still worth it to do this test with 'undefined'?
      , QC.testProperty
          "prop_fmap_keys"
          (prop_fmap_keys (undefined :: Int -> Int) :: WTrie Int -> Bool)
      -}
      , QC.testProperty
          -- TODO: generalize to other functions.
          "prop_fmap_toList"
          (prop_fmap_toList (+1) :: WTrie Int -> Bool)
      ]
    , Tasty.testGroup "Applicative (@Int)"
      [ QC.testProperty
          "prop_ApplicativeIdentity"
          (prop_ApplicativeIdentity :: WTrie Int -> Bool)
      -- TODO: prop_ApplicativeCompose, prop_ApplicativeHom, prop_ApplicativeInterchange
      ]
    , Tasty.testGroup "Monad (@Int)"
      [ QC.testProperty
          "prop_MonadIdentityR"
          (prop_MonadIdentityR :: WTrie Int -> Bool)
      -- TODO: prop_MonadIdentityL, prop_MonadAssoc
      ]
    -- TODO: Foldable, Traversable
#if MIN_VERSION_base(4,9,0)
    , Tasty.testGroup "Semigroup (@Sum Int)"
      [ QC.testProperty
          -- This one is a bit more expensive: ~1sec instead of <=0.5sec
          "prop_Semigroup"
          (prop_Semigroup :: WTrie (Sum Int) -> WTrie (Sum Int) -> WTrie (Sum Int) -> Bool)
      ]
#endif
    , Tasty.testGroup "Monoid (@Sum Int)"
      [ QC.testProperty
          "prop_MonoidIdentityL"
          (prop_MonoidIdentityL :: WTrie (Sum Int) -> Bool)
      , QC.testProperty
          "prop_MonoidIdentityR"
          (prop_MonoidIdentityR :: WTrie (Sum Int) -> Bool)
      ]
    ]
  , Tasty.testGroup "Other mapping/filtering (@Int)"
    [ QC.testProperty
        "prop_filterMap_ident"
        (prop_filterMap_ident :: WTrie Int -> Bool)
    , QC.testProperty
        "prop_filterMap_empty"
        (prop_filterMap_empty :: WTrie Int -> Bool)
    , QC.testProperty
        "prop_mapBy_keys"
        (prop_mapBy_keys :: WTrie Int -> Bool)
    , QC.testProperty
        "prop_contextualMap_ident"
        (prop_contextualMap_ident :: WTrie Int -> Bool)
    , QC.testProperty
        "prop_contextualMap'_ident"
        (prop_contextualMap'_ident :: WTrie Int -> Bool)
    , QC.testProperty
        "prop_contextualFilterMap_ident"
        (prop_contextualFilterMap_ident :: WTrie Int -> Bool)
    , QC.testProperty
        "prop_contextualMapBy_keys"
        (prop_contextualMapBy_keys :: WTrie Int -> Bool)
    , QC.testProperty
        "prop_contextualMapBy_ident"
        (prop_contextualMapBy_ident :: WTrie Int -> Bool)
    , QC.testProperty
        "prop_contextualMapBy_empty"
        (prop_contextualMapBy_empty :: WTrie Int -> Bool)
    ]
  ]


adjustSmallCheckDepth
    :: (SC.SmallCheckDepth -> SC.SmallCheckDepth)
    -> Tasty.TestTree -> Tasty.TestTree
adjustSmallCheckDepth = Tasty.adjustOption

-- Throughout, we try to use 'W' or @()@ whenever we can, to reduce
-- the exponential growth problem.
smallcheckTests :: Tasty.TestTree
smallcheckTests
  = Tasty.testGroup "SmallCheck"
  [ Tasty.testGroup "Data.Trie.ByteStringInternal"
    [ adjustSmallCheckDepth (+1)
    $ SC.testProperty
        "prop_breakMaximalPrefix"
        (prop_breakMaximalPrefix :: WS -> WS -> Bool)
    ]
  , Tasty.testGroup "Trivialities (@W/@())"
    [ adjustSmallCheckDepth (+2)
    $ SC.testProperty
        -- This one can easily handle depth=6 fine (~0.1sec), but d=7 (~4sec)
        "prop_singleton"
        (prop_singleton     :: WS -> W -> Bool)
    , SC.testProperty
        -- Warning: depth 4 takes >10sec!
        "prop_insert_lookup"
        (prop_insert_lookup :: WS -> W -> WTrie W -> Bool)
    , SC.testProperty
        -- Don't waste any depth on the values!
        -- Warning: depth 4 takes >10sec!
        "prop_delete_lookup"
        (prop_delete_lookup :: WS -> WTrie () -> Bool)
    , SC.testProperty
        -- Don't waste any depth on the values!
        -- Warning: depth 4 takes >10sec!
        "prop_insert_size"
        (prop_insert_size   :: WS -> () -> WTrie () -> Bool)
    , SC.testProperty
        -- Don't waste any depth on the values!
        -- Warning: depth 4 takes >10sec!
        -- FIXME: at depth 3: 388/410 (94%) did not meet the condition!
        -- (That is, before rephrasing to avoid using 'CheckGuard')
        "prop_delete_size"
        (prop_delete_size   :: WS -> WTrie () -> Bool)
    , SC.testProperty
        -- Note: at depth 3: 136/2120 (6.4%) did not meet the condition.
        "prop_insert_delete"
        (prop_insert_delete :: WS -> W -> WTrie W -> SC.Property IO)
    ]
  , Tasty.testGroup "Submap (@()/@W)"
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
  , Tasty.testGroup "Intersection (@W/@Int)"
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
  , Tasty.testGroup "Matching (@()/@W)"
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
  , Tasty.testGroup "Priority-queue (@W)"
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
  $ Tasty.testGroup "List-conversion (@()/@W)"
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
  -- TODO: do we want to smallcheck any of the "Type classes" or "Other mapping/filtering" stuff we do in quickcheck?
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

-- | 'TI.breakMaximalPrefix' satisfies the documented equalities.
prop_breakMaximalPrefix :: WS -> WS -> Bool
prop_breakMaximalPrefix (WS s) (WS z) =
    let (pre,s',z') = TI.breakMaximalPrefix s z
    in (pre <> s') == s
    && (pre <> z') == z

-- | A singleton, is.
prop_singleton :: (Eq a) => WS -> a -> Bool
prop_singleton (WS k) v =
    T.singleton k v == T.insert k v T.empty

-- | If you insert a value, you can look it up.
prop_insert_lookup :: (Eq a) => WS -> a -> WTrie a -> Bool
prop_insert_lookup (WS k) v (WT t) =
    (T.lookup k . T.insert k v $ t) == Just v

-- | If you delete a value, you can't look it up.
prop_delete_lookup :: WS -> WTrie a -> Bool
prop_delete_lookup (WS k) =
    isNothing . T.lookup k . T.delete k . unWT
    where
    isNothing Nothing  = True
    isNothing (Just _) = False

-- TODO: print/record diagnostics re what proportion of calls have
-- @n=0@ vs @n=1@, to ensure proper coverage.
prop_insert_size :: WS -> a -> WTrie a -> Bool
prop_insert_size (WS k) v (WT t) =
    ((T.size . T.insert k v) .==. ((n +) . T.size)) $ t
    where
    n = if T.member k t then 0 else 1

-- TODO: print/record diagnostics re what proportion of calls have
-- @n=0@ vs @n=1@, to ensure proper coverage.
prop_delete_size :: WS -> WTrie a -> Bool
prop_delete_size (WS k) (WT t) =
    ((T.size . T.delete k) .==. (subtract n . T.size)) $ t
    where
    n = if T.member k t then 1 else 0

prop_insert_delete :: (Eq a, CheckGuard Bool b) => WS -> a -> WTrie a -> b
prop_insert_delete (WS k) v =
    ((not . T.member k) .==>. (T.delete k . T.insert k v) .==. id) . unWT

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

-- | Lift a function to apply to the 'fst' of pairs, retaining the 'snd'.
first :: (a -> b) -> (a,c) -> (b,c)
first f (x,y) = (f x, y)

-- | Lift a function to apply to the 'snd' of pairs, retaining the 'fst'.
second :: (b -> c) -> (a,b) -> (a,c)
second f (x,y) = (x, f y)

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

prop_FunctorIdentity :: Eq a => WTrie a -> Bool
prop_FunctorIdentity = (fmap id .==. id) . unWT

{- -- TODO: is there any way to make this remotely testable?
prop_FunctorCompose :: Eq c => (b -> c) -> (a -> b) -> WTrie a -> Bool
prop_FunctorCompose f g = (fmap (f . g) .==. (fmap f . fmap g)) . unWT
-}

{-
-- Both of these test only a subset of what 'prop_fmap_toList' tests.  I was hoping they'd help simplify the function-generation problem, but if we're testing 'prop_fmap_toList' anyways then there's no point in testing these too.

-- | 'fmap' doesn't affect the keys. This is safe to call with an
-- undefined function, thereby proving that the function cannot
-- affect things.
prop_fmap_keys :: Eq b => (a -> b) -> WTrie a -> Bool
prop_fmap_keys f = ((T.keys . fmap f) .==. T.keys) . unWT

prop_fmap_elems :: Eq b => (a -> b) -> WTrie a -> Bool
prop_fmap_elems f = ((T.elems . fmap f) .==. (map f . T.elems)) . unWT
-}

-- TODO: is there any way to generate halfway useful functions for testing here?
prop_fmap_toList :: Eq b => (a -> b) -> WTrie a -> Bool
prop_fmap_toList f =
    ((T.toList . fmap f) .==. (map (second f) . T.toList)) . unWT

prop_filterMap_ident :: Eq a => WTrie a -> Bool
prop_filterMap_ident = (T.filterMap Just .==. id) . unWT

prop_filterMap_empty :: Eq a => WTrie a -> Bool
prop_filterMap_empty = (T.filterMap const_Nothing .==. const T.empty) . unWT
    where
    -- Have to fix the result type here.
    const_Nothing :: a -> Maybe a
    const_Nothing = const Nothing

justConst :: a -> b -> Maybe a
justConst x _ = Just x

prop_mapBy_keys :: WTrie a -> Bool
prop_mapBy_keys = all (uncurry (==)) . T.toList . T.mapBy justConst . unWT

prop_contextualMap_ident :: Eq a => WTrie a -> Bool
prop_contextualMap_ident = (TI.contextualMap const .==. id) . unWT

prop_contextualMap'_ident :: Eq a => WTrie a -> Bool
prop_contextualMap'_ident = (TI.contextualMap' const .==. id) . unWT

prop_contextualFilterMap_ident :: Eq a => WTrie a -> Bool
prop_contextualFilterMap_ident =
    (TI.contextualFilterMap justConst .==. id) . unWT

prop_contextualMapBy_keys :: WTrie a -> Bool
prop_contextualMapBy_keys =
    all (uncurry (==)) . T.toList . TI.contextualMapBy f . unWT
    where
    f k _ _ = Just k

prop_contextualMapBy_ident :: Eq a => WTrie a -> Bool
prop_contextualMapBy_ident = (TI.contextualMapBy f .==. id) . unWT
    where
    f _ v _ = Just v

prop_contextualMapBy_empty :: Eq a => WTrie a -> Bool
prop_contextualMapBy_empty = (TI.contextualMapBy f .==. const T.empty) . unWT
    where
    -- Have to fix the result type here.
    f :: S.ByteString -> a -> T.Trie a -> Maybe a
    f _ _ _ = Nothing

prop_ApplicativeIdentity :: Eq a => WTrie a -> Bool
prop_ApplicativeIdentity = ((pure id <*>) .==. id) . unWT

{- -- (remaining, untestable) Applicative laws
prop_ApplicativeCompose  = pure (.) <*> u <*> v <*> w == u <*> (v <*> w)
prop_ApplicativeHom      = pure f <*> pure x == pure (f x)
prop_ApplicativeInterchange = u <*> pure y == pure ($ y) <*> u
-}

prop_MonadIdentityR :: Eq a => WTrie a -> Bool
prop_MonadIdentityR = ((>>= return) .==. id) . unWT

{- -- (remaining, untestable) Monad laws
prop_MonadIdentityL = (return a >>= k) == k a
prop_MonadAssoc     = m >>= (\x -> k x >>= h) == (m >>= k) >>= h
-}

#if MIN_VERSION_base(4,9,0)
prop_Semigroup :: (Semigroup a, Eq a) => WTrie a -> WTrie a -> WTrie a -> Bool
prop_Semigroup (WT a) (WT b) (WT c) = a <> (b <> c) == (a <> b) <> c
#endif

-- N.B., base-4.11.0.0 is when Semigroup became superclass of Monoid
prop_MonoidIdentityL :: (Monoid a, Eq a) => WTrie a -> Bool
prop_MonoidIdentityL = ((mempty `mappend`) .==. id) . unWT

prop_MonoidIdentityR :: (Monoid a, Eq a) => WTrie a -> Bool
prop_MonoidIdentityR = ((`mappend` mempty) .==. id) . unWT

----------------------------------------------------------------
----------------------------------------------------------- fin.
