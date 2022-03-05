{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
{-# LANGUAGE CPP
           , MultiParamTypeClasses
           , FlexibleContexts
           #-}

----------------------------------------------------------------
--                                                  ~ 2022.03.05
-- |
-- Module      :  Test.Properties
-- Copyright   :  2008--2022 wren romano
-- License     :  BSD-3-Clause
-- Maintainer  :  wren@cpan.org
-- Stability   :  provisional
-- Portability :  semi-portable (MPTC,...)
--
-- Property testing 'Trie's.
----------------------------------------------------------------
module Test.Properties (smallcheckTests, quickcheckTests) where

import Test.Utils

import qualified Data.Trie              as T
import qualified Data.Trie.Internal     as TI
import qualified Data.Trie.Convenience  as TC
import qualified Data.ByteString        as S

import qualified Test.Tasty             as Tasty
import qualified Test.Tasty.SmallCheck  as SC
import qualified Test.Tasty.QuickCheck  as QC

import Control.Applicative  (liftA2)
import Control.Monad        (join, (<=<), guard)
#if MIN_VERSION_base(4,13,0)
import Data.Functor         ((<&>)) -- Or else we'll define it ourselves.
#endif
import Data.List            (nubBy, sortBy)
import Data.Maybe           (fromJust, isJust)
import Data.Ord             (comparing)

import qualified Data.Binary   as B
import qualified Data.Foldable as F

#if MIN_VERSION_base(4,13,0)
-- [aka GHC 8.8.1]: Prelude re-exports 'Semigroup' and '<>'.
#elif MIN_VERSION_base(4,9,0)
-- [aka GHC 8.0.1]: "Data.Semigroup" added to base.
import Data.Semigroup      (Semigroup(..))
#elif MIN_VERSION_base(4,5,0)
-- [aka GHC 7.4.1]: @(<>)@ added to "Data.Monoid".
import Data.Monoid         ((<>))
#endif

#if MIN_VERSION_base(4,9,0)
import Data.Semigroup       (Sum(..)) -- Or else we'll define it ourselves.
import Data.Functor.Classes (Eq1)
import Data.Functor.Compose (Compose(Compose))
#endif

#if MIN_VERSION_base(4,8,0)
import Data.Functor.Identity (Identity(Identity))
#endif

----------------------------------------------------------------
#if !(MIN_VERSION_base(4,13,0))
-- [aka GHC 8.8.1]
(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap
#endif

#if !(MIN_VERSION_base(4,9,0))
newtype Sum a = Sum { getSum :: a }
    deriving (Eq, Ord, Read, Show, Bounded, Num)
instance Num a => Monoid (Sum a) where
    mempty = Sum 0
    mappend (Sum x) (Sum y) = Sum (x + y)

-- TODO: reimplement 'Data.Functor.Compose.Compose'
#endif

#if !(MIN_VERSION_base(4,8,0))
-- TODO: reimplement 'Data.Functor.Identity.Identity'
#endif

----------------------------------------------------------------
----------------------------------------------------------------
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
testProp resolve it depending on the PropChecker.  So is there a way
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
        (prop_insert_delete :: WS -> Int -> WTrie Int -> Bool)
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
  $ Tasty.testGroup "unionWith (@Int)"
    [ QC.testProperty
        "prop_unionL"
        (prop_unionL    :: WTrie Int -> WTrie Int -> Bool)
    , QC.testProperty
        "prop_unionR"
        (prop_unionR    :: WTrie Int -> WTrie Int -> Bool)
    , QC.testProperty
        "prop_unionPlus"
        (prop_unionPlus :: WTrie Int -> WTrie Int -> Bool)
    ]
  , Tasty.localOption (QC.QuickCheckMaxSize 300)
    -- BUG: fix that 'Tasty.localOption'
  $ Tasty.testGroup "intersectBy (@Int)"
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
  , Tasty.testGroup "toList (@Int)"
    [ QC.testProperty
        "prop_toListBy_keysOrdered"
        (prop_toListBy_keysOrdered  :: WTrie Int -> Bool)
    , QC.testProperty
        "prop_keys"
        (prop_keys :: WTrie Int -> Bool)
    , QC.testProperty
        "prop_elems"
        (prop_elems :: WTrie Int -> Bool)
    ]
  , Tasty.testGroup "fromList (@Int)"
    [ QC.testProperty
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
      -- TODO: prop_fmap_elems
      -}
#if __GLASGOW_HASKELL__
      , QC.testProperty
          "prop_fmapConst"
          (prop_fmapConst :: Int -> WTrie Int -> Bool)
#endif
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
    , Tasty.testGroup "Foldable (@Int/Sum Int)"
      [ QC.testProperty
          "prop_foldmapIdentity"
          (prop_foldmapIdentity :: WTrie (Sum Int) -> Bool)
      , QC.testProperty
          "prop_foldmapFusion"
          (prop_foldmapFusion Sum :: WTrie Int -> Bool)
      , QC.testProperty
          "prop_length"
          (prop_length :: WTrie Int -> Bool)
      , QC.testProperty
          "prop_foldr_vs_foldrWithKey"
          (prop_foldr_vs_foldrWithKey :: WTrie Int -> Bool)
      , QC.testProperty
          "prop_foldr'_vs_foldrWithKey'"
          (prop_foldr'_vs_foldrWithKey' :: WTrie Int -> Bool)
      , QC.testProperty
          "prop_foldl_vs_foldlWithKey"
          (prop_foldl_vs_foldlWithKey :: WTrie Int -> Bool)
      , QC.testProperty
          "prop_foldl'_vs_foldlWithKey'"
          (prop_foldl'_vs_foldlWithKey' :: WTrie Int -> Bool)
#if MIN_VERSION_base(4,6,0)
      , QC.testProperty
          "prop_foldr_vs_foldr'"
          (prop_foldr_vs_foldr' :: WTrie Int -> Bool)
      , QC.testProperty
          "prop_foldl_vs_foldl'"
          (prop_foldl_vs_foldl' :: WTrie Int -> Bool)
#endif
#if MIN_VERSION_base(4,13,0)
      , QC.testProperty
          "prop_foldMap_vs_foldMap'"
          (prop_foldMap_vs_foldMap' :: WTrie Int -> Bool)
#endif
      ]
    , Tasty.testGroup "Traversable (@Int)"
      [ QC.testProperty
          "prop_TraverseIdentity"
          (prop_TraverseIdentity :: WTrie Int -> Bool)
      -- TODO: prop_TraverseNaturality, prop_TraverseComposition
      , QC.testProperty
          "prop_SequenceaIdentity"
          (prop_SequenceaIdentity :: WTrie Int -> Bool)
      -- TODO: prop_SequenceaNaturality, prop_SequenceaComposition
      ]
    , Tasty.testGroup "Filterable (@Int)"
      [ QC.testProperty
          "prop_FiltermapFission"
          (prop_FiltermapFission remTriple :: WTrie Int -> Bool)
      , QC.testProperty
          "prop_FiltermapFusion"
          (prop_FiltermapFusion (`rem` 3) (not . isTriple) :: WTrie Int -> Bool)
      , QC.testProperty
          "prop_FiltermapConservation"
          (prop_FiltermapConservation (+1) :: WTrie Int -> Bool)
      -- TODO: prop_FiltermapVertComposition
      , QC.testProperty
          "prop_FilterDefinition"
          (prop_FilterDefinition isTriple :: WTrie Int -> Bool)
      , QC.testProperty
          "prop_FilterVertComposition"
          (prop_FilterVertComposition even isTriple :: WTrie Int -> Bool)
      ]
    , Tasty.testGroup "Witherable (@Int)"
      [ QC.testProperty
          "prop_WitherConservation"
          (prop_WitherConservation (Identity . (+1)) :: WTrie Int -> Bool)
      -- TODO: prop_WitherNaturality, prop_WitherPurity, prop_WitherHorizComposition
      , QC.testProperty
          "prop_FilteraDefinition"
          (prop_FilteraDefinition (Identity . even) :: WTrie Int -> Bool)
      -- TODO: prop_FilteraNaturality, prop_FilteraPurity, prop_FilteraHorizComposition, prop_FilteraHorizComposition_alt
      ]
#if MIN_VERSION_base(4,9,0)
    , Tasty.testGroup "Semigroup (@Sum Int)"
      [ QC.testProperty
          -- This one is a bit more expensive: 1~2sec instead of <=0.5sec
          -- TODO: see if we can't fix that without ruining the utility of the test.
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
    , Tasty.testGroup "Binary (@Int)"
      [ QC.testProperty
          "prop_Binary"
          (prop_Binary :: WTrie Int -> Bool)
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


----------------------------------------------------------------
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
        -- (That is, before rephrasing to avoid using 'CheckGuard')
        "prop_insert_delete"
        (prop_insert_delete :: WS -> W -> WTrie W -> Bool)
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
  , Tasty.testGroup "unionWith (@W/@Int)"
    -- Warning: Using depth=4 here is bad (the first two take about
    -- 26.43sec; the last one much longer).
    [ SC.testProperty
        "prop_unionL"
        (prop_unionL    :: WTrie W -> WTrie W -> Bool)
    , SC.testProperty
        "prop_unionR"
        (prop_unionR    :: WTrie W -> WTrie W -> Bool)
    , SC.testProperty
        "prop_unionPlus"
        (prop_unionPlus :: WTrie Int -> WTrie Int -> Bool)
    ]
  , Tasty.testGroup "intersectBy (@W/@Int)"
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
  , Tasty.testGroup "toList (@()/@W)"
    -- These can handle depth 4, but it takes >1sec (but still <5sec)
    [ SC.testProperty
        "prop_toListBy_keysOrdered"
        (prop_toListBy_keysOrdered  :: WTrie () -> Bool)
    , SC.testProperty
        "prop_keys"
        (prop_keys :: WTrie () -> Bool)
    , SC.testProperty
        "prop_elems"
        (prop_elems :: WTrie W -> Bool)
    -- TODO: move these into the section for 'Foldable', to agree with QC
    , SC.testProperty
        "prop_foldr_vs_foldrWithKey"
        (prop_foldr_vs_foldrWithKey :: WTrie W -> Bool)
#if MIN_VERSION_base(4,6,0)
    , SC.testProperty
        "prop_foldr_vs_foldr'"
        (prop_foldr_vs_foldr' :: WTrie W -> Bool)
    , SC.testProperty
        "prop_foldl_vs_foldl'"
        (prop_foldl_vs_foldl' :: WTrie W -> Bool)
#endif
    -- TODO: prop_foldMap_vs_foldMap' requires (Num W) if we want to use W.
    ]
  , Tasty.adjustOption (+ (1::SC.SmallCheckDepth))
  $ Tasty.testGroup "fromList (@()/@W)"
    [ SC.testProperty
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

-- | 'TI.breakMaximalPrefix' satisfies the documented equalities.
prop_breakMaximalPrefix :: WS -> WS -> Bool
prop_breakMaximalPrefix (WS s) (WS z) =
    let (pre,s',z') = TI.breakMaximalPrefix s z
    in (pre <> s') == s
    && (pre <> z') == z

{-
-- FIXME: need to export the RLBS stuff if we are to test it...
prop_toStrict :: [WS] -> Bool
prop_toStrict =
    (S.concat .==. (TI.toStrict . foldl' (+>) Epsilon)) . map unWS
-}

----------------------------------------------------------------
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

prop_insert_delete :: (Eq a) => WS -> a -> WTrie a -> Bool
prop_insert_delete (WS k) v (WT t)
    | T.member k t = ((T.delete k . T.insert k v) .==. T.delete k) $ t
    | otherwise    = ((T.delete k . T.insert k v) .==. id)         $ t

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

-- | 'T.submap' and 'T.deleteSubmap' partition every trie for every key.
prop_deleteSubmap_disunion :: (Eq a) => WS -> WTrie a -> Bool
prop_deleteSubmap_disunion (WS q) (WT t) =
    t == (T.submap q t `TC.disunion` T.deleteSubmap q t)

prop_unionWith :: Eq a => (a -> a -> a) -> WTrie a -> WTrie a -> Bool
prop_unionWith f (WT x) (WT y) =
    TI.wip_unionWith f x y == TC.unionWith f x y

prop_unionL :: Eq a => WTrie a -> WTrie a -> Bool
prop_unionL = prop_unionWith (\x _ -> x)

prop_unionR :: Eq a => WTrie a -> WTrie a -> Bool
prop_unionR = prop_unionWith (\_ y -> y)

prop_unionPlus :: (Eq a, Num a) => WTrie a -> WTrie a -> Bool
prop_unionPlus = prop_unionWith (+)

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
    ((view2trie . TI.updateMaxViewBy (\_ v -> Just v)) .==. id) . unWT

prop_keys :: WTrie a -> Bool
prop_keys = (T.keys .==. (fmap fst . T.toList)) . unWT

prop_elems :: (Eq a) => WTrie a -> Bool
prop_elems = (T.elems .==. (fmap snd . T.toList)) . unWT

----------------------------------------------------------------
-- ~~~~~ 'Foldable' properties

-- * The laws as stated in the documentation.

prop_foldmapIdentity :: (Monoid m, Eq m) => WTrie m -> Bool
prop_foldmapIdentity = (F.fold .==. F.foldMap id) . unWT

prop_foldmapFusion :: (Monoid m, Eq m) => (a -> m) -> WTrie a -> Bool
prop_foldmapFusion f = (F.foldMap f .==. (F.fold . fmap f)) . unWT
    -- entails: foldMap f . fmap g ≡ foldMap (f . g)

{- -- TODO:
foldr f z t ≡ appEndo (F.foldMap (Endo . f) t ) z
foldl f z t ≡ appEndo (getDual (F.foldMap (Dual . Endo . flip f) t)) z
-}

-- By definition we already have @T.size ≡ F.length@.
prop_length :: WTrie a -> Bool
prop_length = (F.length .==. getSum . F.foldMap (Sum . const 1)) . unWT

{- -- TODO: handle the cases of old @base@
prop_sum :: (Num a, Eq a) => WTrie a -> Bool
prop_sum = (F.sum .==. (getSum . F.foldMap' Sum)) . unWT

prop_product :: (Num a, Eq a) => WTrie a -> Bool
prop_product = (F.product .==. (getProduct . F.foldMap' Product)) . unWT

-- These two laws aren't stated in the documentation, but obviously should be.
prop_minimum :: (Ord a, Eq a) => NonEmpty (WTrie a) -> Bool
prop_minimum = (F.minimum .==. (getMin . F.foldMap' Min)) . unWT

prop_maximum :: (Ord a, Eq a) => NonEmpty (WTrie a) -> Bool
prop_maximum = (F.maximum .==. (getMax . F.foldMap' Max)) . unWT
-}


-- * At least make sure these at least have the same order.
-- TODO: better equivalence tests

prop_foldr_vs_foldrWithKey :: Eq a => WTrie a -> Bool
prop_foldr_vs_foldrWithKey =
    (F.foldr (:) [] .==. TI.foldrWithKey (const (:)) []) . unWT

prop_foldr'_vs_foldrWithKey' :: Eq a => WTrie a -> Bool
prop_foldr'_vs_foldrWithKey' =
    (F.foldr' (:) [] .==. TI.foldrWithKey' (const (:)) []) . unWT

prop_foldl_vs_foldlWithKey :: Eq a => WTrie a -> Bool
prop_foldl_vs_foldlWithKey =
    (F.foldl snoc [] .==. TI.foldlWithKey (const . snoc) []) . unWT
    where
    snoc = flip (:)

prop_foldl'_vs_foldlWithKey' :: Eq a => WTrie a -> Bool
prop_foldl'_vs_foldlWithKey' =
    (F.foldl' snoc [] .==. TI.foldlWithKey' (const . snoc) []) . unWT
    where
    snoc = flip (:)

#if MIN_VERSION_base(4,6,0)
prop_foldr_vs_foldr' :: (Eq a) => WTrie a -> Bool
prop_foldr_vs_foldr' = (F.foldr (:) [] .==. F.foldr' (:) []) . unWT

prop_foldl_vs_foldl' :: (Eq a) => WTrie a -> Bool
prop_foldl_vs_foldl' = (F.foldl snoc [] .==. F.foldl' snoc []) . unWT
    where
    snoc = flip (:)
#endif

#if MIN_VERSION_base(4,13,0)
-- TODO: use a non-commutative Monoid, to ensure the order is the same.
prop_foldMap_vs_foldMap' ::  (Num a, Eq a) => WTrie a -> Bool
prop_foldMap_vs_foldMap' = (F.foldMap Sum .==. F.foldMap' Sum) . unWT
#endif

-- TODO: how can we best test that fold{l,r,Map}{,'} are sufficiently lazy\/strict?
-- See: <https://github.com/haskell/containers/blob/master/containers-tests/tests/intmap-strictness.hs>

-- TODO: #if MIN_VERSION_base(4,8,0), check that 'F.null' isn't cyclic definition

----------------------------------------------------------------
-- ~~~~~ 'Traversable' laws

-- | Horizontal composition of Kleisli arrows.
under :: Functor f => (b -> g c) -> (a -> f b) -> a -> Compose f g c
under g f = Compose . fmap g . f

{-
-- TODO: We can't really autogenerate these, but maybe we could
-- have a select few interesting ones?
--
-- | An \"applicative transformation\".
data AT f g where
    AT :: (Applicative f, Applicative g) => forall a. f a -> g a
        -- Such that:
        -- @t (pure x) ≡ pure x@
        -- @t (f <*> x) ≡ t f <*> t x@

prop_TraverseNaturality
    :: (Applicative f, Applicative g, Eq1 g, Eq b)
    => AT f g -> (a -> f b) -> WTrie a -> Bool
prop_TraverseNaturality (AT t) f =
    ((t . traverse f) .==. traverse (t . f)) . unWT
-}

prop_TraverseIdentity :: Eq a => WTrie a -> Bool
prop_TraverseIdentity = (traverse Identity .==. Identity) . unWT

-- TODO: how could we actually test this? Any good @f,g@ to try in particular?
prop_TraverseComposition
    :: (Applicative f, Applicative g, Eq1 g, Eq1 f, Eq c)
    => (b -> g c) -> (a -> f b) -> WTrie a -> Bool
prop_TraverseComposition g f =
    (traverse (g `under` f) .==. (traverse g `under` traverse f)) . unWT

-- NOTE: The next two are redundant, since we don't provide a bespoke
-- implementation of 'sequenceA'.

{-
prop_SequenceaNaturality
    :: (Applicative f, Applicative g, Eq1 g, Eq a)
    => AT f g -> WTrie (f a) -> Bool
prop_SequenceaNaturality (AT t) =
    ((t . sequenceA) .==. (sequenceA . fmap t)) . unWT
-}

prop_SequenceaIdentity :: Eq a => WTrie a -> Bool
prop_SequenceaIdentity =
    ((sequenceA . fmap Identity) .==. Identity) . unWT

-- TODO: how could we actually test this? Any good @f,g@ to try in particular?
prop_SequenceaComposition
    :: (Applicative f, Applicative g, Eq1 g, Eq1 f, Eq a)
    => WTrie (g (f a)) -> Bool
prop_SequenceaComposition =
    ((sequenceA . fmap Compose) .==. (sequenceA `under` sequenceA)) . unWT

-- TODO: reiterate the laws for 'traverseWithKey'...

----------------------------------------------------------------
-- ~~~~~ 'Filterable' laws
-- TODO: how could we actually test these effectively?  Instead of
-- just with particular example functions?

-- | Shorthand alias for a filtering function.
type F a b = a -> Maybe b

-- | Shorthand alias for a predicate function.
type P a = a -> Bool

prop_FiltermapFission :: Eq b => F a b -> WTrie a -> Bool
prop_FiltermapFission f =
    (T.filterMap f .==. (fmap (fromJust . f) . TI.filter (isJust . f))) . unWT

prop_FiltermapFusion :: Eq b => (a -> b) -> P a -> WTrie a -> Bool
prop_FiltermapFusion f g =
    ((fmap f . TI.filter g) .==. T.filterMap (\v -> f v <$ guard (g v))) . unWT

prop_FiltermapConservation :: Eq b => (a -> b) -> WTrie a -> Bool
prop_FiltermapConservation f =
    (T.filterMap (Just . f) .==. fmap f) . unWT

prop_FiltermapVertComposition :: Eq c => F b c -> F a b -> WTrie a -> Bool
prop_FiltermapVertComposition f g =
    ((T.filterMap f . T.filterMap g) .==. T.filterMap (f <=< g)) . unWT

prop_FilterDefinition :: Eq a => P a -> WTrie a -> Bool
prop_FilterDefinition f =
    (TI.filter f .==. T.filterMap (\v -> v <$ guard (f v))) . unWT

prop_FilterVertComposition :: Eq a => P a -> P a -> WTrie a -> Bool
prop_FilterVertComposition f g =
    ((TI.filter f . TI.filter g) .==. TI.filter (liftA2 (&&) f g)) . unWT

-- A handful of interesting predicates...
isTriple :: Integral a => P a
isTriple x = rem x 3 == 0

remTriple :: Integral a => F a a
remTriple x
    | r == 0    = Nothing
    | otherwise = Just r
    where r = rem x 3

quotTriple :: Integral a => F a a
quotTriple x
    | r == 0    = Just q
    | otherwise = Nothing
    where (q,r) = quotRem x 3

----------------------------------------------------------------
-- ~~~~~ 'Witherable' laws
-- TODO: how could we actually test these effectively?

-- | Shorthand alias for a withering function.
type FA a f b = a -> f (Maybe b)

{-
prop_WitherNaturality
    :: (Applicative f, Applicative g, Eq1 g, Eq b)
    => AT f g -> FA a f b -> WTrie a -> Bool
prop_WitherNaturality (AT t) f =
    (TI.wither (t . f) .==. (t . TI.wither f)) . unWT
-}

{-
-- TODO: how can we nail down the functor @f@ but in a way that's portable beyond GHC?
prop_WitherPurity :: (Applicative f, Eq1 f, Eq b) => F a b -> WTrie a -> Bool
prop_WitherPurity f =
    (TI.wither (pure . f) .==. (pure . T.filterMap f)) . unWT
-}

-- HACK: adding the 'Compose' wrapper to force GHC to deduce @Eq
-- (f (T.Trie b))@ from the provided @(Eq1 f, Eq b)@.
prop_WitherConservation
    :: (Applicative f, Eq1 f, Eq b) => (a -> f b) -> WTrie a -> Bool
prop_WitherConservation f =
    ((Compose . TI.wither (fmap Just . f)) .==. (Compose . traverse f)) . unWT

prop_WitherHorizComposition
    :: (Applicative f, Applicative g, Eq1 g, Eq1 f, Eq c)
    => FA b g c -> FA a f b -> WTrie a -> Bool
prop_WitherHorizComposition f g =
    ((TI.wither f `under` TI.wither g) .==. TI.wither (wither_Maybe f `under` g)) . unWT

-- | Variant of wither for Maybe instead of Trie.
wither_Maybe :: Applicative f => FA a f b -> Maybe a -> f (Maybe b)
wither_Maybe f = fmap join . traverse f

-- | Shorthand alias for an effectful predicate function.
type PA a f = a -> f Bool

-- HACK: adding the 'Compose' wrapper to force GHC to deduce @Eq
-- (f (T.Trie a))@ from the provided @(Eq1 f, Eq a)@.
prop_FilteraDefinition :: (Applicative f, Eq1 f, Eq a) => PA a f -> WTrie a -> Bool
prop_FilteraDefinition f =
    ((Compose . TI.filterA f) .==. (Compose . TI.wither (\v -> (v <$) . guard <$> f v))) . unWT

{-
prop_FilteraNaturality
    :: (Applicative f, Applicative g, Eq1 g, Eq a)
    => AT f g -> PA a f -> WTrie a -> Bool
prop_FilteraNaturality (AT t) f =
    (TI.filterA (t . f) .==. (t . TI.filterA f)) . unWT
-}

{-
-- TODO: how can we nail down the @f@ but in a way that's portable beyond GHC?
prop_FilteraPurity :: (Functor f, Eq1 f, Eq a) => P a -> WTrie a -> Bool
prop_FilteraPurity f =
    (TI.filterA (pure . f) .==. (pure . TI.filter f)) . unWT
-}

prop_FilteraHorizComposition
    :: (Applicative f, Applicative g, Eq1 g, Eq1 f, Eq a)
    => PA a f -> PA a g -> WTrie a -> Bool
prop_FilteraHorizComposition f g =
    ((TI.filterA f `under` TI.filterA g) .==. TI.filterA (underA2 (&&) f g)) . unWT

underA2 :: (Applicative f, Applicative g)
        => (b -> c -> d)
        -> (a -> g b)
        -> (a -> f c)
        -> a -> Compose f g d
underA2 h g f = liftA2 (liftA2 h) (g `under` pure) (pure `under` f)

prop_FilteraHorizComposition_alt
    :: (Applicative f, Applicative g, Eq1 g, Eq1 f, Eq a)
    => PA a f -> PA a g -> WTrie a -> Bool
prop_FilteraHorizComposition_alt f g =
    ((TI.filterA f `under` TI.filterA g) .==. TI.filterA (underF2 (&&) g f)) . unWT

underF2 :: (Functor f, Functor g)
        => (b -> c -> d)
        -> (a -> f b)
        -> (a -> g c)
        -> a -> Compose f g d
underF2 h f g a = Compose (f a <&> ((g a <&>) . h))


----------------------------------------------------------------
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

----------------------------------------------------------------
-- ~~~~~ 'Functor' properties

prop_FunctorIdentity :: Eq a => WTrie a -> Bool
prop_FunctorIdentity = (fmap id .==. id) . unWT

{- -- TODO: is there any way to make this remotely testable?
prop_FunctorCompose :: Eq c => (b -> c) -> (a -> b) -> WTrie a -> Bool
prop_FunctorCompose f g = (fmap (f . g) .==. (fmap f . fmap g)) . unWT
-}

#if __GLASGOW_HASKELL__
-- | @(<$)@ agrees with its default definition.
prop_fmapConst :: Eq a => a -> WTrie b -> Bool
prop_fmapConst v = ((v <$) .==. fmap (const v)) . unWT
#endif

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

----------------------------------------------------------------
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

----------------------------------------------------------------
-- ~~~~~ 'Applicative' laws

prop_ApplicativeIdentity :: Eq a => WTrie a -> Bool
prop_ApplicativeIdentity = ((pure id <*>) .==. id) . unWT

{- -- (remaining, untestable) Applicative laws
prop_ApplicativeCompose  = pure (.) <*> u <*> v <*> w == u <*> (v <*> w)
prop_ApplicativeHom      = pure f <*> pure x == pure (f x)
prop_ApplicativeInterchange = u <*> pure y == pure ($ y) <*> u
-}

----------------------------------------------------------------
-- ~~~~~ 'Monad' laws

prop_MonadIdentityR :: Eq a => WTrie a -> Bool
prop_MonadIdentityR = ((>>= return) .==. id) . unWT

{- -- (remaining, untestable) Monad laws
prop_MonadIdentityL = (return a >>= k) == k a
prop_MonadAssoc     = m >>= (\x -> k x >>= h) == (m >>= k) >>= h
-}

----------------------------------------------------------------
-- ~~~~~ 'Semigroup' laws

#if MIN_VERSION_base(4,9,0)
prop_Semigroup :: (Semigroup a, Eq a) => WTrie a -> WTrie a -> WTrie a -> Bool
prop_Semigroup (WT a) (WT b) (WT c) = a <> (b <> c) == (a <> b) <> c

-- TODO: laws for 'stimes'
#endif

----------------------------------------------------------------
-- ~~~~~ 'Monoid' laws

-- N.B., base-4.11.0.0 is when Semigroup became superclass of Monoid
prop_MonoidIdentityL :: (Monoid a, Eq a) => WTrie a -> Bool
prop_MonoidIdentityL = ((mempty `mappend`) .==. id) . unWT

prop_MonoidIdentityR :: (Monoid a, Eq a) => WTrie a -> Bool
prop_MonoidIdentityR = ((`mappend` mempty) .==. id) . unWT

----------------------------------------------------------------
-- ~~~~~ 'Binary' laws

prop_Binary :: (B.Binary a, Eq a) => WTrie a -> Bool
prop_Binary = ((B.decode . B.encode) .==. id) . unWT

----------------------------------------------------------------
----------------------------------------------------------- fin.
