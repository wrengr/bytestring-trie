{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
{-# LANGUAGE CPP
           , MultiParamTypeClasses
           , FlexibleContexts
           #-}

----------------------------------------------------------------
--                                                  ~ 2021.12.13
-- |
-- Module      :  Test.Main
-- Copyright   :  2008--2021 wren gayle romano
-- License     :  BSD3
-- Maintainer  :  wren@cpan.org
-- Stability   :  provisional
-- Portability :  semi-portable (MPTC,...)
--
-- Testing 'Trie's.
----------------------------------------------------------------
module Main (main) where

#if !(MIN_VERSION_base(4,13,0))
-- [aka GHC 8.8.1]: For some reason we get an \"unused import\"
-- warning for these versions, even though the Prelude doesn't
-- actually re-export 'Data.Semigroup.Sum'.
-- FIXME: getting that unused import issue again for GHC 8.0--8.6.
import Shared.Sum
#endif
import Test.Utils
import Test.Properties (smallcheckTests, quickcheckTests)

import qualified Data.Trie              as T
import qualified Data.ByteString        as S

import qualified System.Exit            as System (exitSuccess, exitFailure)
import qualified System.IO              as System (hPutStrLn, stderr)
import qualified Test.Tasty.Ingredients as Tasty (tryIngredients)
import qualified Test.Tasty.Options     as Tasty (singleOption, OptionSet)
import qualified Test.Tasty.Runners     as Tasty (installSignalHandlers, parseOptions)
import qualified Test.Tasty             as Tasty
import qualified Test.Tasty.SmallCheck  as SC
import qualified Test.Tasty.QuickCheck  as QC

#if MIN_VERSION_base(4,13,0)
-- [aka GHC 8.8.1]: Prelude re-exports 'Semigroup'.
#elif MIN_VERSION_base(4,9,0)
-- [aka GHC 8.0.1]: "Data.Semigroup" added to base.
import Data.Semigroup      (Semigroup((<>)))
#elif MIN_VERSION_base(4,5,0)
-- [aka GHC 7.4.1]: @(<>)@ added to "Data.Monoid".
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
----------------------------------------------------------- fin.
