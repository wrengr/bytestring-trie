{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
{-# LANGUAGE CPP, BangPatterns #-}

----------------------------------------------------------------
--                                                  ~ 2022.02.12
-- |
-- Module      :  Bench.MatchOne
-- Copyright   :  2008--2022 wren romano
-- License     :  BSD-3-Clause
-- Maintainer  :  wren@cpan.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Benchmarking definitions for 'TI.match_'
----------------------------------------------------------------

module Main (main) where

import qualified Data.Trie           as T
import qualified Data.Trie.Internal  as TI
import qualified Data.ByteString     as S
import           Control.Applicative (liftA2)
import           Control.DeepSeq     (NFData)
import qualified Test.QuickCheck     as QC
import qualified Criterion.Main      as C
-- TODO: consider trying <https://hackage.haskell.org/package/tasty-bench>.  Especially so we can add tests to ensure that all these implementations are correct; but also for the comparisons across versions (since @bench-show@ and @benchgraph@ are both too heavy-weight for our needs).
----------------------------------------------------------------

-- Using NOINLINE to improve the reliability\/quality of the benchmarks.
match_v027, match_foldl, match_foldr, match_foldr_alt
    :: T.Trie a -> S.ByteString -> Maybe (Int, a)

-- The manual implementation.
{-# NOINLINE match_v027 #-}
match_v027 = TI.match_

-- This implementation is based on @base-4.16.0.0@:'GHC.List.last'.
-- N.B., the implementation uses 'foldl' in order to be a good
-- consumer, contrary to the 'foldr' we would have expected; see:
-- <https://gitlab.haskell.org/ghc/ghc/-/issues/9339>
-- Also, the actual definition of 'GHC.List.last' must be eta-expanded
-- in order to actually have 'foldl' make it a good consumer; see:
-- <https://gitlab.haskell.org/ghc/ghc/-/issues/10260>
-- (Not that that's relevant for us, since we're inlining the
-- definition anyways, and the call-site is indeed saturated.)
--
-- TODO: since when does 'foldl' make good consumers?  Did GHC
-- switch from build\/foldr to unfold\/destroy?
--
-- Per the benchmarks below, this one is by far the slowest; thereby
-- suggesting that 'foldl' is not in fact a good consumer!
{-# NOINLINE match_foldl #-}
match_foldl t q = foldl (\_ x -> Just x) Nothing (TI.matches_ t q)

-- This uses the definition of 'GHC.List.last' prior to #9339.  Note
-- how @step@ returns the @Just@ immediately, and only lazily does
-- the case analysis.
--
-- Per the benchmarks below, this is better than 'match_foldl' but
-- not as good as 'match_foldr_alt'
{-# NOINLINE match_foldr #-}
match_foldr t q = foldr step Nothing (TI.matches_ t q)
    where
    step x m = Just (case m of { Nothing -> x; Just y -> y })

-- And here's a version that doesn't do that @Just@-before-case...
--
-- Per the benchmarks below, this is better than 'match_foldr' but
-- still not as good as 'match_v027'.
{-# NOINLINE match_foldr_alt #-}
match_foldr_alt t q = foldr step Nothing (TI.matches_ t q)
    where
    step x Nothing = Just x
    step _ y       = y

-- TODO: maybe try a Codensity version of the above two, to avoid
-- redundant case analysis on the intermediate 'Maybe'

----------------------------------------------------------------
-- TODO: move this stuff off to the shared file.
-- TODO: should have argument to bound the 'Word8'
arbitraryBS :: Int -> QC.Gen S.ByteString
arbitraryBS maxL = do
    l  <- QC.chooseInt (0, maxL)
    xs <- QC.vector l
    return $! S.pack xs

arbitraryTrie :: Int -> Int -> QC.Gen (T.Trie Int)
arbitraryTrie maxK maxL = do
    k    <- QC.chooseInt (0, maxK)
    keys <- QC.vectorOf k $ arbitraryBS maxL
    return $! T.fromList (zip keys [0..])

-- TODO: really need a better environment to work on than this...
generateEnv :: IO ([T.Trie Int], [S.ByteString])
generateEnv = QC.generate $ do
    ts <- QC.vectorOf 10 $ arbitraryTrie 30 10
    qs <- QC.vectorOf 10 $ arbitraryBS 12
    return (ts, qs)

cartesianNF :: NFData c => (a -> b -> c) -> [a] -> [b] -> C.Benchmarkable
cartesianNF f xs ys = C.nf (liftA2 f xs) ys

----------------------------------------------------------------
-- TODO: <https://github.com/haskell/containers/blob/master/containers-tests/benchmarks/IntMap.hs>
-- uses 'Control.Exception.evaluate' instead of 'C.env'.  Is that
-- because of using the @gauge@ library instead of @criterion@, or
-- is there some other reason?
main :: IO ()
main = C.defaultMain
  [ C.env generateEnv $ \ ~(ts, qs) ->
    C.bgroup "MatchOne"
    [ C.bench "match_v027"      $ cartesianNF match_v027  ts qs
    , C.bench "match_foldl"     $ cartesianNF match_foldl ts qs
    , C.bench "match_foldr"     $ cartesianNF match_foldr ts qs
    , C.bench "match_foldr_alt" $ cartesianNF match_foldr_alt ts qs
    ]
  ]

----------------------------------------------------------------
----------------------------------------------------------- fin.
