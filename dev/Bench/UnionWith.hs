{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
{-# LANGUAGE CPP, BangPatterns #-}

----------------------------------------------------------------
--                                                  ~ 2022.02.21
-- |
-- Module      :  Bench.UnionWith
-- Copyright   :  2008--2022 wren romano
-- License     :  BSD-3-Clause
-- Maintainer  :  wren@cpan.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Benchmarking the benefits of 'TI.wip_unionWith' vs 'TC.unionWith'.
----------------------------------------------------------------

module Main (main) where

import qualified Data.Trie           as T
import qualified Data.Trie.Internal  as TI
import qualified Data.Trie.Convenience as TC
import qualified Data.ByteString     as S
import           Control.Applicative (liftA2)
import           Control.DeepSeq     (NFData)
import qualified Test.QuickCheck     as QC
import qualified Criterion.Main      as C
----------------------------------------------------------------


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
generateEnv :: IO [T.Trie Int]
generateEnv = QC.generate $ QC.vectorOf 10 $ arbitraryTrie 30 10

cartesianNF :: NFData c => (a -> b -> c) -> [a] -> [b] -> C.Benchmarkable
cartesianNF f xs ys = C.nf (liftA2 f xs) ys

----------------------------------------------------------------
-- TODO: <https://github.com/haskell/containers/blob/master/containers-tests/benchmarks/IntMap.hs>
-- uses 'Control.Exception.evaluate' instead of 'C.env'.  Is that
-- because of using the @gauge@ library instead of @criterion@, or
-- is there some other reason?
main :: IO ()
main = C.defaultMain
  [ C.env generateEnv $ \ ts ->
    C.bgroup "UnionWith"
    [ C.bench "TI.wip_unionWith" $ cartesianNF (TI.wip_unionWith (+)) ts ts
    , C.bench "TC.unionWith"     $ cartesianNF (TC.unionWith     (+)) ts ts
    ]
  ]

----------------------------------------------------------------
----------------------------------------------------------- fin.
