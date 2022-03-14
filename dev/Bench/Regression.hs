{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
{-# LANGUAGE CPP, BangPatterns #-}

----------------------------------------------------------------
--                                                  ~ 2022.03.12
-- |
-- Module      :  Bench.Regression
-- Copyright   :  2008--2022 wren romano
-- License     :  BSD-3-Clause
-- Maintainer  :  wren@cpan.org
-- Stability   :  provisional
-- Portability :  portable (with bang-patterns)
--
----------------------------------------------------------------

module Bench.Regression (main) where

import           Shared.BaseCompat
import qualified Data.Trie          as T
-- import qualified Data.Trie.Internal as TI
import qualified Data.ByteString    as S

#if MIN_VERSION_base(4,7,0)
-- [GHC 7.8.1]: "Data.Coerce" added to base.
import           Data.Coerce        (Coercible, coerce)
#endif
import qualified Data.Foldable      as F
import qualified Criterion.Main     as C
import qualified System.IO          as Sys (withFile, IOMode(..), hIsEOF)
import qualified System.Environment as Sys (getProgName, getArgs, withArgs)
import qualified System.Exit        as Sys (exitFailure)
----------------------------------------------------------------

-- | Read a file and insert each line into a trie with its (base-0)
-- line number.
readTrieFromFile :: FilePath -> IO (T.Trie Int)
readTrieFromFile file = Sys.withFile file Sys.ReadMode (go 0 T.empty)
    where
    go !i !t !h = do
        b <- Sys.hIsEOF h
        if b
          then return t
          else do
            line <- S.hGetLine h
            go (i+1) (T.insert line i t) h

-- TODO: maybe add some more CLI structure, for things like choosing
-- to use random inputs instead of file, or parameters of randomness,
-- etc.
main :: IO ()
main  = do
    args <- Sys.getArgs
    case args of
      [] -> do
        prog <- Sys.getProgName
        putStrLn "ERROR: Missing file argument"
        putStrLn $ "Usage: " ++ prog ++ " FILE [criterionFlags]"
        Sys.exitFailure
      file:rest ->
        Sys.withArgs rest $ C.defaultMain
          [ C.env (readTrieFromFile file) $ \ t ->
            bgroup_Foldable t
            -- TODO: more benchmark suites here.
          ]

----------------------------------------------------------------
----------------------------------------------------------------
type BenchmarkE     env = env -> C.Benchmark
type BenchmarkableE env = env -> C.Benchmarkable

bgroupE :: String -> [BenchmarkE env] -> BenchmarkE env
bgroupE name bs = \e -> C.bgroup name (($ e) <$> bs)
{-# INLINE bgroupE #-}

benchE :: String -> BenchmarkableE env -> BenchmarkE env
benchE name b = \e -> C.bench name (b e)
{-# INLINE benchE #-}

----------------------------------------------------------------
#if MIN_VERSION_base(4,7,0)
{-
-- | From "Data.Functor.Utils", but isn't exported.  Is used heavily
-- by the default implementations, since they use so many newtype
-- wrappers.
(#.) :: Coercible b c => (b -> c) -> (a -> b) -> (a -> c)
(#.) _ = coerce
{-# INLINE (#.) #-}
-}

-- My own variant, ignoring the second argument instead.
(.#) :: Coercible a b => (b -> c) -> (a -> b) -> (a -> c)
(.#) f _ = coerce f
{-# INLINE (.#) #-}
#endif

----------------------------------------------------------------
-- Hopefully the @fmap Sum@ will be rewritten into 'coerce'.  Though
-- we may want to do that explicitly...
bgroup_Foldable :: BenchmarkE (T.Trie Int)
bgroup_Foldable =
  bgroupE "Foldable"
  [ benchE "fold" $ C.nf
-- FIXME: the version using @(. fmap Sum)@ is monstrously slower
-- than the version using 'coerce'.  On the one hand that makes
-- sense, but on the other hand it means we really should provide
-- rewrite rules for our Functor instance.
#if MIN_VERSION_base(4,7,0)
      (F.fold .# fmap Sum)
#else
      (F.fold . fmap Sum)
#endif
  , benchE "foldMap"   $ C.nf (F.foldMap  Sum)
#if MIN_VERSION_base(4,13,0)
  , benchE "foldMap'"  $ C.nf (F.foldMap' Sum)
#endif
-- (2021.03.12): according to this benchmark, our hand-rolled 'foldr'
-- is indeed monstrously faster than the default implementation.
  , benchE "foldr"     $ C.nf (F.foldr  (+) 0)
#if MIN_VERSION_base(4,6,0)
  , benchE "foldr'"    $ C.nf (F.foldr' (+) 0)
#endif
  , benchE "foldl"     $ C.nf (F.foldl  (+) 0)
#if MIN_VERSION_base(4,6,0)
  , benchE "foldl'"    $ C.nf (F.foldl' (+) 0)
#endif
#if MIN_VERSION_base(4,8,0)
  , benchE "length" $ C.nf F.length
  {-
  , benchE "maximum" $ C.nf F.maximum -- Must first ensure non-empty!
  , benchE "minimum" $ C.nf F.minimum -- Must first ensure non-empty!
  -}
#else
  , benchE "size" $ C.nf T.size
#endif
  ]

----------------------------------------------------------------
----------------------------------------------------------- fin.
