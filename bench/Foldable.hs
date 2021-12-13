{-# OPTIONS_GHC -Wall -fwarn-tabs #-}

----------------------------------------------------------------
--                                                  ~ 2021.12.12
-- |
-- Module      :  bench/Foldable
-- Copyright   :  2008--2021 wren gayle romano
-- License     :  BSD3
-- Maintainer  :  wren@cpan.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Benchmarking definitions for 'Foldable'
----------------------------------------------------------------

module Main (main) where

import qualified Data.Trie           as T
import qualified Data.Trie.Internal  as TI
import qualified Data.ByteString     as S
import           Data.Coerce         (Coercible, coerce)
import           Data.Semigroup      (Endo(..))
import           Data.Word           (Word8)
import           Control.DeepSeq     (NFData(rnf))
import qualified Test.QuickCheck     as QC
import qualified Criterion.Main      as C
import qualified Criterion.Measurement.Types as C (Benchmarkable(..))
import qualified Criterion.Measurement.Types.Internal as C (nf')
-- TODO: consider using the @gauge@ library instead of @criterion@.
-- It's a clone of criterion, also by BOS; with the intention of
-- having reduced dependencies.  It doesn't do the HTML output, but
-- otherwise should be fine for our needs.  It's what the @containers@
-- library is using these days.
----------------------------------------------------------------

-- | Reference definition, to avoid needing to expose constructors
-- of the real definition.
--
-- TODO: should maybe have a CPP macro to expose them for benchmarking,
-- to ensure this file stays in sync!
data Trie a
    = Branch {-# UNPACK #-} !Word8 -- Prefix
             {-# UNPACK #-} !Word8 -- Mask
                            !(Trie a)
                            !(Trie a)
    | Arc    {-# UNPACK #-} !S.ByteString
                            !(Maybe a)
                            !(Trie a)
    | Empty
    deriving Show

-- Needed for 'C.env'
instance NFData a => NFData (Trie a) where
    rnf Empty            = ()
    rnf (Arc _ mv t)     = rnf mv `seq` rnf t
    rnf (Branch _ _ l r) = rnf l `seq` rnf r

----------------------------------------------------------------
foldr_EndoDefault, foldr_compose, foldr_eta, foldr_cps_eta, foldr_cps, foldr_noClosure
    :: (a -> b -> b) -> b -> Trie a -> b

-- The default definition as of base-4.16.0.0
-- Actually a pretty solid baseline.
foldr_EndoDefault f z t =
    appEndo (foldMap_bytestringtrie000207 (Endo #. f) t) z

-- From "Data.Functor.Utils", but isn't exported.
(#.) :: Coercible b c => (b -> c) -> (a -> b) -> (a -> c)
(#.) _f = coerce
{-# INLINE (#.) #-}

-- bytestring-trie-0.2.7 definition
foldMap_bytestringtrie000207 :: Monoid m => (a -> m) -> Trie a -> m
foldMap_bytestringtrie000207 f = go
    where
    go Empty              = mempty
    go (Arc _ Nothing  t) = go t
    go (Arc _ (Just v) t) = f v `mappend` go t
    go (Branch _ _ l r)   = go l `mappend` go r

-- bytestring-trie-0.2.7 definition
-- Identical allocation as EndoDefault; about the same speed, or a
-- slightly slower
foldr_compose f z0 = \t -> go t z0 -- eta for better inlining.
    where
    go Empty              = id
    go (Arc _ Nothing  t) =       go t
    go (Arc _ (Just v) t) = f v . go t
    go (Branch _ _ l r)   = go l . go r

-- Usually little faster than EndoDefault, occasionally about the
-- same, rarely slower.  However, this allocates (consistently) ~43% more.
-- [I'm thinking because (a) larger thunks to store the @z@, but
-- (b) fully-saturated thunks so faster apply?]
foldr_eta f z0 = \t -> go t z0 -- eta for better inlining.
    where
    go Empty              z = z
    go (Arc _ Nothing  t) z =      go t z
    go (Arc _ (Just v) t) z = f v (go t z)
    go (Branch _ _ l r)   z = go l (go r z)

-- Much slower; also allocates much more.
foldr_cps_eta f z0 = \t -> go t id z0 -- eta for better inlining.
    where
    go Empty              c z = c z
    go (Arc _ Nothing  t) c z = go t c z
    go (Arc _ (Just v) t) c z = go t (c . f v) z
    go (Branch _ _ l r)   c z = go r (go l c) z

-- Even slower; and even more allocation.
foldr_cps f z0 = \t -> go t id z0 -- eta for better inlining.
    where
    go Empty              c = c
    go (Arc _ Nothing  t) c = go t c
    go (Arc _ (Just v) t) c = go t (c . f v)
    go (Branch _ _ l r)   c = go r (go l c)

-- Faster than 'foldr_cps_eta', but still much slower than
-- EndoDefault/conpose/eta; Also allocates about as much as
-- 'foldr_cps_eta'.
foldr_noClosure _ z Empty              = z
foldr_noClosure f z (Arc _ Nothing  t) =      foldr_noClosure f z t
foldr_noClosure f z (Arc _ (Just v) t) = f v (foldr_noClosure f z t)
foldr_noClosure f z (Branch _ _ l r)   =
    foldr_noClosure f (foldr_noClosure f z r) l

----------------------------------------------------------------
{-
-- TODO: the base-4.16.0.0 defaults are shown

fold       = foldMap id
foldMap  f = foldr (mappend . f) mempty
foldMap' f = foldl' (\ acc a -> acc <> f a) mempty
foldr f z t = appEndo (foldMap (Endo #. f) t) z
foldr' f z0 xs = foldl f' id xs z0 where f' k x z = k $! f x z
foldl f z t = appEndo (getDual (foldMap (Dual . Endo . flip f) t)) z
foldl' f z0 xs = foldr f' id xs z0 where f' x k z = k $! f z x
{-# INLINE toList #-} toList t = build (\ c n -> foldr c n t)
null = foldr (\_ _ -> False) True
length = foldl' (\c _ -> c+1) 0
elem = any . (==)
-- With standalone definition: any p = getAny #. foldMap (Any #. p)

{-# INLINEABLE maximum #-}
maximum = fromMaybe (errorWithoutStackTrace "maximum: empty structure") .
    getMax . foldMap' (Max #. (Just :: a -> Maybe a)) -- -XScopedTypeVariables
{-# INLINEABLE minimum #-}
minimum = fromMaybe (errorWithoutStackTrace "minimum: empty structure") .
    getMin . foldMap' (Min #. (Just :: a -> Maybe a)) -- -XScopedTypeVariables
{-# INLINEABLE sum #-}
sum = getSum #. foldMap' Sum
{-# INLINEABLE product #-}
product = getProduct #. foldMap' Product
-}




----------------------------------------------------------------
----------------------------------------------------------------
arbitraryTrie :: Int -> Int -> QC.Gen (Trie Int)
arbitraryTrie maxK maxL = do
    k    <- QC.chooseInt (0, maxK)
    keys <- QC.vectorOf k $ do
        l  <- QC.chooseInt (0, maxL)
        xs <- QC.vector l
        return $ S.pack xs
    return . realTrie_to_benchTrie . T.fromList $ zip keys [0..]

-- TODO: still gotta reconstruct the 'Prefix' and 'Mask'.
-- (Even though none of the folds depend on them...)
realTrie_to_benchTrie :: T.Trie a -> Trie a
realTrie_to_benchTrie = TI.cata_ Arc (Branch 0 0) Empty

-- | Generate a new argument for each batch. (This code is based
-- on 'C.perBatchEnv')
--
-- BUG: actually this is no good for our needs, since we want to
-- reuse the same generated values for all the benchmarks (else
-- we'll get garbage numbers due to the wide variance in input
-- argument size).  And alas, the \"batch\" notion only exists at
-- the 'C.Benchmarkable' level, not at the 'C.Benchmark' level; so
-- our idea can't be done actually.
generatePerBatch
    :: (NFData a, NFData b) => QC.Gen a -> (a -> b) -> C.Benchmarkable
generatePerBatch gen f =
    C.Benchmarkable
        (\_ -> QC.generate gen)
        (\_ _ -> return ())
        (C.nf' rnf f)
        False

----------------------------------------------------------------
-- BUG: since we started generating a collection of tries, we've
-- started to see different results: each test progressively slower
-- than the last.  Seems bogus, but not sure what's up since criterion
-- ought to be forcing all of @ts@ early and doing GC between each
-- benchmark...
main :: IO ()
main = C.defaultMain
  [ C.env (QC.generate $ QC.vectorOf 5 $ arbitraryTrie 50 20) $ \ ts ->
    C.bgroup "arbitrary"
    [ C.bgroup "foldr"
      [ C.bench "EndoDefault" $ C.nf (foldr_EndoDefault (+) 0 <$>) ts
      , C.bench "compose"     $ C.nf (foldr_compose     (+) 0 <$>) ts
      , C.bench "eta"         $ C.nf (foldr_eta         (+) 0 <$>) ts
      , C.bench "cps_eta"     $ C.nf (foldr_cps_eta     (+) 0 <$>) ts
      , C.bench "cps"         $ C.nf (foldr_cps         (+) 0 <$>) ts
      , C.bench "noClosure"   $ C.nf (foldr_noClosure   (+) 0 <$>) ts
      ]
    ]
  ]

----------------------------------------------------------------
----------------------------------------------------------- fin.
