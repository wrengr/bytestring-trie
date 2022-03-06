{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
{-# LANGUAGE BangPatterns #-}

----------------------------------------------------------------
--                                                  ~ 2022.03.04
-- |
-- Module      :  Bench.Foldable
-- Copyright   :  2008--2022 wren romano
-- License     :  BSD-3-Clause
-- Maintainer  :  wren@cpan.org
-- Stability   :  provisional
-- Portability :  portable (with bang-patterns)
--
-- Benchmarking definitions for 'Foldable'
----------------------------------------------------------------

module Bench.Foldable (main, realTrie_to_benchTrie, bgroup_Foldable) where

import           Shared.Sum
import qualified Data.Trie           as T
import qualified Data.Trie.Internal  as TI

import qualified Data.ByteString     as S
-- TODO: "Data.Coerce" requires MIN_VERSION_base(4,7,0)
import           Data.Coerce         (Coercible, coerce)
import           Data.Semigroup      (Endo(..))
import           Data.Monoid         (Dual(..))
import           Data.Word           (Word8)
import           Control.DeepSeq     (NFData(rnf))
import qualified Test.QuickCheck     as QC
import qualified Criterion.Main      as C
-- TODO: consider using the @gauge@ library instead of @criterion@.
-- It's a clone of criterion, also by BOS; with the intention of
-- having reduced dependencies.  It doesn't do the HTML output, but
-- otherwise should be fine for our needs.  It's what the @containers@
-- library is using these days.
-- BUG: @gauge@ depends on @basement@ which as of version 0.0.12
-- doesn't support GHC 9.2; so we'll have to revisit this later.
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
-- | From "Data.Functor.Utils", but isn't exported.  Is used heavily
-- by the default implementations, since they use so many newtype
-- wrappers.
(#.) :: Coercible b c => (b -> c) -> (a -> b) -> (a -> c)
(#.) _f = coerce
{-# INLINE (#.) #-}

----------------------------------------------------------------
fold_foldMap, fold_foldrCompose, fold_foldrEta, fold_v027
    :: Monoid m => Trie m -> m

-- | The default 'fold' definition as of base-4.16.0.0
fold_foldMap = foldMap_v027 id

-- | Default 'fold' via default 'foldMap' (using 'foldr_compose'),
-- but inlining the 'id' away.
-- Far worse than 'fold_foldMap', both for runtime and allocation.
fold_foldrCompose = foldr_compose mappend mempty

-- | Default 'fold' via default 'foldMap' (using 'foldr_eta'),
-- but inlining the 'id' away.
-- This is somewhat worse than 'fold_foldrCompose' even.
fold_foldrEta = foldr_eta mappend mempty

-- | bytestring-trie-0.2.7 definition.
-- This is a clear win over 'fold_foldMap': about half the runtime,
-- and one eighth the allocations.
fold_v027 = go
    where
    go Empty              = mempty
    go (Arc _ Nothing  t) = go t
    go (Arc _ (Just v) t) = v `mappend` go t
    go (Branch _ _ l r)   = go l `mappend` go r

----------------------------------------------------------------
foldMap_foldrCompose, foldMap_foldrEta, foldMap_v027
    :: Monoid m => (a -> m) -> Trie a -> m

-- | The default 'fold' definition as of base-4.16.0.0 (using 'foldr_compose').
foldMap_foldrCompose f = foldr_compose (mappend . f) mempty
-- | The default 'fold' definition as of base-4.16.0.0 (using 'foldr_eta').
foldMap_foldrEta     f = foldr_eta     (mappend . f) mempty

-- | bytestring-trie-0.2.7 definition, also used elsewhere for defaults.
-- (This was also the bytestring-trie-0.2.6 definition; the only
-- thing defined for that version.)
-- Again, this is the clear winner over the default implementation.
foldMap_v027 f = go
    where
    go Empty              = mempty
    go (Arc _ Nothing  t) = go t
    go (Arc _ (Just v) t) = f v `mappend` go t
    go (Branch _ _ l r)   = go l `mappend` go r

----------------------------------------------------------------
foldMap'_default, foldMap'_v027, foldMap'_v027_cps, foldMap'_v027_flop, foldMap'_v027_bang, foldMap'_v027_flop_bang
    :: Monoid m => (a -> m) -> Trie a -> m

-- | default definition.
foldMap'_default f = foldl'_v027 (\ acc a -> acc <> f a) mempty

-- | bytestring-trie-0.2.7 definition.
-- Again, this is the clear winner over the default implementation.
foldMap'_v027 f = go mempty
    where
    go !m Empty              = m
    go  m (Arc _ Nothing  t) = go m t
    go  m (Arc _ (Just v) t) = go (m `mappend` f v) t
    go  m (Branch _ _ l r)   = go (go m l) r

-- | CPS variant, to ensure proper tail-recursion.
-- In between 'foldMap'_v027' and 'foldMap'_default'.
foldMap'_v027_cps f = \t -> go t mempty id
    where
    go Empty              !m c = c m
    go (Arc _ Nothing  t)  m c = go t m c
    go (Arc _ (Just v) t)  m c = go t (m `mappend` f v) c
    go (Branch _ _ l r)    m c = go l m (\m' -> go r m' c)

-- | Based on our experience with 'foldl_v027_flop', trying the flop here.
-- On the small tries tested here, is slightly slower than
-- 'foldMap'_v027'; identical allocation.  On larger tries this
-- gets to be slightly better than 'foldMap'_v027'.
foldMap'_v027_flop f = \t -> go t mempty
    where
    go Empty              !m = m
    go (Arc _ Nothing  t)  m = go t m
    go (Arc _ (Just v) t)  m = go t (m `mappend` f v)
    go (Branch _ _ l r)    m = go r (go l m)

-- | This really shouldn't matter, but given our experience with 'foldl'_v027_flop_bang'...
-- And yet, is slightly better than 'foldMap'_v027_flop', slightly
-- worse than 'foldMap'_v027_flop_bang'; identical allocation for
-- all these variants.
foldMap'_v027_bang f = go mempty
    where
    go !m Empty              = m
    go  m (Arc _ Nothing  t) = go m t
    go  m (Arc _ (Just v) t) = (go $! mappend m (f v)) t
    go  m (Branch _ _ l r)   = (go $! go m l) r

-- | This really shouldn't matter, but given our experience with 'foldl'_v027_flop_bang'...
-- And yet, is slightly better than 'foldMap'_v027_bang', slightly
-- worse than 'foldMap'_v027'; identical allocation for all these
-- variants.  For larger tries, is slightly slower than 'foldMap'_v027_flop'.
foldMap'_v027_flop_bang f = \t -> go t mempty
    where
    go Empty              !m = m
    go (Arc _ Nothing  t)  m = go t m
    go (Arc _ (Just v) t)  m = go t $! mappend m (f v)
    go (Branch _ _ l r)    m = go r $! go l m

----------------------------------------------------------------
foldr_default, foldr_compose, foldr_eta, foldr_cps_eta, foldr_cps, foldr_noClosure
    :: (a -> b -> b) -> b -> Trie a -> b

-- | The default definition as of base-4.16.0.0
-- Actually a pretty solid baseline, for small tries.  And for
-- larger tries it's far and away the clear winner.
foldr_default f z t =
    appEndo (foldMap_v027 (Endo #. f) t) z

-- | bytestring-trie-0.2.7 definition
-- Identical allocation as EndoDefault; about the same speed, or a
-- slightly slower; but only for the small tries tested here.  For
-- larger tries it's far worse than 'foldr_default'.
foldr_compose f z0 = \t -> go t z0 -- eta for better inlining.
    where
    go Empty              = id
    go (Arc _ Nothing  t) =       go t
    go (Arc _ (Just v) t) = f v . go t
    go (Branch _ _ l r)   = go l . go r

-- |
-- Usually little faster than EndoDefault, occasionally about the
-- same, rarely slower.  However, this allocates (consistently) ~43% more.
-- [I'm thinking because (a) larger thunks to store the @z@, but
-- (b) fully-saturated thunks so faster apply?]
-- For larger tries, is also far worse than 'foldr_default'.
foldr_eta f z0 = \t -> go t z0 -- eta for better inlining.
    where
    go Empty              z = z
    go (Arc _ Nothing  t) z =      go t z
    go (Arc _ (Just v) t) z = f v (go t z)
    go (Branch _ _ l r)   z = go l (go r z)

-- | Much slower; also allocates much more. (For larger tries too)
foldr_cps_eta f z0 = \t -> go t id z0 -- eta for better inlining.
    where
    go Empty              c z = c z
    go (Arc _ Nothing  t) c z = go t c z
    go (Arc _ (Just v) t) c z = go t (c . f v) z
    go (Branch _ _ l r)   c z = go r (go l c) z

-- | Even slower; and even more allocation. (For larger tries too)
foldr_cps f z0 = \t -> go t id z0 -- eta for better inlining.
    where
    go Empty              c = c
    go (Arc _ Nothing  t) c = go t c
    go (Arc _ (Just v) t) c = go t (c . f v)
    go (Branch _ _ l r)   c = go r (go l c)

-- | Faster than 'foldr_cps_eta', but still much slower than
-- EndoDefault/compose/eta; Also allocates about as much as
-- 'foldr_cps_eta'.
foldr_noClosure _ z Empty              = z
foldr_noClosure f z (Arc _ Nothing  t) =      foldr_noClosure f z t
foldr_noClosure f z (Arc _ (Just v) t) = f v (foldr_noClosure f z t)
foldr_noClosure f z (Branch _ _ l r)   =
    foldr_noClosure f (foldr_noClosure f z r) l

----------------------------------------------------------------
foldr'_default, foldr'_v027, foldr'_v027_cps, foldr'_cps, foldr'_v027_flop, foldr'_v027_bang, foldr'_v027_flop_bang
    :: (a -> b -> b) -> b -> Trie a -> b

-- | The default definition as of base-4.16.0.0 (including the phrasing
-- of using a where-clause rather than a lambda).
-- The worst of the lot.
foldr'_default f z0 xs =
    foldl_v027 f' id xs z0
    where f' k x z = k $! f x z

-- | bytestring-trie-0.2.7 definition.
-- This one is the clear winner, both for time and allocation.
foldr'_v027 f z0 = go z0 -- eta for better inlining
    where
    go !z Empty              = z
    go  z (Arc _ Nothing  t) = go z t
    go  z (Arc _ (Just v) t) = f v $! go z t
    go  z (Branch _ _ l r)   = go (go z r) l

-- TODO: (2022.03.05) Try a variant of 'foldr'_v027' which doesn't
-- closure over @f@.

-- | Worse than 'foldr'_v027' but still better than the rest.
foldr'_v027_cps f z0 = \t -> go t z0 id -- eta for better inlining
    where
    go Empty              !z c = c z
    go (Arc _ Nothing  t)  z c = go t z c
    go (Arc _ (Just v) t)  z c = go t z (\ !z' -> c $! f v z')
    go (Branch _ _ l r)    z c = go r z (\ !z' -> go l z' c)

-- | Worse than 'foldr'_v027_cps' but still better than 'foldr'_default'
foldr'_cps f z0 = \t -> go t id z0 -- eta for better inlining
    where
    go Empty              c = c
    go (Arc _ Nothing  t) c = go t c
    go (Arc _ (Just v) t) c = go t (\ !z -> c $! f v z)
    go (Branch _ _ l r)   c = go r (go l c)

-- | Based on our experience with 'foldl_v027_flop', trying the flop here.
-- Actually faster than 'foldr'_v027' (but only for small tries);
-- identical allocation. For larger tries, this is a bit slower
-- than 'foldr'_v027'.
foldr'_v027_flop f z0 = \t -> go t z0 -- eta for better inlining
    where
    go Empty              !z = z
    go (Arc _ Nothing  t)  z = go t z
    go (Arc _ (Just v) t)  z = f v $! go t z
    go (Branch _ _ l r)    z = go l (go r z)

-- | This really shouldn't matter, but given our experience with 'foldl'_v027_flop_bang'...
-- And yet, faster than 'foldr'_cps', slower than 'foldr'_v027_flop'.
foldr'_v027_bang f z0 = go z0 -- eta for better inlining
    where
    go !z Empty              = z
    go  z (Arc _ Nothing  t) = go z t
    go  z (Arc _ (Just v) t) = f v $! go z t
    go  z (Branch _ _ l r)   = (go $! go z r) l

-- | This really shouldn't matter, but given our experience with 'foldl'_v027_flop_bang'...
-- And yet, marginally faster than 'foldr'_v027_flop', more
-- significantly slower than 'foldr'_v027'.
foldr'_v027_flop_bang f z0 = \t -> go t z0 -- eta for better inlining
    where
    go Empty              !z = z
    go (Arc _ Nothing  t)  z = go t z
    go (Arc _ (Just v) t)  z = f v $! go t z
    go (Branch _ _ l r)    z = go l $! go r z

----------------------------------------------------------------
foldl_default, foldl_default_Coerce, foldl_v027, foldl_v027_flop
    :: (b -> a -> b) -> b -> Trie a -> b

-- | The default definition as of base-4.16.0.0.
foldl_default f z t =
    appEndo (getDual (foldMap_v027 (Dual . Endo . flip f) t)) z

-- | Variant of the default that uses @('#.')@ in lieu of @('.')@.
-- This one performs massively better than 'foldl_default'.
-- TODO: send a patch to @base@ for this.
-- Hrm, for larger tries it looks like the benefit is only minimal...
foldl_default_Coerce f z t =
    appEndo (getDual (foldMap_v027 (Dual #. Endo #. flip f) t)) z

-- | bytestring-trie-0.2.7 definition, also used for defaults.
-- This one is faster than 'foldl_default_Coerce'; though it allocates
-- ~56% more than 'foldl_default_Coerce' (albeit ~11% /less/ than
-- 'foldl_default').
foldl_v027 f z0 = go z0 -- eta for better inlining
    where
    go z Empty              = z
    go z (Arc _ Nothing  t) = go z t
    go z (Arc _ (Just v) t) = go (f z v) t
    go z (Branch _ _ l r)   = go (go z l) r

-- | Since 'foldr_eta' allocates ~43% more than 'foldr_default',
-- yet 'foldl_v027' allocates ~56% more: see if swapping the order
-- of arguments in the recursion changes that.
-- For small tries:
-- * Marginally slower than 'foldl_v027' (<1%).
-- * Allocates ~8% less than 'foldl_v027'; ~19% less than 'foldl_default';
-- * albeit still ~42% more than 'foldl_default_Coerce'.
-- For larger tries: far and away the best of the lot.
foldl_v027_flop f z0 = \t -> go t z0 -- eta for better inlining
    where
    go Empty              z = z
    go (Arc _ Nothing  t) z = go t z
    go (Arc _ (Just v) t) z = go t (f z v)
    go (Branch _ _ l r)   z = go r (go l z)

-- We could try CPSing to restore the tail-recursion; but since
-- that's worse everywhere else, there seems little point.

----------------------------------------------------------------
foldl'_defaultCompose, foldl'_defaultEta, foldl'_v027, foldl'_v027_flop, foldl'_v027_flop_bang
    :: (b -> a -> b) -> b -> Trie a -> b

-- | The default  definition as of base-4.16.0.0 (using 'foldr_compose').
foldl'_defaultCompose f z0 xs =
    foldr_compose f' id xs z0
    where f' x k z = k $! f z x

-- | The default  definition as of base-4.16.0.0 (using 'foldr_eta').
-- Much worse than 'foldl'_defaultCompose'.
foldl'_defaultEta f z0 xs =
    foldr_eta f' id xs z0
    where f' x k z = k $! f z x

-- | bytestring-trie-0.2.7 definition, also used for defaults.
-- Clear winner, for small tries. For large tries it's about halfway
-- between the two defaults above, and the two flops below.
foldl'_v027 f z0 = go z0 -- eta for better inlining
    where
    go !z Empty              = z
    go  z (Arc _ Nothing  t) = go z t
    go  z (Arc _ (Just v) t) = go (f z v) t
    go  z (Branch _ _ l r)   = go (go z l) r

-- | For small tries: worse than 'foldl'_v027': ~10% slower, ~119%
-- more allocation.  But for large tries it's much faster.
foldl'_v027_flop f z0 = \t -> go t z0 -- eta for better inlining
    where
    go Empty              !z = z
    go (Arc _ Nothing  t)  z = go t z
    go (Arc _ (Just v) t)  z = go t (f z v)
    go (Branch _ _ l r)    z = go r (go l z)

-- FIXME: why on earth does this perform better?! Why aren't bang
-- patterns giving me the same thing?!
--
-- | For small tries: ~4.4% faster than 'foldl'_v027'; but allocates
-- ~32% more still.  For large tries it's only marginally faster
-- than 'foldl'_v027_flop' (which is to say massively faster than
-- 'foldl'_v027')
foldl'_v027_flop_bang f z0 = \t -> go t z0 -- eta for better inlining
    where
    go Empty              !z = z
    go (Arc _ Nothing  t)  z = go t z
    go (Arc _ (Just v) t)  z = go t $! f z v
    go (Branch _ _ l r)    z = go r $! go l z

-- TODO: CPS to restore the purely tail-call for @Branch@?

----------------------------------------------------------------
{-
-- TODO: the base-4.16.0.0 defaults are shown
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
-- Definitely not the most efficient thing...
-- TODO: define an anamorphism function in "TI" to use here...
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

----------------------------------------------------------------
intToSum :: (Trie (Sum Int) -> a) -> [Trie Int] -> [a]
intToSum f = fmap f . coerce

-- BUG: since we started generating a collection of tries, we've
-- started to see different results: each test progressively slower
-- than the last.  Seems bogus, but not sure what's up since criterion
-- ought to be forcing all of @ts@ early and doing GC between each
-- benchmark...
main :: IO ()
main = C.defaultMain
  [ C.env (QC.generate $ QC.vectorOf 10 $ arbitraryTrie 30 10) $ \ ts ->
    bgroup_Foldable ts
  ]

bgroup_Foldable :: [Trie Int] -> C.Benchmark
bgroup_Foldable ts =
  C.bgroup "Foldable"
  [ C.bgroup "fold"
    [ C.bench "default (foldMap)"       $ C.nf (intToSum fold_foldMap     ) ts
    , C.bench "default (foldr_compose)" $ C.nf (intToSum fold_foldrCompose) ts
    , C.bench "default (foldr_eta)"     $ C.nf (intToSum fold_foldrEta    ) ts
    , C.bench "v0.2.7"                  $ C.nf (intToSum fold_v027        ) ts
    ]
  , C.bgroup "foldMap"
    [ C.bench "default (foldr_compose)" $ C.nf (intToSum (foldMap_foldrCompose id)) ts
    , C.bench "default (foldr_eta)"     $ C.nf (intToSum (foldMap_foldrEta     id)) ts
    , C.bench "v0.2.7"                  $ C.nf (intToSum (foldMap_v027         id)) ts
    ]
  , C.bgroup "foldMap'"
    [ C.bench "default (foldl')"  $ C.nf (intToSum (foldMap'_default   id)) ts
    , C.bench "v0.2.7"            $ C.nf (intToSum (foldMap'_v027      id)) ts
    , C.bench "v0.2.7 +cps"       $ C.nf (intToSum (foldMap'_v027_cps  id)) ts
    , C.bench "v0.2.7 +flopped"   $ C.nf (intToSum (foldMap'_v027_flop id)) ts
    , C.bench "v0.2.7 +($!)"      $ C.nf (intToSum (foldMap'_v027_bang id)) ts
    , C.bench "v0.2.7 +flopped +($!)" $ C.nf (intToSum (foldMap'_v027_flop_bang id)) ts
    ]
  , C.bgroup "foldr"
    [ C.bench "default (foldMap)" $ C.nf (foldr_default     (+) 0 <$>) ts
    , C.bench "compose"           $ C.nf (foldr_compose     (+) 0 <$>) ts
    , C.bench "eta"               $ C.nf (foldr_eta         (+) 0 <$>) ts
    , C.bench "cps_eta"           $ C.nf (foldr_cps_eta     (+) 0 <$>) ts
    , C.bench "cps"               $ C.nf (foldr_cps         (+) 0 <$>) ts
    , C.bench "noClosure"         $ C.nf (foldr_noClosure   (+) 0 <$>) ts
    ]
  , C.bgroup "foldr'"
    [ C.bench "default"           $ C.nf (foldr'_default     (+) 0 <$>) ts
    , C.bench "v0.2.7"            $ C.nf (foldr'_v027        (+) 0 <$>) ts
    , C.bench "v0.2.7 +cps"       $ C.nf (foldr'_v027_cps    (+) 0 <$>) ts
    , C.bench "cps, no eta"       $ C.nf (foldr'_cps         (+) 0 <$>) ts
    , C.bench "v0.2.7 +flopped"   $ C.nf (foldr'_v027_flop   (+) 0 <$>) ts
    , C.bench "v0.2.7 +($!)"      $ C.nf (foldr'_v027_bang   (+) 0 <$>) ts
    , C.bench "v0.2.7 +flopped +($!)" $ C.nf (foldr'_v027_flop_bang (+) 0 <$>) ts
    ]
  , C.bgroup "foldl"
    [ C.bench "default (foldMap)" $ C.nf (foldl_default        (+) 0 <$>) ts
    , C.bench "default +Coerce"   $ C.nf (foldl_default_Coerce (+) 0 <$>) ts
    , C.bench "v0.2.7"            $ C.nf (foldl_v027           (+) 0 <$>) ts
    , C.bench "v0.2.7 +flopped"   $ C.nf (foldl_v027_flop      (+) 0 <$>) ts
    ]
  , C.bgroup "foldl'"
    [ C.bench "default (foldr_compose)" $ C.nf (foldl'_defaultCompose (+) 0 <$>) ts
    , C.bench "default (foldr_eta)"     $ C.nf (foldl'_defaultEta     (+) 0 <$>) ts
    , C.bench "v0.2.7"                  $ C.nf (foldl'_v027           (+) 0 <$>) ts
    , C.bench "v0.2.7 +flopped"         $ C.nf (foldl'_v027_flop      (+) 0 <$>) ts
    , C.bench "v0.2.7 +flopped +($!)"   $ C.nf (foldl'_v027_flop_bang (+) 0 <$>) ts
    ]
  ]

----------------------------------------------------------------
----------------------------------------------------------- fin.
