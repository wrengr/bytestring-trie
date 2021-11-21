-- To make GHC stop warning about the Prelude
{-# OPTIONS_GHC -Wall -fwarn-tabs -fno-warn-unused-imports #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- For list fusion on toListBy, and guarding `base` versions.
{-# LANGUAGE CPP #-}

------------------------------------------------------------
--                                              ~ 2021.11.20
-- |
-- Module      :  Data.Trie.Internal
-- Copyright   :  Copyright (c) 2008--2021 wren gayle romano
-- License     :  BSD3
-- Maintainer  :  wren@cpan.org
-- Stability   :  provisional
-- Portability :  portable (with CPP)
--
-- Internal definition of the 'Trie' data type and generic functions
-- for manipulating them. Almost everything here is re-exported
-- from "Data.Trie", which is the preferred API for users. This
-- module is for developers who need deeper (and potentially fragile)
-- access to the abstract type.
------------------------------------------------------------

module Data.Trie.Internal
    (
    -- * Data types
      Trie(), showTrie

    -- * Functions for 'ByteString's
    , breakMaximalPrefix

    -- * Basic functions
    , empty, null, singleton, size

    -- * Conversion and folding functions
    , toListBy, foldrWithKey, cata_, cata

    -- * Query functions
    , lookupBy_, submap
    , match_, matches_

    -- * Simple modification
    , alterBy, alterBy_, adjust

    -- * Combining tries
    , mergeBy, intersectBy

    -- * Mapping functions
    , mapBy
    , filterMap
    , contextualMap
    , contextualMap'
    , contextualFilterMap
    , contextualMapBy

    -- * Priority-queue functions
    , minAssoc, maxAssoc
    , updateMinViewBy, updateMaxViewBy
    ) where

import Prelude hiding    (null, lookup)
import qualified Prelude (null, lookup)

import qualified Data.ByteString as S
import Data.Trie.ByteStringInternal
import Data.Trie.BitTwiddle

import Data.Binary         (Binary(..), Get, Word8)
#if MIN_VERSION_base(4,9,0)
import Data.Semigroup      (Semigroup(..))
#elif MIN_VERSION_base(4,5,0)
-- So we can abbreviate 'S.append'
import Data.Monoid         ((<>))
#endif
import Data.Monoid         (Monoid(..))
import Control.Monad       (liftM, liftM3, liftM4)
import Control.Applicative (Applicative(..), (<$>))
import Data.Foldable       (Foldable(foldMap))
import Data.Traversable    (Traversable(traverse))

#ifdef __GLASGOW_HASKELL__
import GHC.Exts (build)
#endif
------------------------------------------------------------
------------------------------------------------------------

#if (!(MIN_VERSION_base(4,5,0)))
infixr 6 <>
-- Only ever used to abbreviate 'S.append'
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
{-# INLINE (<>) #-}
#endif

{-----------------------------------------------------------
-- ByteString Big-endian Patricia Trie datatype
-----------------------------------------------------------}
{-
In our idealized representation, we use a (directed) discrete graph
to represent our finite state machine. To organize the set of
outgoing arcs from a given Node we have ArcSet be a big-endian
patricia tree like Data.IntMap. In order to simplify things we then
go through a series of derivations.

    data Node a   = Accept a (ArcSet a)
                  | Reject   (Branch a)
    data Arc a    = Arc    ByteString (Node a)
    data ArcSet a = None
                  | One    KeyElem (Arc a)
                  | Many           (Branch a)
    data Branch a = Branch {Prefix} {Mask} (ArcSet a) (ArcSet a)
    data Trie a   = Empty
                  | Start  (Arc a)  -- [1]

[1]: N.B., we must allow constructing @Start(Arc pre (Reject b))@
for non-null @pre@, so that we can have a shared prefix even though
that prefix itself doesn't have an associated value.

** Squash Arc into One and Start:
For One, this allows combining the initial KeyElem with the rest
of the ByteString, which is purely beneficial.  However, it does
introduce some invariants since now we must distinguish NonEmptyBS
vs NullableBS.

    newtype NonEmptyBS = NonEmptyBS ByteString  -- Invariant: never empty
    newtype NullableBS = NullableBS Bytestring  -- May be empty.

    data Node a   = Accept a (ArcSet a)
                  | Reject   (Branch a)
    data ArcSet a = None
                  | Arc    NonEmptyBS (Node a)
                  | Many              (Branch a)
    data Branch a = Branch {Prefix} {Mask} (ArcSet a) (ArcSet a)
    data Trie a   = Empty
                  | Start  NullableBS (Node a)

** Squash Accept and Reject together:
Most likely beneficial, though it complicates stating the invariants
about Node's recursion.

    data Node a   = Node (Maybe a) (ArcSet a)
                    -- Invariant: if Nothing then must be Branch
    data ArcSet a = None
                  | Arc    NonEmptyBS (Node a)
                  | Many              (Branch a)
    data Branch a = Branch {Prefix} {Mask} (ArcSet a) (ArcSet a)
    data Trie a   = Empty
                  | Start  NullableBS (Node a)

** Squash Branch into Many:
Purely beneficial, since there's no point in keeping them distinct anymore.

    data Node a   = Node (Maybe a) (ArcSet a)
                    -- Invariant: if Nothing then must be Branch
    data ArcSet a = None
                  | Arc    NonEmptyBS (Node a)
                  | Branch {Prefix} {Mask} (ArcSet a) (ArcSet a)
    data Trie a   = Empty
                  | Start  NullableBS (Node a)

** Squash Empty/None and Arc/Start together:
Alas, this complicates the invariants about non-empty strings.

    data Node a = Node (Maybe a) (ArcSet a)
                    -- Invariant: if Nothing then must be Branch
    data Trie a = Empty
                | Arc    ByteString (Node a)
                    -- Invariant: empty string only allowed if both
                    -- (a) the Arc is at the root, and
                    -- (b) the Node has a value.
                | Branch {Prefix} {Mask} (Trie a) (Trie a)

** Squash Node into Arc:
By this point, purely beneficial.  However, the two unseen invariants remain.

[2] Maybe we shouldn't unpack the ByteString. We could specialize
or inline the breakMaximalPrefix function to prevent constructing
a new ByteString from the parts...
-}
-- | A map from 'ByteString's to @a@. For all the generic functions,
-- note that tries are strict in the @Maybe@ but not in @a@.
--
-- The 'Monad' instance is strange. If a key @k1@ is a prefix of
-- other keys, then results from binding the value at @k1@ will
-- override values from longer keys when they collide. If this is
-- useful for anything, or if there's a more sensible instance, I'd
-- be curious to know.

data Trie a = Empty
            | Arc    {-# UNPACK #-} !ByteString
                                    !(Maybe a)
                                    !(Trie a)
            | Branch {-# UNPACK #-} !Prefix
                     {-# UNPACK #-} !Mask
                                    !(Trie a)
                                    !(Trie a)
    deriving Eq
    -- Prefix/Mask should be deterministic regardless of insertion order
    -- TODO: prove this is so.


-- TODO? add Ord instance like Data.Map?

{-----------------------------------------------------------
-- Trie instances: serialization et cetera
-----------------------------------------------------------}

-- This instance does not unveil the innards of our abstract type.
-- It doesn't emit truly proper Haskell code though, since ByteStrings
-- are printed as (ASCII) Strings, but that's not our fault. (Also
-- 'fromList' is in "Data.Trie" instead of here.)
instance (Show a) => Show (Trie a) where
    showsPrec p t = showParen (p > 10)
                  $ ("Data.Trie.fromList "++) . shows (toListBy (,) t)


-- | Visualization fuction for debugging.
showTrie :: (Show a) => Trie a -> String
showTrie t = shows' id t ""
    where
    spaces f = map (const ' ') (f "")

    shows' _  Empty            = (".\n"++)
    shows' ss (Branch p m l r) =
        let s'  = ("--"++) . shows p . (","++) . shows m . ("-+"++)
            ss' = ss . (tail (spaces s') ++)
        in s'              . shows' (ss' . ("|"++)) l
           . ss' . ("|\n"++)
           . ss' . ("`"++) . shows' (ss' . (" "++)) r
    shows' ss (Arc k mv t') =
        let s' = ("--"++) . shows k
                 . maybe id (\v -> ("-("++) . shows v . (")"++)) mv
                 . ("--"++)
        in  s' . shows' (ss . (spaces s' ++)) t'


-- TODO?? a Read instance? hrm... should I?

-- TODO: consider an instance more like the new one for Data.Map. Better?
instance (Binary a) => Binary (Trie a) where
    put Empty            = do put (0 :: Word8)
    put (Arc k m t)      = do put (1 :: Word8); put k; put m; put t
    put (Branch p m l r) = do put (2 :: Word8); put p; put m; put l; put r

    -- BUG(github#21): need to verify the invariants!
    get = do tag <- get :: Get Word8
             case tag of
                 0 -> return Empty
                 1 -> liftM3 Arc get get get
                 _ -> liftM4 Branch get get get get


{-----------------------------------------------------------
-- Trie instances: Abstract Nonsense
-----------------------------------------------------------}

instance Functor Trie where
    fmap f = go
        where
        go Empty              = Empty
        go (Arc k Nothing  t) = Arc k Nothing      (go t)
        go (Arc k (Just v) t) = Arc k (Just (f v)) (go t)
        go (Branch p m l r)   = Branch p m (go l) (go r)


instance Foldable Trie where
    -- If our definition of foldr is so much faster than the Endo
    -- default, then maybe we should remove this and use the default
    -- foldMap based on foldr
    foldMap f = go
        where
        go Empty              = mempty
        go (Arc _ Nothing  t) = go t
        go (Arc _ (Just v) t) = f v `mappend` go t
        go (Branch _ _ l r)   = go l `mappend` go r

    {- This definition is much faster, but it's also wrong
    -- (or at least different than foldrWithKey)
    foldr f = \z t -> go t id z
        where
        go Empty              k x = k x
        go (Branch _ _ l r)   k x = go r (go l k) x
        go (Arc _ Nothing t)  k x = go t k x
        go (Arc _ (Just v) t) k x = go t k (f v x)

    foldl f = \z t -> go t id z
        where
        go Empty              k x = k x
        go (Branch _ _ l r)   k x = go l (go r k) x
        go (Arc _ Nothing t)  k x = go t k x
        go (Arc _ (Just v) t) k x = go t k (f x v)
    -}

-- TODO: newtype Keys = K Trie  ; instance Foldable Keys
-- TODO: newtype Assoc = A Trie ; instance Foldable Assoc

instance Traversable Trie where
    traverse f = go
        where
        go Empty              = pure Empty
        go (Arc k Nothing  t) = Arc k Nothing        <$> go t
        go (Arc k (Just v) t) = Arc k . Just <$> f v <*> go t
        go (Branch p m l r)   = Branch p m <$> go l <*> go r

instance Applicative Trie where
    pure    = singleton S.empty
    m <*> n = m >>= (<$> n)

-- Does this even make sense? It's not nondeterminism like lists
-- and sets. If no keys were prefixes of other keys it'd make sense
-- as a decision-tree; but since keys /can/ prefix, tries formed
-- from shorter keys can shadow the results from longer keys due
-- to the 'unionL'. It does seem to follow the laws though... What
-- computation could this possibly represent?
--
--  1. return x >>= f  == f x
--  2. m >>= return    == m
--  3. (m >>= f) >>= g == m >>= (\x -> f x >>= g)
instance Monad Trie where
-- Since base-4.8 (ghc-7.10.1) we have the default @return = pure@.
-- Since ghc-9.2.1 we get a warning about providing any other
-- definition, and should instead define both 'pure' and @(*>)@
-- directly, leaving 'return' and @(>>)@ as their defaults so they
-- can eventually be removed from the class.
-- <https://gitlab.haskell.org/ghc/ghc/-/wikis/proposal/monad-of-no-return>
#if (!(MIN_VERSION_base(4,8,0)))
    return = pure
#endif

    (>>=) Empty              _ = empty
    (>>=) (Branch p m l r)   f = branch p m (l >>= f) (r >>= f)
    (>>=) (Arc k Nothing  t) f = arc_ k (t >>= f)
    (>>=) (Arc k (Just v) t) f = arc_ k (f v `unionL` (t >>= f))
                               where
                               unionL = mergeBy (\x _ -> Just x)


#if MIN_VERSION_base(4,9,0)
-- The "Data.Semigroup" module is in base since 4.9.0.0; but having
-- the 'Semigroup' superclass for the 'Monoid' instance only comes
-- into force in base 4.11.0.0.
instance (Semigroup a) => Semigroup (Trie a) where
    (<>) = mergeBy $ \x y -> Just (x <> y)
    -- TODO: optimized implementations of:
    -- sconcat :: NonEmpty a -> a
    -- stimes :: Integral b => b -> a -> a
#endif

-- This instance is more sensible than Data.IntMap and Data.Map's
instance (Monoid a) => Monoid (Trie a) where
    mempty  = empty
#if (!(MIN_VERSION_base(4,11,0)))
    mappend = mergeBy $ \x y -> Just (x `mappend` y)
#endif


-- Since the Monoid instance isn't natural in @a@, I can't think
-- of any other sensible instance for MonadPlus. It's as specious
-- as Maybe, IO, and STM's instances though.
--
-- MonadPlus laws: <http://www.haskell.org/haskellwiki/MonadPlus>
--  1. <Trie a, mzero, mplus> forms a monoid
--  2. mzero >>= f        === mzero
--  3. m >> mzero         === mzero
--  4. mplus m n >>= k    === mplus (m >>= k) (n >>= k)
--  4' mplus (return a) n === return a
{-
-- Follows #1, #1, and #3. But it does something like 4' instead
-- of actually doing #4 (since we'd merge the trees generated by
-- @k@ for conflicting values)
--
-- TODO: cf Control.Applicative.Alternative (base-4, but not Hugs).
-- But (<*>) gets odd when the function is not 'pure'... maybe
-- helpful though.
instance MonadPlus Trie where
    mzero = empty
    mplus = unionL where unionL = mergeBy (\x _ -> Just x)
-}


{-----------------------------------------------------------
-- Extra mapping functions
-----------------------------------------------------------}

-- | Apply a function to all values, potentially removing them.
filterMap :: (a -> Maybe b) -> Trie a -> Trie b
filterMap f = go
    where
    go Empty              = empty
    go (Arc k Nothing  t) = arc_ k       (go t)
    go (Arc k (Just v) t) = arc  k (f v) (go t)
    go (Branch p m l r)   = branch p m (go l) (go r)


-- | Generic version of 'fmap'. This function is notably more
-- expensive than 'fmap' or 'filterMap' because we have to reconstruct
-- the keys.
mapBy :: (ByteString -> a -> Maybe b) -> Trie a -> Trie b
mapBy f = go S.empty
    where
    go _ Empty              = empty
    go q (Arc k Nothing  t) = arc_ k          (go q' t) where q' = q <> k
    go q (Arc k (Just v) t) = arc  k (f q' v) (go q' t) where q' = q <> k
    go q (Branch p m l r)   = branch p m (go q l) (go q r)


-- | A variant of 'fmap' which provides access to the subtrie rooted
-- at each value.
contextualMap :: (a -> Trie a -> b) -> Trie a -> Trie b
contextualMap f = go
    where
    go Empty              = Empty
    go (Arc k Nothing  t) = Arc k Nothing        (go t)
    go (Arc k (Just v) t) = Arc k (Just (f v t)) (go t)
    go (Branch p m l r)   = Branch p m (go l) (go r)


-- | A variant of 'contextualMap' which applies the function strictly.
contextualMap' :: (a -> Trie a -> b) -> Trie a -> Trie b
contextualMap' f = go
    where
    go Empty              = Empty
    go (Arc k Nothing  t) = Arc k Nothing         (go t)
    go (Arc k (Just v) t) = Arc k (Just $! f v t) (go t)
    go (Branch p m l r)   = Branch p m (go l) (go r)


-- | A contextual variant of 'filterMap'.
contextualFilterMap :: (a -> Trie a -> Maybe b) -> Trie a -> Trie b
contextualFilterMap f = go
    where
    go Empty              = empty
    go (Arc k Nothing  t) = arc_ k         (go t)
    go (Arc k (Just v) t) = arc  k (f v t) (go t)
    go (Branch p m l r)   = branch p m (go l) (go r)


-- | A contextual variant of 'mapBy'. Again note that this is
-- expensive since we must reconstruct the keys.
contextualMapBy :: (ByteString -> a -> Trie a -> Maybe b) -> Trie a -> Trie b
contextualMapBy f = go S.empty
    where
    go _ Empty              = empty
    go q (Arc k Nothing  t) = arc_ k            (go q' t) where q' = q <> k
    go q (Arc k (Just v) t) = arc  k (f q' v t) (go q' t) where q' = q <> k
    go q (Branch p m l r)   = branch p m (go q l) (go q r)


{-----------------------------------------------------------
-- Smart constructors and helper functions for building tries
-----------------------------------------------------------}

-- | Smart constructor to prune @Empty@ from @Branch@es.
branch :: Prefix -> Mask -> Trie a -> Trie a -> Trie a
{-# INLINE branch #-}
branch _ _ Empty r     = r
branch _ _ l     Empty = l
branch p m l     r     = Branch p m l r


-- | Smart constructor to prune @Arc@s that lead nowhere.
-- N.B if mv=Just then doesn't check whether t=epsilon. It's up to
-- callers to ensure that invariant isn't broken.
arc :: ByteString -> Maybe a -> Trie a -> Trie a
{-# INLINE arc #-}
arc k mv@(Just _) t = Arc k mv t
arc k    Nothing  t = arc_ k t

-- | Variant of `arc` smart constructor, for when we know there's no value.
arc_ :: ByteString -> Trie a -> Trie a
{-# INLINE arc_ #-}
arc_ _   Empty            = Empty
arc_ k t@(Branch{})       = if S.null k then t else Arc k Nothing t
arc_ k   (Arc k' mv' t')  = Arc (k <> k') mv' t'
{-
-- TODO: benchmark using this version instead.
-- Changes: Resolves the @S.null k@ check before case analysis on
-- @t@, and shares the Arc when @k@ is null.
arc_ k t | S.null k    = t
arc_ _ t@Empty         = t
arc_ k t@(Branch{})    = Arc k Nothing t
arc_ k (Arc k' mv' t') = Arc (k <> k') mv' t'
-}
-- TODO: that variant also raises the suggestion of defining a
-- further specialized variant for when we statically know the sring
-- is non-null (e.g. all the @(Arc _ Nothing_)@ cases in the
-- folds\/maps above; since we know Arc only allows null when there's
-- a value.

-- | Smart constructor to join two tries into a @Branch@ with maximal
-- prefix sharing. Requires knowing the prefixes, but can combine
-- either @Branch@es or @Arc@s.
--
-- N.B. /do not/ use if prefixes could match entirely!
branchMerge :: Prefix -> Trie a -> Prefix -> Trie a -> Trie a
{-# INLINE branchMerge #-}
branchMerge _ Empty _ t2    = t2
branchMerge _  t1   _ Empty = t1
branchMerge p1 t1  p2 t2
    | zero p1 m             = Branch p m t1 t2
    | otherwise             = Branch p m t2 t1
    where
    m = branchMask p1 p2
    p = mask p1 m


-- It would be better if Arc used
-- Data.ByteString.TrieInternal.wordHead somehow, that way
-- we can see 4/8/?*Word8 at a time instead of just one.
-- But that makes maintaining invariants ...difficult :(
getPrefix :: Trie a -> Prefix
{-# INLINE getPrefix #-}
getPrefix (Branch p _ _ _)        = p
getPrefix (Arc k _ _) | S.null k  = 0 -- for lack of a better value
                      | otherwise = S.head k
getPrefix Empty                   = error "getPrefix: no Prefix of Empty"


{-----------------------------------------------------------
-- Error messages
-----------------------------------------------------------}

-- TODO: move off to "Data.Trie.Errors"?
-- TODO: shouldn't we inline the logic and just NOINLINE the string
-- constant? There are only three use sites, which themselves aren't
-- inlined...
errorLogHead :: String -> ByteString -> ByteStringElem
{-# NOINLINE errorLogHead #-}
errorLogHead fn q
    | S.null q  = error $ "Data.Trie.Internal." ++ fn ++": found null subquery"
    | otherwise = S.head q


------------------------------------------------------------
------------------------------------------------------------

{-----------------------------------------------------------
-- Basic functions
-----------------------------------------------------------}

-- | /O(1)/, Construct the empty trie.
empty :: Trie a
{-# INLINE empty #-}
empty = Empty


-- | /O(1)/, Is the trie empty?
null :: Trie a -> Bool
{-# INLINE null #-}
null Empty = True
null _     = False


-- | /O(1)/, Construct a singleton trie.
singleton :: ByteString -> a -> Trie a
{-# INLINE singleton #-}
singleton k v = Arc k (Just v) Empty
-- For singletons, don't need to verify invariant on arc length >0


-- | /O(n)/, Get count of elements in trie.
size  :: Trie a -> Int
{-# INLINE size #-}
size t = size' t id 0

-- | /O(n)/, CPS accumulator helper for calculating 'size'.
size' :: Trie a -> (Int -> Int) -> Int -> Int
size' Empty              f n = f n
size' (Branch _ _ l r)   f n = size' l (size' r f) n
size' (Arc _ Nothing t)  f n = size' t f n
size' (Arc _ (Just _) t) f n = size' t f $! n + 1


{-----------------------------------------------------------
-- Conversion functions
-----------------------------------------------------------}

-- Still rather inefficient
--
-- TODO: rewrite list-catenation to be lazier (real CPS instead of
-- function building? is the function building really better than
-- (++) anyways?)
-- N.B. If our manual definition of foldr/foldl (using function
-- application) is so much faster than the default Endo definition
-- (using function composition), then we should make this use
-- application instead too.
--
-- TODO: the @q@ accumulator should be lazy ByteString and only
-- forced by @fcons@. It's already non-strict, but we should ensure
-- O(n) not O(n^2) when it's forced.
--
-- BUG: not safe for deep strict @fcons@, only for WHNF-strict like (:)
-- Where to put the strictness to amortize it?
--
-- | Convert a trie into a list (in key-sorted order) using a
-- function, folding the list as we go.
foldrWithKey :: (ByteString -> a -> b -> b) -> b -> Trie a -> b
foldrWithKey fcons nil = \t -> go S.empty t nil
    where
    go _ Empty            = id
    go q (Branch _ _ l r) = go q l . go q r
    go q (Arc k mv t)     =
        case mv of
        Nothing -> rest
        Just v  -> fcons k' v . rest
        where
        rest = go k' t
        k'   = q <> k


-- | Catamorphism for tries.  Unlike most other functions (`mapBy`,
-- `contextualMapBy`, `foldrWithKey`, etc), this function does *not*
-- reconstruct the full `ByteString` for each value; instead it
-- only returns the suffix since the previous value or branch point.
--
-- This function is a direct\/literal catamorphism of the implementation
-- datatype, erasing only some bitmasking metadata for the branches.
-- For a more semantic catamorphism, see `cata`.
--
-- /Since: 0.2.6/
cata_
    :: (ByteString -> Maybe a -> b -> b)
    -> (b -> b -> b)
    -> b
    -> Trie a -> b
cata_ a b e = go
    where
    go Empty            = e
    go (Arc k mv t)     = a k mv (go t)
    go (Branch _ _ l r) = b (go l) (go r)


-- | Catamorphism for tries.  Unlike most other functions (`mapBy`,
-- `contextualMapBy`, `foldrWithKey`, etc), this function does *not*
-- reconstruct the full `ByteString` for each value; instead it
-- only returns the suffix since the previous value or branch point.
--
-- This function is a semantic catamorphism; that is, it tries to
-- express the invariants of the implementation, rather than exposing
-- the literal structure of the implementation.  For a more literal
-- catamorphism, see `cata_`.
--
-- /Since: 0.2.6/
cata
    :: (ByteString -> a -> b -> b)
    -> (ByteString -> [b] -> b)
    -> b
    -> Trie a -> b
cata a b e = start
    where
    start Empty                 = e
    start (Arc k mv t)          = step k mv t
    start (Branch _ _ l r)      = b S.empty (collect l (collect r []))

    step k (Just v) t           = a k v (start t)
    step k Nothing  t           = b k (collect t [])

    collect Empty            bs = bs
    collect (Arc k mv t)     bs = step k mv t : bs
    collect (Branch _ _ l r) bs = collect l (collect r bs)


-- cf Data.ByteString.unpack
-- <http://hackage.haskell.org/packages/archive/bytestring/0.9.1.4/doc/html/src/Data-ByteString.html>
--
-- | Convert a trie into a list using a function. Resulting values
-- are in key-sorted order.
toListBy :: (ByteString -> a -> b) -> Trie a -> [b]
{-# INLINE toListBy #-}
#if !defined(__GLASGOW_HASKELL__)
-- TODO: should probably inline foldrWithKey
-- TODO: compare performance of that vs both this and the GHC version
toListBy f t = foldrWithKey (((:) .) . f) [] t
#else
-- Written with 'build' to enable the build\/foldr fusion rules.
toListBy f t = build (toListByFB f t)

-- TODO: should probably have a specialized version for strictness,
-- and a rule to rewrite generic lazy version into it. As per
-- Data.ByteString.unpack and the comments there about strictness
-- and fusion.
toListByFB :: (ByteString -> a -> b) -> Trie a -> (b -> c -> c) -> c -> c
{-# INLINE [0] toListByFB #-}
toListByFB f t cons nil = foldrWithKey ((cons .) . f) nil t
#endif


{-----------------------------------------------------------
-- Query functions (just recurse)
-----------------------------------------------------------}

-- | Generic function to find a value (if it exists) and the subtrie
-- rooted at the prefix. The first function argument is called if and
-- only if a node is exactly reachable by the query; if no node is
-- exactly reachable the default value is used; if the middle of
-- an arc is reached, the second function argument is used.
--
-- This function is intended for internal use. For the public-facing
-- version, see 'Data.Trie.lookupBy'.
lookupBy_ :: (Maybe a -> Trie a -> b) -> b -> (Trie a -> b)
          -> ByteString -> Trie a -> b
lookupBy_ f z a = lookupBy_'
    where
    -- | Deal with epsilon query (when there is no epsilon value)
    lookupBy_' q t@(Branch{}) | S.null q = f Nothing t
    lookupBy_' q t                       = go q t

    -- | The main recursion
    go _    Empty       = z
    go q   (Arc k mv t) =
        let (_,k',q')   = breakMaximalPrefix k q
        in case (S.null k', S.null q') of
                (False, True)  -> a (Arc k' mv t)
                (False, False) -> z
                (True,  True)  -> f mv t
                (True,  False) -> go q' t
    go q t_@(Branch{}) = findArc t_
        where
        qh = errorLogHead "lookupBy_" q
        -- | /O(min(m,W))/, where /m/ is number of @Arc@s in this
        -- branching, and /W/ is the word size of the Prefix,Mask type.
        findArc (Branch p m l r)
            | nomatch qh p m  = z
            | zero qh m       = findArc l
            | otherwise       = findArc r
        findArc t@(Arc{})     = go q t
        findArc Empty         = z


-- This function needs to be here, not in "Data.Trie", because of
-- 'arc' which isn't exported. We could use the monad instance
-- instead, though it'd be far more circuitous.
--     arc k Nothing  t === singleton k () >> t
--     arc k (Just v) t === singleton k v  >>= unionR t . singleton S.empty
--         (...except 'arc' doesn't do the invariant correction
--           of (>>=) for epsilon`elem`t)
--
-- | Return the subtrie containing all keys beginning with a prefix.
submap :: ByteString -> Trie a -> Trie a
{-# INLINE submap #-}
submap q = lookupBy_ (arc q) empty (arc_ q) q
{-  -- Disable superfluous error checking.
    -- @submap'@ would replace the first argument to @lookupBy_@
    where
    submap' Nothing Empty   = errorEmptyAfterNothing "submap"
    submap' Nothing (Arc{}) = errorArcAfterNothing   "submap"
    submap' mx      t       = Arc q mx t

errorInvariantBroken :: String -> String -> a
{-# NOINLINE errorInvariantBroken #-}
errorInvariantBroken s e =  error (s ++ ": Invariant was broken" ++ e')
    where
    e' = if Prelude.null e then e else ", found: " ++ e

errorArcAfterNothing    :: String -> a
{-# NOINLINE errorArcAfterNothing #-}
errorArcAfterNothing   s = errorInvariantBroken s "Arc after Nothing"

errorEmptyAfterNothing  :: String -> a
{-# NOINLINE errorEmptyAfterNothing #-}
errorEmptyAfterNothing s = errorInvariantBroken s "Empty after Nothing"
-- -}



-- TODO: would it be worth it to have a variant like 'lookupBy_' which takes the three continuations?

-- | Given a query, find the longest prefix with an associated value
-- in the trie, returning the length of that prefix and the associated
-- value.
--
-- This function may not have the most useful return type. For a
-- version that returns the prefix itself as well as the remaining
-- string, see 'Data.Trie.match'.
match_ :: Trie a -> ByteString -> Maybe (Int, a)
match_ = flip start
    where
    -- | Deal with epsilon query (when there is no epsilon value)
    start q (Branch{}) | S.null q = Nothing
    start q t                     = goNothing 0 q t
        -- TODO: for the non-null Branch case, maybe we should jump directly to 'findArc' (i.e., inline that case of 'goNothing')

    -- | The initial recursion
    goNothing _ _    Empty       = Nothing
    goNothing n q   (Arc k mv t) =
        let (p,k',q') = breakMaximalPrefix k q
            n'        = n + S.length p
        in n' `seq`
            case (S.null k', S.null q') of
            (False, _)    -> Nothing
            (True, True)  -> (,) n' <$> mv
            (True, False) ->
                case mv of
                Nothing -> goNothing   n' q' t
                Just v  -> goJust n' v n' q' t
    goNothing n q t_@(Branch{}) = findArc t_
        where
        qh = errorLogHead "match_" q
        -- | /O(min(m,W))/, where /m/ is number of @Arc@s in this
        -- branching, and /W/ is the word size of the Prefix,Mask type.
        findArc (Branch p m l r)
            | nomatch qh p m  = Nothing
            | zero qh m       = findArc l
            | otherwise       = findArc r
        findArc t@(Arc{})     = goNothing n q t
        findArc Empty         = Nothing

    -- | The main recursion
    goJust n0 v0 _ _    Empty       = Just (n0,v0)
    goJust n0 v0 n q   (Arc k mv t) =
        let (p,k',q') = breakMaximalPrefix k q
            n'        = n + S.length p
        in n' `seq`
            case (S.null k', S.null q') of
            (False, _)   -> Just (n0,v0)
            (True, True) ->
                case mv of
                Nothing -> Just (n0,v0)
                Just v  -> Just (n',v)
            (True, False) ->
                case mv of
                Nothing -> goJust n0 v0 n' q' t
                Just v  -> goJust n' v  n' q' t
    goJust n0 v0 n q t_@(Branch{}) = findArc t_
        where
        qh = errorLogHead "match_" q
        -- | /O(min(m,W))/, where /m/ is number of @Arc@s in this
        -- branching, and /W/ is the word size of the Prefix,Mask type.
        findArc (Branch p m l r)
            | nomatch qh p m  = Just (n0,v0)
            | zero qh m       = findArc l
            | otherwise       = findArc r
        findArc t@(Arc{})     = goJust n0 v0 n q t
        findArc Empty         = Just (n0,v0)


-- | Given a query, find all prefixes with associated values in the
-- trie, and return the length of each prefix with their value, in
-- order from shortest prefix to longest.  This function is a good
-- producer for list fusion.
--
-- This function may not have the most useful return type. For a
-- version that returns the prefix itself as well as the remaining
-- string, see 'Data.Trie.matches'.
matches_ :: Trie a -> ByteString -> [(Int,a)]
matches_ t q =
#if !defined(__GLASGOW_HASKELL__)
    matchFB_ t q (((:) .) . (,)) []
#else
    build (\cons nil -> matchFB_ t q ((cons .) . (,)) nil)
{-# INLINE matches_ #-}
#endif

matchFB_ :: Trie a -> ByteString -> (Int -> a -> r -> r) -> r -> r
matchFB_ = \t q cons nil -> matchFB_' cons q t nil
    where
    matchFB_' cons = start
        where
        -- | Deal with epsilon query (when there is no epsilon value)
        start q (Branch{}) | S.null q = id
        start q t                     = go 0 q t

        -- | The main recursion
        go _ _    Empty       = id
        go n q   (Arc k mv t) =
            let (p,k',q') = breakMaximalPrefix k q
                n'        = n + S.length p
            in n' `seq`
                if S.null k'
                then
                    case mv of { Nothing -> id; Just v  -> cons n' v}
                    .
                    if S.null q' then id else go n' q' t
                else id
        go n q t_@(Branch{}) = findArc t_
            where
            qh = errorLogHead "matches_" q
            -- | /O(min(m,W))/, where /m/ is number of @Arc@s in this
            -- branching, and /W/ is the word size of the Prefix,Mask type.
            findArc (Branch p m l r)
                | nomatch qh p m  = id
                | zero qh m       = findArc l
                | otherwise       = findArc r
            findArc t@(Arc{})     = go n q t
            findArc Empty         = id


{-----------------------------------------------------------
-- Simple modification functions (recurse and clone spine)
-----------------------------------------------------------}

-- TODO: We should CPS on Empty to avoid cloning spine if no change.
-- Difficulties arise with the calls to 'branch' and 'arc'. Will
-- have to create a continuation chain, so no savings on memory
-- allocation; but would have savings on held memory, if they're
-- still holding the old one...
--
-- | Generic function to alter a trie by one element with a function
-- to resolve conflicts (or non-conflicts).
alterBy :: (ByteString -> a -> Maybe a -> Maybe a)
         -> ByteString -> a -> Trie a -> Trie a
alterBy f q x = alterBy_ (\mv t -> (f q x mv, t)) q
-- TODO: use GHC's 'inline' function so that this gets specialized away.
-- TODO: benchmark to be sure that this doesn't introduce unforseen
--  performance costs because of the uncurrying etc.
-- TODO: move to "Data.Trie" itself instead of here, since it doesn't
--  depend on any internals (unless we actually do the CPS optimization).
-- TODO: would there be any benefit in basing this off a different
--  function that captures the invariant that the subtrie is left
--  alone?


-- | A variant of 'alterBy' which also allows modifying the sub-trie.
--
-- /Type changed in 0.2.6/
alterBy_
    :: (Maybe a -> Trie a -> (Maybe a, Trie a))
    -> ByteString -> Trie a -> Trie a
alterBy_ f q_
    | S.null q_ = alterEpsilon
    | otherwise = go q_
    where
    nothing q = uncurry (arc q) (f Nothing Empty)

    alterEpsilon t_@Empty                    = uncurry (arc q_) (f Nothing t_)
    alterEpsilon t_@(Branch{})               = uncurry (arc q_) (f Nothing t_)
    alterEpsilon t_@(Arc k mv t) | S.null k  = uncurry (arc q_) (f mv      t)
                                 | otherwise = uncurry (arc q_) (f Nothing t_)

    go q Empty            = nothing q
    go q t@(Branch p m l r)
        | nomatch qh p m  = branchMerge p t  qh (nothing q)
        | zero qh m       = branch p m (go q l) r
        | otherwise       = branch p m l (go q r)
        where
        qh = errorLogHead "alterBy_" q
    go q t_@(Arc k mv t) =
        let (p,k',q') = breakMaximalPrefix k q in
        case (S.null k', S.null q') of
        (False, True)  -> -- add node to middle of arc
                          uncurry (arc p) (f Nothing (Arc k' mv t))
        (False, False) ->
            case nothing q' of
            Empty -> t_ -- Nothing to add, reuse old arc
            l     -> arc' (branchMerge (getPrefix l) l (getPrefix r) r)
                    where
                    r = Arc k' mv t
                    -- inlined variant of @arc_ p@, which captures
                    -- the invariant that the result of 'branchMerge'
                    -- above must be a Branch (because neither @l@ nor
                    -- @r@ are Empty)
                    arc' | S.null p  = id
                         | otherwise = Arc p Nothing
        (True, True)  -> uncurry (arc k) (f mv t)
        (True, False) -> arc k mv (go q' t)


-- TODO: benchmark vs the definition with alterBy/liftM
-- TODO: add a variant that's strict in the function.
--
-- /Since: 0.2.6/ for being exported from "Data.Trie.Internal".
--
-- | Apply a function to the value at a key.  If the key is not
-- present, then the trie is returned unaltered.
adjust :: (a -> a) -> ByteString -> Trie a -> Trie a
adjust f = start
    where
    start q t                  | not (S.null q) = go q t
    start _ (Arc k (Just v) t) | S.null k       = Arc k (Just (f v)) t
    start _ t                                   = t

    go _ Empty            = Empty
    go q t@(Branch p m l r)
        | nomatch qh p m  = t
        | zero qh m       = Branch p m (go q l) r
        | otherwise       = Branch p m l (go q r)
        where
        qh = errorLogHead "adjust" q
    go q t_@(Arc k mv t) =
        let (_,k',q') = breakMaximalPrefix k q in
        case (S.null k', S.null q') of
        (False, True)  -> t_ -- don't break arc inline
        (False, False) -> t_ -- don't break arc branching
        (True,  True)  -> Arc k (liftM f mv) t
        (True,  False) -> Arc k mv (go q' t)


{-----------------------------------------------------------
-- Trie-combining functions
-----------------------------------------------------------}

-- TEST CASES: foldr (unionL . uncurry singleton) empty t
--             foldr (uncurry insert) empty t
--    where t = map (\s -> (pk s, 0))
--                  ["heat","hello","hoi","apple","appa","hell","appb","appc"]
--
-- | Take the union of two tries, using a function to resolve collisions.
-- This can only define the space of functions between union and
-- symmetric difference but, with those two, all set operations can
-- be defined (albeit inefficiently).
mergeBy :: (a -> a -> Maybe a) -> Trie a -> Trie a -> Trie a
mergeBy f = mergeBy'
    where
    -- | Deals with epsilon entries, before recursing into @go@
    mergeBy'
        t0_@(Arc k0 mv0 t0)
        t1_@(Arc k1 mv1 t1)
        | S.null k0 && S.null k1 = arc k0 (mergeMaybe f mv0 mv1) (go t0 t1)
        | S.null k0              = arc k0 mv0 (go t0 t1_)
        |              S.null k1 = arc k1 mv1 (go t1 t0_)
    mergeBy'
        (Arc k0 mv0@(Just _) t0)
        t1_@(Branch{})
        | S.null k0              = Arc k0 mv0 (go t0 t1_)
    mergeBy'
        t0_@(Branch{})
        (Arc k1 mv1@(Just _) t1)
        | S.null k1              = Arc k1 mv1 (go t1 t0_)
    mergeBy' t0_ t1_             = go t0_ t1_

    -- | The main recursion
    go Empty t1    = t1
    go t0    Empty = t0
    -- /O(n+m)/ for this part where /n/ and /m/ are sizes of the branchings
    go  t0@(Branch p0 m0 l0 r0)
        t1@(Branch p1 m1 l1 r1)
        | shorter m0 m1  = union0
        | shorter m1 m0  = union1
        | p0 == p1       = branch p0 m0 (go l0 l1) (go r0 r1)
        | otherwise      = branchMerge p0 t0 p1 t1
        where
        union0  | nomatch p1 p0 m0  = branchMerge p0 t0 p1 t1
                | zero p1 m0        = branch p0 m0 (go l0 t1) r0
                | otherwise         = branch p0 m0 l0 (go r0 t1)
        union1  | nomatch p0 p1 m1  = branchMerge p0 t0 p1 t1
                | zero p0 m1        = branch p1 m1 (go t0 l1) r1
                | otherwise         = branch p1 m1 l1 (go t0 r1)
    -- We combine these branches of @go@ in order to clarify where
    -- the definitions of @p0@, @p1@, @m'@, @p'@ are relevant.
    -- However, this may introduce inefficiency in the pattern
    -- matching automaton...
    -- TODO: check; and get rid of @go'@ if it does.
    go t0_ t1_ = go' t0_ t1_
        where
        p0 = getPrefix t0_
        p1 = getPrefix t1_
        m' = branchMask p0 p1
        p' = mask p0 m'

        go' (Arc k0 mv0 t0)
            (Arc k1 mv1 t1)
            | m' == 0 =
                let (pre,k0',k1') = breakMaximalPrefix k0 k1 in
                if S.null pre
                then error "mergeBy: no mask, but no prefix string"
                else
                    let {-# INLINE t0' #-}
                        t0' = Arc k0' mv0 t0
                        {-# INLINE t1' #-}
                        t1' = Arc k1' mv1 t1
                    in
                    case (S.null k0', S.null k1') of
                    (True, True)  -> arc  pre (mergeMaybe f mv0 mv1) (go t0 t1)
                    (True, False) -> arc  pre mv0 (go t0  t1')
                    (False,True)  -> arc  pre mv1 (go t0' t1)
                    (False,False) -> arc_ pre     (go t0' t1')
        go' (Arc{})
            (Branch _p1 m1 l r)
            | nomatch p0 p1 m1 = branchMerge p1 t1_  p0 t0_
            | zero p0 m1       = branch p1 m1 (go t0_ l) r
            | otherwise        = branch p1 m1 l (go t0_ r)
        go' (Branch _p0 m0 l r)
            (Arc{})
            | nomatch p1 p0 m0 = branchMerge p0 t0_  p1 t1_
            | zero p1 m0       = branch p0 m0 (go l t1_) r
            | otherwise        = branch p0 m0 l (go r t1_)
        -- Inlined branchMerge. Both tries are disjoint @Arc@s now.
        go' _ _ | zero p0 m'   = Branch p' m' t0_ t1_
        go' _ _                = Branch p' m' t1_ t0_


mergeMaybe :: (a -> a -> Maybe a) -> Maybe a -> Maybe a -> Maybe a
{-# INLINE mergeMaybe #-}
mergeMaybe _ Nothing      Nothing  = Nothing
mergeMaybe _ Nothing mv1@(Just _)  = mv1
mergeMaybe _ mv0@(Just _) Nothing  = mv0
mergeMaybe f (Just v0)   (Just v1) = f v0 v1


-- | Take the intersection of two tries, using a function to resolve
-- collisions.
--
-- /Since: 0.2.6/
intersectBy :: (a -> b -> Maybe c) -> Trie a -> Trie b -> Trie c
intersectBy f = intersectBy'
    where
    -- | Deals with epsilon entries, before recursing into @go@
    intersectBy'
        t0_@(Arc k0 mv0 t0)
        t1_@(Arc k1 mv1 t1)
        | S.null k0 && S.null k1 =  arc  k0 (intersectMaybe f mv0 mv1) (go t0 t1)
        | S.null k0              =  arc_ k0 (go t0 t1_)
        |              S.null k1 =  arc_ k1 (go t0_ t1)
    intersectBy'
        (Arc k0 (Just _) t0)
        t1_@(Branch{})
        | S.null k0              =  arc_ k0 (go t0 t1_)
    intersectBy'
        t0_@(Branch{})
        (Arc k1 (Just _) t1)
        | S.null k1              =  arc_ k1 (go t0_ t1)
    intersectBy' t0_ t1_         =  go t0_ t1_

    -- | The main recursion
    go Empty _    =  Empty
    go _    Empty =  Empty
    go  t0@(Branch p0 m0 l0 r0)
        t1@(Branch p1 m1 l1 r1)
        | shorter m0 m1  =  intersection0
        | shorter m1 m0  =  intersection1
        | p0 == p1       =  branch p0 m0 (go l0 l1) (go r0 r1)
        | otherwise      =  Empty
        where
        intersection0
            | nomatch p1 p0 m0  = Empty
            | zero p1 m0        = branch p0 m0 (go l0 t1) Empty
            | otherwise         = branch p0 m0 Empty (go r0 t1)
        intersection1
            | nomatch p0 p1 m1  = Empty
            | zero p0 m1        = branch p1 m1 (go t0 l1) Empty
            | otherwise         = branch p1 m1 Empty (go t0 r1)
    go t0_@(Arc k0 mv0 t0)
       t1_@(Arc k1 mv1 t1)
        | m' == 0 =
            let (pre,k0',k1') = breakMaximalPrefix k0 k1 in
            if S.null pre
            then error "intersectBy: no mask, but no prefix string"
            else
                let {-# INLINE t0' #-}
                    t0' = Arc k0' mv0 t0
                    {-# INLINE t1' #-}
                    t1' = Arc k1' mv1 t1
                in
                case (S.null k0', S.null k1') of
                (True, True)  -> arc  pre (intersectMaybe f mv0 mv1) (go t0 t1)
                (True, False) -> arc_ pre (go t0  t1')
                (False,True)  -> arc_ pre (go t0' t1)
                (False,False) -> arc_ pre (go t0' t1')
        where
        p0 = getPrefix t0_
        p1 = getPrefix t1_
        m' = branchMask p0 p1
    go t0_@(Arc{})
       t1_@(Branch _p1 m1 l r)
        | nomatch p0 p1 m1 = Empty
        | zero p0 m1       = branch p1 m1 (go t0_ l) Empty
        | otherwise        = branch p1 m1 Empty (go t0_ r)
        where
        p0 = getPrefix t0_
        p1 = getPrefix t1_
    go t0_@(Branch _p0 m0 l r)
       t1_@(Arc{})
        | nomatch p1 p0 m0 = Empty
        | zero p1 m0       = branch p0 m0 (go l t1_) Empty
        | otherwise        = branch p0 m0 Empty (go r t1_)
        where
        p0 = getPrefix t0_
        p1 = getPrefix t1_
    go _ _ =  Empty


intersectMaybe :: (a -> b -> Maybe c) -> Maybe a -> Maybe b -> Maybe c
{-# INLINE intersectMaybe #-}
intersectMaybe f (Just v0) (Just v1) = f v0 v1
intersectMaybe _ _         _         = Nothing


-- TODO(github#23): add `differenceBy`


{-----------------------------------------------------------
-- Priority-queue functions
-----------------------------------------------------------}

-- | Return the lexicographically smallest 'ByteString' and the
-- value it's mapped to; or 'Nothing' for the empty trie.  When one
-- entry is a prefix of another, the prefix will be returned.
minAssoc :: Trie a -> Maybe (ByteString, a)
minAssoc = go S.empty
    where
    go _ Empty              = Nothing
    go q (Arc k (Just v) _) = Just (q <> k, v)
    go q (Arc k Nothing  t) = go   (q <> k) t
    go q (Branch _ _ l _)   = go q l


-- | Return the lexicographically largest 'ByteString' and the
-- value it's mapped to; or 'Nothing' for the empty trie.  When one
-- entry is a prefix of another, the longer one will be returned.
maxAssoc :: Trie a -> Maybe (ByteString, a)
maxAssoc = go S.empty
    where
    go _ Empty                  = Nothing
    go q (Arc k (Just v) Empty) = Just (q <> k, v)
    go q (Arc k _        t)     = go   (q <> k) t
    go q (Branch _ _ _ r)       = go q r


mapView :: (Trie a -> Trie a)
        -> Maybe (ByteString, a, Trie a) -> Maybe (ByteString, a, Trie a)
{-# INLINE mapView #-}
mapView _ Nothing        = Nothing
mapView f (Just (k,v,t)) = Just (k,v, f t)


-- | Update the 'minAssoc' and return the old 'minAssoc'.
updateMinViewBy :: (ByteString -> a -> Maybe a)
                -> Trie a -> Maybe (ByteString, a, Trie a)
updateMinViewBy f = go S.empty
    where
    go _ Empty              = Nothing
    go q (Arc k (Just v) t) = Just (q',v, arc k (f q' v) t) where q' = q <> k
    go q (Arc k Nothing  t) = mapView (arc_ k) (go (q <> k) t)
    go q (Branch p m l r)   = mapView (\l' -> branch p m l' r) (go q l)


-- | Update the 'maxAssoc' and return the old 'maxAssoc'.
updateMaxViewBy :: (ByteString -> a -> Maybe a)
                -> Trie a -> Maybe (ByteString, a, Trie a)
updateMaxViewBy f = go S.empty
    where
    go _ Empty                  = Nothing
    go q (Arc k (Just v) Empty) = Just (q',v, arc k (f q' v) Empty) where q' = q <> k
    go q (Arc k mv       t)     = mapView (arc k mv) (go (q <> k) t)
    go q (Branch p m l r)       = mapView (branch p m l) (go q r)

------------------------------------------------------------
------------------------------------------------------- fin.
