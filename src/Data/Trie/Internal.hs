-- Not using -Wcompat, because it wants outdated things for GHC 8.0/8.2
{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
{-# OPTIONS_HADDOCK not-home #-}

{-# LANGUAGE NoImplicitPrelude, CPP, BangPatterns #-}
#if __GLASGOW_HASKELL__ >= 701
-- Alas, "GHC.Exts" isn't considered safe, even though 'build' surely is.
{-# LANGUAGE Trustworthy #-}
#endif
#if __GLASGOW_HASKELL__ >= 708
-- For 'GHC.Exts.IsList'
{-# LANGUAGE TypeFamilies #-}
#endif
------------------------------------------------------------
--                                              ~ 2021.12.05
-- |
-- Module      :  Data.Trie.Internal
-- Copyright   :  Copyright (c) 2008--2021 wren romano
-- License     :  BSD3
-- Maintainer  :  wren@cpan.org
-- Stability   :  experimental
-- Portability :  portable (with CPP)
--
-- Internal definition of the 'Trie' data type and generic functions
-- for manipulating them. Almost everything here is re-exported
-- from "Data.Trie", which is the preferred API for users. This
-- module is for developers who need deeper (and potentially fragile)
-- access to the abstract type.
--
-- @since 0.1.3
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
    , fromList
    , toList, toListBy, elems
    , foldrWithKey -- TODO: foldrWithKey', foldlWithKey, foldlWithKey'
    , cata_, cata

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

import Prelude hiding      (null, lookup)

import qualified Data.ByteString as S
import qualified Data.ByteString.Unsafe as SU
import Data.Trie.ByteStringInternal
import Data.Trie.BitTwiddle
import Data.Trie.Errors    (impossible)

import Data.Binary         (Binary(..), Get, Word8)
import Data.Bits           (xor)

#if MIN_VERSION_base(4,9,0)
-- [aka GHC 8.0.1]: "Data.Semigroup" added to base.
--
-- Note: Until [base-4.11 / GHC 8.4.1] there's a naming conflict
-- between 'Data.Semigroup.<>' vs 'Data.Monoid.<>'.  From this
-- version onward we'll prefer the semigroup one since it's future
-- compatible.  And besides, other than giving our instance, we
-- only actually *use* @(<>)@ as an alias for 'S.append', and
-- "Data.ByteString" nicely defines both 'Data.Semigroup.<>' and
-- 'Data.Monoid.mappend' to resolve to 'S.append'.
--
-- Note: [base-4.13.0 / GHC 8.8.1] has Prelude re-export 'Semigroup'
-- (the class name) and 'Data.Semigroup.<>'; however it does not
-- re-export 'stimes' nor (I assume) 'sconcat'!
import Data.Semigroup      (Semigroup(..))
#elif MIN_VERSION_base(4,8,0)
-- [aka GHC 7.10.1]: Prelude re-exports 'Monoid', but not 'Data.Monoid.<>'.
import Data.Monoid         ((<>))
#elif MIN_VERSION_base(4,5,0)
-- [aka GHC 7.4.1]: @(<>)@ added to "Data.Monoid".
import Data.Monoid         (Monoid(..), (<>))
#else
-- We'll just define our own @(<>)@...
import Data.Monoid         (Monoid(..))
#endif

import Control.DeepSeq     (NFData(rnf))
import Control.Monad       (liftM3, liftM4)

import qualified Data.Foldable as F
#if MIN_VERSION_base(4,8,0)
-- [aka GHC 7.10.1]: Prelude re-exports 'Applicative', @(<$>)@,
-- 'Foldable', and 'Traversable'.
#else
import Control.Applicative (Applicative(..), (<$>))
import Data.Foldable       (Foldable())
import Data.Traversable    (Traversable(traverse))
#endif

#if MIN_VERSION_base(4,9,0)
import Data.Functor.Classes (Eq1(..), Ord1(..), Show1(..), Read1(..))
import qualified Data.Functor.Classes as FC
#endif

#ifdef __GLASGOW_HASKELL__
import qualified Text.Read as R
import GHC.Exts (build)
#endif
#if __GLASGOW_HASKELL__ >= 708
import qualified GHC.Exts (IsList(..))
#endif

------------------------------------------------------------
------------------------------------------------------------

#if (!(MIN_VERSION_base(4,5,0)))
infixr 6 <>
-- | Only ever used to abbreviate 'S.append'
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
{-# INLINE (<>) #-}
#endif

-- | Infix variant of `uncurry`.  Currently only used in 'alterBy_'.
-- The fixity-level is like @(<$>)@; but I'm making it nonassociative
-- to avoid any possible\/potential confusion.
infix 4 $$
($$) :: (a -> b -> c) -> (a, b) -> c
($$) = uncurry
{-# INLINE ($$) #-}

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
Maybe[2] beneficial.  However, it complicates stating the invariants
about Node's recursion.

    data Node a   = Node (Maybe a) (ArcSet a)
                    -- Invariant: if Nothing then must be Branch
    data ArcSet a = None
                  | Arc    NonEmptyBS (Node a)
                  | Many              (Branch a)
    data Branch a = Branch {Prefix} {Mask} (ArcSet a) (ArcSet a)
    data Trie a   = Empty
                  | Start  NullableBS (Node a)

[2]: The above change is certainly beneficial from the perspective
of source-code size/repetition for 'mergeBy', 'intersectBy', and
other operations that operate on two tries in tandem; though it's
unclear whether/how much that transfers to compiled-code size/bloat.
Also since the 'Maybe' isn't unpacked, this introduces an additional
indirection to reach values.  Starting at version 0.2.7 there's an
ongoing effort to try to determine whether this change is beneficial
or not, and to quantify how much it affects things.

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
-}


-- [Note: Order of constructors]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- As discussed in "Data.IntMap.Internal", the order of constructors
-- in the definition is the order in which case analysis will always
-- test them.  On GHC 9.2.1 I haven't noticed a significant difference
-- from changing the order, but that could be due to bad benchmarking
-- (i.e., reusing the test suite); so I'll trust that this hasn't
-- changed so much since GHC 7.0.  The only question is whether
-- @Arc@ or @Branch@ should come first, which depends a lot on the
-- dataset being used.
--
-- This reordering change was performed for version 0.2.7
-- <https://github.com/wrengr/bytestring-trie/commit/75f3d32f7de7457dc7d029b60be3cce8b99c5e80>


-- | A map from 'ByteString's to @a@. For all the generic functions,
-- note that tries are strict in the @Maybe@ but not in @a@.
--
-- The 'Monad' instance is strange. If a key @k1@ is a prefix of
-- other keys, then results from binding the value at @k1@ will
-- override values from longer keys when they collide. If this is
-- useful for anything, or if there's a more sensible instance, I'd
-- be curious to know.

data Trie a
    = Branch {-# UNPACK #-} !Prefix
             {-# UNPACK #-} !Mask
                            !(Trie a)
                            !(Trie a)
    | Arc    {-# UNPACK #-} !ByteString
                            !(Maybe a)
                            !(Trie a)
    | Empty
    deriving Eq
    -- Prefix/Mask should be deterministic regardless of insertion order
    -- TODO: prove this is so.


{-----------------------------------------------------------
-- Smart constructors
-----------------------------------------------------------}

-- | Smart constructor to prune @Empty@ from @Branch@es.
branch :: Prefix -> Mask -> Trie a -> Trie a -> Trie a
{-# INLINE branch #-}
branch _ _ Empty r     = r
branch _ _ l     Empty = l
branch p m l     r     = Branch p m l r

{-
-- | A common precondition for ensuring the safety of the following
-- smart constructors.
ifJustThenNoEpsilon :: Maybe a -> Trie a -> Bool
ifJustThenNoEpsilon (Just _) (Arc k (Just _) _) = not (S.null k)
ifJustThenNoEpsilon _ _ = True
-}

-- | Smart constructor to prune @Arc@s that lead nowhere.
--
-- __Preconditions__
-- * @arc _ mv t | ifJustThenNoEpsilon mv t@
arc :: ByteString -> Maybe a -> Trie a -> Trie a
{-# INLINE arc #-}
arc k mv@(Just _) = Arc k mv
arc k    Nothing  = arc_ k

-- | > arc_ k ≡ arc k Nothing
--
-- This function is only very rarely needed; most of the time you
-- already know that the string is non-null, and thus you can call
-- 'prepend' directly.
arc_ :: ByteString -> Trie a -> Trie a
{-# INLINE arc_ #-}
arc_ k
    | S.null k  = id
    | otherwise = prepend k

-- | Variant of 'arc' where the string is known to be non-null.
--
-- __Preconditions__
-- * @arcNN k _  _ | not (S.null k)@
-- * @arcNN _ mv t | ifJustThenNoEpsilon mv t@
arcNN :: ByteString -> Maybe a -> Trie a -> Trie a
{-# INLINE arcNN #-}
arcNN k mv@(Just _) = Arc k mv
arcNN k    Nothing  = prepend k

-- | Prepend a non-empty string to a trie.
--
-- __Preconditions__
-- * @prepend k _ | not (S.null k)@
prepend :: ByteString -> Trie a -> Trie a
{-# INLINE prepend #-}
prepend _ t@Empty         = t
prepend k t@(Branch{})    = Arc k Nothing t
prepend k (Arc k' mv' t') = Arc (k <> k') mv' t'

-- | > epsilon mv ≡ arc S.empty mv
--
-- __Preconditions__
-- * @epsilon mv t | ifJustThenNoEpsilon mv t@
epsilon :: Maybe a -> Trie a -> Trie a
{-# INLINE epsilon #-}
epsilon Nothing     = id
epsilon mv@(Just _) = Arc S.empty mv


-- | Smart constructor to join two tries into a @Branch@ with maximal
-- prefix sharing. Requires knowing the prefixes, but can combine
-- either @Branch@es or @Arc@s.
--
-- __Preconditions__
-- * /do not/ use if prefixes could match entirely!
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

-- | Get the equivalent of the 'Prefix' stored in a @Branch@, but
-- for an @Arc@.
arcPrefix :: ByteString -> Prefix
{-# INLINE arcPrefix #-}
arcPrefix k
    | S.null k  = 0 -- for lack of a better value
    | otherwise = SU.unsafeHead k


{-----------------------------------------------------------
-- Error messages
-----------------------------------------------------------}

-- TODO: move off to "Data.Trie.Errors"?
-- TODO: shouldn't we inline the logic and just NOINLINE the string
-- constant? There are only three use sites, which themselves aren't
-- inlined...
-- TODO: this is almost identical to 'arcPrefix'; the only difference
-- is that we use this one for matching a query against a trie,
-- whereas we use 'arcPrefix' when matching two tries together.
-- That said, since our test suite never throws this error, it
-- should be safe to use 'arcPrefix' everywhere instead.  Or, if
-- we want to preserve the semantic distinction, then we could start
-- using 'Control.Exception.assert' to hoist the null-check out to
-- where it belongs and still allow it to compile away.  Conversely,
-- note that 'arcPrefix' is never called with a null string either
-- (since null strings are only ever allowed for epsilon values;
-- and all the use-sites of 'arcPrefix' are after handling those
-- epsilons, or otherwise guarded).
errorLogHead :: String -> ByteString -> ByteStringElem
{-# NOINLINE errorLogHead #-}
errorLogHead fn q
    | S.null q  = error $ "Data.Trie.Internal." ++ fn ++": found null subquery"
    | otherwise = SU.unsafeHead q

------------------------------------------------------------
------------------------------------------------------------


{-----------------------------------------------------------
-- Instances: Eq, Eq1
-----------------------------------------------------------}

{-
-- 'IntMap' defines their own instance so as to check the Mask
-- before the Prefix; and they have done so since at least version
-- 0.5.0.0 (2011).  So I assume the performance benefits of doing
-- that are good enough to be worth it; thus, we'll do the same.
--
-- TODO: benchmark!!
instance Eq a => Eq (Trie a) where
    (==) = equal
    (/=) = unequal

-- TODO: mark this INLINABLE to specialize on the Eq instance?  Why doesn't IntMap?
-- TODO: Alternatively, why doesn't IntMap simply reuse the 'liftEq' implementation?
equal :: Eq a => Trie a -> Trie a -> Bool
equal (Branch p0 m0 l0 r0)
      (Branch p1 m1 l1 r1) = m0 == m1 && p0 == p1 && equal l0 l1 && equal r0 r1
equal (Arc k0 mv0 t0)
      (Arc k1 mv1 t1)      = k0 == k1 && mv0 == mv1 && equal t0 t1
equal Empty Empty          = True
equal _     _              = False

unequal :: Eq a => Trie a -> Trie a -> Bool
unequal (Branch p0 m0 l0 r0)
        (Branch p1 m1 l1 r1) = m0 /= m1 || p0 /= p1 || unequal l0 l1 || unequal r0 r1
unequal (Arc k0 mv0 t0)
        (Arc k1 mv1 t1)      = k0 /= k1 || mv0 /= mv1 || unequal t0 t1
unequal Empty Empty          = False
unequal _     _              = True
-}

#if MIN_VERSION_base(4,9,0)
-- | @since 0.2.7
instance Eq1 Trie where
  liftEq = equal1

-- TODO: why doesn't IntMap close over @eq@?  Does it really cost so much more?
-- TODO: INLINEABLE?
equal1 :: (a -> b -> Bool) -> Trie a -> Trie b -> Bool
equal1 eq (Branch p0 m0 l0 r0) (Branch p1 m1 l1 r1) =
    m0 == m1 && p0 == p1 && equal1 eq l0 l1 && equal1 eq r0 r1
equal1 eq (Arc k0 mv0 t0) (Arc k1 mv1 t1) =
    k0 == k1 && liftEq eq mv0 mv1 && equal1 eq t0 t1
equal1 _ Empty Empty = True
equal1 _ _     _     = False
#endif

{-----------------------------------------------------------
-- Instances: Ord, Ord1
-----------------------------------------------------------}

-- TODO: Both of these instances are terribly inefficient, because
-- they unnecessarily reconstruct the keys.
--
-- | @since 0.2.7
instance Ord a => Ord (Trie a) where
    compare t0 t1 = compare (toList t0) (toList t1)

#if MIN_VERSION_base(4,9,0)
-- | @since 0.2.7
instance Ord1 Trie where
    liftCompare cmp t0 t1 =
        liftCompare (liftCompare cmp) (toList t0) (toList t1)
#endif

{-----------------------------------------------------------
-- Instances: Show, Show1
-----------------------------------------------------------}

-- This instance does not unveil the innards of our abstract type.
-- It doesn't emit truly proper Haskell code though, since ByteStrings
-- are printed as (ASCII) Strings, but that's not our fault. (Also
-- 'fromList' is in "Data.Trie" instead of here.)
--
-- | @since 0.2.2
instance (Show a) => Show (Trie a) where
    showsPrec p t = showParen (p > 10)
                  $ ("fromList " ++) . shows (toList t)

#if MIN_VERSION_base(4,9,0)
-- | @since 0.2.7
instance Show1 Trie where
    liftShowsPrec sp sl p t =
        FC.showsUnaryWith (liftShowsPrec sp' sl') "fromList" p (toList t)
        where
        sp' = liftShowsPrec sp sl
        sl' = liftShowList  sp sl
#endif

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

{-----------------------------------------------------------
-- Instances: Read, Read1
-----------------------------------------------------------}

-- | @since 0.2.7
instance (Read a) => Read (Trie a) where
#ifdef __GLASGOW_HASKELL__
    readPrec = R.parens . R.prec 10 $ do
        R.Ident "fromList" <- R.lexP
        fromList <$> R.readPrec

    readListPrec = R.readListPrecDefault
#else
    readsPrec p = readParen (p > 10) $ \ r0 -> do
        ("fromList", r1) <- lex r0
        (xs, r2) <- reads r1
        return (fromList xs, r2)
#endif

#if MIN_VERSION_base(4,9,0)
-- | @since 0.2.7
instance Read1 Trie where
    liftReadsPrec rp rl =
        FC.readsData $ FC.readsUnaryWith (liftReadsPrec rp' rl')
            "fromList" fromList
        where
        rp' = liftReadsPrec rp rl
        rl' = liftReadList  rp rl
#endif

{-----------------------------------------------------------
-- Instances: Binary
-----------------------------------------------------------}

-- TODO: consider an instance more like the new one for Data.Map. Better?
instance (Binary a) => Binary (Trie a) where
    put Empty            = do put (0 :: Word8)
    put (Arc k mv t)     = do put (1 :: Word8); put k; put mv; put t
    put (Branch p m l r) = do put (2 :: Word8); put p; put m; put l; put r

    -- BUG(github#21): need to verify the invariants!
    get = do tag <- get :: Get Word8
             case tag of
                 0 -> return Empty
                 1 -> liftM3 Arc    get get get
                 _ -> liftM4 Branch get get get get

{-----------------------------------------------------------
-- Instances: NFData
-----------------------------------------------------------}

-- | @since 0.2.7
instance NFData a => NFData (Trie a) where
    rnf Empty            = ()
    rnf (Arc _ mv t)     = rnf mv `seq` rnf t
    rnf (Branch _ _ l r) = rnf l `seq` rnf r

{-
-- TODO: do we want/need this one?
#if __GLASGOW_HASKELL__
instance Data.Data.Data (Trie a) where ...
-- See 'IntMap' for how to do this without sacrificing abstraction.
#endif
-}

{-----------------------------------------------------------
-- Instances: Functor
-----------------------------------------------------------}

-- TODO: IntMap floats the definition of 'fmap' out of the instance
-- so that it can provide rewrite rules (for @map f . map g@ and
-- for @map coerce@).  Should we do the same?
instance Functor Trie where
    fmap f = go
        where
        go Empty              = Empty
        go (Arc k Nothing  t) = Arc k Nothing      (go t)
        go (Arc k (Just v) t) = Arc k (Just (f v)) (go t)
        go (Branch p m l r)   = Branch p m (go l) (go r)
#if __GLASGOW_HASKELL__
    -- Non-default definition since 0.2.7
    -- Avoiding closure over @v@ because that's what IntMap does.
    _ <$ Empty              = Empty
    v <$ (Arc k Nothing  t) = Arc k Nothing  (v <$ t)
    v <$ (Arc k (Just _) t) = Arc k (Just v) (v <$ t)
    v <$ (Branch p m l r)   = Branch p m (v <$ l) (v <$ r)
#endif

{-----------------------------------------------------------
-- Instances: Foldable
-----------------------------------------------------------}
-- TODO: move this one to the bottom, or there abouts

instance Foldable Trie where
    {-# INLINABLE fold #-}
    fold = go
        where
        go Empty              = mempty
        go (Arc _ Nothing  t) = go t
        go (Arc _ (Just v) t) = v `mappend` go t
        go (Branch _ _ l r)   = go l `mappend` go r
    -- If our definition of foldr is so much faster than the Endo
    -- default, then maybe we should remove this and use the default
    -- foldMap based on foldr
    {-# INLINE foldMap #-}
    foldMap f = go
        where
        go Empty              = mempty
        go (Arc _ Nothing  t) = go t
        go (Arc _ (Just v) t) = f v `mappend` go t
        go (Branch _ _ l r)   = go l `mappend` go r
    -- TODO: benchmark this one against the Endo-based default.
    -- TODO: if it's slower, try expanding out the CPS stuff again.
    -- (Though to be sure that doesn't interfere with being a
    -- good-producer for list fusion.)
    -- TODO: is this safe for deep-strict @f@, or only WHNF-strict?
    -- (Cf., the bug note at 'foldrWithKeys')
    -- TODO: do we need to float this definition out of the class
    --  in order to get it to inline appropriately for list fusion etc?
    {-# INLINE foldr #-}
    foldr f z0 = \t -> go t z0 -- eta for better inlining
        where
        -- TODO: would it be better to eta-expand this?
        go Empty              = id
        go (Arc _ Nothing  t) =       go t
        go (Arc _ (Just v) t) = f v . go t
        go (Branch _ _ l r)   = go l . go r
    {-# INLINE foldr' #-}
    foldr' f z0 = \t -> go z0 t -- eta for better inlining
        where
        go !z Empty              = z
        go  z (Arc _ Nothing  t) = go z t
        go  z (Arc _ (Just v) t) = f v $! go z t
        go  z (Branch _ _ l r)   = go (go z r) l
    {-
    -- TODO: benchmark this variant vs the above.
    foldr' f z0 = \t -> go t z0 id -- eta for better inlining
        where
        go Empty              !z c = c z
        go (Arc _ Nothing  t)  z c = go t z c
        go (Arc _ (Just v) t)  z c = go t z (\ !z' -> c $! f v z')
        go (Branch _ _ l r)    z c = go r z (\ !z' -> go l z' c)
    -}
    {-# INLINE foldl #-}
    foldl f z0 = \t -> go z0 t -- eta for better inlining
        where
        -- TODO: CPS to restore the tail-call for @Branch@?
        go z Empty              = z
        go z (Arc _ Nothing  t) = go z t
        go z (Arc _ (Just v) t) = go (f z v) t
        go z (Branch _ _ l r)   = go (go z l) r
    {-# INLINE foldl' #-}
    foldl' f z0 = \t -> go z0 t -- eta for better inlining
        where
        -- TODO: CPS to restore the tail-call for @Branch@?
        go !z Empty              = z
        go  z (Arc _ Nothing  t) = go z t
        go  z (Arc _ (Just v) t) = go (f z v) t
        go  z (Branch _ _ l r)   = go (go z l) r
#if MIN_VERSION_base(4,8,0)
    {-# INLINE length #-}
    length = size
    {-# INLINE null #-}
    null   = null -- FIXME: ensure this isn't cyclic definition!
    {-# INLINE toList #-}
    toList = elems -- NB: Foldable.toList /= Trie.toList
    {-
    -- TODO: need to move these definitions here...
    -- TODO: may want to give a specialized implementation of 'member' then
    {-# INLINE elem #-}
    elem = member
    -}
    -- TODO: why does IntMap define these two specially, rather than using foldl' or foldl1' ?
    {-# INLINABLE maximum #-}
    maximum = start
        where
        start Empty              = error "Data.Foldable.maximum @Trie: empty trie"
        start (Arc _ Nothing  t) = start t
        start (Arc _ (Just v) t) = go v t
        start (Branch _ _ l r)   = go (start l) r
        go !w Empty              = w
        go  w (Arc _ Nothing  t) = go w t
        go  w (Arc _ (Just v) t) = go (max w v) t
        go  w (Branch _ _ l r)   = go (go w l) r
    {-# INLINABLE minimum #-}
    minimum = start
        where
        start Empty              = error "Data.Foldable.minimum @Trie: empty trie"
        start (Arc _ Nothing  t) = start t
        start (Arc _ (Just v) t) = go v t
        start (Branch _ _ l r)   = go (start l) r
        go !w Empty              = w
        go  w (Arc _ Nothing  t) = go w t
        go  w (Arc _ (Just v) t) = go (min w v) t
        go  w (Branch _ _ l r)   = go (go w l) r
    {-# INLINABLE sum #-}
    sum = F.foldl' (+) 0
    {-# INLINABLE product #-}
    product = F.foldl' (*) 1
#endif

-- TODO: newtype Keys = K Trie  ; instance Foldable Keys
-- TODO: newtype Assoc = A Trie ; instance Foldable Assoc

{-----------------------------------------------------------
-- Instances: Traversable, Applicative, Monad
-----------------------------------------------------------}

instance Traversable Trie where
    traverse f = go
        where
        go Empty              = pure Empty
        go (Arc k Nothing  t) = Arc k Nothing        <$> go t
        go (Arc k (Just v) t) = Arc k . Just <$> f v <*> go t
        go (Branch p m l r)   = Branch p m <$> go l <*> go r

-- TODO: 'traverseWithKey', like 'IntMap' has...

------------------------------------------------------------
-- | @since 0.2.2
instance Applicative Trie where
    pure      = singleton S.empty
    t0 <*> t1 = t0 >>= (<$> t1)
    -- TODO: can we do better than these defaults?
    -- t0 *> t1       = (id    <$  t0) <*> t1
    -- t0 <* t1       = (const <$> t0) <*> t1
    -- liftA2 f t0 t1 = (f     <$> t0) <*> t1
    {-
    -- Inlining and case-of-case yields the following (which GHC
    -- could surely derive on its own):
    Empty            *> _  = Empty
    Branch p m l r   *> t1 = branch p m (l *> t1) (r *> t1)
    Arc k Nothing  s *> t1 = prepend k           (s *> t1)
    Arc k (Just _) s *> t1 = arc_ k (t1 `unionL` (s *> t1))

    -- This one is marginally better, since we can use @(<$)@ in the Accept case.
    Empty            <* _  = Empty
    Branch p m l r   <* t1 = branch p m (l <* t1) (r <* t1)
    Arc k Nothing  s <* t1 = prepend k                  (s <* t1)
    Arc k (Just v) s <* t1 = arc_ k ((v <$ t1) `unionL` (s <* t1))

    -- This one took a lot of inlining\/massaging, so might be worth it...
    -- It's easier to see the structure if we define a closure
    -- @(liftA2 f _ t1)@, but unclear if that would hurt the improvement
    -- of the implementation.
    liftA2 f Empty              _  = empty
    liftA2 f (Branch p m l r)   t1 = branch p m (liftA2 f l t1) (liftA2 f r t1)
    liftA2 f (Arc k Nothing  s) t1 = prepend k (liftA2 f s t1)
    liftA2 f (Arc k (Just v) s) t1 = arc_ k ((f v <$> t1) `unionL` liftA2 f s t1)
    -}

------------------------------------------------------------
-- Does this even make sense? It's not nondeterminism like lists
-- and sets. If no keys were prefixes of other keys it'd make sense
-- as a decision-tree; but since keys /can/ prefix, tries formed
-- from shorter keys can shadow the results from longer keys due
-- to the 'unionL'. It does seem to follow the laws though... What
-- computation could this possibly represent?
--
--  1. return x >>= f  ≡ f x
--  2. m >>= return    ≡ m
--  3. (m >>= f) >>= g ≡ m >>= (\x -> f x >>= g)
--
-- | @since 0.2.2
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
    (>>=) (Arc k Nothing  t) f = prepend k (t >>= f)
    (>>=) (Arc k (Just v) t) f = arc_ k (f v `unionL` (t >>= f))
                               where
                               unionL = mergeBy (\x _ -> Just x)


{-----------------------------------------------------------
-- Instances: Semigroup, Monoid
-----------------------------------------------------------}

#if MIN_VERSION_base(4,9,0)
-- The "Data.Semigroup" module is in base since 4.9.0.0; but having
-- the 'Semigroup' superclass for the 'Monoid' instance only comes
-- into force in base 4.11.0.0.
-- | @since 0.2.5.0
instance (Semigroup a) => Semigroup (Trie a) where
    (<>) = mergeBy $ \x y -> Just (x <> y)
    -- Non-default definition since 0.2.7
    stimes n = fmap (stimes n)
#endif
{-
-- TODO: We surely don't want to use this definition for the instance,
-- because it would only be cheaper when the semigroup's 'sconcat'
-- is overwhelmingly cheaper than iterating @(<>)@; however, we
-- might consider exposing it as an auxilliary function, if there
-- are any such semigroups worth considering...
sconcatExpensive :: Semigroup a => NonEmpty (Trie a) -> Trie a
sconcatExpensive (t :| ts) = (sconcat . mkNE) <$> aggregate (t:ts)
    where
    mkNE []     = impossible "sconcatExpensive"
    mkNE (v:vs) = v :| vs

aggregate :: [Trie a] -> Trie [a]
aggregate = foldr (mergeBy (\x y -> Just (x ++ y)) . fmap return) empty
-}


-- This instance is more sensible than Data.IntMap and Data.Map's
instance (Monoid a) => Monoid (Trie a) where
    mempty = empty
    -- TODO: optimized implementation of 'mconcat'
#if MIN_VERSION_base(4,11,0)
    -- Now that the canonical instance is the default, don't define
    -- 'mappend', in anticipation of Phase 4 of:
    -- <https://gitlab.haskell.org/ghc/ghc/-/wikis/proposal/semigroup-monoid>
{-
--if MIN_VERSION_base(4,9,0)
    -- GHC 8.0/8.2 -Wnoncanonical-monoid-instances wants this
    -- definition, even though 'Semigroup' isn't a superclass of
    -- 'Monoid' until base-4.11 so it would require additional
    -- constraints on the instance.  Since we're only supporting
    -- these older versions for legacy reasons, there's no reason
    -- to bother adhering to the -Wcompat here.
    mappend = (<>)
-}
#else
    mappend = mergeBy $ \x y -> Just (x `mappend` y)
#endif


{-----------------------------------------------------------
-- Instances: Alternative, MonadPlus
-----------------------------------------------------------}

-- Since the Monoid instance isn't natural in @a@, I can't think
-- of any other sensible instance for MonadPlus. It's as specious
-- as Maybe, IO, and STM's instances though.
--
-- MonadPlus laws: <http://www.haskell.org/haskellwiki/MonadPlus>
--  1. <Trie a, mzero, mplus> forms a monoid
--  2. mzero >>= f        ≡ mzero
--  3. m >> mzero         ≡ mzero
--  4. mplus m n >>= k    ≡ mplus (m >>= k) (n >>= k)
--  4' mplus (return a) n ≡ return a
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

{-
-- TODO: This one is less popular, but imposes few additional constraints.
-- <https://hackage.haskell.org/package/filtrable>
instance Filterable Trie where
    mapMaybe  = filterMap
    -- > catMaybes = filterMap id
    -- > filter p = mapMaybe ((<$) <*> guard . p)
    --            ≡ mapMaybe (\x -> if p x then Just x else Nothing)

    -- The @witherable@ package breaks these two out into a separate class
    mapMaybeA -- aka 'wither'
        :: (Applicative f) => (a -> f (Maybe b)) -> Trie a -> f (Trie b)
    -- > mapMaybeA f xs = catMaybes <$> traverse f xs
    filterA    :: (Applicative f) => (a -> f Bool) -> Trie a -> f (Trie a)
    -- > filterA f = mapMaybeA (\ x -> (x <$) . guard <$> f x)
    --             ≡ mapMaybeA $ \x -> (\b -> if b then Just x else Nothing) <$> f x


    mapEither :: (a -> Either b c) -> f a -> (f b, f c)
    -- > mapEither f = (,) <$> mapMaybe (either Just (pure Nothing) . f) <*> mapMaybe (either (pure Nothing) Just . f)
    partitionEithers :: f (Either a b) -> (f a, f b)
    -- > partitionEithers = mapEither id
    mapEitherA :: (Applicative f) => (a -> f (Either b c)) -> Trie a -> f (Trie b, Trie c)
    -- > mapEitherA f = liftA2 (,) <$> mapMaybeA (fmap (Just `either` pure Nothing) . f) <*> mapMaybeA (fmap (pure Nothing `either` Just) . f)

    -- [Laws]
    -- > mapMaybe Just ≡ id                                        -- special case of the \"conservation\" below.
    -- > mapMaybe f ≡ catMaybes . fmap f                           -- default definition
    -- > catMaybes  ≡ mapMaybe id                                  -- default definition
    -- > filter f   ≡ mapMaybe (\x -> bool Nothing (Just x) (f x)) -- default definition, for @witherable@
    -- > mapMaybe g . mapMaybe f ≡ mapMaybe (g <=< f)
    -- > foldMap g . filter f ≡ foldMap (\x -> bool mempty (g x) (f x))

-- TODO: This one is more popular, but has many many dependencies.
-- <https://hackage.haskell.org/package/witherable>
instance Filterable Trie where
    mapMaybe = filterMap
    -- The default implementations should be perfectly efficient,
    -- just so long as the 'filterMap' gets inlined so that the
    -- function argument can get counter-inlined into the loop and
    -- thus doesn't generate a closure nor lambda-call overhead.
    -- > catMaybes = filterMap id
    -- > filter p  = filterMap (\v -> if p v then Just v else Nothing)

    -- [Laws]
    -- > mapMaybe (Just . f) ≡ fmap f                  -- \"conservation\"
    -- > mapMaybe f . mapMaybe g ≡ mapMaybe (f <=< g)  -- \"composition\"

-- minimal instance requires nothing...
instance Witherable Trie where
    wither :: Applicative f => (a -> f (Maybe b)) -> Trie a -> f (Trie b)
    -- > wither f = fmap catMaybes . traverse f
    witherM :: Monad m => (a -> m (Maybe b)) -> Trie a -> m (Trie b)
    -- > witherM = wither
    filterA :: Applicative f => (a -> f Bool) -> Trie a -> f (Trie a)
    -- > filterA p = wither $ \x -> (\b -> if b then Just x else Nothing) <$> p x
    witherMap :: Applicative f => (Trie b -> r) -> (a -> f (Maybe b)) -> Trie a -> f r
    -- > witherMap f g = fmap f . wither g

    -- [Laws]
    -- > wither (Identity . Just) ≡ Identity
    -- > Compose . fmap (wither f) . wither g ≡ wither (Compose . fmap (wither f) . g)
    -- > wither (fmap Just . f) ≡ traverse f
    -- > wither (Identity . f) ≡ Identity . mapMaybe f
    -- And naturality provides:
    -- > t . wither f ≡ wither (t . f) -- if @t@ is an applicative-transformation


-- TODO: even if we don't do those instances, maybe we ought to
-- provide 'filter' and 'wither' or 'witherMap' directly anyways.
--
-- Assuming I didn't make a mistake somewhere:
wither :: Applicative f => (a -> f (Maybe b)) -> Trie a -> f (Trie b)
wither f = go
    where
    go Empty              = pure Empty
    go (Arc k Nothing  t) = prepend k     <$> go t
    go (Arc k (Just v) t) = arc k <$> f v <*> go t
    go (Branch p m l r)   = branch p m <$> go l <*> go r
-- And hence
witherMap g f = go
    where
    go Empty              = pure (g Empty)
    go (Arc k Nothing  t) = g . prepend k         <$> go t
    go (Arc k (Just v) t) = (g .) . arc k <$> f v <*> go t
    go (Branch p m l r)   = (g .) . branch p m <$> go l <*> go r
filterA p = go
    where
    go Empty              = pure Empty
    go (Arc k Nothing  t) = prepend k        <$> go t
    go (Arc k (Just v) t) = arcB k v <$> p v <*> go t
    go (Branch p m l r)   = branch p m <$> go l <*> go r
-- And separately derived we have:
filter :: (a -> Bool) -> Trie a -> Trie b
filter p = go
    where
    go Empty              = empty
    go (Arc k Nothing  t) = prepend k      (go t)
    go (Arc k (Just v) t) = arcB k v (p v) (go t)
    go (Branch p m l r)   = branch p m (go l) (go r)

-- > arcB k v b ≡ arc k (if b then Just v else Nothing)
arcB k v True  = Arc k (Just v)
arcB k _ False = arc_ k
-}


-- | Apply a function to all values, potentially removing them.
-- This function satisfies the laws:
--
-- [/conservation/]
--   @'filterMap' ('Just' '.' f) ≡ 'fmap' f@
--
-- [/composition/]
--   @'filterMap' f '.' 'filterMap' g ≡ 'filterMap' (f '<=<' g)@
filterMap :: (a -> Maybe b) -> Trie a -> Trie b
filterMap f = go
    where
    go Empty              = empty
    go (Arc k Nothing  t) = prepend k   (go t)
    go (Arc k (Just v) t) = arc k (f v) (go t)
    go (Branch p m l r)   = branch p m (go l) (go r)
-- TODO: rewrite rule for both laws.

-- TODO: why not implement as @contextualFilterMap (const . f)@ ?
-- Does that actually incur additional overhead?


-- | Generic version of 'fmap'. This function is notably more
-- expensive than 'fmap' or 'filterMap' because we have to reconstruct
-- the keys.
mapBy :: (ByteString -> a -> Maybe b) -> Trie a -> Trie b
mapBy f = go S.empty
    where
    go _ Empty              = empty
    go q (Arc k Nothing  t) = prepend k      (go q' t) where q' = q <> k
    go q (Arc k (Just v) t) = arc k (f q' v) (go q' t) where q' = q <> k
    go q (Branch p m l r)   = branch p m (go q l) (go q r)

-- TODO: why not implement as @contextualMapBy (\k v _ -> f k v)@ ?
-- Does that actually incur additional overhead?


-- | A variant of 'fmap' which provides access to the subtrie rooted
-- at each value.
--
-- @since 0.2.3
contextualMap :: (a -> Trie a -> b) -> Trie a -> Trie b
contextualMap f = go
    where
    go Empty              = Empty
    go (Arc k Nothing  t) = Arc k Nothing        (go t)
    go (Arc k (Just v) t) = Arc k (Just (f v t)) (go t)
    go (Branch p m l r)   = Branch p m (go l) (go r)


-- | A variant of 'contextualMap' which applies the function strictly.
--
-- @since 0.2.3
contextualMap' :: (a -> Trie a -> b) -> Trie a -> Trie b
contextualMap' f = go
    where
    go Empty              = Empty
    go (Arc k Nothing  t) = Arc k Nothing         (go t)
    go (Arc k (Just v) t) = Arc k (Just $! f v t) (go t)
    go (Branch p m l r)   = Branch p m (go l) (go r)


-- | A contextual variant of 'filterMap'.
--
-- @since 0.2.3
contextualFilterMap :: (a -> Trie a -> Maybe b) -> Trie a -> Trie b
contextualFilterMap f = go
    where
    go Empty              = empty
    go (Arc k Nothing  t) = prepend k     (go t)
    go (Arc k (Just v) t) = arc k (f v t) (go t)
    go (Branch p m l r)   = branch p m (go l) (go r)


-- | A contextual variant of 'mapBy'. Again note that this is
-- expensive since we must reconstruct the keys.
--
-- @since 0.2.3
contextualMapBy :: (ByteString -> a -> Trie a -> Maybe b) -> Trie a -> Trie b
contextualMapBy f = go S.empty
    where
    go _ Empty              = empty
    go q (Arc k Nothing  t) = prepend k        (go q' t) where q' = q <> k
    go q (Arc k (Just v) t) = arc k (f q' v t) (go q' t) where q' = q <> k
    go q (Branch p m l r)   = branch p m (go q l) (go q r)


{-----------------------------------------------------------
-- Basic functions
-----------------------------------------------------------}
-- TODO: probably want to hoist this up top

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
size' Empty              f !n = f n
size' (Branch _ _ l r)   f  n = size' l (size' r f) n
size' (Arc _ Nothing t)  f  n = size' t f n
size' (Arc _ (Just _) t) f  n = size' t f (n + 1)
    -- TODO: verify we've retained the correct strictness for this last case


{-----------------------------------------------------------
-- Extra folding functions
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
-- forced by @f@. It's already non-strict, but we should ensure
-- O(n) not O(n^2) when it's forced.
--
-- BUG: not safe for deep strict @f@, only for WHNF-strict like (:)
-- Where to put the strictness to amortize it?
--
-- | Variant of 'foldr' which passes the keys too.  This function
-- is notably more expensive than 'foldr', because we have to
-- reconstruct the keys.
--
-- @since 0.2.2
foldrWithKey :: (ByteString -> a -> b -> b) -> b -> Trie a -> b
{-# INLINE foldrWithKey #-}
foldrWithKey f z0 = \t -> go S.empty t z0 -- eta for better inlining
    where
    -- TODO: eta-expand and/or CPS?
    go _ Empty              = id
    go q (Branch _ _ l r)   = go q l . go q r
    go q (Arc k Nothing  t) =          go q' t where q' = q <> k
    go q (Arc k (Just v) t) = f q' v . go q' t where q' = q <> k

{-
-- TODO: benchmark the non-WithKey variants so we can be comfortable
-- in our implementation choices, before uncommenting these.

-- | Variant of 'foldr'' which passes the keys too.  This function
-- is notably more expensive than 'foldr'', because we have to
-- reconstruct the keys.
--
-- @since 0.2.7
foldrWithKey' :: (ByteString -> a -> b -> b) -> b -> Trie a -> b
{-# INLINE foldrWithKey' #-}
foldrWithKey' f z0 = \t -> go S.empty z0 t -- eta for better inlining
    where
    -- TODO: benchmark this vs the CPS'ed variant, a~la foldr' above.
    go _ !z Empty              = z
    go q  z (Branch _ _ l r)   = go q (go q z r) l
    go q  z (Arc k Nothing  t) =           go q' z t where q' = q <> k
    go q  z (Arc k (Just v) t) = f q' v $! go q' z t where q' = q <> k

-- | Variant of 'foldl' which passes the keys too.  This function
-- is notably more expensive than 'foldl', because we have to
-- reconstruct the keys.
--
-- @since 0.2.7
foldlWithKey :: (b -> ByteString -> a -> b) -> b -> Trie a -> b
{-# INLINE foldlWithKey #-}
foldlWithKey f z0 = \t -> go S.empty z0 t -- eta for better inlining
    where
    -- TODO: CPS to restore the tail-call for @Branch@?
    go _ z Empty              = z
    go q z (Arc k Nothing  t) = go q'    z       t where q' = q <> k
    go q z (Arc k (Just v) t) = go q' (f z q' v) t where q' = q <> k
    go q z (Branch _ _ l r)   = go q (go q z l) r

-- | Variant of 'foldl'' which passes the keys too.  This function
-- is notably more expensive than 'foldl'', because we have to
-- reconstruct the keys.
--
-- @since 0.2.7
foldlWithKey' :: (b -> ByteString -> a -> b) -> b -> Trie a -> b
{-# INLINE foldlWithKey' #-}
foldlWithKey' f z0 = \t -> go z0 t -- eta for better inlining
    where
    -- TODO: CPS to restore the tail-call for @Branch@?
    go _ !z Empty              = z
    go q  z (Arc k Nothing  t) = go q'    z       t where q' = q <> k
    go q  z (Arc k (Just v) t) = go q' (f z q' v) t where q' = q <> k
    go q  z (Branch _ _ l r)   = go q (go q z l) r
-}

-- | Catamorphism for tries.  Unlike most other functions (`mapBy`,
-- `contextualMapBy`, `foldrWithKey`, etc), this function does *not*
-- reconstruct the full `ByteString` for each value; instead it
-- only returns the suffix since the previous value or branch point.
--
-- This function is a direct\/literal catamorphism of the implementation
-- datatype, erasing only some bitmasking metadata for the branches.
-- For a more semantic catamorphism, see `cata`.
--
-- @since 0.2.6
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
-- @since 0.2.6
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


{-----------------------------------------------------------
-- Instances: IsList
-----------------------------------------------------------}

#if __GLASGOW_HASKELL__ >= 708
-- | @since 0.2.7
instance GHC.Exts.IsList (Trie a) where
    type Item (Trie a) = (ByteString, a)
    fromList = fromList
    toList   = toList
#endif


-- /Moved to "Data.Trie.Internal" since 0.2.7/
-- We define this here because 'GHC.Exts.IsList' wants it.
--
-- | Convert association list into a trie.  On key conflict, values
-- earlier in the list shadow later ones.
fromList :: [(ByteString,a)] -> Trie a
{-# INLINE fromList #-}
fromList = foldr (uncurry insert) empty
    where
    insert = alterBy (\_ x _ -> Just x)


-- /Moved to "Data.Trie.Internal" since 0.2.7/
-- We define this here simply because so many instances want to use it.
-- TODO: would it be worth defining this directly, for optimizing
-- the case where list fusion doesn't eliminate the list?
--
-- | Convert trie into association list.  The list is ordered
-- according to the keys.
toList :: Trie a -> [(ByteString,a)]
{-# INLINE toList #-}
toList = toListBy (,)


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

-- /Moved to "Data.Trie.Internal" since 0.2.7/
-- So that we can do list-fusion, and reuse the definition for Foldable
--
-- | Return all values in the trie, in key-sorted order.
--
-- @since 0.2.2
elems :: Trie a -> [a]
{-# INLINE elems #-}
#ifdef __GLASGOW_HASKELL__
elems t = build (\cons nil -> F.foldr cons nil t)
#else
elems = F.foldr (:) []
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
--
-- __NOTE__: /Type changed in 0.2.7/
lookupBy_
    :: (a -> Trie a -> b)   -- ^ The query matches a value.
    -> (Trie a -> b)        -- ^ The query doesn't match, but an extension might.
    -> b                    -- ^ The query doesn't match, nor does any extension.
    -> ByteString -> Trie a -> b
lookupBy_ found missing clash = start
    where
    -- | Deal with epsilon query (when there is no epsilon value)
    start q t@(Branch{}) | S.null q = missing t
    start q t                       = go q t
    -- | The main recursion
    go _    Empty       = clash
    go q   (Arc k mv t) =
        let (_,k',q')   = breakMaximalPrefix k q
        in case (S.null k', S.null q') of
                (False, True)  -> missing (Arc k' mv t)
                (False, False) -> clash
                (True,  True)  ->
                    case mv of
                    Nothing -> missing t
                    Just v  -> found v t
                (True,  False) -> go q' t
    go q t_@(Branch{}) = findArc t_
        where
        qh = errorLogHead "lookupBy_" q
        -- | /O(min(m,W))/, where /m/ is number of @Arc@s in this
        -- branching, and /W/ is the word size of the Prefix,Mask type.
        findArc (Branch p m l r)
            | nomatch qh p m  = clash
            | zero qh m       = findArc l
            | otherwise       = findArc r
        findArc t@(Arc{})     = go q t
        findArc Empty         = impossible "lookupBy_" -- see [Note1]

-- [Note1]: Our use of the 'branch' and 'branchMerge' smart
-- constructors ensure that 'Empty' never occurs in a 'Branch' tree
-- ('Empty' can only occur at the root, or under an 'Arc' with
-- value); therefore the @findArc Empty@ case is unreachable.  If
-- we allowed such nodes, however, then this case should return the
-- same result as the 'nomatch' case.


-- This function needs to be here, not in "Data.Trie", because of
-- 'arc' which isn't exported. We could use the monad instance
-- instead, though it'd be far more circuitous.
--     arc k Nothing  t ≡ singleton k () >> t
--     arc k (Just v) t ≡ singleton k v  >>= unionR t . singleton S.empty
--         (...except 'arc' doesn't do the invariant correction
--           of (>>=) for epsilon`elem`t)
--
-- | Return the subtrie containing all keys beginning with a prefix.
submap :: ByteString -> Trie a -> Trie a
{-# INLINE submap #-}
submap q
    | S.null q  = id
    | otherwise = lookupBy_ (Arc q . Just) (prepend q) empty q

{-
-- TODO: would it be worth it to define this specialization?  The
-- definition is nothing special; but inlining away the first three
-- arguments to 'lookupBy_' does allow to avoid any sort of dynamic
-- dispatch or closures.
lookup :: ByteString -> Trie a -> Maybe a
lookup = start
    where
    -- | Deal with epsilon query (when there is no epsilon value)
    start q t@(Branch{}) | S.null q = Nothing
    start q t                       = go q t
    -- | The main recursion
    go _    Empty       = Nothing
    go q   (Arc k mv t) =
        let (_,k',q')   = breakMaximalPrefix k q
        in case (S.null k', S.null q') of
                (False, _)     -> Nothing
                (True,  True)  -> mv
                (True,  False) -> go q' t
    go q t_@(Branch{}) = findArc t_
        where
        qh = errorLogHead "lookup" q
        -- | /O(min(m,W))/, where /m/ is number of @Arc@s in this
        -- branching, and /W/ is the word size of the Prefix,Mask type.
        findArc (Branch p m l r)
            | nomatch qh p m  = Nothing
            | zero qh m       = findArc l
            | otherwise       = findArc r
        findArc t@(Arc{})     = go q t
        findArc Empty         = impossible "lookup" -- see [Note1]
-}


-- TODO: would it be worth it to have a variant like 'lookupBy_'
-- which takes the three continuations?


-- TODO: is this really beneficial over simply taking the last match
-- from 'matches_'?  Benchmark how much this actually saves.
--
-- | Given a query, find the longest prefix with an associated value
-- in the trie, returning the length of that prefix and the associated
-- value.
--
-- This function may not have the most useful return type. For a
-- version that returns the prefix itself as well as the remaining
-- string, see 'Data.Trie.match'.
--
-- @since 0.2.4
match_ :: Trie a -> ByteString -> Maybe (Int, a)
match_ = flip start
    where
    -- | Deal with epsilon query (when there is no epsilon value)
    start q (Branch{}) | S.null q = Nothing
    start q t                     = match1 0 q t
        -- TODO: for the non-null Branch case, maybe we should jump directly to 'findArc' (i.e., inline that case of 'match1')
    -- | Find the first match, or return Nothing if there isn't one.
    match1 _ _ Empty        = Nothing
    match1 n q (Arc k mv t) =
        let (p,k',q') = breakMaximalPrefix k q
            !n'       = n + S.length p
        in case (S.null k', S.null q') of
            (False, _)    -> Nothing
            (True, True)  -> (,) n' <$> mv
            (True, False) ->
                case mv of
                Nothing -> match1      n' q' t
                Just v  -> matchN n' v n' q' t
    match1 n q t_@(Branch{}) = findArc t_
        where
        qh = errorLogHead "match_" q
        -- | /O(min(m,W))/, where /m/ is number of @Arc@s in this
        -- branching, and /W/ is the word size of the Prefix,Mask type.
        findArc (Branch p m l r)
            | nomatch qh p m  = Nothing
            | zero qh m       = findArc l
            | otherwise       = findArc r
        findArc t@(Arc{})     = match1 n q t
        findArc Empty         = impossible "match_" -- see [Note1]
    -- | Find the next match, or return the previous one if there are no more.
    matchN n0 v0 _ _ Empty        = Just (n0,v0)
    matchN n0 v0 n q (Arc k mv t) =
        let (p,k',q') = breakMaximalPrefix k q
            !n'       = n + S.length p
        in case (S.null k', S.null q') of
            (False, _)   -> Just (n0,v0)
            (True, True) ->
                case mv of
                Nothing -> Just (n0,v0)
                Just v  -> Just (n',v)
            (True, False) ->
                case mv of
                Nothing -> matchN n0 v0 n' q' t
                Just v  -> matchN n' v  n' q' t
    matchN n0 v0 n q t_@(Branch{}) = findArc t_
        where
        qh = errorLogHead "match_" q
        -- | /O(min(m,W))/, where /m/ is number of @Arc@s in this
        -- branching, and /W/ is the word size of the Prefix,Mask type.
        findArc (Branch p m l r)
            | nomatch qh p m  = Just (n0,v0)
            | zero qh m       = findArc l
            | otherwise       = findArc r
        findArc t@(Arc{})     = matchN n0 v0 n q t
        findArc Empty         = impossible "match_" -- see [Note1]


-- | Given a query, find all prefixes with associated values in the
-- trie, and return the length of each prefix with their value, in
-- order from shortest prefix to longest.  This function is a good
-- producer for list fusion.
--
-- This function may not have the most useful return type. For a
-- version that returns the prefix itself as well as the remaining
-- string, see 'Data.Trie.matches'.
--
-- @since 0.2.4
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
                !n'       = n + S.length p
            in if S.null k'
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
            findArc Empty         = impossible "matches_" -- see [Note1]


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
-- If the function returns @(Just v, t)@ and @lookup S.empty t == Just w@,
-- then the @w@ will be overwritten by @v@.
--
-- @since 0.2.3
-- __NOTE__: /Type changed in 0.2.6/
alterBy_
    :: (Maybe a -> Trie a -> (Maybe a, Trie a))
    -> ByteString -> Trie a -> Trie a
alterBy_ f = start
    where
    start q t            | not (S.null q) = go q t
    start _ (Arc k mv s) | S.null k       = epsilon $$ f mv      s
    start _ t                             = epsilon $$ f Nothing t

    -- @go@ is always called with non-null @q@, therefore @nothing@ is too.
    nothing q = arcNN q $$ f Nothing Empty

    go q Empty            = nothing q
    go q t@(Branch p m l r)
        | nomatch qh p m  = branchMerge p t  qh (nothing q)
        | zero qh m       = branch p m (go q l) r
        | otherwise       = branch p m l (go q r)
        where
        qh = errorLogHead "alterBy_" q
    go q t@(Arc k mv s) =
        let (p,k',q') = breakMaximalPrefix k q in
        case (S.null k', S.null q') of
        (False, True)  -> -- add node to middle of Arc
                          arcNN p $$ f Nothing (Arc k' mv s)
        (False, False) ->
            case nothing q' of
            Empty     -> t -- Nothing to add, reuse old Arc
            Branch{}  -> impossible "alterBy_" -- 'arcNN' can't Branch
            l@(Arc{}) ->
                -- Inlined version of @arc_ p@, capturing
                -- the invariant that the 'branchMerge' must be a
                -- Branch (since neither trie argument is Empty).
                (if S.null p then id else Arc p Nothing)
                -- 'arcNN' will always have that the string in @l@
                -- must begin with @q'@, which is non-null here and
                -- therefore @arcPrefix q'@ is equivalent to taking
                -- the 'arcPrefix' of the string in @l@.
                $ (branchMerge (arcPrefix q') l (arcPrefix k') (Arc k' mv s))
        (True, True)  -> arc k $$ f mv s
        (True, False) -> arc k mv (go q' s)


-- TODO: benchmark vs the definition with alterBy/liftM
-- TODO: add a variant that's strict in the function.
--
-- /Since: 0.2.6/ for being exported from "Data.Trie.Internal"
-- rather than "Data.Trie"
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
    go q t@(Arc k mv s) =
        let (_,k',q') = breakMaximalPrefix k q in
        case (S.null k', S.null q') of
        (False, True)  -> t -- don't break Arc inline
        (False, False) -> t -- don't break Arc branching
        (True,  True)  -> Arc k (f <$> mv) s
        (True,  False) -> Arc k mv (go q' s)


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
mergeBy f = start
    where
    -- | Deals with epsilon entries, before recursing into @go@
    start
        t0@(Arc k0 mv0 s0)
        t1@(Arc k1 mv1 s1)
        | S.null k0 && S.null k1 = epsilon (mergeMaybe f mv0 mv1) (go s0 s1)
        | S.null k0              = epsilon mv0 (go s0 t1)
        |              S.null k1 = epsilon mv1 (go t0 s1)
    start
        (Arc k0 mv0@(Just _) s0)
        t1@(Branch{})
        | S.null k0              = Arc k0 mv0 (go s0 t1)
    start
        t0@(Branch{})
        (Arc k1 mv1@(Just _) s1)
        | S.null k1              = Arc k1 mv1 (go t0 s1)
    start t0 t1                  = go t0 t1

    -- | The main recursion
    go Empty t1    = t1
    go t0    Empty = t0
    -- /O(n+m)/ for this part where /n/ and /m/ are sizes of the branchings
    go t0@(Branch p0 m0 l0 r0)
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
    --
    go t0@(Arc k0 mv0 s0)
       t1@(Arc k1 mv1 s1)
        | m' == 0 =
            let (pre,k0',k1') = breakMaximalPrefix k0 k1 in
            if S.null pre
            then error "mergeBy: no mask, but no prefix string"
            else
                let {-# INLINE t0' #-}
                    t0' = Arc k0' mv0 s0
                    {-# INLINE t1' #-}
                    t1' = Arc k1' mv1 s1
                in
                case (S.null k0', S.null k1') of
                (True, True)  -> arcNN pre (mergeMaybe f mv0 mv1) (go s0 s1)
                (True, False) -> arcNN pre mv0 (go s0  t1')
                (False,True)  -> arcNN pre mv1 (go t0' s1)
                (False,False) -> prepend pre   (go t0' t1')
        -- Inlined 'branchMerge'; the two @Arc@s are disjoint.
        | zero p0 m'       = Branch p' m' t0 t1
        | otherwise        = Branch p' m' t1 t0
        where
        p0 = arcPrefix k0
        p1 = arcPrefix k1
        m' = branchMask p0 p1
        p' = mask p0 m'
    go t0@(Arc k0 _ _)
       t1@(Branch p1 m1 l r)
        | nomatch p0 p1 m1 = branchMerge p1 t1  p0 t0
        | zero p0 m1       = branch p1 m1 (go t0 l) r
        | otherwise        = branch p1 m1 l (go t0 r)
        where p0 = arcPrefix k0
    go t0@(Branch p0 m0 l r)
       t1@(Arc k1 _ _)
        | nomatch p1 p0 m0 = branchMerge p0 t0  p1 t1
        | zero p1 m0       = branch p0 m0 (go l t1) r
        | otherwise        = branch p0 m0 l (go r t1)
        where p1 = arcPrefix k1


mergeMaybe :: (a -> a -> Maybe a) -> Maybe a -> Maybe a -> Maybe a
{-# INLINE mergeMaybe #-}
mergeMaybe _ Nothing      Nothing  = Nothing
mergeMaybe _ Nothing mv1@(Just _)  = mv1
mergeMaybe _ mv0@(Just _) Nothing  = mv0
mergeMaybe f (Just v0)   (Just v1) = f v0 v1


-- | Take the intersection of two tries, using a function to resolve
-- collisions.
--
-- @since 0.2.6
intersectBy :: (a -> b -> Maybe c) -> Trie a -> Trie b -> Trie c
intersectBy f = start
    where
    -- | Deals with epsilon entries, before recursing into @go@
    start (Arc k0 mv0 s0) (Arc k1 mv1 s1) | S.null k0 && S.null k1
        = epsilon (intersectMaybe f mv0 mv1)   (go s0 s1)
    start (Arc k0 (Just _) s0) t1 | S.null k0 = go s0 t1
    start t0 (Arc k1 (Just _) s1) | S.null k1 = go t0 s1
    start t0 t1                               = go t0 t1

    -- | The main recursion
    go Empty _    =  Empty
    go _    Empty =  Empty
    go t0@(Branch p0 m0 l0 r0)
       t1@(Branch p1 m1 l1 r1)
        | shorter m0 m1 = isect0
        | shorter m1 m0 = isect1
        | p0 == p1      = branch p0 m0 (go l0 l1) (go r0 r1)
        | otherwise     = Empty
        where
        isect0  | nomatch p1 p0 m0  = Empty
                | zero p1 m0        = go l0 t1
                | otherwise         = go r0 t1
        isect1  | nomatch p0 p1 m1  = Empty
                | zero p0 m1        = go t0 l1
                | otherwise         = go t0 r1
    go (Arc k0 mv0 s0)
       (Arc k1 mv1 s1)
        -- We can simplify 'branchMask' to 'xor' here, avoiding the
        -- cost of the 'highestBitMask'; because we don't care about
        -- the actual mask itself, just the nonzero-ness.
        | xor (arcPrefix k0) (arcPrefix k1) /= 0 = Empty
        | otherwise =
            let (pre,k0',k1') = breakMaximalPrefix k0 k1 in
            if S.null pre
            then error "intersectBy: no mask, but no prefix string"
            else
                let {-# INLINE t0' #-}
                    t0' = Arc k0' mv0 s0
                    {-# INLINE t1' #-}
                    t1' = Arc k1' mv1 s1
                in
                case (S.null k0', S.null k1') of
                (True, True)  -> arcNN pre (intersectMaybe f mv0 mv1) (go s0 s1)
                (True, False) -> prepend pre (go s0  t1')
                (False,True)  -> prepend pre (go t0' s1)
                (False,False) -> prepend pre (go t0' t1')
    go t0@(Arc k0 _ _)
       (Branch p1 m1 l r)
        | nomatch p0 p1 m1 = Empty
        | zero p0 m1       = go t0 l
        | otherwise        = go t0 r
        where p0 = arcPrefix k0
    go (Branch p0 m0 l r)
       t1@(Arc k1 _ _)
        | nomatch p1 p0 m0 = Empty
        | zero p1 m0       = go l t1
        | otherwise        = go r t1
        where p1 = arcPrefix k1


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
--
-- @since 0.2.2
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
--
-- @since 0.2.2
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
--
-- @since 0.2.2
updateMinViewBy :: (ByteString -> a -> Maybe a)
                -> Trie a -> Maybe (ByteString, a, Trie a)
updateMinViewBy f = go S.empty
    where
    go _ Empty              = Nothing
    go q (Arc k (Just v) t) = Just (q',v, arc k (f q' v) t) where q' = q <> k
    go q (Arc k Nothing  t) = mapView (prepend k) (go (q <> k) t)
    go q (Branch p m l r)   = mapView (\l' -> branch p m l' r) (go q l)


-- | Update the 'maxAssoc' and return the old 'maxAssoc'.
--
-- @since 0.2.2
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
