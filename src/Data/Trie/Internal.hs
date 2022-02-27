-- Not using -Wcompat, because it wants outdated things for GHC 8.0/8.2
{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
-- Note: can also pass show-extensions to get Haddock to show
-- everything implicit/entailed by the things we actually request.
-- Apparently we're getting: MonoLocalBinds, KindSignatures,
-- ExplicitNamespaces; regardless of whether we set the default to Haskell98 or Haskell2010
{-# OPTIONS_HADDOCK not-home #-}

{-# LANGUAGE NoImplicitPrelude, CPP, BangPatterns #-}
#if __GLASGOW_HASKELL__ >= 708
-- For defining the 'IsList' instance.
{-# LANGUAGE TypeFamilies #-}
#endif
#if __GLASGOW_HASKELL__ >= 701
-- Alas, "GHC.Exts" isn't considered safe, even though 'build' and
-- 'IsList'(..) surely are.
{-# LANGUAGE Trustworthy #-}
#endif
------------------------------------------------------------
--                                              ~ 2022.02.27
-- |
-- Module      :  Data.Trie.Internal
-- Copyright   :  2008--2022 wren romano
-- License     :  BSD-3-Clause
-- Maintainer  :  wren@cpan.org
-- Stability   :  experimental
-- Portability :  portable (with CPP)
--
-- Internal definition of the 'Trie' data type and generic functions
-- for manipulating them. Almost everything here is re-exported
-- from "Data.Trie", which is the preferred API for users. This
-- module is for developers who need deeper (and less stable) access
-- to the abstract type.
--
-- @since 0.1.3
------------------------------------------------------------

module Data.Trie.Internal
    (
    -- * Data types
      Trie()
    -- BUG: can't seem to put this at the top: it'll gobble up the
    -- following section name and replace it.  (I'm guessing that's
    -- something to do with the section not having any exported
    -- entities?)
    -- *** Performance Warning
    -- $bug25

    -- * Basic functions
    , empty, null, singleton, size

    -- * List-conversion functions
    , fromList
    , toList, toListBy, elems

    -- * Query functions
    , lookupBy_, submap
    , match_, matches_

    -- * Simple modification
    , alterBy, alterBy_, adjust

    -- * Combining tries
    , wip_unionWith
    , mergeBy, intersectBy

    -- * Priority-queue functions
    , minAssoc, maxAssoc
    , updateMinViewBy, updateMaxViewBy

    -- * Mapping, filtering, folding, and traversing
    -- ** Filterable
    , filter
    , filterMap
    , mapBy
    -- ** Witherable
    , filterA
    , wither
    -- TODO: 'witherBy' (effectful 'mapBy')
    -- ** Contextual filtering\/mapping functions
    , contextualMap
    , contextualMap'
    , contextualFilterMap
    , contextualMapBy
    -- TODO: 'contextualWither'
    -- TODO: 'contextualWitherBy' (effectful 'contextualMapBy')
    -- ** Foldable
    , foldrWithKey, foldrWithKey', foldlWithKey, foldlWithKey'
    , cata_, cata
    -- ** Traverse
    , traverseWithKey

    -- * Internal utility functions
    , showTrie
    , breakMaximalPrefix
    ) where

import Prelude hiding      (null, lookup, filter)

import qualified Data.ByteString as S
import qualified Data.ByteString.Unsafe as SU
import Data.Trie.Internal.ByteString
import Data.Trie.Internal.BitTwiddle
import Data.Trie.Internal.Errors    (impossible)

import Data.Binary         (Binary(..), Get, Word8)
import Data.Bits           (xor)

#if MIN_VERSION_base(4,9,0)
-- [aka GHC 8.0.1]: "Data.Semigroup" added to base.
--
-- Note: [base-4.13.0 / GHC 8.8.1] has Prelude re-export 'Semigroup'
-- (the class name) and 'Data.Semigroup.<>'; however it does not
-- re-export 'stimes' nor (I assume) 'sconcat'!
import Data.Semigroup      (Semigroup(..))
#endif
#if !(MIN_VERSION_base(4,8,0))
-- [aka GHC 7.10.1]: Prelude re-exports 'Monoid'.
import Data.Monoid         (Monoid(..))
#endif

import Control.DeepSeq     (NFData(rnf))
import Control.Monad       (liftM3, liftM4)

import qualified Data.Foldable as F
-- [base-4.10.0.0 / GHC 8.2.1] moved 'liftA2' into the 'Applicative'
-- class for performance reasons; so we want to use it wherever
-- possible, for those same performance reasons.  However, while
-- the Prelude re-exports all the other methods of 'Applicative'
-- since base-4.8, it does not re-export 'liftA2' (at least not up
-- through base-4.16.0.0 / GHC 9.2.1).
import Control.Applicative (liftA2)
#if MIN_VERSION_base(4,8,0)
-- [aka GHC 7.10.1]: Prelude re-exports 'Applicative', @(<$>)@,
-- 'Foldable', and 'Traversable'. But not 'liftA2'.
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
-- $bug25
-- #bug25#
-- Many (but not all) functions which need to reconstruct bytestrings
-- suffer from an asymptotic slowdown; see:
-- <https://github.com/wrengr/bytestring-trie/issues/25 Bug #25>.
-- For clarity, all functions affected by this bug will have a link
-- to this section.  This is not a new bug, it has affected all
-- prior versions of this library as well.  However, compared to
-- older versions, /bytestring-trie-0.2.7/ mitigates the severity
-- of the bug, and in certain cases to avoids it entirely.
--
-- In particular, this affects the \"keyed\" variants of functions
-- (for folding, traversing, filtering, etc), and anything built
-- from them, including 'toListBy' and various instances which use
-- it.
--
-- Conversely, functions which are unaffected include: those like
-- 'alterBy' which merely pass the query back to the user as a
-- convenience; those which only need to reconstruct a single
-- bytestring (e.g., the priority-queue functions); and
-- 'Data.Trie.matches'\/'matches_'.


------------------------------------------------------------
-- | Infix variant of 'uncurry'.  Currently only used in 'alterBy_'.
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

    newtype NonEmptyBS = NonEmptyBS ByteString  -- Invariant: never null.
    newtype NullableBS = NullableBS Bytestring  -- May be null.

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
Alas, this complicates the invariants about non-null strings.

    data Node a = Node (Maybe a) (ArcSet a)
                    -- Invariant: if Nothing then must be Branch
    data Trie a = Empty
                | Arc    ByteString (Node a)
                    -- Invariant: null string only allowed if both
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
                            !(Trie a) -- Must not be @Empty@.
                            !(Trie a) -- Must not be @Empty@.
    | Arc    {-# UNPACK #-} !ByteString -- Has nontrivial invariants.
                            !(Maybe a)
                            !(Trie a) -- Has complex invariants.
    | Empty
    deriving Eq
    -- Prefix/Mask should be deterministic regardless of insertion order
    -- TODO: prove this is so.


{-----------------------------------------------------------
-- Smart constructors
-----------------------------------------------------------}

{-
-- | A common precondition for ensuring the safety of the following
-- smart constructors.
ifJustThenNoEpsilon :: Maybe a -> Trie a -> Bool
ifJustThenNoEpsilon (Just _) (Arc k (Just _) _) = not (S.null k)
ifJustThenNoEpsilon _ _ = True
-}

-- FIXME: [bug26] <https://github.com/wrengr/bytestring-trie/issues/26>
-- We need to adjust 'arc', 'prepend', 'arcNN', and 'prependNN' to behave
-- more like a zipper, to avoid asymptotic slowdown in corner cases.

-- | Smart constructor to prune @Arc@s that lead nowhere.
--
-- __Preconditions__
-- * @arc _ mv t | ifJustThenNoEpsilon mv t@
arc :: ByteString -> Maybe a -> Trie a -> Trie a
{-# INLINE arc #-}
arc !k mv@(Just _) = Arc k mv
arc  k    Nothing  = prepend k

-- | Variant of 'arc' where the string is known to be non-null.
--
-- __Preconditions__
-- * @arcNN k _  _ | not (S.null k)@
-- * @arcNN _ mv t | ifJustThenNoEpsilon mv t@
arcNN :: ByteString -> Maybe a -> Trie a -> Trie a
{-# INLINE arcNN #-}
arcNN !k mv@(Just _) = Arc k mv
arcNN  k    Nothing  = prependNN k

-- | Prepend a possibly-null string to a trie.
--
-- This function is only very rarely needed; most of the time you
-- already know that the string is non-null, and thus you can call
-- 'prependNN' directly.
--
-- TODO: may actually consider exporting this one, since it could
-- be generally useful and it has no preconditions.  Of course,
-- it's susceptible to [bug25][bug26] if used incorrectly...
prepend :: ByteString -> Trie a -> Trie a
{-# INLINE prepend #-}
prepend k
    | S.null k  = id
    | otherwise = prependNN k

-- | Prepend a non-null string to a trie.
--
-- __Preconditions__
-- * @prependNN k _ | not (S.null k)@
prependNN :: ByteString -> Trie a -> Trie a
{-# INLINE prependNN #-}
prependNN !_ t@Empty      = t
prependNN  q t@(Branch{}) = Arc q Nothing t
prependNN  q (Arc k mv s) = Arc (S.append q k) mv s
    -- TODO: see [bug26]; should ensure that callers do not nest
    -- calls to this function which all take this @Arc@ case.

-- | > mayEpsilon mv ≡ arc S.empty mv
--
-- __Preconditions__
-- * @mayEpsilon mv t | ifJustThenNoEpsilon mv t@
mayEpsilon :: Maybe a -> Trie a -> Trie a
{-# INLINE mayEpsilon #-}
mayEpsilon (Just v) = epsilon v
mayEpsilon Nothing  = id

-- | Canonical name for the empty arc at the top of the trie.
--
-- > epsilon ≡ Arc S.empty . Just
--
-- __Preconditions__
-- * The trie argument must not already have an epsilon value.
epsilon :: a -> Trie a -> Trie a
{-# INLINE epsilon #-}
epsilon = Arc S.empty . Just


-- | Smart @Branch@ constructor: prunes @Empty@ on both sides.
-- This function does no other work besides pruning, so the @Prefix@,
-- @Mask@, and ordering of the 'Trie's must all be as if calling
-- the @Branch@ constructor directly.
branch :: Prefix -> Mask -> Trie a -> Trie a -> Trie a
{-# INLINE branch #-}
branch !_ !_ Empty r     = r
branch  _  _ l     Empty = l
branch  p  m l     r     = Branch p m l r

-- | Smart @Branch@ constructor: prunes @Empty@ on left side only.
--
-- __Preconditions__
-- * the right trie is not @Empty@.
branchL :: Prefix -> Mask -> Trie a -> Trie a -> Trie a
{-# INLINE branchL #-}
branchL !_ !_ Empty r = r
branchL  p  m l     r = Branch p m l r

-- | Smart @Branch@ constructor: prunes @Empty@ on right side only.
--
-- __Preconditions__
-- * the left trie is not @Empty@.
branchR :: Prefix -> Mask -> Trie a -> Trie a -> Trie a
{-# INLINE branchR #-}
branchR !_ !_ l Empty = l
branchR  p  m l r     = Branch p m l r

-- | Smart constructor to join two tries into a @Branch@ with maximal
-- prefix sharing, and in the correct left\/right order.  Requires
-- knowing the prefixes, but can combine either @Branch@es or @Arc@s.
--
-- __Preconditions__
-- * Both tries must be non-@Empty@.
-- * The two prefixes /must not/ be able to match entirely!
graft :: Prefix -> Trie a -> Prefix -> Trie a -> Trie a
{-# INLINE graft #-}
graft p0 t0 p1 t1
    | zero p0 m = Branch p m t0 t1
    | otherwise = Branch p m t1 t0
    where
    m = getMask p0 p1
    p = applyMask p0 m

-- | Shorthand for prepending a non-null string to a 'graft'.  This
-- is mainly useful for when @(p,k0,k1)@ came from 'breakMaximalPrefix'.
--
-- __Preconditions__
-- * The prefix must be non-null.
-- * Each trie must agree with their key (hence must also be non-@Empty@).
-- * The keys must not have matching prefixes.
wye :: ByteString
    -> ByteString -> Trie a
    -> ByteString -> Trie a
    -> Trie a
wye p k0 t0 k1 t1 =
    Arc p Nothing $ graft (arcPrefix k0) t0 (arcPrefix k1) t1

-- TODO: this is only used by 'mergeBy' (since 'intersectBy' returns
-- @Empty@ in lieu of @Branch@ for the latter cases); so maybe we
-- should move this to be a local definition there?
--
-- | Smart constructor to join two @Arc@s into a @Branch@ when possible,
-- and to 'breakMaximalPrefix' otherwise.
--
-- __Preconditions__
-- * Both tries must be non-@Empty@.
arcMerge
    :: ByteString -> Trie a
    -> ByteString -> Trie a
    -> (ByteString -> ByteString -> ByteString -> Trie a)
    -> Trie a
{-# INLINE arcMerge #-}
arcMerge k0 t0 k1 t1 whenMatch
    | m == 0 =
        case breakMaximalPrefix k0 k1 of
        (pre, k0', k1')
            -- TODO: change this into an 'assert' instead.
            | S.null pre -> impossible "arcMerge" -- perfect 'arcPrefix' match, yet no 'breakMaximalPrefix' prefix.
            | otherwise  -> whenMatch pre k0' k1'
    | zero p0 m = Branch p m t0 t1
    | otherwise = Branch p m t1 t0
    where
    p0 = arcPrefix k0
    p1 = arcPrefix k1
    m  = getMask p0 p1
    p  = applyMask p0 m

-- It would be better if arcs used
-- 'Data.ByteString.TrieInternal.wordHead' somehow, that way
-- we can see 4/8/?*Word8 at a time instead of just one.
-- But that makes maintaining invariants ...difficult :(

-- | Get the equivalent of the @Prefix@ stored in a @Branch@, but
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
-- (2021.12.31): remove the definition of @(/=)@ for:
-- <https://github.com/haskell/core-libraries-committee/issues/3>
--
-- 'IntMap' defines their own instance so as to check the Mask
-- before the Prefix; and they have done so since at least version
-- 0.5.0.0 (2011).  So I assume the performance benefits of doing
-- that are good enough to be worth it; thus, we'll do the same.
--
-- TODO: benchmark!!
instance Eq a => Eq (Trie a) where
    (==) = equal

-- TODO: mark this INLINABLE to specialize on the Eq instance?  Why doesn't IntMap?
-- TODO: Alternatively, why doesn't IntMap simply reuse the 'liftEq' implementation?
equal :: Eq a => Trie a -> Trie a -> Bool
equal (Branch p0 m0 l0 r0)
      (Branch p1 m1 l1 r1) = m0 == m1 && p0 == p1 && equal l0 l1 && equal r0 r1
equal (Arc k0 mv0 t0)
      (Arc k1 mv1 t1)      = k0 == k1 && mv0 == mv1 && equal t0 t1
equal Empty Empty          = True
equal _     _              = False
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

-- |
-- __Warning__: This instance suffers unnecessarily from
-- <Data-Trie-Internal.html#bug25 Bug #25>.
--
-- @since 0.2.7
instance Ord a => Ord (Trie a) where
    compare t0 t1 = compare (toList t0) (toList t1)

#if MIN_VERSION_base(4,9,0)
-- |
-- __Warning__: This instance suffers unnecessarily from
-- <Data-Trie-Internal.html#bug25 Bug #25>.
--
-- @since 0.2.7
instance Ord1 Trie where
    liftCompare cmp t0 t1 =
        liftCompare (liftCompare cmp) (toList t0) (toList t1)
#endif

{-----------------------------------------------------------
-- Instances: Show, Show1
-----------------------------------------------------------}

-- This instance does not unveil the innards of our abstract type.
-- It doesn't emit truly proper Haskell code though, since ByteStrings
-- are printed as (ASCII) Strings, but that's not our fault.
--
-- |
-- __Warning__: This instance suffers <Data-Trie-Internal.html#bug25 Bug #25>.
--
-- @since 0.2.2
instance (Show a) => Show (Trie a) where
    showsPrec p t = showParen (p > 10)
                  $ ("fromList " ++) . shows (toList t)

#if MIN_VERSION_base(4,9,0)
-- |
-- __Warning__: This instance suffers <Data-Trie-Internal.html#bug25 Bug #25>.
--
-- @since 0.2.7
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

{-
-- TODO: consider an instance more like the new one for Data.Map
-- (also for Data.IntMap), that is:
instance (Binary a) => Binary (Set.Set a) where
    put s = put (size s) <> mapM_ put (toAscList s)
    get   = liftM fromDistinctAscList get
-- It would require redoing all the work to split bytestrings, and
-- have the overhead from storing duplicated prefixes, but is forward
-- compatible to whatever representation changes, and doesn't have
-- the invariants problem.
-- BUG: However, that would suffer from
-- <https://github.com/wrengr/bytestring-trie/issues/25>, because
-- 'toList'\/@toAscList@ does.
-}
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

-- TODO: consider adding _cereal_:'Serialize' instance. (Though that adds dependencies on: array, bytestring-builder, fail, ghc-prim). THe instances for Map/IntMap are similar to the commented ones above, though the getter uses 'fromList' rather than assuming distinct or ascending.

-- TODO: potentially consider <https://github.com/mgsloan/store#readme> as well, though probably not since that has a ton of dependencies.

-- TODO: also consider 'Storable', though that seems less appropriate for data structures than for structs and scalars...

{-----------------------------------------------------------
-- Instances: NFData
-----------------------------------------------------------}

-- | @since 0.2.7
instance NFData a => NFData (Trie a) where
    rnf Empty            = ()
    rnf (Arc _ mv t)     = rnf mv `seq` rnf t
    rnf (Branch _ _ l r) = rnf l `seq` rnf r

{-
-- TODO: do we want/need these?
#if __GLASGOW_HASKELL__
instance Data.Data.Data (Trie a) where ...
-- See 'IntMap' for how to do this without sacrificing abstraction.
#endif

-- I think this macro is defined in "containers.h"?
INSTANCE_TYPEABLE(Trie)

-- What about deriving Generic?
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

-- TODO: strict version of fmap. Is there a canonical name\/class for this yet?

{-----------------------------------------------------------
-- Instances: Traversable, Applicative, Monad
-----------------------------------------------------------}

instance Traversable Trie where
    traverse f = go
        where
        go Empty              = pure   Empty
        go (Arc k Nothing  t) = fmap   (Arc k Nothing)      (go t)
        go (Arc k (Just v) t) = liftA2 (Arc k . Just) (f v) (go t)
        go (Branch p m l r)   = liftA2 (Branch p m) (go l) (go r)

-- | Keyed version of 'traverse'.
--
-- __Warning__: This function suffers <Data-Trie-Internal.html#bug25 Bug #25>.
--
-- @since 0.2.7
traverseWithKey
    :: Applicative f => (ByteString -> a -> f b) -> Trie a -> f (Trie b)
{-# INLINE traverseWithKey #-}
traverseWithKey f = go Nil
    where
    -- See [Note:LazyRLBS].
    go _ Empty              = pure   Empty
    go q (Branch p m l r)   = liftA2 (Branch p m) (go q l) (go q r)
    go q (Arc k Nothing  t) = fmap   (Arc k Nothing) (go (q +>! k) t)
    go q (Arc k (Just v) t) =
        let q' = toStrict (q +>? k)
        in liftA2 (Arc k . Just) (f q' v) (go (fromStrict q') t)

-- [Note:LazyRLBS]: We avoid making the RLBS parameter strict, to
-- avoid incuring the cost of 'toStrict' if the user's function
-- does not force it.  However, if they do force it, then we'll
-- still have the <https://github.com/wrengr/bytestring-trie/issues/25>
-- problem.  Using RLBS only reduces the constant factor of the
-- quadratic.

------------------------------------------------------------
-- TODO: would make more sense to use intersection\/zip semantics here,
-- rather than the overlaid-unionL semantics of the 'Monad'.  Alas,
-- done is done.  In a future major version we can try changing
-- that, and introducing newtype wrappers for this overlaid\/unionL
-- version (and the prospective underlaid\/unionR version).
-- TODO: see also <https://hackage.haskell.org/package/semialign>
--
-- | @since 0.2.2
instance Applicative Trie where
    pure      = singleton S.empty
    t0 <*> t1 = t0 >>= (<$> t1)
    -- TODO: can we do better than these defaults?
    -- t0 *> t1       = (id    <$  t0) <*> t1
    -- t0 <* t1       = (const <$> t0) <*> t1 -- actually uses @liftA2 const t0 t1@
#if MIN_VERSION_base(4,10,0)
    -- liftA2 f t0 t1 = (f     <$> t0) <*> t1
#endif
    {-
    -- Inlining and case-of-case yields the following (which GHC
    -- could surely derive on its own):
    Empty            *> _  = Empty
    Branch p m l r   *> t1 = branch p m (l *> t1) (r *> t1)
    Arc k Nothing  s *> t1 = prependNN k            (s *> t1)
    Arc k (Just _) s *> t1 = prepend k (t1 `unionL` (s *> t1))

    -- This one is marginally better, since we can use @(<$)@ in the Accept case.
    Empty            <* _  = Empty
    Branch p m l r   <* t1 = branch p m (l <* t1) (r <* t1)
    Arc k Nothing  s <* t1 = prependNN k                   (s <* t1)
    Arc k (Just v) s <* t1 = prepend k ((v <$ t1) `unionL` (s <* t1))

    -- This one took a lot of inlining\/massaging, so might be worth it...
    -- It's easier to see the structure if we define a closure
    -- @(liftA2 f _ t1)@, but unclear if that would hurt the improvement
    -- of the implementation.
    liftA2 f Empty              _  = Empty
    liftA2 f (Branch p m l r)   t1 = branch p m (liftA2 f l t1) (liftA2 f r t1)
    liftA2 f (Arc k Nothing  s) t1 = prependNN k (liftA2 f s t1)
    liftA2 f (Arc k (Just v) s) t1 = prepend k ((f v <$> t1) `unionL` liftA2 f s t1)
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
    -- FIXME: See [bug26].
    (>>=) Empty              _ = empty
    (>>=) (Branch p m l r)   f = branch p m (l >>= f) (r >>= f)
    (>>=) (Arc k Nothing  t) f = prependNN k             (t >>= f)
    (>>=) (Arc k (Just v) t) f = prepend k (f v `unionL` (t >>= f))
                               where unionL = mergeBy (\x _ -> Just x)


{-----------------------------------------------------------
-- Instances: Semigroup, Monoid
-----------------------------------------------------------}

#if MIN_VERSION_base(4,9,0)
-- The "Data.Semigroup" module is in base since 4.9.0.0; but having
-- the 'Semigroup' superclass for the 'Monoid' instance only comes
-- into force in base 4.11.0.0.
-- | @since 0.2.5
instance (Semigroup a) => Semigroup (Trie a) where
    (<>) = mergeBy $ \x y -> Just (x <> y)
    -- Non-default definition since 0.2.7
    stimes = fmap . stimes
#endif


-- This instance is more sensible than Data.IntMap and Data.Map's
instance (Monoid a) => Monoid (Trie a) where
    mempty = empty
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
-- Extra mapping and filtering functions
-----------------------------------------------------------}

{-----------------------------------------------------------
-- Pseudo-instances: Filterable, Witherable
-----------------------------------------------------------}

-- We avoid depending on the _filterable_ package because it combines
-- too many things into its @Filterable@ class.  And we avoid using
-- the _witherable_ package because it has too many dependencies,
-- and too many orphan instances.  However, we go with the names
-- (mostly[1]) and laws as phrased by _witherable_.
--
-- [1]: I'm rather not a fan of @mapMaybe@, despite its pervasiveness.
-- And similarly for @catMaybes@ etc.  That's actually one of the
-- reasons I prefer the _witherable_ package over _filterable_:
-- because of the name 'wither' instead of @mapMaybeA@ :)


-- | Apply a function to all values, potentially removing them.
--
-- ==== __Laws__
-- [/Fission/]
--   @'filterMap' f ≡ 'fmap' ('Data.Maybe.fromJust' . f) . 'filter' ('Data.Maybe.isJust' . f)@
--
-- [/Fusion/]
--   @'fmap' f . 'filter' g ≡ 'filterMap' (\\v -> f v '<$' 'Control.Monad.guard' (g v))@
--
-- [/Conservation/]
--   @'filterMap' ('Just' . f) ≡ 'fmap' f@
--
-- [/Composition/]
--   @'filterMap' f . 'filterMap' g ≡ 'filterMap' (f 'Control.Monad.<=<' g)@
--
-- The fission\/fusion laws are essentially the same, they differ
-- only in which direction is more \"natural\" for use as a rewrite
-- rule.  The conservation law is just a special case of fusion,
-- but it's a particularly helpful one to take note of.
--
filterMap :: (a -> Maybe b) -> Trie a -> Trie b
filterMap f = start
    where
    -- Handle epsilon values before entering the main recursion.
    start (Arc k (Just v) t) = arc k (f v) (go t)
    start t                  = go t
    -- FIXME: See [bug26].
    go Empty              = empty
    go (Arc k Nothing  t) = prependNN k   (go t)
    go (Arc k (Just v) t) = arcNN k (f v) (go t)
    go (Branch p m l r)   = branch p m (go l) (go r)
-- TODO: rewrite rule for the latter three laws. (The fission law
-- is unlikely to be very helpful.)
-- TODO: why not implement as @contextualFilterMap (const . f)@ ?
-- Does that actually incur additional overhead?


-- Some translations:
--   @'filter' f ≡ 'filterMap' (\\v -> if f v then 'Just' v else 'Nothing')@
--   @'filter' f ≡ 'filterMap' (('<$') '<*>' 'Control.Monad.guard' . f)@
--   @'filter' f ≡ 'filterMap' (\\v -> v '<$' 'Control.Monad.guard' (f v))@
--
-- | Retain only those values which satisfy some predicate.
--
-- ==== __Laws__
-- [/Definition/]
--   @'filter' f ≡ 'filterMap' (\\v -> v '<$' 'Control.Monad.guard' (f v))@
--
-- [/Composition/]
--   @'filter' f . 'filter' g ≡ 'filter' ('liftA2' ('&&') f g)@
--
-- The definition above is a special case of the fusion law for
-- 'filterMap'.  (Also, the name just means definitional-equality;
-- it's not the actual implementation used.)
--
-- @since 0.2.7
filter :: (a -> Bool) -> Trie a -> Trie a
filter f = start
    where
    -- Handle epsilon values before entering the main recursion.
    start (Arc k (Just v) t) = arcB k v (f v) (go t)
    start t                  = go t
    -- FIXME: See [bug26].
    go Empty              = empty
    go (Arc k Nothing  t) = prependNN k      (go t)
    go (Arc k (Just v) t) = arcNNB k v (f v) (go t)
    go (Branch p m l r)   = branch p m (go l) (go r)

-- | > arcB k v b ≡ arc k (v <$ guard b)
arcB :: ByteString -> a -> Bool -> Trie a -> Trie a
{-# INLINE arcB #-}
arcB k v True  = Arc k (Just v)
arcB k _ False = prepend k

-- | > arcNNB k v b ≡ arcNN k (v <$ guard b)
arcNNB :: ByteString -> a -> Bool -> Trie a -> Trie a
{-# INLINE arcNNB #-}
arcNNB k v True  = Arc k (Just v)
arcNNB k _ False = prependNN k


{-
#if MIN_VERSION_base(4,9,0)
import Data.Functor.Compose (Compose(Compose))
#endif
#if MIN_VERSION_base(4,8,0)
import Data.Functor.Identity (Identity(Identity))
#endif
-}

-- BUG: Is there any other way to get the Haddock to force linebreaks,
-- other than using birdtracks which have a whole different
-- stylization?
-- TODO: is commutative monad sufficient, or are there other
-- requirements too?  If it does in fact hold for commutative monads,
-- then we should state it as a law (like we do for naturality).
--
-- | An effectful version of 'filterMap'.
--
-- ==== __Laws__
-- [/Naturality/]
--   @'wither' (t . f) ≡ t . 'wither' f@,
--   for any /applicative-transformation/ @t@
--
-- [/Purity/]
--   @'wither' ('pure' . f) ≡ 'pure' . 'filterMap' f@
--
-- [/Conservation/]
--   @'wither' ('fmap' 'Just' . f) ≡ 'traverse' f@
--
-- [/Horizontal Composition/]
--   @'wither' f \`under\` 'wither' g ≡ 'wither' (wither_Maybe f \`under\` g)@,
--   where:
--
-- > under :: Functor f
-- >       => (b -> g c)
-- >       -> (a -> f b)
-- >       -> a -> Compose f g c
-- > under g f = Compose . fmap g . f
-- >
-- > -- | Variant of wither for Maybe instead of Trie.
-- > wither_Maybe :: Applicative f
-- >              => (a -> f (Maybe b))
-- >              -> Maybe a -> f (Maybe b)
-- > wither_Maybe f = fmap join . traverse f
--
-- Note that the horizontal composition law is using two different
-- applicative functors.  Conversely, a vertical composition law
-- would have the form: @'wither' f 'Control.Monad.<=<' 'wither' g ≡ ...@;
-- however, we cannot have such a law except when the applicative
-- functor is in fact a commutative monad (i.e., the order of effects
-- doesn't matter).  For the curious, the terminology of
-- <https://ncatlab.org/nlab/show/horizontal+composition \"horizontal\" composition> vs
-- <https://ncatlab.org/nlab/show/vertical+composition \"vertical\" composition>
-- comes from category theory.
--
-- Although the horizontal composition law may look baroque, it is
-- helpful to compare it to the composition law for 'traverse'
-- itself:
--
-- @'traverse' f \`under\` 'traverse' g ≡ 'traverse' (f \`under\` g)@
--
-- @since 0.2.7
wither :: Applicative f => (a -> f (Maybe b)) -> Trie a -> f (Trie b)
wither f = start
    where
    -- Handle epsilon values before entering the main recursion.
    start (Arc k (Just v) t) = liftA2 (arc k) (f v) (go t)
    start t                  = go t
    -- FIXME: See [bug26].
    go Empty              = pure   empty
    go (Arc k Nothing  t) = fmap   (prependNN k)   (go t)
    go (Arc k (Just v) t) = liftA2 (arcNN k) (f v) (go t)
    go (Branch p m l r)   = liftA2 (branch p m) (go l) (go r)

-- Some other spellings of the translation:
--   @'filterA' f ≡ 'wither' (\\v -> (\\b -> if b then 'Just' v else 'Nothing') '<$>' f v)@
--   @'filterA' f ≡ 'wither' (\\v -> (\\b -> v '<$' 'Control.Monad.guard' b) '<$>' f v)@
--   @'filterA' f ≡ 'wither' ('fmap' . (. 'Control.Monad.guard') . ('<$') '<*>' f)@
--
-- An alternative variant with a more straightforward derivation
-- for the definition, but a bizarre type (since the layering of
-- effects doesn't match the order of arguments).  Though re the
-- /name/, this one makes more sense than the other one.
-- > underA2 :: (Applicative f, Applicative g)
-- >         => (b -> c -> d)
-- >         -> (a -> g b)
-- >         -> (a -> f c)
-- >         -> a -> Compose f g d
-- > underA2 h g f = liftA2 (liftA2 h) (g `under` pure) (pure `under` f)
--
--
-- | An effectful version of 'filter'.
--
-- ==== __Laws__
-- [/Definition/]
--   @'filterA' f ≡ 'wither' (\\v -> (v '<$') . 'Control.Monad.guard' '<$>' f v)@
--
-- [/Naturality/]
--   @'filterA' (t . f) ≡ t . 'filterA' f@,
--   for any /applicative-transformation/ @t@
--
-- [/Purity/]
--   @'filterA' ('pure' . f) ≡ 'pure' . 'filter' f@
--
-- [/Horizontal Composition/]
--   @'filterA' f \`under\` 'filterA' g ≡ 'filterA' (underF2 ('&&') f g)@,
--   where
--
-- > -- Like 'liftA2' for the @(a->)@ monad, but horizontal.
-- > underF2 :: (Functor f, Functor g)
-- >         => (b -> c -> d)
-- >         -> (a -> f b)
-- >         -> (a -> g c)
-- >         -> a -> Compose f g d
-- > underF2 h f g a = Compose (f a <&> ((g a <&>) . h))
--
-- For the definition of @under@ and more details about horizontal
-- composition, see the laws section of 'wither'.
--
-- @since 0.2.7
filterA :: Applicative f => (a -> f Bool) -> Trie a -> f (Trie a)
filterA f = start
    where
    -- Handle epsilon values before entering the main recursion.
    start (Arc k (Just v) t) = liftA2 (arcB k v) (f v) (go t)
    start t                  = go t
    -- FIXME: See [bug26].
    go Empty              = pure   empty
    go (Arc k Nothing  t) = fmap   (prependNN k)      (go t)
    go (Arc k (Just v) t) = liftA2 (arcNNB k v) (f v) (go t)
    go (Branch p m l r)   = liftA2 (branch p m) (go l) (go r)


-- | Keyed version of 'filterMap'.
--
-- __Warning__: This function suffers <Data-Trie-Internal.html#bug25 Bug #25>.
mapBy :: (ByteString -> a -> Maybe b) -> Trie a -> Trie b
-- TODO: why not implement as @contextualMapBy (\k v _ -> f k v)@ ?
-- Does that actually incur additional overhead?
mapBy f = start
    where
    -- Handle epsilon values before entering the main recursion.
    start (Arc k (Just v) t) = arc k (f k v) (go (fromStrict k) t)
    start t                  = go Nil t
    -- FIXME: See [bug26].
    -- See [Note:LazyRLBS].
    go _ Empty              = empty
    go q (Branch p m l r)   = branch p m (go q l) (go q r)
    go q (Arc k Nothing  t) = prependNN k (go (q +>! k) t)
    go q (Arc k (Just v) t) = arcNN k (f q' v) (go (fromStrict q') t)
                            where q' = toStrict (q +>? k)


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


-- | A variant of 'contextualMap' which evaluates the function strictly.
--
-- @since 0.2.3
contextualMap' :: (a -> Trie a -> b) -> Trie a -> Trie b
contextualMap' f = go
    where
    go Empty              = Empty
    go (Arc k Nothing  t) = Arc k Nothing         (go t)
    go (Arc k (Just v) t) = Arc k (Just $! f v t) (go t)
    go (Branch p m l r)   = Branch p m (go l) (go r)


-- | Contextual variant of 'filterMap'.
--
-- @since 0.2.3
contextualFilterMap :: (a -> Trie a -> Maybe b) -> Trie a -> Trie b
contextualFilterMap f = start
    where
    -- Handle epsilon values before entering the main recursion.
    start (Arc k (Just v) t) = arc k (f v t) (go t)
    start t                  = go t
    -- FIXME: See [bug26].
    go Empty              = empty
    go (Arc k Nothing  t) = prependNN k     (go t)
    go (Arc k (Just v) t) = arcNN k (f v t) (go t)
    go (Branch p m l r)   = branch p m (go l) (go r)


-- | Contextual variant of 'mapBy', aka keyed variant of 'contextualFilterMap'.
--
-- __Warning__: This function suffers <Data-Trie-Internal.html#bug25 Bug #25>.
--
-- @since 0.2.3
contextualMapBy :: (ByteString -> a -> Trie a -> Maybe b) -> Trie a -> Trie b
contextualMapBy f = start
    where
    -- Handle epsilon values before entering the main recursion.
    start (Arc k (Just v) t) = arc k (f k v t) (go (fromStrict k) t)
    start t                  = go Nil t
    -- FIXME: See [bug26].
    -- See [Note:LazyRLBS].
    go _ Empty              = empty
    go q (Branch p m l r)   = branch p m (go q l) (go q r)
    go q (Arc k Nothing  t) = prependNN k (go (q +>! k) t)
    go q (Arc k (Just v) t) = arcNN k (f q' v t) (go (fromStrict q') t)
                            where q' = toStrict (q +>? k)


{-----------------------------------------------------------
-- Basic functions
-----------------------------------------------------------}
-- TODO: probably want to hoist this up top

-- | \(\mathcal{O}(1)\). Construct the empty trie.
empty :: Trie a
{-# INLINE empty #-}
empty = Empty


-- | \(\mathcal{O}(1)\). Is the trie empty?
null :: Trie a -> Bool
{-# INLINE null #-}
null Empty = True
null _     = False


-- | \(\mathcal{O}(1)\). Construct a singleton trie.
singleton :: ByteString -> a -> Trie a
{-# INLINE singleton #-}
singleton k v = Arc k (Just v) Empty
-- For singletons, don't need to verify invariant on arc length >0


-- | \(\mathcal{O}(n)\). Get count of elements in trie.
size  :: Trie a -> Int
{-# INLINE size #-}
size t = size' t id 0

-- | \(\mathcal{O}(n)\). CPS accumulator helper for calculating 'size'.
size' :: Trie a -> (Int -> Int) -> Int -> Int
size' Empty              f !n = f n
size' (Branch _ _ l r)   f  n = size' l (size' r f) n
size' (Arc _ Nothing t)  f  n = size' t f n
size' (Arc _ (Just _) t) f  n = size' t f (n + 1)
    -- TODO: verify we've retained the correct strictness for this last case
    -- TODO: benchmark vs getting rid of the CPS (just leaving the
    -- accumulator); since everywhere else the CPSing introduces a
    -- performance penalty.
    -- TODO: also benchmark vs using @F.foldl' (\n _ -> n+1) 0@;
    -- to see how much specializing on the function actually helps.


{-----------------------------------------------------------
-- Instances: Foldable
-----------------------------------------------------------}

-- [Note:FoldEta]: For all the folding functions, we take only the
-- two algebra arguments on the left of the \"=\", leaving the
-- 'Trie' argument as a lambda on the right of the \"=\".  This is
-- to allow the functions to be inlined when passed only the two
-- algebra arguments, rather than requiring all three arguments
-- before being inlined.

instance Foldable Trie where
    {-# INLINABLE fold #-}
    fold = go
        where
        go Empty              = mempty
        go (Arc _ Nothing  t) = go t
        go (Arc _ (Just v) t) = v `mappend` go t
        go (Branch _ _ l r)   = go l `mappend` go r
    {-# INLINE foldMap #-}
    foldMap f = go
        where
        go Empty              = mempty
        go (Arc _ Nothing  t) = go t
        go (Arc _ (Just v) t) = f v `mappend` go t
        go (Branch _ _ l r)   = go l `mappend` go r
#if MIN_VERSION_base(4,13,0)
    -- TODO: float out this definition so folks can still use it on earlier versions of base?
    -- TODO: verify order of 'mappend' on some non-commutative monoid!
    {-# INLINE foldMap' #-}
    foldMap' f = go mempty
        where
        -- Benchmarking on GHC 9.2.1 indicates that for this function
        -- the (m,t) argument ordering is somewhat (~3%) faster
        -- than the (t,m) order; and both allocate the same.
        -- This differs from the case for 'foldr'' and 'foldl';
        -- though I'm not sure why.
        -- TODO: Once we disable HPC, now it's looking like the
        -- flopped version is faster afterall...
        go !m Empty              = m
        go  m (Arc _ Nothing  t) = go m t
        go  m (Arc _ (Just v) t) = go (m `mappend` f v) t
        go  m (Branch _ _ l r)   = go (go m l) r
#endif
    --
    -- TODO: This implementation, a variation with eta-expanded
    -- recursion, and the 'Endo'-based default are all about the
    -- same performance.  The eta-expanded version trends towards
    -- being slightly faster, but it costs ~43% more allocation
    -- (larger thunks?).  So should we just leave the default or
    -- eta-expand?
    {-# INLINE foldr #-}
    foldr f z0 = \t -> go t z0 -- See [Note:FoldEta].
        where
        go Empty              = id
        go (Arc _ Nothing  t) =       go t
        go (Arc _ (Just v) t) = f v . go t
        go (Branch _ _ l r)   = go l . go r
#if MIN_VERSION_base(4,6,0)
    -- TODO: float out this definition so folks can still use it on earlier versions of base?
    {-# INLINE foldr' #-}
    foldr' f z0 = \t -> go t z0 -- See [Note:FoldEta].
        where
        -- Benchmarking on GHC 9.2.1 indicates that for this function
        -- the (t,z) argument order is ~10% faster than (z,t);
        -- allocation is the same for both.  Also, weirdly,
        -- benchmarking indicates that the @($!)@ in the Branch
        -- case slightly improved things.
        -- TODO: what's going on with the @($!)@; bogus?
        -- TODO: once HPC disabled, now it's saying the unflopped
        -- version without the extra @($!)@ is the faster one!
        -- (unflopped with @($!)@ is only marginally slower; probably
        -- noise).
        go Empty              !z = z
        go (Arc _ Nothing  t)  z = go t z
        go (Arc _ (Just v) t)  z = f v $! go t z
        go (Branch _ _ l r)    z = go l $! go r z
#endif
    {-# INLINE foldl #-}
    foldl f z0 = \t -> go t z0 -- See [Note:FoldEta].
        where
        -- Benchmarking on GHC 9.2.1 indicates that for this function
        -- the (t,z) argument order is slightly faster (~0.8%) and
        -- allocates ~8.4% less, compared to the (z,t) order.
        -- I've no idea why the allocation would differ, especially
        -- when it doesn't for 'foldr'' and 'foldMap''.
        -- TODO: once HPC disabled, now it's showing the flopped
        -- version is ~2x faster! bogus?
        go Empty              z = z
        go (Arc _ Nothing  t) z = go t z
        go (Arc _ (Just v) t) z = go t (f z v)
        go (Branch _ _ l r)   z = go r (go l z)
#if MIN_VERSION_base(4,6,0)
    -- TODO: float out this definition so folks can still use it on earlier versions of base?
    {-# INLINE foldl' #-}
    foldl' f z0 = go z0 -- See [Note:FoldEta].
        where
        -- Benchmarking on GHC 9.2.1 indicates that for this function
        -- the (z,t) argument order is significantly faster (~10%) and
        -- allocates half as much.
        -- TODO: figure out why\/how the allocation could differ so much; bogus?
        -- TODO: figure out why benchmarking indicates the \"flop_bang\"
        -- version is ~4% faster (albeit ~32% more allocation); bogus?
        -- TODO: once HPC disabled, the flopped version is showing ~2x faster; bogus?
        go !z Empty              = z
        go  z (Arc _ Nothing  t) = go z t
        go  z (Arc _ (Just v) t) = go (f z v) t
        go  z (Branch _ _ l r)   = go (go z l) r
#endif
    -- TODO: any point in doing foldr1,foldl1?
#if MIN_VERSION_base(4,8,0)
    -- TODO: float out this definition so folks can still use it on earlier versions of base?
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
    maximum = go0
        where
        go0   Empty              = error "Data.Foldable.maximum @Trie: empty trie"
        go0   (Arc _ Nothing  t) = go0 t
        go0   (Arc _ (Just v) t) = go v t
        go0   (Branch _ _ l r)   = go (go0 l) r
        go !w Empty              = w
        go  w (Arc _ Nothing  t) = go w t
        go  w (Arc _ (Just v) t) = go (max w v) t
        go  w (Branch _ _ l r)   = go (go w l) r
    {-# INLINABLE minimum #-}
    minimum = go0
        where
        go0   Empty              = error "Data.Foldable.minimum @Trie: empty trie"
        go0   (Arc _ Nothing  t) = go0 t
        go0   (Arc _ (Just v) t) = go v t
        go0   (Branch _ _ l r)   = go (go0 l) r
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
-- Extra folding functions
-----------------------------------------------------------}

-- TODO: be sure to keep this in sync with whatever implementation
-- choice we use for 'F.foldr'; especially since that's the one
-- method of 'Foldable' where we can't improve substantially over
-- the default implementation.
--
-- | Keyed variant of 'F.foldr'.
--
-- __Warning__: This function suffers <Data-Trie-Internal.html#bug25 Bug #25>.
--
-- @since 0.2.2
foldrWithKey :: (ByteString -> a -> b -> b) -> b -> Trie a -> b
{-# INLINE foldrWithKey #-}
foldrWithKey f z0 = \t -> go Nil t z0 -- See [Note:FoldEta].
    where
    -- See [Note:LazyRLBS].
    go _ Empty              = id
    go q (Branch _ _ l r)   = go q l . go q r
    go q (Arc k Nothing  t) =          go (q +>! k) t
    go q (Arc k (Just v) t) = f q' v . go (fromStrict q') t
                            where q' = toStrict (q +>? k)

-- TODO: probably need to benchmark these separately from the
-- non-keyed variants, since the extra recursive argument will
-- surely sway things like whether to flop or not.
-- TODO: Consider just giving an
-- <https://hackage.haskell.org/package/indexed-traversable-0.1.2/docs/Data-Foldable-WithIndex.html>
-- instance, instead of naming all these separately.  That adds a
-- lot of additional dependencies just to define the class, but...
-- Or maybe give an <https://hackage.haskell.org/package/keys-3.12.3/docs/Data-Key.html>
-- instance. Again, lots of added dependencies just for the class,...
-- Then again, maybe we should just stick with doing everything
-- outside of classes; that way we could introduce a Cabal flag for
-- deciding whether the user wants either of those classes (and
-- should do the same for Witherable).

-- | Keyed variant of 'F.foldr''.
--
-- __Warning__: This function suffers <Data-Trie-Internal.html#bug25 Bug #25>.
--
-- @since 0.2.7
foldrWithKey' :: (ByteString -> a -> b -> b) -> b -> Trie a -> b
{-# INLINE foldrWithKey' #-}
foldrWithKey' f z0 = go Nil z0 -- See [Note:FoldEta].
    where
    -- See [Note:LazyRLBS].
    go _ !z Empty              = z
    go q  z (Branch _ _ l r)   = go q (go q z r) l
    go q  z (Arc k Nothing  t) =           go (q +>! k) z t
    go q  z (Arc k (Just v) t) = f q' v $! go (fromStrict q') z t
                                where q' = toStrict (q +>? k)

-- | Keyed variant of 'F.foldl'.
--
-- __Warning__: This function suffers <Data-Trie-Internal.html#bug25 Bug #25>.
--
-- @since 0.2.7
foldlWithKey :: (b -> ByteString -> a -> b) -> b -> Trie a -> b
{-# INLINE foldlWithKey #-}
foldlWithKey f z0 = go Nil z0 -- See [Note:FoldEta].
    where
    -- See [Note:LazyRLBS].
    go _ z Empty              = z
    go q z (Branch _ _ l r)   = go q (go q z l) r
    go q z (Arc k Nothing  t) = go (q +>! k) z t
    go q z (Arc k (Just v) t) = go (fromStrict q') (f z q' v) t
                                where q' = toStrict (q +>? k)

-- | Keyed variant of 'F.foldl''.
--
-- __Warning__: This function suffers <Data-Trie-Internal.html#bug25 Bug #25>.
--
-- @since 0.2.7
foldlWithKey' :: (b -> ByteString -> a -> b) -> b -> Trie a -> b
{-# INLINE foldlWithKey' #-}
foldlWithKey' f z0 = go Nil z0 -- See [Note:FoldEta].
    where
    -- See [Note:LazyRLBS].
    go _ !z Empty              = z
    go q  z (Branch _ _ l r)   = go q (go q z l) r
    go q  z (Arc k Nothing  t) = go (q +>! k) z t
    go q  z (Arc k (Just v) t) = go (fromStrict q') (f z q' v) t
                                where q' = toStrict (q +>? k)

-- | Catamorphism for tries.  Unlike most other functions ('mapBy',
-- 'contextualMapBy', 'foldrWithKey', etc), this function does /not/
-- reconstruct the full 'ByteString' for each value; instead it
-- only returns the suffix since the previous value or branch point.
--
-- This function is a direct\/literal catamorphism of the implementation
-- datatype, erasing only some bitmasking metadata for the branches.
-- For a more semantic catamorphism, see 'cata'.
--
-- @since 0.2.6
cata_
    :: (ByteString -> Maybe a -> b -> b)    -- ^ Algebra for arc.
    -> (b -> b -> b)                        -- ^ Algebra for binary branch.
    -> b                                    -- ^ Algebra for empty trie.
    -> Trie a -> b
{-# INLINE cata_ #-}
cata_ a b e = go
    where
    go Empty            = e
    go (Arc k mv t)     = a k mv (go t)
    go (Branch _ _ l r) = b (go l) (go r)


-- | Catamorphism for tries.  Unlike most other functions ('mapBy',
-- 'contextualMapBy', 'foldrWithKey', etc), this function does /not/
-- reconstruct the full 'ByteString' for each value; instead it
-- only returns the suffix since the previous value or branch point.
--
-- This function is a semantic catamorphism; that is, it tries to
-- express the invariants of the implementation, rather than exposing
-- the literal structure of the implementation.  For a more literal
-- catamorphism, see 'cata_'.
--
-- @since 0.2.6
cata
    :: (ByteString -> a -> b -> b)  -- ^ Algebra for accepting arcs.
    -> (ByteString -> [b] -> b)     -- ^ Algebra for n-ary branch with prefix.
    -> b                            -- ^ Algebra for empty trie.
    -> Trie a -> b
cata a b e = go
    where
    step k (Just v) t           = a k v (go t)
    step k Nothing  t           = b k (collect t [])
    go      Empty               = e
    go      (Arc k mv t)        = step k mv t
    go      (Branch _ _ l r)    = b S.empty (collect l (collect r []))
    -- TODO: would it be profitable to use 'build' for these lists?
    collect Empty            bs = bs
    collect (Arc k mv t)     bs = step k mv t : bs
    collect (Branch _ _ l r) bs = collect l (collect r bs)



{-----------------------------------------------------------
-- Instances: IsList
-----------------------------------------------------------}

#if __GLASGOW_HASKELL__ >= 708
-- |
-- __Warning__: The 'toList' method of this instance suffers
-- <Data-Trie-Internal.html#bug25 Bug #25>.
--
-- @since 0.2.7
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
--
-- __Warning__: This function suffers <Data-Trie-Internal.html#bug25 Bug #25>.
toList :: Trie a -> [(ByteString,a)]
{-# INLINE toList #-}
toList = toListBy (,)


-- cf Data.ByteString.unpack
-- <http://hackage.haskell.org/packages/archive/bytestring/0.9.1.4/doc/html/src/Data-ByteString.html>
--
-- | Convert a trie into a list using a function. Resulting values
-- are in key-sorted order.
--
-- __Warning__: This function suffers <Data-Trie-Internal.html#bug25 Bug #25>.
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
-- __Note__: Prior to version 0.2.7, this function suffered
-- <Data-Trie-Internal.html#bug25 Bug #25>; but it no longer does.
--
-- @since 0.2.2
elems :: Trie a -> [a]
{-# INLINE elems #-}
#ifdef __GLASGOW_HASKELL__
elems t = build (\cons nil -> F.foldr cons nil t)
#else
elems = F.foldr (:) []
#endif


------------------------------------------------------------
------------------------------------------------------------


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
-- __Note__: /Type changed in 0.2.7/
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
        -- | \(\mathcal{O}(\min(m,W))\), where \(m\) is number of
        -- @Arc@s in this branching, and \(W\) is the word size of
        -- the Prefix,Mask type.
        findArc Empty         = impossible "lookupBy_" -- see [Note1]
        findArc t@(Arc{})     = go q t
        findArc (Branch p m l r)
            | nomatch qh p m  = clash
            | zero qh m       = findArc l
            | otherwise       = findArc r

-- [Note1]: Our use of the 'branch' and 'graft' smart constructors
-- ensure that 'Empty' never occurs in a 'Branch' tree ('Empty' can
-- only occur at the root, or under an 'Arc' with value); therefore
-- the @findArc Empty@ case is unreachable.  If we allowed such
-- nodes, however, then this case should return the same result as
-- the 'nomatch' case.


-- This function needs to be here, not in "Data.Trie", because of
-- 'arc' which isn't exported. We could use the monad instance
-- instead, though it'd be far more circuitous.
--     arc k Nothing  t ≡ singleton k () >> t
--     arc k (Just v) t ≡ singleton k v  >>= unionR t . singleton S.empty
--         (...except 'arc' doesn't do the invariant correction
--           of (>>=) for epsilon'elem't)
--
-- | Return the subtrie containing all keys beginning with a prefix.
submap :: ByteString -> Trie a -> Trie a
{-# INLINE submap #-}
submap q
    | S.null q  = id
    | otherwise = lookupBy_ (Arc q . Just) (prependNN q) empty q

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
        -- | \(\mathcal{O}(\min(m,W))\), where \(m\) is number of
        -- @Arc@s in this branching, and \(W\) is the word size of
        -- the Prefix,Mask type.
        findArc Empty         = impossible "lookup" -- see [Note1]
        findArc t@(Arc{})     = go q t
        findArc (Branch p m l r)
            | nomatch qh p m  = Nothing
            | zero qh m       = findArc l
            | otherwise       = findArc r
-}


-- TODO: would it be worth it to have a variant like 'lookupBy_'
-- which takes the three continuations?


-- According to our "Bench.MatchOne" benchmark, this is in fact
-- much faster than using 'matches_' and relying on list fusion.
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
        -- | \(\mathcal{O}(\min(m,W))\), where \(m\) is number of
        -- @Arc@s in this branching, and \(W\) is the word size of
        -- the Prefix,Mask type.
        findArc Empty         = impossible "match_" -- see [Note1]
        findArc t@(Arc{})     = match1 n q t
        findArc (Branch p m l r)
            | nomatch qh p m  = Nothing
            | zero qh m       = findArc l
            | otherwise       = findArc r
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
        -- | \(\mathcal{O}(\min(m,W))\), where \(m\) is number of
        -- @Arc@s in this branching, and \(W\) is the word size of
        -- the Prefix,Mask type.
        findArc Empty         = impossible "match_" -- see [Note1]
        findArc t@(Arc{})     = matchN n0 v0 n q t
        findArc (Branch p m l r)
            | nomatch qh p m  = Just (n0,v0)
            | zero qh m       = findArc l
            | otherwise       = findArc r


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
            -- | \(\mathcal{O}(\min(m,W))\), where \(m\) is number
            -- of @Arc@s in this branching, and \(W\) is the word
            -- size of the Prefix,Mask type.
            findArc Empty         = impossible "matches_" -- see [Note1]
            findArc t@(Arc{})     = go n q t
            findArc (Branch p m l r)
                | nomatch qh p m  = id
                | zero qh m       = findArc l
                | otherwise       = findArc r


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


-- Not susceptible to [bug26] because it can only delete a single value\/subtrie.
--
-- | A variant of 'alterBy' which also allows modifying the sub-trie.
-- If the function returns @(Just v, t)@ and @lookup 'S.empty' t == Just w@,
-- then the @w@ will be overwritten by @v@.
--
-- @since 0.2.3
-- __Note__: /Type changed in 0.2.6/
alterBy_
    :: (Maybe a -> Trie a -> (Maybe a, Trie a))
    -> ByteString -> Trie a -> Trie a
alterBy_ f = start
    where
    start q t            | not (S.null q) = go q t
    start _ (Arc k mv s) | S.null k       = mayEpsilon $$ f mv      s
    start _ t                             = mayEpsilon $$ f Nothing t

    -- @go@ is always called with non-null @q@, therefore @nothing@ is too.
    nothing q = arcNN q $$ f Nothing Empty

    go q Empty            = nothing q
    go q t@(Branch p m l r)
        | nomatch qh p m  =
            case nothing q of
            Empty -> t
            s     -> graft p t qh s
        | zero qh m       = branchL p m (go q l) r
        | otherwise       = branchR p m l (go q r)
        where qh = errorLogHead "alterBy_" q
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
                -- Inlined version of @prepend p@, capturing the
                -- invariant that the 'graft' must be a @Branch@.
                (if S.null p then id else Arc p Nothing)
                -- 'arcNN' will always have that the string in @l@
                -- must begin with @q'@, which is non-null here and
                -- therefore @arcPrefix q'@ is equivalent to taking
                -- the 'arcPrefix' of the string in @l@.
                $ graft (arcPrefix q') l (arcPrefix k') (Arc k' mv s)
        (True, True)  -> arcNN k $$ f mv s
        (True, False) -> arcNN k mv (go q' s)


-- TODO: benchmark vs the definition with alterBy\/liftM
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
        where qh = errorLogHead "adjust" q
    go q t@(Arc k mv s) =
        let (_,k',q') = breakMaximalPrefix k q in
        case (S.null k', S.null q') of
        (False, _)     -> t
        (True,  True)  -> Arc k (f <$> mv) s
        (True,  False) -> Arc k mv (go q' s)


{-----------------------------------------------------------
-- Trie-combining functions
-----------------------------------------------------------}

-- TODO: it may be helpful to have a version of 'mergeBy' where the
-- function doesn't return 'Maybe' (i.e., 'Data.Trie.Convenience.unionWith');
-- because knowing we can't delete elements would allow to use true
-- constructors directly, rather than smart constructors that patch
-- up the deletion cases.  Especially since the vast majority of
-- our own uses of 'mergeBy' fall into this category.

-- Not susceptible to [bug26] because it doesn't delete any values.
--
-- Alas, benchmarking indicates that this gives only a very trivial
-- benefit over 'TC.unionWith' as implemented via 'mergeBy'.
--
-- | Take the union of two tries, using a function to resolve
-- conflicts.  The resulting trie is constructed strictly, but the
-- results of the combining function are evaluated lazily.
wip_unionWith :: (a -> a -> a) -> Trie a -> Trie a -> Trie a
wip_unionWith f = start
    where
    -- | Deals with epsilon entries, before recursing into @go@
    -- TODO: for all of these, add assertions that null bytestring entails must be Just; instead of pattern matching on it directly.
    start (Arc k0 (Just v0) s0) (Arc k1 (Just v1) s1) | S.null k0 && S.null k1
                                               = epsilon (f v0 v1) (go s0 s1)
    start (Arc k0 (Just v0) s0) t1 | S.null k0 = epsilon v0 (go s0 t1)
    start t0 (Arc k1 (Just v1) s1) | S.null k1 = epsilon v1 (go t0 s1)
    start t0 t1                                = go t0 t1

    -- | The main recursion
    go Empty t1    = t1
    go t0    Empty = t0
    -- \(\mathcal{O}(n+m)\) for this part where \(n\) and \(m\) are
    -- sizes of the branchings.
    go t0@(Branch p0 m0 l0 r0)
       t1@(Branch p1 m1 l1 r1)
        | shorter m0 m1  = union0
        | shorter m1 m0  = union1
        | p0 == p1       = Branch p0 m0 (go l0 l1) (go r0 r1)
        | otherwise      = graft p0 t0 p1 t1
        where
        union0  | nomatch p1 p0 m0  = graft p0 t0 p1 t1
                | zero p1 m0        = Branch p0 m0 (go l0 t1) r0
                | otherwise         = Branch p0 m0 l0 (go r0 t1)
        union1  | nomatch p0 p1 m1  = graft p0 t0 p1 t1
                | zero p0 m1        = Branch p1 m1 (go t0 l1) r1
                | otherwise         = Branch p1 m1 l1 (go t0 r1)
    --
    go t0@(Arc k0 mv0 s0)
       t1@(Arc k1 mv1 s1)
        = arcMerge k0 t0 k1 t1 $ \ pre k0' k1' ->
            let {-# INLINE t0' #-}
                t0' = Arc k0' mv0 s0
                {-# INLINE t1' #-}
                t1' = Arc k1' mv1 s1
            in
            case (S.null k0', S.null k1') of
            (True, True)  -> arcNN pre (mergeMaybe (\v0 v1 -> Just (f v0 v1)) mv0 mv1) (go s0 s1) -- TODO: if both arcs are reject, then both @s0,s1@ are branches so we can simplify the 'arcNN' to avoid the case analysis in 'prependNN'.
            (True, False) -> Arc pre mv0 (go s0  t1')
            (False,True)  -> Arc pre mv1 (go t0' s1)
            (False,False) -> wye pre k0' t0' k1' t1'
    go t0@(Arc k0 _ _)
       t1@(Branch p1 m1 l r)
        | nomatch p0 p1 m1 = graft p1 t1  p0 t0
        | zero p0 m1       = Branch p1 m1 (go t0 l) r
        | otherwise        = Branch p1 m1 l (go t0 r)
        where p0 = arcPrefix k0
    go t0@(Branch p0 m0 l r)
       t1@(Arc k1 _ _)
        | nomatch p1 p0 m0 = graft p0 t0  p1 t1
        | zero p1 m0       = Branch p0 m0 (go l t1) r
        | otherwise        = Branch p0 m0 l (go r t1)
        where p1 = arcPrefix k1


-- FIXME: See [bug26].
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
    -- TODO: for all of these, add assertions that null bytestring entails must be Just; instead of pattern matching on it directly.
    start (Arc k0 (Just v0) s0) (Arc k1 (Just v1) s1) | S.null k0 && S.null k1
                                               = mayEpsilon (f v0 v1) (go s0 s1)
    start (Arc k0 (Just v0) s0) t1 | S.null k0 = epsilon v0 (go s0 t1)
    start t0 (Arc k1 (Just v1) s1) | S.null k1 = epsilon v1 (go t0 s1)
    start t0 t1                                = go t0 t1

    -- | The main recursion
    go Empty t1    = t1
    go t0    Empty = t0
    -- \(\mathcal{O}(n+m)\) for this part where \(n\) and \(m\) are
    -- sizes of the branchings.
    go t0@(Branch p0 m0 l0 r0)
       t1@(Branch p1 m1 l1 r1)
        | shorter m0 m1  = union0
        | shorter m1 m0  = union1
        | p0 == p1       = branch p0 m0 (go l0 l1) (go r0 r1)
        | otherwise      = graft p0 t0 p1 t1
        where
        union0  | nomatch p1 p0 m0  = graft p0 t0 p1 t1
                | zero p1 m0        = branchL p0 m0 (go l0 t1) r0
                | otherwise         = branchR p0 m0 l0 (go r0 t1)
        union1  | nomatch p0 p1 m1  = graft p0 t0 p1 t1
                | zero p0 m1        = branchL p1 m1 (go t0 l1) r1
                | otherwise         = branchR p1 m1 l1 (go t0 r1)
    --
    go t0@(Arc k0 mv0 s0)
       t1@(Arc k1 mv1 s1)
        = arcMerge k0 t0 k1 t1 $ \ pre k0' k1' ->
            let {-# INLINE t0' #-}
                t0' = Arc k0' mv0 s0
                {-# INLINE t1' #-}
                t1' = Arc k1' mv1 s1
            in
            -- TODO: can be smarter than 'arcNN' here...
            case (S.null k0', S.null k1') of
            (True, True)  -> arcNN pre (mergeMaybe f mv0 mv1) (go s0 s1)
            (True, False) -> arcNN pre mv0 (go s0  t1')
            (False,True)  -> arcNN pre mv1 (go t0' s1)
            (False,False) -> wye pre k0' t0' k1' t1'
    go t0@(Arc k0 _ _)
       t1@(Branch p1 m1 l r)
        | nomatch p0 p1 m1 = graft p1 t1  p0 t0
        | zero p0 m1       = branchL p1 m1 (go t0 l) r
        | otherwise        = branchR p1 m1 l (go t0 r)
        where p0 = arcPrefix k0
    go t0@(Branch p0 m0 l r)
       t1@(Arc k1 _ _)
        | nomatch p1 p0 m0 = graft p0 t0  p1 t1
        | zero p1 m0       = branchL p0 m0 (go l t1) r
        | otherwise        = branchR p0 m0 l (go r t1)
        where p1 = arcPrefix k1


mergeMaybe :: (a -> a -> Maybe a) -> Maybe a -> Maybe a -> Maybe a
{-# INLINE mergeMaybe #-}
mergeMaybe _ Nothing      Nothing  = Nothing
mergeMaybe _ Nothing mv1@(Just _)  = mv1
mergeMaybe _ mv0@(Just _) Nothing  = mv0
mergeMaybe f (Just v0)   (Just v1) = f v0 v1


-- FIXME: See [bug26].
-- | Take the intersection of two tries, using a function to resolve
-- collisions.
--
-- @since 0.2.6
intersectBy :: (a -> b -> Maybe c) -> Trie a -> Trie b -> Trie c
intersectBy f = start
    where
    -- | Deals with epsilon entries, before recursing into @go@
    start (Arc k0 mv0 s0) (Arc k1 mv1 s1) | S.null k0 && S.null k1
        = mayEpsilon (intersectMaybe f mv0 mv1) (go s0 s1)
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
        -- We can simplify 'getMask' to 'xor' here, avoiding the
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
                -- TODO: be smarter about the recursion and 'prependNN'
                case (S.null k0', S.null k1') of
                (True, True)  -> arcNN pre (intersectMaybe f mv0 mv1) (go s0 s1)
                (True, False) -> prependNN pre (go s0  t1')
                (False,True)  -> prependNN pre (go t0' s1)
                (False,False) -> prependNN pre (go t0' t1')
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


-- TODO(github#23): add 'differenceBy'


{-----------------------------------------------------------
-- Priority-queue functions
-----------------------------------------------------------}
-- TODO: should verify that all of these are now free of the quadratic
-- slowdown from reconstructing keys. They should be, but just to
-- verify that some new quadratic hasn't accidentally crept in...

-- | Return the lexicographically smallest 'ByteString' and the
-- value it's mapped to; or 'Nothing' for the empty trie.  When one
-- entry is a prefix of another, the prefix will be returned.
--
-- __Note__: Prior to version 0.2.7, this function suffered
-- <Data-Trie-Internal.html#bug25 Bug #25>; but it no longer does.
--
-- @since 0.2.2
minAssoc :: Trie a -> Maybe (ByteString, a)
minAssoc = go Nil
    where
    go !_ Empty              = Nothing
    go  q (Arc k (Just v) _) = Just (toStrict (q +>? k), v)
    go  q (Arc k Nothing  t) = go (q +>! k) t
    go  q (Branch _ _ l _)   = go q l


-- | Return the lexicographically largest 'ByteString' and the
-- value it's mapped to; or 'Nothing' for the empty trie.  When one
-- entry is a prefix of another, the longer one will be returned.
--
-- __Note__: Prior to version 0.2.7, this function suffered
-- <Data-Trie-Internal.html#bug25 Bug #25>; but it no longer does.
--
-- @since 0.2.2
maxAssoc :: Trie a -> Maybe (ByteString, a)
maxAssoc = go Nil
    where
    go !_ Empty                  = Nothing
    go  q (Arc k (Just v) Empty) = Just (toStrict (q +>? k), v)
    go  q (Arc k (Just _) t)     = go (q +>? k) t
    go  q (Arc k Nothing  t)     = go (q +>! k) t
    go  q (Branch _ _ _ r)       = go q r


mapView :: (Trie a -> Trie a)
        -> Maybe (ByteString, a, Trie a) -> Maybe (ByteString, a, Trie a)
{-# INLINE mapView #-}
mapView _ Nothing        = Nothing
mapView f (Just (k,v,t)) = Just (k,v, f t)


-- Not susceptible to [bug26] because it can only delete a single value.
--
-- | Update the 'minAssoc' and return the old 'minAssoc'.
--
-- __Note__: Prior to version 0.2.7, this function suffered
-- <Data-Trie-Internal.html#bug25 Bug #25>; but it no longer does.
--
-- @since 0.2.2
updateMinViewBy :: (ByteString -> a -> Maybe a)
                -> Trie a -> Maybe (ByteString, a, Trie a)
updateMinViewBy f = go Nil
    where
    go !_ Empty              = Nothing
    go  q (Arc k (Just v) t) = let q' = toStrict (q +>? k)
                               in Just (q',v, arc k (f q' v) t)
    go  q (Arc k Nothing  t) = mapView (prependNN k) (go (q +>! k) t)
    go  q (Branch p m l r)   = mapView (\l' -> branchL p m l' r) (go q l)


-- Not susceptible to [bug26] because it can only delete a single value.
--
-- | Update the 'maxAssoc' and return the old 'maxAssoc'.
--
-- __Note__: Prior to version 0.2.7, this function suffered
-- <Data-Trie-Internal.html#bug25 Bug #25>; but it no longer does.
--
-- @since 0.2.2
updateMaxViewBy :: (ByteString -> a -> Maybe a)
                -> Trie a -> Maybe (ByteString, a, Trie a)
updateMaxViewBy f = go Nil
    where
    go !_ Empty                  = Nothing
    go  q (Arc k (Just v) Empty) = let q' = toStrict (q +>? k)
                                   in Just (q',v, arc k (f q' v) Empty)
    go  q (Arc k mv@(Just _) t)  = mapView (Arc k mv) (go (q +>? k) t)
    go  q (Arc k Nothing     t)  = mapView (prepend k)   (go (q +>! k) t)
    go  q (Branch p m l r)       = mapView (branchR p m l) (go q r)

------------------------------------------------------------
------------------------------------------------------- fin.
