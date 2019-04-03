-- To make GHC stop warning about the Prelude
{-# OPTIONS_GHC -Wall -fwarn-tabs -fno-warn-unused-imports #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- For list fusion on toListBy, and guarding `base` versions.
{-# LANGUAGE CPP #-}
#ifdef __GLASGOW_HASKELL__
{-# LANGUAGE StandaloneDeriving, DeriveGeneric #-}
#endif

------------------------------------------------------------
--                                              ~ 2019.02.24
-- |
-- Module      :  Data.Trie.Internal
-- Copyright   :  Copyright (c) 2008--2019 wren gayle romano
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

module Data.Trie.Text.Internal
    (
    -- * Data types
      TrieText(), showTrieText

    -- * Functions for 'Text'
    , breakMaximalPrefixText

    -- * Basic functions
    , emptyText, nullText, singletonText, sizeText

    -- * Conversion and folding functions
    , foldrWithKeyText, toListByText

    -- * Query functions
    , lookupByText_, submapText
    , matchText_, matchesText_

    -- * Single-value modification
    , alterByText, alterByText_, adjustByText

    -- * Combining tries
    , mergeByText

    -- * Mapping functions
    , mapByText
    , filterMapText
    , contextualMapText
    , contextualMapText'
    , contextualFilterMapText
    , contextualMapByText

    -- * Priority-queue functions
    , minAssocText, maxAssocText
    , updateMinViewByText, updateMaxViewByText

    -- * Internal Text access
    , toList16
    ) where

import Prelude hiding    (null, lookup)
import qualified Prelude (null, lookup)

import Data.Trie.Text.BitTwiddle
import Data.Trie.TextInternal

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Internal as TI
import qualified Data.Text.Unsafe as TU
import qualified Data.Text.Array as TA

import Data.Binary
#if MIN_VERSION_base(4,9,0)
import Data.Semigroup      (Semigroup(..))
#endif
import Data.Monoid         (Monoid(..))
import Control.Monad       (liftM, liftM3, liftM4)
import Control.Monad       (ap)
import Control.Applicative (Applicative(..), (<$>))
import Data.Foldable       (Foldable(foldMap))
import Data.Traversable    (Traversable(traverse))

#ifdef __GLASGOW_HASKELL__
import GHC.Exts (build)
import GHC.Generics (Generic, Generic1)
#endif
------------------------------------------------------------
------------------------------------------------------------


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
              | Reject   (Branch a)          -- Invariant: Must be Branch
data Arc a    = Arc    ByteString (Node a)   -- Invariant: never empty string
data ArcSet a = None
              | One    {KeyElem} (Arc a)
              | Branch {Prefix} {Mask} (ArcSet a) (ArcSet a)
data Trie a   = Empty
              | Start  ByteString (Node a)   -- Maybe empty string [1]

[1] If we maintain the invariants on how Nodes recurse, then we
can't simply have Start(Node a) because we may have a shared prefix
where the prefix itself is not Accept'ed.


-- Squash Arc into One:
-- (pure good)
data Node a   = Accept a (ArcSet a)
              | Reject   (Branch a)
data ArcSet a = None
              | Arc    ByteString (Node a)
              | Branch {Prefix} {Mask} (ArcSet a) (ArcSet a)
data Trie a   = Empty
              | Start  ByteString (Node a)


-- Squash Node together:
-- (most likely good)
data Node a   = Node (Maybe a) (ArcSet a)
data ArcSet a = None
              | Arc    ByteString (Node a)
              | Branch {Prefix} {Mask} (ArcSet a) (ArcSet a)
data Trie a   = Empty
              | Start  ByteString (Node a)


-- Squash Empty/None and Arc/Start together:
-- (This complicates invariants about non-empty strings and Node's
-- recursion, but those can be circumvented by using smart
-- constructors.)
data Node a = Node (Maybe a) (ArcSet a)
data Trie a = Empty
            | Arc    ByteString (Node a)
            | Branch {Prefix} {Mask} (Trie a) (Trie a)


-- Squash Node into Arc:
-- (By this point, pure good)
-- Unseen invariants:
-- * ByteString non-empty, unless Arc is absolute root of tree
-- * If (Maybe a) is Nothing, then (Trie a) is Branch
--   * With views, we could re-expand Arc into accepting and
--     nonaccepting variants
--
-- [2] Maybe we shouldn't unpack the ByteString. We could specialize
-- or inline the breakMaximalPrefix function to prevent constructing
-- a new ByteString from the parts...
-}
-- | A map from 'ByteString's to @a@. For all the generic functions,
-- note that tries are strict in the @Maybe@ but not in @a@.
--
-- The 'Monad' instance is strange. If a key @k1@ is a prefix of
-- other keys, then results from binding the value at @k1@ will
-- override values from longer keys when they collide. If this is
-- useful for anything, or if there's a more sensible instance, I'd
-- be curious to know.

data TrieText a = EmptyText
                | ArcText    {-# UNPACK #-} !Text
                                            !(Maybe a)
                                            !(TrieText a)
                | BranchText {-# UNPACK #-} !PrefixText
                             {-# UNPACK #-} !MaskText
                                            !(TrieText a)
                                            !(TrieText a)
    deriving Eq

#ifdef __GLASGOW_HASKELL__
deriving instance Generic1 TrieText
deriving instance Generic a => Generic (TrieText a)
#endif


-- TODO? add Ord instance like Data.Map?

{-----------------------------------------------------------
-- Trie instances: serialization et cetera
-----------------------------------------------------------}

-- This instance does not unveil the innards of our abstract type.
-- It doesn't emit truly proper Haskell code though, since ByteStrings
-- are printed as (ASCII) Strings, but that's not our fault. (Also
-- 'fromList' is in "Data.Trie" instead of here.)
instance (Show a) => Show (TrieText a) where
    showsPrec p t = showParen (p > 10)
                  $ ("Data.Trie.fromListText "++) . shows (toListByText (,) t)

-- | Visualization fuction for debugging.
showTrieText :: (Show a) => TrieText a -> String
showTrieText t = shows' id t ""
    where
    spaces f = map (const ' ') (f "")

    shows' _  EmptyText            = (".\n"++)
    shows' ss (BranchText p m l r) =
        let s'  = ("--"++) . shows p . (","++) . shows m . ("-+"++)
            ss' = ss . (tail (spaces s') ++)
        in s'              . shows' (ss' . ("|"++)) l
           . ss' . ("|\n"++)
           . ss' . ("`"++) . shows' (ss' . (" "++)) r
    shows' ss (ArcText k mv t') =
        let s' = ("--"++) . shows k
                 . maybe id (\v -> ("-("++) . shows v . (")"++)) mv
                 . ("--"++)
        in  s' . shows' (ss . (spaces s' ++)) t'


-- TODO?? a Read instance? hrm... should I?

-- TODO: consider an instance more like the new one for Data.Map. Better?
instance (Binary a) => Binary (TrieText a) where
    put EmptyText            = do put (0 :: Word8)
    put (ArcText k m t)      = do put (1 :: Word8); put k; put m; put t
    put (BranchText p m l r) = do put (2 :: Word8); put p; put m; put l; put r

    get = do tag <- get :: Get Word8
             case tag of
                 0 -> return EmptyText
                 1 -> liftM3 ArcText get get get
                 _ -> liftM4 BranchText get get get get


{-----------------------------------------------------------
-- Trie instances: Abstract Nonsense
-----------------------------------------------------------}

instance Functor TrieText where
    fmap f = go
        where
        go EmptyText              = EmptyText
        go (ArcText k Nothing  t) = ArcText k Nothing      (go t)
        go (ArcText k (Just v) t) = ArcText k (Just (f v)) (go t)
        go (BranchText p m l r)   = BranchText p m (go l) (go r)

instance Foldable TrieText where
    foldMap f = go
        where
        go EmptyText              = mempty
        go (ArcText _ Nothing  t) = go t
        go (ArcText _ (Just v) t) = f v `mappend` go t
        go (BranchText _ _ l r)   = go l `mappend` go r

-- TODO: newtype Keys = K Trie  ; instance Foldable Keys
-- TODO: newtype Assoc = A Trie ; instance Foldable Assoc

instance Traversable TrieText where
    traverse f = go
        where
        go EmptyText              = pure EmptyText
        go (ArcText k Nothing  t) = ArcText k Nothing        <$> go t
        go (ArcText k (Just v) t) = ArcText k . Just <$> f v <*> go t
        go (BranchText p m l r)   = BranchText p m <$> go l <*> go r

instance Applicative TrieText where
    pure  = return
    (<*>) = ap

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
instance Monad TrieText where
    return = singletonText T.empty

    (>>=) EmptyText              _ = emptyText
    (>>=) (BranchText p m l r)   f = branchText p m (l >>= f) (r >>= f)
    (>>=) (ArcText k Nothing  t) f = arcText k Nothing (t >>= f)
    (>>=) (ArcText k (Just v) t) f = arcText k Nothing (f v `unionLText` (t >>= f))
                               where
                               unionLText = mergeByText (\x _ -> Just x)


#if MIN_VERSION_base(4,9,0)
-- The "Data.Semigroup" module is in base since 4.9.0.0; but having
-- the 'Semigroup' superclass for the 'Monoid' instance only comes
-- into force in base 4.11.0.0.

instance (Semigroup a) => Semigroup (TrieText a) where
    (<>) = mergeByText $ \x y -> Just (x <> y)
    -- TODO: optimized implementations of:
    -- sconcat :: NonEmpty a -> a
    -- stimes :: Integral b => b -> a -> a
#endif

-- This instance is more sensible than Data.IntMap and Data.Map's
instance (Monoid a) => Monoid (TrieText a) where
    mempty  = emptyText
    mappend = mergeByText $ \x y -> Just (x `mappend` y)

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
filterMapText :: (a -> Maybe b) -> TrieText a -> TrieText b
filterMapText f = go
    where
    go EmptyText              = emptyText
    go (ArcText k Nothing  t) = arcText k Nothing (go t)
    go (ArcText k (Just v) t) = arcText k (f v)   (go t)
    go (BranchText p m l r)   = branchText p m (go l) (go r)


-- | Generic version of 'fmap'. This function is notably more
-- expensive than 'fmap' or 'filterMap' because we have to reconstruct
-- the keys.
mapByText :: (Text -> a -> Maybe b) -> TrieText a -> TrieText b
mapByText f = go T.empty
    where
    go _ EmptyText              = emptyText
    go q (ArcText k Nothing  t) = arcText k Nothing  (go q' t) where q' = T.append q k
    go q (ArcText k (Just v) t) = arcText k (f q' v) (go q' t) where q' = T.append q k
    go q (BranchText p m l r)   = branchText p m (go q l) (go q r)

-- | A variant of 'fmap' which provides access to the subtrie rooted
-- at each value.
contextualMapText :: (a -> TrieText a -> b) -> TrieText a -> TrieText b
contextualMapText f = go
    where
    go EmptyText              = EmptyText
    go (ArcText k Nothing  t) = ArcText k Nothing        (go t)
    go (ArcText k (Just v) t) = ArcText k (Just (f v t)) (go t)
    go (BranchText p m l r)   = BranchText p m (go l) (go r)


-- | A variant of 'contextualMap' which applies the function strictly.
contextualMapText' :: (a -> TrieText a -> b) -> TrieText a -> TrieText b
contextualMapText' f = go
    where
    go EmptyText              = EmptyText
    go (ArcText k Nothing  t) = ArcText k Nothing         (go t)
    go (ArcText k (Just v) t) = ArcText k (Just $! f v t) (go t)
    go (BranchText p m l r)   = BranchText p m (go l) (go r)


-- | A contextual variant of 'filterMap'.
contextualFilterMapText :: (a -> TrieText a -> Maybe b) -> TrieText a -> TrieText b
contextualFilterMapText f = go
    where
    go EmptyText              = emptyText
    go (ArcText k Nothing  t) = arcText k Nothing (go t)
    go (ArcText k (Just v) t) = arcText k (f v t) (go t)
    go (BranchText p m l r)   = branchText p m (go l) (go r)

-- | A contextual variant of 'mapBy'. Again note that this is
-- expensive since we must reconstruct the keys.
contextualMapByText :: (Text -> a -> TrieText a -> Maybe b) -> TrieText a -> TrieText b
contextualMapByText f = go T.empty
    where
    go _ EmptyText              = emptyText
    go q (ArcText k Nothing  t) = arcText k Nothing (go (T.append q k) t)
    go q (ArcText k (Just v) t) = let q' = T.append q k
                              in arcText k (f q' v t) (go q' t)
    go q (BranchText p m l r)   = branchText p m (go q l) (go q r)



{-----------------------------------------------------------
-- Smart constructors and helper functions for building tries
-----------------------------------------------------------}

-- | Smart constructor to prune @Empty@ from @Branch@es.
branchText :: PrefixText -> MaskText -> TrieText a -> TrieText a -> TrieText a
{-# INLINE branchText #-}
branchText _ _ EmptyText r     = r
branchText _ _ l     EmptyText = l
branchText p m l     r     = BranchText p m l r


-- | Smart constructor to prune @Arc@s that lead nowhere.
-- N.B if mv=Just then doesn't check whether t=epsilon. It's up to callers to ensure that invariant isn't broken.
arcText :: Text -> Maybe a -> TrieText a -> TrieText a
{-# INLINE arcText #-}
arcText k mv@(Just _)   t                                = ArcText k mv t
arcText _    Nothing    EmptyText                        = EmptyText
arcText k    Nothing  t@(BranchText _ _ _ _) | T.null k  = t
                                             | otherwise = ArcText k Nothing t
arcText k    Nothing    (ArcText k' mv' t')              = ArcText (T.append k k') mv' t'



-- | Smart constructor to join two tries into a @Branch@ with maximal
-- prefix sharing. Requires knowing the prefixes, but can combine
-- either @Branch@es or @Arc@s.
--
-- N.B. /do not/ use if prefixes could match entirely!
branchMergeText :: PrefixText -> TrieText a -> PrefixText -> TrieText a -> TrieText a
{-# INLINE branchMergeText #-}
branchMergeText _ EmptyText _ t2    = t2
branchMergeText _  t1   _ EmptyText = t1
branchMergeText p1 t1  p2 t2
    | zeroText p1 m             = BranchText p m t1 t2
    | otherwise             = BranchText p m t2 t1
    where
    m = branchMaskText p1 p2
    p = maskText p1 m

-- It would be better if Arc used
-- Data.ByteString.TrieInternal.wordHead somehow, that way
-- we can see 4/8/?*Word8 at a time instead of just one.
-- But that makes maintaining invariants ...difficult :(
getPrefixText :: TrieText a -> PrefixText
{-# INLINE getPrefixText #-}
getPrefixText (BranchText p _ _ _)        = p
getPrefixText (ArcText k _ _) | T.null k  = 0 -- for lack of a better value
                              | otherwise = head16 k
getPrefixText EmptyText                   = error "getPrefixText: no PrefixText of EmptyText"


{-----------------------------------------------------------
-- Error messages
-----------------------------------------------------------}

head16 :: Text -> Word16
head16 (TI.Text xs i0 _) = xs `TA.unsafeIndex` i0

tail16 :: Text -> Maybe Text
tail16 xs | T.null xs = Nothing
          | otherwise = Just $ TU.dropWord16 1 xs

toList16 :: Text -> [Word16]
toList16 xs =
  case tail16 xs of
    Nothing -> []
    Just ys -> head16 xs : toList16 ys


-- TODO: shouldn't we inline the logic and just NOINLINE the string constant? There are only three use sites, which themselves aren't inlined...
errorLogHeadText :: String -> Text -> TextElem
{-# NOINLINE errorLogHeadText #-}
errorLogHeadText fn q
    | T.null q  = error $ "Data.Trie.Internal." ++ fn ++": found null subquery"
    | otherwise = head16 q


------------------------------------------------------------
------------------------------------------------------------

{-----------------------------------------------------------
-- Basic functions
-----------------------------------------------------------}

-- | /O(1)/, Construct the empty trie.
emptyText :: TrieText a
{-# INLINE emptyText #-}
emptyText = EmptyText


-- | /O(1)/, Is the trie empty?
nullText :: TrieText a -> Bool
{-# INLINE nullText #-}
nullText EmptyText = True
nullText _         = False


-- | /O(1)/, Construct a singleton trie.
singletonText :: Text -> a -> TrieText a
{-# INLINE singletonText #-}
singletonText k v = ArcText k (Just v) EmptyText
-- For singletons, don't need to verify invariant on arc length >0

-- | /O(n)/, Get count of elements in trie.
sizeText  :: TrieText a -> Int
{-# INLINE sizeText #-}
sizeText t = sizeText' t id 0


-- | /O(n)/, CPS accumulator helper for calculating 'size'.
sizeText' :: TrieText a -> (Int -> Int) -> Int -> Int
sizeText' EmptyText              f n = f n
sizeText' (BranchText _ _ l r)   f n = sizeText' l (sizeText' r f) n
sizeText' (ArcText _ Nothing t)  f n = sizeText' t f n
sizeText' (ArcText _ (Just _) t) f n = sizeText' t f $! n + 1



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
foldrWithKeyText :: (Text -> a -> b -> b) -> b -> TrieText a -> b
foldrWithKeyText fcons nil = \t -> go T.empty t nil
    where
    go _ EmptyText            = id
    go q (BranchText _ _ l r) = go q l . go q r
    go q (ArcText k mv t)     =
        case mv of
        Nothing -> rest
        Just v  -> fcons k' v . rest
        where
        rest = go k' t
        k'   = T.append q k



-- cf Data.ByteString.unpack
-- <http://hackage.haskell.org/packages/archive/bytestring/0.9.1.4/doc/html/src/Data-ByteString.html>
--
-- | Convert a trie into a list using a function. Resulting values
-- are in key-sorted order.
toListByText :: (Text -> a -> b) -> TrieText a -> [b]
{-# INLINE toListByText #-}
#if !defined(__GLASGOW_HASKELL__)
-- TODO: should probably inline foldrWithKey
-- TODO: compare performance of that vs both this and the GHC version
toListByText f t = foldrWithKeyText (((:) .) . f) [] t
#else
-- Written with 'build' to enable the build\/foldr fusion rules.
toListByText f t = build (toListByTextFB f t)

-- TODO: should probably have a specialized version for strictness,
-- and a rule to rewrite generic lazy version into it. As per
-- Data.ByteString.unpack and the comments there about strictness
-- and fusion.
toListByTextFB :: (Text -> a -> b) -> TrieText a -> (b -> c -> c) -> c -> c
{-# INLINE [0] toListByTextFB #-}
toListByTextFB f t cons nil = foldrWithKeyText ((cons .) . f) nil t
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
-- version, see @lookupBy@ in "Data.Trie".
lookupByText_ :: (Maybe a -> TrieText a -> b) -> b -> (TrieText a -> b)
          -> Text -> TrieText a -> b
lookupByText_ f z a = lookupByText_'
    where
    -- | Deal with epsilon query (when there is no epsilon value)
    lookupByText_' q t@(BranchText _ _ _ _) | T.null q = f Nothing t
    lookupByText_' q t                                 = go q t

    -- | The main recursion
    go _    EmptyText       = z

    go q   (ArcText k mv t) =
        let (_,k',q')       = breakMaximalPrefixText k q
        in case (not $ T.null k', T.null q') of
                (True,  True)  -> a (ArcText k' mv t)
                (True,  False) -> z
                (False, True)  -> f mv t
                (False, False) -> go q' t

    go q t_@(BranchText _ _ _ _) = findArcText t_
        where
        qh = errorLogHeadText "lookupByText_" q

        -- | /O(min(m,W))/, where /m/ is number of @Arc@s in this
        -- branching, and /W/ is the word size of the Prefix,Mask type.
        findArcText (BranchText p m l r)
            | nomatchText qh p m  = z
            | zeroText qh m       = findArcText l
            | otherwise       = findArcText r
        findArcText t@(ArcText _ _ _) = go q t
        findArcText EmptyText         = z




-- This function needs to be here, not in "Data.Trie", because of
-- 'arc' which isn't exported. We could use the monad instance
-- instead, though it'd be far more circuitous.
--     arc k Nothing  t === singleton k () >> t
--     arc k (Just v) t === singleton k v  >>= unionR t . singleton S.empty
--         (...except 'arc' doesn't do the invariant correction
--           of (>>=) for epsilon`elem`t)
--
-- | Return the subtrie containing all keys beginning with a prefix.
submapText :: Text -> TrieText a -> TrieText a
{-# INLINE submapText #-}
submapText q = lookupByText_ (arcText q) emptyText (arcText q Nothing) q
{-  -- Disable superfluous error checking.
    -- @submap'@ would replace the first argument to @lookupBy_@
    where
    submap' Nothing Empty       = errorEmptyAfterNothing "submap"
    submap' Nothing (Arc _ _ _) = errorArcAfterNothing   "submap"
    submap' mx      t           = Arc q mx t

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

-- | Length of `Text` in `Word16`'s
length16 :: Text -> Int
length16 (TI.Text _ off len) = len - off

-- | Given a query, find the longest prefix with an associated value
-- in the trie, returning the length of that prefix and the associated
-- value.
--
-- This function may not have the most useful return type. For a
-- version that returns the prefix itself as well as the remaining
-- string, see @match@ in "Data.Trie".
matchText_ :: TrieText a -> Text -> Maybe (Int, a)
matchText_ = flip start
    where
    -- | Deal with epsilon query (when there is no epsilon value)
    start q (BranchText _ _ _ _) | T.null q = Nothing
    start q t                               = goNothing 0 q t

    -- | The initial recursion
    goNothing _ _    EmptyText       = Nothing

    goNothing n q   (ArcText k mv t) =
        let (p,k',q') = breakMaximalPrefixText k q
            n'        = n + length16 p
        in n' `seq`
            if T.null k'
            then
                if T.null q'
                then (,) n' <$> mv
                else
                    case mv of
                    Nothing -> goNothing   n' q' t
                    Just v  -> goJust n' v n' q' t
            else Nothing

    goNothing n q t_@(BranchText _ _ _ _) = findArc t_
        where
        qh = errorLogHeadText "matchText_" q

        -- | /O(min(m,W))/, where /m/ is number of @Arc@s in this
        -- branching, and /W/ is the word size of the Prefix,Mask type.
        findArc (BranchText p m l r)
            | nomatchText qh p m  = Nothing
            | zeroText qh m       = findArc l
            | otherwise       = findArc r
        findArc t@(ArcText _ _ _) = goNothing n q t
        findArc EmptyText         = Nothing

    -- | The main recursion
    goJust n0 v0 _ _    EmptyText       = Just (n0,v0)

    goJust n0 v0 n q   (ArcText k mv t) =
        let (p,k',q') = breakMaximalPrefixText k q
            n'        = n + length16 p
        in n' `seq`
            if T.null k'
            then
                if T.null q'
                then
                    case mv of
                    Nothing -> Just (n0,v0)
                    Just v  -> Just (n',v)
                else
                    case mv of
                    Nothing -> goJust n0 v0 n' q' t
                    Just v  -> goJust n' v  n' q' t
            else Just (n0,v0)

    goJust n0 v0 n q t_@(BranchText _ _ _ _) = findArc t_
        where
        qh = errorLogHeadText "matchText_" q

        -- | /O(min(m,W))/, where /m/ is number of @Arc@s in this
        -- branching, and /W/ is the word size of the Prefix,Mask type.
        findArc (BranchText p m l r)
            | nomatchText qh p m  = Just (n0,v0)
            | zeroText qh m       = findArc l
            | otherwise       = findArc r
        findArc t@(ArcText _ _ _) = goJust n0 v0 n q t
        findArc EmptyText         = Just (n0,v0)



-- | Given a query, find all prefixes with associated values in the
-- trie, returning their lengths and values. This function is a
-- good producer for list fusion.
--
-- This function may not have the most useful return type. For a
-- version that returns the prefix itself as well as the remaining
-- string, see @matches@ in "Data.Trie".
matchesText_ :: TrieText a -> Text -> [(Int,a)]
matchesText_ t q =
#if !defined(__GLASGOW_HASKELL__)
    matchFBText_ t q (((:) .) . (,)) []
#else
    build (\cons nil -> matchFBText_ t q ((cons .) . (,)) nil)
{-# INLINE matchesText_ #-}
#endif

matchFBText_ :: TrieText a -> Text -> (Int -> a -> r -> r) -> r -> r
matchFBText_ = \t q cons nil -> matchFB_' cons q t nil
    where
    matchFB_' cons = start
        where
        -- | Deal with epsilon query (when there is no epsilon value)
        start q (BranchText _ _ _ _) | T.null q = id
        start q t                               = go 0 q t

        -- | The main recursion
        go _ _    EmptyText       = id

        go n q   (ArcText k mv t) =
            let (p,k',q') = breakMaximalPrefixText k q
                n'        = n + length16 p
            in n' `seq`
                if T.null k'
                then
                    case mv of { Nothing -> id; Just v  -> cons n' v}
                    .
                    if T.null q' then id else go n' q' t
                else id

        go n q t_@(BranchText _ _ _ _) = findArc t_
            where
            qh = errorLogHeadText "matches_" q

            -- | /O(min(m,W))/, where /m/ is number of @Arc@s in this
            -- branching, and /W/ is the word size of the Prefix,Mask type.
            findArc (BranchText p m l r)
                | nomatchText qh p m  = id
                | zeroText qh m       = findArc l
                | otherwise       = findArc r
            findArc t@(ArcText _ _ _) = go n q t
            findArc EmptyText         = id



{-----------------------------------------------------------
-- Single-value modification functions (recurse and clone spine)
-----------------------------------------------------------}

-- TODO: We should CPS on Empty to avoid cloning spine if no change.
-- Difficulties arise with the calls to 'branch' and 'arc'. Will
-- have to create a continuation chain, so no savings on memory
-- allocation; but would have savings on held memory, if they're
-- still holding the old one...
--
-- | Generic function to alter a trie by one element with a function
-- to resolve conflicts (or non-conflicts).
alterByText :: (Text -> a -> Maybe a -> Maybe a)
             -> Text -> a -> TrieText a -> TrieText a
alterByText f = alterByText_ (\k v mv t -> (f k v mv, t))
-- TODO: use GHC's 'inline' function so that this gets specialized away.
-- TODO: benchmark to be sure that this doesn't introduce unforseen performance costs because of the uncurrying etc.



-- | A variant of 'alterBy' which also allows modifying the sub-trie.
alterByText_ :: (Text -> a -> Maybe a -> TrieText a -> (Maybe a, TrieText a))
              -> Text -> a -> TrieText a -> TrieText a
alterByText_ f_ q_ x_
    | T.null q_ = alterEpsilon
    | otherwise = go q_
    where
    f         = f_ q_ x_
    nothing q = uncurry (arcText q) (f Nothing EmptyText)

    alterEpsilon t_@EmptyText                    = uncurry (arcText q_) (f Nothing t_)
    alterEpsilon t_@(BranchText _ _ _ _)         = uncurry (arcText q_) (f Nothing t_)
    alterEpsilon t_@(ArcText k mv t) | T.null k  = uncurry (arcText q_) (f mv      t)
                                     | otherwise = uncurry (arcText q_) (f Nothing t_)


    go q EmptyText            = nothing q

    go q t@(BranchText p m l r)
        | nomatchText qh p m  = branchMergeText p t  qh (nothing q)
        | zeroText qh m       = branchText p m (go q l) r
        | otherwise       = branchText p m l (go q r)
        where
        qh = errorLogHeadText "alterBy" q

    go q t_@(ArcText k mv t) =
        let (p,k',q') = breakMaximalPrefixText k q in
        case (not $ T.null k', T.null q') of
        (True,  True)  -> -- add node to middle of arc
                          uncurry (arcText p) (f Nothing (ArcText k' mv t))
        (True,  False) ->
            case nothing q' of
            EmptyText -> t_ -- Nothing to add, reuse old arc
            l     -> arc' (branchMergeText (getPrefixText l) l (getPrefixText r) r)
                    where
                    r = ArcText k' mv t

                    -- inlined version of 'arc'
                    arc' | T.null p  = id
                         | otherwise = ArcText p Nothing

        (False, True)  -> uncurry (arcText k) (f mv t)
        (False, False) -> arcText k mv (go q' t)


-- | Alter the value associated with a given key. If the key is not
-- present, then the trie is returned unaltered. See 'alterBy' if
-- you are interested in inserting new keys or deleting old keys.
-- Because this function does not need to worry about changing the
-- trie structure, it is somewhat faster than 'alterBy'.
adjustByText :: (Text -> a -> a -> a)
              -> Text -> a -> TrieText a -> TrieText a
adjustByText f_ q_ x_
    | T.null q_ = adjustEpsilon
    | otherwise = go q_
    where
    f = f_ q_ x_

    adjustEpsilon (ArcText k (Just v) t) | T.null k = ArcText k (Just (f v)) t
    adjustEpsilon t_                                = t_

    go _ EmptyText            = EmptyText

    go q t@(BranchText p m l r)
        | nomatchText qh p m  = t
        | zeroText qh m       = BranchText p m (go q l) r
        | otherwise       = BranchText p m l (go q r)
        where
        qh = errorLogHeadText "adjustByText" q

    go q t_@(ArcText k mv t) =
        let (_,k',q') = breakMaximalPrefixText k q in
        case (not $ T.null k', T.null q') of
        (True,  True)  -> t_ -- don't break arc inline
        (True,  False) -> t_ -- don't break arc branching
        (False, True)  -> ArcText k (liftM f mv) t
        (False, False) -> ArcText k mv (go q' t)


{-----------------------------------------------------------
-- Trie-combining functions
-----------------------------------------------------------}

-- TEST CASES: foldr (unionL . uncurry singleton) empty t
--             foldr (uncurry insert) empty t
--    where t = map (\s -> (pk s, 0))
--                  ["heat","hello","hoi","apple","appa","hell","appb","appc"]
--
-- | Combine two tries, using a function to resolve collisions.
-- This can only define the space of functions between union and
-- symmetric difference but, with those two, all set operations can
-- be defined (albeit inefficiently).
mergeByText :: (a -> a -> Maybe a) -> TrieText a -> TrieText a -> TrieText a
mergeByText f = mergeBy'
    where
    -- | Deals with epsilon entries, before recursing into @go@
    mergeBy'
        t0_@(ArcText k0 mv0 t0)
        t1_@(ArcText k1 mv1 t1)
        | T.null k0 && T.null k1 = arcText k0 (mergeMaybe f mv0 mv1) (go t0 t1)
        | T.null k0              = arcText k0 mv0 (go t0 t1_)
        |              T.null k1 = arcText k1 mv1 (go t1 t0_)
    mergeBy'
        (ArcText k0 mv0@(Just _) t0)
        t1_@(BranchText _ _ _ _)
        | T.null k0              = arcText k0 mv0 (go t0 t1_)
    mergeBy'
        t0_@(BranchText _ _ _ _)
        (ArcText k1 mv1@(Just _) t1)
        | T.null k1              = arcText k1 mv1 (go t1 t0_)
    mergeBy' t0_ t1_             = go t0_ t1_


    -- | The main recursion
    go EmptyText t1    = t1
    go t0    EmptyText = t0

    -- /O(n+m)/ for this part where /n/ and /m/ are sizes of the branchings
    go  t0@(BranchText p0 m0 l0 r0)
        t1@(BranchText p1 m1 l1 r1)
        | shorterText m0 m1  = union0
        | shorterText m1 m0  = union1
        | p0 == p1       = branchText p0 m0 (go l0 l1) (go r0 r1)
        | otherwise      = branchMergeText p0 t0 p1 t1
        where
        union0  | nomatchText p1 p0 m0  = branchMergeText p0 t0 p1 t1
                | zeroText p1 m0        = branchText p0 m0 (go l0 t1) r0
                | otherwise         = branchText p0 m0 l0 (go r0 t1)

        union1  | nomatchText p0 p1 m1  = branchMergeText p0 t0 p1 t1
                | zeroText p0 m1        = branchText p1 m1 (go t0 l1) r1
                | otherwise         = branchText p1 m1 l1 (go t0 r1)

    -- We combine these branches of 'go' in order to clarify where the definitions of 'p0', 'p1', 'm'', 'p'' are relevant. However, this may introduce inefficiency in the pattern matching automaton...
    -- TODO: check. And get rid of 'go'' if it does.
    go t0_ t1_ = go' t0_ t1_
        where
        p0 = getPrefixText t0_
        p1 = getPrefixText t1_
        m' = branchMaskText p0 p1
        p' = maskText p0 m'

        go' (ArcText k0 mv0 t0)
            (ArcText k1 mv1 t1)
            | m' == 0 =
                let (pre,k0',k1') = breakMaximalPrefixText k0 k1 in
                if T.null pre
                then error "mergeBy: no mask, but no prefix string"
                else let {-# INLINE arcMergeText #-}
                         arcMergeText mv' t1' t2' = arcText pre mv' (go t1' t2')
                     in case (T.null k0', T.null k1') of
                         (True, True)  -> arcMergeText (mergeMaybe f mv0 mv1) t0 t1
                         (True, False) -> arcMergeText mv0 t0 (ArcText k1' mv1 t1)
                         (False,True)  -> arcMergeText mv1 (ArcText k0' mv0 t0) t1
                         (False,False) -> arcMergeText Nothing (ArcText k0' mv0 t0)
                                                               (ArcText k1' mv1 t1)
        go' (ArcText _ _ _)
            (BranchText _p1 m1 l r)
            | nomatchText p0 p1 m1 = branchMergeText p1 t1_  p0 t0_
            | zeroText p0 m1       = branchText p1 m1 (go t0_ l) r
            | otherwise        = branchText p1 m1 l (go t0_ r)
        go' (BranchText _p0 m0 l r)
            (ArcText _ _ _)
            | nomatchText p1 p0 m0 = branchMergeText p0 t0_  p1 t1_
            | zeroText p1 m0       = branchText p0 m0 (go l t1_) r
            | otherwise        = branchText p0 m0 l (go r t1_)

        -- Inlined branchMerge. Both tries are disjoint @Arc@s now.
        go' _ _ | zeroText p0 m'   = BranchText p' m' t0_ t1_
        go' _ _                    = BranchText p' m' t1_ t0_



mergeMaybe :: (a -> a -> Maybe a) -> Maybe a -> Maybe a -> Maybe a
{-# INLINE mergeMaybe #-}
mergeMaybe _ Nothing      Nothing  = Nothing
mergeMaybe _ Nothing mv1@(Just _)  = mv1
mergeMaybe _ mv0@(Just _) Nothing  = mv0
mergeMaybe f (Just v0)   (Just v1) = f v0 v1


{-----------------------------------------------------------
-- Priority-queue functions
-----------------------------------------------------------}

minAssocText :: TrieText a -> Maybe (Text, a)
minAssocText = go T.empty
    where
    go _ EmptyText              = Nothing
    go q (ArcText k (Just v) _) = Just (T.append q k,v)
    go q (ArcText k Nothing  t) = go   (T.append q k) t
    go q (BranchText _ _ l _)   = go q l


maxAssocText :: TrieText a -> Maybe (Text, a)
maxAssocText = go T.empty
    where
    go _ EmptyText                  = Nothing
    go q (ArcText k (Just v) EmptyText) = Just (T.append q k,v)
    go q (ArcText k _        t)     = go   (T.append q k) t
    go q (BranchText _ _ _ r)       = go q r


mapViewText :: (TrieText a -> TrieText a)
            -> Maybe (Text, a, TrieText a) -> Maybe (Text, a, TrieText a)
mapViewText _ Nothing        = Nothing
mapViewText f (Just (k,v,t)) = Just (k,v, f t)


updateMinViewByText :: (Text -> a -> Maybe a)
                     -> TrieText a -> Maybe (Text, a, TrieText a)
updateMinViewByText f = go T.empty
    where
    go _ EmptyText              = Nothing
    go q (ArcText k (Just v) t) = let q' = T.append q k
                              in Just (q',v, arcText k (f q' v) t)
    go q (ArcText k Nothing  t) = mapViewText (arcText k Nothing) (go (T.append q k) t)
    go q (BranchText p m l r)   = mapViewText (\l' -> branchText p m l' r) (go q l)

updateMaxViewByText :: (Text -> a -> Maybe a)
                    -> TrieText a -> Maybe (Text, a, TrieText a)
updateMaxViewByText f = go T.empty
    where
    go _ EmptyText                  = Nothing
    go q (ArcText k (Just v) EmptyText) = let q' = T.append q k
                                          in Just (q',v, arcText k (f q' v) EmptyText)
    go q (ArcText k mv       t)     = mapViewText (arcText k mv) (go (T.append q k) t)
    go q (BranchText p m l r)       = mapViewText (branchText p m l) (go q r)

------------------------------------------------------------
------------------------------------------------------- fin.

