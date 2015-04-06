-- To make GHC stop warning about the Prelude
{-# OPTIONS_GHC
    -Wall -fwarn-tabs -fno-warn-unused-imports
    -funbox-strict-fields #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE CPP, BangPatterns #-}

-- TODO: benchmark using SA.lookup_ instead of SA.lookup'

----------------------------------------------------------------
--                                                  ~ 2015.04.05
-- |
-- Module      :  Data.Trie.ArrayMapped.Internal
-- Copyright   :  Copyright (c) 2014--2015 wren gayle romano
-- License     :  BSD3
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  provisional
-- Portability :  portable (with CPP)
--
-- Internal definition of the 'Trie' data type and generic functions
-- for manipulating them. Almost everything here is re-exported
-- from "Data.Trie", which is the preferred API for users. This
-- module is for developers who need deeper (and potentially fragile)
-- access to the abstract type.
----------------------------------------------------------------

module Data.Trie.ArrayMapped.Internal
    (
    -- * Data types
      Trie(), showTrie
    
    -- * Basic functions
    , empty, null, singleton, size
    
    -- * Conversion and folding functions
    , foldrWithKey, foldrWithKey', toListBy
    
    -- * Query functions
    , lookupBy_, submap
    
    -- * Single-value modification
    , alterBy, alterBy_, adjustBy
    
    -- * Combining tries
    , mergeBy
    
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

import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import           Data.Trie.ByteStringInternal
import qualified Data.Trie.ArrayMapped.SparseArray as SA
import           Data.Trie.ArrayMapped.SparseArray (SubsingletonView(..))

import Data.Binary

import Data.Typeable       (Typeable(..))
import Control.DeepSeq     (NFData(rnf))

import Data.Monoid         (Monoid(..))
import Control.Monad       (ap, liftM, liftM3, liftM4)
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative (Applicative(..), (<$>))
#endif
import Data.Foldable       (Foldable(foldMap,foldl'))
import Data.Traversable    (Traversable(traverse))


#ifdef __GLASGOW_HASKELL__
import GHC.Exts (build)
#endif
----------------------------------------------------------------
----------------------------------------------------------------


{---------------------------------------------------------------
-- ByteString Array Mapped Trie datatype
---------------------------------------------------------------}


-- INVARIANT: this particular ByteString is not empty...
type NonEmptyByteString = ByteString 


-- | A map from 'ByteString's to @a@. For all the generic functions,
-- note that tries are strict in the @Maybe@ but not in @a@.
--
-- The 'Monad' instance is strange. If a key @k1@ is a prefix of
-- other keys, then results from binding the value at @k1@ will
-- override values from longer keys when they collide. If this is
-- useful for anything, or if there's a more sensible instance, I'd
-- be curious to know.

data Trie a
    = Accept a !(Trunk a)
    | Reject   !(Trunk a)
    deriving (Typeable, Eq)


-- | Outgoing arcs from a node (aka 'Trie'). This data type
-- incorporates all non-root nodes. The root node must be distinguished
-- in order to accept the empty string.

data Trunk a
    = Empty
    | Arc
        !NonEmptyByteString          -- Prefix
        a                            -- Value
        !(Trunk a)                   -- Sub-trie
    | Branch
        !ByteString                  -- Prefix
        !(SA.SparseArray a)          -- Values
        !(SA.SparseArray (Trunk a))  -- Sub-tries
    deriving (Typeable, Eq)
    -- INVARIANT: subtries of Branch are not Empty.
    -- We could inline Empty everywhere in order to guarantee this invariant, but the added complexity doesn't seem worth it...


-- TODO: should we break the Branch apart into Partial and Full, the way unordered-containers does? what are the tradeoffs?


-- TODO: verify that the derived Eq instance is correct


-- TODO? add Ord instance like Data.Map?


{---------------------------------------------------------------
-- Smart constructors
---------------------------------------------------------------}

-- | Case elimination on 'Trie', as a higher-order function.
elimTrie :: (a -> Trunk a -> b) -> (Trunk a -> b) -> Trie a -> b
{-# INLINE elimTrie #-}
elimTrie accept reject (Accept v t) = accept v t
elimTrie accept reject (Reject   t) = reject   t


-- | Smart constructor for 'Trie' type (or for the 'Accept'
-- constructor, depending how you look at it).
mkTrie :: Maybe a -> Trunk a -> Trie a
{-# INLINE mkTrie #-}
mkTrie (Just v) t = Accept v t
mkTrie Nothing  t = Reject   t


-- | Prepend a (possible) 'Arc' to any trunk.
-- N.B., this does not verify the NonEmpty assertion on the argument
-- N.B., this does not clone the string to prune it; callers must do that.
arc :: NonEmptyByteString -> Maybe a -> Trunk a -> Trunk a
{-# INLINE arc #-}
arc s (Just v) = Arc s v
arc s Nothing  = prepend_ s


-- | Construct a (nominal) 'Branch', pruning it down to an 'Arc'
-- or 'Empty' if it's small enough.
branch :: ByteString -> SA.SparseArray a -> SA.SparseArray (Trunk a) -> Trunk a
{-# INLINE branch #-}
branch s vz tz =
    case (SA.viewSubsingleton vz, SA.viewSubsingleton tz) of
    (IsEmpty,         IsEmpty)                    -> Empty
    (IsEmpty,         IsSingleton _ t)            -> prepend s t
    (IsSingleton _ v, IsEmpty)                    -> Arc s v Empty
    (IsSingleton k v, IsSingleton k' t) | k == k' -> Arc s v t
    _                                             -> Branch s vz tz


-- | Create a binary branch of two tries, where @s1@ and @s2@ are disjoint.
branch2
    :: ByteString
    -> NonEmptyByteString -> Trie a
    -> NonEmptyByteString -> Trie a
    -> Trunk a
{-# INLINE branch2 #-}
branch2 s0 s1 vt1 s2 vt2 =
    case (vt1, vt2) of
    (Accept v1 t1, Accept v2 t2) -> go t1 t2 (SA.doubleton k1 v1 k2 v2)
    (Accept v1 t1, Reject    t2) -> go t1 t2 (SA.singleton k1 v1)
    (Reject    t1, Accept v2 t2) -> go t1 t2 (SA.singleton k2 v2)
    (Reject    t1, Reject    t2) -> go t1 t2  SA.empty
    where
    k1 = BSU.unsafeHead s1
    k2 = BSU.unsafeHead s2 
    go t1 t2 vz = Branch s0 vz (SA.doubleton k1 t1 k2 t2)


-- | Extract a non-empty trunk from a possibly 'Empty' trunk.
trunk2maybe :: Trunk a -> Maybe (Trunk a)
{-# INLINE trunk2maybe #-}
trunk2maybe Empty = Nothing
trunk2maybe t     = Just t


-- | Embed non-empty trunks into possibly 'Empty' trunks.
maybe2trunk :: Maybe (Trunk a) -> Trunk a
{-# INLINE maybe2trunk #-}
maybe2trunk Nothing  = Empty
maybe2trunk (Just t) = t


-- Saves a bit on allocation/sharing; though, technically, the call
-- to 'BS.append' will re-check for empty strings.
--
-- | Prepend a bytestring to a trunk, coalescing nodes as necessary.
prepend :: ByteString -> Trunk a -> Trunk a
{-# INLINE prepend #-}
prepend s
    | BS.null s = id
    | otherwise = prepend_ s


-- | Prepend a non-empty bytestring to a trunk, coalescing nodes
-- as necessary.
prepend_ :: NonEmptyByteString -> Trunk a -> Trunk a
{-# INLINE prepend_ #-}
prepend_ s0 Empty            = Empty
prepend_ s0 (Arc    s v  t)  = Arc    (s0 `BS.append` s) v t
prepend_ s0 (Branch s vz tz) = Branch (s0 `BS.append` s) vz tz
    -- TODO: 'BS.append' will recheck whether @s0@ is empty. We can avoid that extraneous check if we create an @unsafeAppend@...


-- | Prepend a non-empty bytestring to a trie, coalescing nodes
-- as necessary.
prependT_ :: NonEmptyByteString -> Trie a -> Trunk a
{-# INLINE prependT_ #-}
prependT_ s = elimTrie (Arc s) (prepend_ s)


-- | Discard the value for the empty string, and return the underlying
-- trunk.
trie2trunk :: Trie a -> Trunk a
{-# INLINE trie2trunk #-}
trie2trunk = elimTrie (flip const) id


{---------------------------------------------------------------
-- Trie instances: serialization et cetera
---------------------------------------------------------------}

-- This instance does not unveil the innards of our abstract type.
-- It doesn't emit truly proper Haskell code though, since ByteStrings
-- are printed as (ASCII) Strings, but that's not our fault. (Also
-- 'fromList' is in "Data.Trie" instead of here.)
instance (Show a) => Show (Trie a) where
    showsPrec p t =
        showParen (p > 10)
            $ ("fromList "++) . shows (toListBy (,) t)


{-
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
-}


-- TODO?? a Read instance? hrm... should I?


-- TODO: How does this compare to the new Binary instance for Data.Map? how does it compare to the instance for unordered-container's Data.HashMap?
instance (Binary a) => Binary (Trie a) where
    put (Accept v t) = do put (0 :: Word8); put v; put t
    put (Reject   t) = do put (1 :: Word8);        put t
    
    get = do
        tag <- get :: Get Word8
        case tag of
            0 -> Accept <$> get <*> get
            _ -> Reject <$> get


instance (Binary a) => Binary (Trunk a) where
    put Empty            = do put (0 :: Word8)
    put (Arc    s v  t)  = do put (1 :: Word8); put s; put v;  put t
    put (Branch s vz tz) = do put (2 :: Word8); put s; put vz; put tz
    
    get = do
        tag <- get :: Get Word8
        case tag of
            0 -> pure Empty
            1 -> Arc    <$> get <*> get <*> get
            _ -> Branch <$> get <*> get <*> get


instance (NFData a) => NFData (Trie a) where
    rnf (Accept v t) = rnf v `seq` rnf t
    rnf (Reject   t) = rnf t
    
instance (NFData a) => NFData (Trunk a) where
    rnf Empty            = ()
    rnf (Arc    _ v  t)  = rnf v  `seq` rnf t
    rnf (Branch _ vz tz) = rnf vz `seq` rnf tz


{---------------------------------------------------------------
-- Trie instances: Abstract Nonsense
---------------------------------------------------------------}

instance Functor Trie where
    fmap f (Accept v t) = Accept (f v) (fmap f t)
    fmap f (Reject   t) = Reject       (fmap f t)

instance Functor Trunk where
    fmap f = go
        where
        go Empty            = Empty
        go (Arc s v t)      = Arc    s (f v)      (go t)
        go (Branch s vz tz) = Branch s (f <$> vz) (go <$> tz)


-- HACK: it looks like GHC 7.10 changed the scoping rules for "where" clauses!! We have to explicitly unfold the case analysis on the second argument in order to get the recursive @go@ to be in scope for both branches, wtf?!

-- TODO: is there a class for this yet?
-- | A strict version of 'fmap'
fmap' :: (a -> b) -> Trie a -> Trie b
fmap' f = \t0 ->
        case t0 of
        Accept v t -> (Accept $! f v) (go t)
        Reject   t -> Reject          (go t)
    where
    go Empty            = Empty
    go (Arc    s v  t)  = (Arc s $! f v) (go t)
    go (Branch s vz tz) = Branch s (f <$!> vz) (go <$!> tz)
    
    (<$!>) = SA.map'
    -- TODO: should the recursive calls in go@Branch be strict?
    -- N.B., the recursive call for go@Arc is implicitly strict
    

-- TODO: newtype Keys = K Trie  ; instance Foldable Keys
-- TODO: newtype Assoc = A Trie ; instance Foldable Assoc

instance Foldable Trie where
    foldMap f (Accept v t) = f v `mappend` foldMap f t
    foldMap f (Reject   t) =               foldMap f t

instance Foldable Trunk where
    foldMap f = go
        where
        go Empty            = mempty
        go (Arc    _ v  t)  = f v `mappend` go t
        go (Branch _ vz tz) = foldMap f vz `mappend` foldMap go tz


instance Traversable Trie where
    traverse f (Accept v t) = Accept <$> f v <*> traverse f t
    traverse f (Reject   t) = Reject <$>         traverse f t

instance Traversable Trunk where
    traverse f = go
        where
        go Empty            = pure Empty
        go (Arc    s v  t)  = Arc    s <$> f v <*> go t
        go (Branch s vz tz) = Branch s <$> traverse f vz <*> traverse go tz


{-
-- TODO: is there a simpler version not dependant on the weird Monad instance?
instance Applicative Trie where
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
instance Monad Trie where
    return v = Accept v Empty
    
    Accept v t >>= f  = mkTrie (f v `unionL` go t)
    Reject   t >>= f  = reject (go t)
        where
        go Empty            = Empty
        -- TODO: fix this so that it actually works...
        go (Arc    s v  t)  = arc s (f v) (go t)
        go (Branch s vz tz) = branch s (f <&> vz) (go <&> tz)
-}


-- This instance is more sensible than Data.IntMap and Data.Map's
-- TODO: use semigroup instead (?)
-- TODO: newtypes for left/right biased unions, a~la IntMap/Map
instance (Monoid a) => Monoid (Trie a) where
    mempty  = empty
    mappend = mergeBy $ \x y -> Just (x `mappend` y)


{-
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
-}


{---------------------------------------------------------------
-- Extra mapping functions
---------------------------------------------------------------}

-- TODO: when reconstructing keys should we be strict or not??

-- | Apply a function to all values, potentially removing them.
filterMap :: (a -> Maybe b) -> Trie a -> Trie b
filterMap f = \t0 ->
        case t0 of
        Accept v t -> mkTrie (f v) (go t)
        Reject   t -> Reject       (go t)
    where
    go Empty            = Empty
    go (Arc    s v  t)  = arc s (f v) (go t)
    go (Branch s vz tz) =
        branch s (SA.filterMap f vz)
            (SA.filterMap (trunk2maybe . go) tz)

-- The inline pragma is to avoid warnings about the rules possibly
-- not firing
{-# NOINLINE [1] filterMap #-}
{-# RULES
-- These ones are probably useless...
"filterMap Just"
        filterMap Just = id
"filterMap (Just . f)"
    forall f.
        filterMap (\x -> Just (f x)) = fmap f
"filterMap (const Nothing)"
        filterMap (\_ -> Nothing) = const empty

-- These ones are probably more worthwhile...
"filterMap . filterMap"
    forall f g xs.
        filterMap f (filterMap g xs) = filterMap (\x -> f =<< g x) xs
"filterMap . fmap"
    forall f g xs.
        filterMap f (fmap g xs) = filterMap (\x -> f (g x)) xs
"filterMap . fmap'"
    forall f g xs.
        filterMap f (fmap' g xs) = filterMap (\x -> f $! g x) xs
"fmap . filterMap"
    forall f g xs.
        fmap f (filterMap g xs) = filterMap (\x -> f <$> g x) xs
"fmap' . filterMap"
    forall f g xs.
        fmap' f (filterMap g xs) =
            filterMap (\x -> case g x of {Just y -> Just $! f y; _ -> Nothing}) xs
    #-}

-- TODO: (?) use a large buffer for the bytestring and overwrite it in place, only copying it off for handing to @f@...? Benchmark it; also Builder stuff
-- TODO: just use contextualMapBy and ignore the Trunk argument?
-- | Generic version of 'fmap'. This function is notably more
-- expensive than 'fmap' or 'filterMap' because we have to reconstruct
-- the keys.
mapBy :: (ByteString -> a -> Maybe b) -> Trie a -> Trie b
mapBy f = \t0 ->
        case t0 of
        Accept v t -> mkTrie (f BS.empty v) (go BS.empty t)
        Reject   t -> Reject                (go BS.empty t)
    where
    go !_ Empty        = Empty
    go s0 (Arc s v t)  =
        let !s' = BS.append s0 s in
        arc s (f s' v) (go s' t)
    go s0 (Branch s vz tz) =
        let !s' = BS.append s0 s in
        branch s (SA.filterMap (f s') vz)
            (SA.filterMap (trunk2maybe . go s') tz)


-- | A variant of 'fmap' which provides access to the subtrie rooted
-- at each value.
contextualMap :: (a -> Trunk a -> b) -> Trie a -> Trie b
contextualMap f = \t0 ->
        case t0 of
        Accept v t -> Accept (f v t) (go t)
        Reject   t -> Reject         (go t)
    where
    go Empty            = Empty
    go (Arc    s v  t)  = Arc s (f v t) (go t)
    go (Branch s vz tz) = Branch s (SA.rzipWith_ f2 f1 tz vz) (fmap go tz)
    
    f1   v = f v Empty
    f2 t v = f v t


-- | A variant of 'contextualMap' which applies the function strictly.
contextualMap' :: (a -> Trunk a -> b) -> Trie a -> Trie b
contextualMap' f = \t0 ->
        case t0 of
        Accept v t -> (Accept $! f v t) (go t)
        Reject   t -> Reject            (go t)
    where
    go Empty            = Empty
    go (Arc    s v  t)  = (Arc s $! f v t) (go t)
    go (Branch s vz tz) = Branch s (SA.rzipWith'_ f2 f1 tz vz) (SA.map' go tz)
        -- TODO: should that SA.map be strict or not?
    
    f1   v = f v Empty
    f2 t v = f v t


-- | A contextual variant of 'filterMap'.
contextualFilterMap :: (a -> Trunk a -> Maybe b) -> Trie a -> Trie b
contextualFilterMap f = \t0 ->
        case t0 of
        Accept v t -> mkTrie (f v t) (go t)
        Reject   t -> Reject         (go t)
    where
    go Empty            = Empty
    go (Arc    s v  t)  = arc s (f v t) (go t)
    go (Branch s vz tz) =
        branch s (SA.rzipFilter_ f2 f1 tz vz)
            (SA.filterMap (trunk2maybe . go) tz)
    
    f1   v = f v Empty
    f2 t v = f v t


-- TODO: (?) use a large buffer for the bytestring and overwrite it in place, only copying it off for handing to @f@...? Benchmark it; also Builder stuff
-- | A contextual variant of 'mapBy'. Again note that this is
-- expensive since we must reconstruct the keys.
contextualMapBy
    :: (ByteString -> a -> Trunk a -> Maybe b) -> Trie a -> Trie b
contextualMapBy f = \t0 ->
        case t0 of
        Accept v t -> mkTrie (f BS.empty v t) (go BS.empty t)
        Reject   t -> Reject                  (go BS.empty t)
    where
    go !_ Empty            = Empty
    go s0 (Arc    s v  t)  =
        let !s' = BS.append s0 s in
        arc s (f s' v t) (go s' t)
    go s0 (Branch s vz tz) =
        error "contextualMapBy: unimplemented" {-
        let !s' = BS.append s0 s in
        branch s (SA.rzipFilter_ (f2 s') (f1 s') vz tz)
            (SA.filterMap (trunk2maybe . go s') tz)
        -}
    
    f1 s0   v = f s0 v Empty
    f2 s0 t v = f s0 v t


{---------------------------------------------------------------
-- Basic functions
---------------------------------------------------------------}

-- | /O(1)/, The empty trie.
empty :: Trie a
{-# INLINE empty #-}
empty = Reject Empty


-- | /O(1)/, Is the trie empty?
null :: Trie a -> Bool
{-# INLINE null #-}
null (Reject Empty) = True
null _              = False


-- | /O(1)/, Construct a singleton trie.
singleton :: ByteString -> a -> Trie a
{-# INLINE singleton #-}
singleton s v
    | BS.null s = Accept v Empty
    | otherwise = Reject (Arc s v Empty) -- TODO: clone @s@ to trim it!


-- TODO: should we offer @subsingleton s = maybe empty (singleton s)@ ?


-- | /O(n)/, Get count of elements in trie.
size  :: Trie a -> Int
size = \t0 ->
        case t0 of
        Accept _ t -> go 1 t
        Reject   t -> go 0 t
    where
    go !z Empty            = z
    go  z (Arc    _ _  t)  = go (z+1) t
    go  z (Branch _ vz tz) = foldl' go (z + length vz) tz


{---------------------------------------------------------------
-- Conversion functions 
---------------------------------------------------------------}

-- TODO: when reconstructing keys should we be strict or not??


-- | Convert a trie into a list (in key-sorted order) using a
-- function, folding the list as we go.
foldrWithKey :: (ByteString -> a -> b -> b) -> b -> Trie a -> b
foldrWithKey f = flip (start BS.empty)
    where
    start !s0 (Accept v t) z = f s0 v (go s0 t z)
    start  s0 (Reject   t) z =         go s0 t z
    
    go !_ Empty            z = z
    go s0 (Arc    s v  t)  z = f s' v (go s' t z) where !s' = BS.append s0 s
    go s0 (Branch s vz tz) z =
        SA.foldrWithKey' (start . appendSnoc s0 s) z
            (SA.unionWith_ (\v -> Accept v Empty) Accept Reject vz tz)


-- | Convert a trie into a list (in key-sorted order) using a
-- function, folding the list as we go.
foldrWithKey' :: (ByteString -> a -> b -> b) -> b -> Trie a -> b
foldrWithKey' f = flip (start BS.empty)
    where
    start !s0 (Accept v t) !z = f s0 v $! go s0 t z
    start  s0 (Reject   t)  z =           go s0 t z
    
    go !_ Empty            !z = z
    go s0 (Arc    s v  t)   z = f s' v $! go s' t z where !s' = BS.append s0 s
    go s0 (Branch s vz tz)  z =
        SA.foldrWithKey' (start . appendSnoc s0 s) z
            (SA.unionWith_ (\v -> Accept v Empty) Accept Reject vz tz)
    

-- cf Data.ByteString.unpack
-- <http://hackage.haskell.org/packages/archive/bytestring/0.9.1.4/doc/html/src/Data-ByteString.html>
--
-- | Convert a trie into a list using a function. Resulting values
-- are in key-sorted order.
toListBy :: (ByteString -> a -> b) -> Trie a -> [b]
#if !defined(__GLASGOW_HASKELL__)
-- TODO: should probably inline foldrWithKey
-- TODO: compare performance of that vs both this and the GHC version
toListBy f t = foldrWithKey (((:) .) . f) [] t
#else
-- Written with 'build' to enable the build\/foldr fusion rules.
toListBy f t = build (toListByFB f t)
{-# INLINE toListBy #-}

-- TODO: should probably have a specialized version for strictness,
-- and a rule to rewrite generic lazy version into it. As per
-- Data.ByteString.unpack and the comments there about strictness
-- and fusion.
toListByFB :: (ByteString -> a -> b) -> Trie a -> (b -> c -> c) -> c -> c
toListByFB f t cons nil = foldrWithKey ((cons .) . f) nil t
{-# INLINE [0] toListByFB #-}
#endif


{---------------------------------------------------------------
-- Query functions (just recurse)
---------------------------------------------------------------}

-- Ideologically speaking this is true, but we can't actually
-- abstract over k; and besides, it's not always a good idea to
-- duplicate the continuation like that.
{- RULES
"CPS:lookupBy_"
    forall k accept reject s t.
        k (lookupBy_ accept reject s t) =
            lookupBy_ ((k .) . accept) (k . reject) s t
    -}


-- | Does a string have a value in the trie?
--
-- > member = (isJust .) . lookup
member :: ByteString -> Trie a -> Bool
member = lookupBy_ (\_ _ -> True) (const False)
{-# INLINE member #-}
{- RULES
-- Alas, we can't define a rule for the built-in if_then_else_
"ifThenElse/member"
    forall true false s t.
        ifThenElse (member s t) true false =
            lookupBy_ (\_ _ -> true) (const false) s t
    -}


-- | Return the value associated with a string, if it exists.
--
-- > lookup = lookupBy const
lookup :: ByteString -> Trie a -> Maybe a
lookup = lookupBy_ (\v _ -> Just v) (const Nothing)
{-# INLINE lookup #-}
{- RULES
-- Alas, we can't define a rule for the built-in case_of_
"maybe/lookup"
    forall just nothing s t.
        maybe just nothing (lookup s t) =
            lookupBy_ (\v _ -> just v) (const nothing) s t
    -}



-- | Return the quotient trie containing the keys beginning with a
-- prefix. That is, the following definition is satisfied:
--
-- > lookup s2 (submap s1 t) = if BS.isPrefixOf s1 s2 then lookup s2 t else False
--
submap :: ByteString -> Trie a -> Trie a
submap s
    | BS.null s = id
    | otherwise = Reject . lookupBy_ (Arc s) (prepend_ s) s
{-# INLINE submap #-}


-- | Return the subtrie rooted at a prefix. That is, the following
-- definition is satisfied:
--
-- > lookup s2 (subtrie s1 t) = lookup (s1 `BS.append` s2) t
--
subtrie :: ByteString -> Trie a -> Trie a
subtrie = lookupBy_ Accept Reject
{-# INLINE subtrie #-}
{- RULES
-- Alas, we can't define a rule for the built-in case_of_
"elimTrie/subtrie"
    forall accept reject s t.
        elimTrie accept reject (subtrie s t) =
            lookupBy_ accept reject s t
    -}


-- | Generic function to find a value (if it exists) and the subtrie
-- rooted at the prefix. If you're going to do case analysis on the
-- 'Maybe' in order to decide what to do next, then you should use
-- 'lookupBy_' instead.
lookupBy :: (Maybe a -> Trunk a -> b) -> ByteString -> Trie a -> b
lookupBy f = lookupBy_ (f . Just) (f Nothing)
{-# INLINE lookupBy #-}


-- TODO: we might need to hoist go out as unsafeLookupBy_ in order
-- to avoid BS.null check of start which is redundant for the
-- call-site in 'submap'
--
-- | Generic function to find a value (if it exists) and the subtrie
-- rooted at the prefix. Like 'lookupBy' but avoids constructing
-- 'Maybe' terms.
lookupBy_ :: (a -> Trunk a -> b) -> (Trunk a -> b) -> ByteString -> Trie a -> b
lookupBy_ accept reject = start
    where
    start q
        | BS.null q = elimTrie accept reject
        | otherwise = go q . trie2trunk
    
    go !_ Empty       = reject Empty
    go q  (Arc s v t) =
        let (_,q',s') = breakMaximalPrefix q s in
        case (BS.null q', BS.null s') of
        (True,  True)  -> accept v t
        (True,  False) -> reject (prepend_ s' t)
        (False, True)  -> go q' t
        (False, False) -> reject Empty
    go q (Branch s vz tz) =
        let (_,q',s') = breakMaximalPrefix q s in
        case BS.uncons q' of
        Nothing     -> reject (Branch s' vz tz)
        Just (w,ws) ->
            if BS.null s'
            then
                if BS.null ws
                then maybe reject accept (SA.lookup' w vz)
                        (maybe2trunk (SA.lookup' w tz))
                else maybe (reject Empty) (go ws) (SA.lookup' w tz)
            else reject Empty
    


-- TODO: would it be worth it to have a variant like 'lookupBy_' which takes the two continuations?

-- | Given a query, find the longest prefix with an associated value
-- in the trie, returning the length of that prefix and the associated
-- value.
--
-- This function may not have the most useful return type. For a
-- version that returns the prefix itself as well as the remaining
-- string, see @match@ in "Data.Trie.ArrayMapped.Base".
match_ :: Trie a -> ByteString -> Maybe (Int, a)
match_ = flip start
    where
    start q (Accept v t)
        | BS.null q = Just (0,v)
        | otherwise = goJust 0 v 0 q t
    start q (Reject t)
        | BS.null q = Nothing
        | otherwise = goNothing 0 q t
    
    goNothing !_ !_ Empty       = Nothing
    goNothing n  q  (Arc s v t) =
        let (p,q',s') = breakMaximalPrefix q s in
        if BS.null s'
        then
            let n' = n + BS.length p in n' `seq`
            if BS.null q'
            then Just (n',v)
            else goJust n' v n' q' t
        else Nothing
    goNothing n q (Branch s vz tz) =
        let (p,q',s') = breakMaximalPrefix q s in
        case BS.uncons q' of
        Nothing     -> Nothing
        Just (w,ws) ->
            let n' = n + BS.length p + 1 in n' `seq`
            case (BS.null ws, SA.lookup' w tz) of
            (False, Just t) -> 
                case SA.lookup' w vz of
                Nothing -> goNothing   n' ws t
                Just v  -> goJust n' v n' ws t
            _           -> (,) n' <$> SA.lookup' w vz

    goJust !n0 v0 !_ !_ Empty       = Just (n0,v0)
    goJust  n0 v0 n  q  (Arc s v t) =
        let (p,q',s') = breakMaximalPrefix q s in
        if BS.null s'
        then
            let n' = n + BS.length p in n' `seq`
            if BS.null q'
            then Just (n',v)
            else goJust n' v n' q' t
        else Just (n0,v0)
    goJust n0 v0 n q (Branch s vz tz) =
        let (p,q',s') = breakMaximalPrefix q s in
        case BS.uncons q' of
        Nothing     -> Just (n0,v0)
        Just (w,ws) ->
            let n' = n + BS.length p + 1 in n' `seq`
            case (BS.null ws, SA.lookup' w tz) of
            (False, Just t) -> 
                case SA.lookup' w vz of
                Nothing -> goJust n0 v0 n' ws t
                Just v  -> goJust n' v  n' ws t
            _ ->
                case SA.lookup' w vz of
                Nothing -> Just (n0,v0)
                Just v  -> Just (n',v)


{-
-- | Given a query, find all prefixes with associated values in the
-- trie, returning their lengths and values. This function is a
-- good producer for list fusion.
--
-- This function may not have the most useful return type. For a
-- version that returns the prefix itself as well as the remaining
-- string, see @matches@ in "Data.Trie.ArrayMapped.Base".
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
        where ...
-}


{---------------------------------------------------------------
-- Single-value modification functions (recurse and clone spine)
---------------------------------------------------------------}

alterSubtrie :: (Trie a -> Trie a) -> ByteString -> Trie a -> Trie a
alterSubtrie f = alterSubtrie_ ((f .) . Accept) (f . Reject)
{-# INLINE alterSubtrie #-}


alterSubtrie_
    :: (a -> Trunk a -> Trie a) -> (Trunk a -> Trie a)
    -> ByteString -> Trie a -> Trie a
alterSubtrie_ accept reject = start
    where
    start q (Accept v t)
        | BS.null q = accept v t
        | otherwise = Accept v (go q t)
    start q (Reject t)
        | BS.null q = reject t
        | otherwise = Reject (go q t)

    -- [#1] N.B., since @q@ isn't null, by our invariant, therefore
    -- we can't have both @p@ and @q'@ be null. And since @q'@ is
    -- null, by the guard, therefore @p@ mustn't be; and therefore
    -- 'prependT_' is safe to call.
    go !q Empty       = prependT_ q (reject Empty)
    go  q (Arc s v t) =
        let (p,q',s') = breakMaximalPrefix q s in
        case (BS.null q', BS.null s') of
        (True,  True)  -> prependT_ p (accept v t) -- p == q == s
        (True,  False) -> prependT_ p (reject $ Arc s' v t) -- #1
        (False, True)  -> Arc s v (go q' t)
        (False, False) -> Branch p SA.empty $ SA.doubleton
                            (BSU.unsafeHead s') (Arc s' v t)
                            (BSU.unsafeHead q') (prependT_ q' $ reject Empty)
    go  q (Branch s vz tz) =
        let (p,q',s') = breakMaximalPrefix q s in
        case BS.uncons q' of
        Nothing     -> prependT_ p (reject $ Branch s' vz tz)
        Just (w,ws) ->
            if BS.null s'
            then
                if BS.null ws
                then
                    case maybe reject accept (SA.lookup' w vz)
                        (maybe2trunk (SA.lookup' w tz))
                    of
                    Accept v' t' ->
                        Branch s (SA.insert w v' vz) (SA.insert w t' tz)
                    Reject    t' ->
                        Branch s (SA.remove w vz) (SA.insert w t' tz)
                else prependT_ p $
                    case SA.lookup' w tz of
                    Nothing -> SA.insert w (prependT_ ws $ reject Empty) tz
                    Just t' -> go ws t'
            else branch2 p s' (Reject $ Branch BS.empty vz tz)
                           q' (reject Empty)



adjustWithKey :: (ByteString -> a -> a) -> ByteString -> Trie a -> Trie a
adjustWithKey f s = adjust (f s) s


-- | Alter the value associated with a given key. If the key is not
-- present, then the trie is returned unaltered. See 'alterBy' if
-- you are interested in inserting new keys or deleting old keys.
-- Because this function does not need to worry about changing the
-- trie structure, it is somewhat faster than 'alterBy'.
adjust :: (a -> a) -> ByteString -> Trie a -> Trie a
adjust f = start
    where
    start q t
        | BS.null q = elimTrie (Accept . f) (const t) t
        | otherwise = go q (trie2trunk t)

    go !q Empty       = Empty
    go  q (Arc s v t) =
        let (p,q',s') = breakMaximalPrefix q s in
        case (BS.null q', BS.null s') of
        (True,  True)  -> prepend_ p (Accept v t) -- p == q == s
        (True,  False) -> prepend_ p (Reject (prepend_ s' t))
        (False, True)  -> Arc s v (go q' t)
        (False, False) -> branch2 p s' (Accept v t) q' (Reject Empty)
    go  q (Branch s vz tz) =
        let (sh,q',s') = breakMaximalPrefix q s in
        error "adjust@Branch: unimplemented" -- TODO


----------------------------------------------------------------
----------------------------------------------------------------
----------------------------------------------------------------
----------------------------------------------------------------

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
alterBy f = alterBy_ (\k v mv t -> mkTrie (f k v mv) t)


-- | A variant of 'alterBy' which also allows modifying the sub-trie. 
alterBy_ :: (ByteString -> a -> Trie a -> Trie a)
         -> ByteString -> a -> Trie a -> Trie a
alterBy_ = error "alterBy_: unimplemented" -- TODO


-- | Alter the value associated with a given key. If the key is not
-- present, then the trie is returned unaltered. See 'alterBy' if
-- you are interested in inserting new keys or deleting old keys.
-- Because this function does not need to worry about changing the
-- trie structure, it is somewhat faster than 'alterBy'.
adjustBy :: (ByteString -> a -> a -> a)
         -> ByteString -> a -> Trie a -> Trie a
adjustBy = error "adjustBy: unimplemented" -- TODO


{---------------------------------------------------------------
-- Trie-combining functions
---------------------------------------------------------------}

-- TEST CASES: foldr (unionL . uncurry singleton) empty t
--             foldr (uncurry insert) empty t
--    where t = map (\s -> (pk s, 0))
--                  ["heat","hello","hoi","apple","appa","hell","appb","appc"]
--
-- | Combine two tries, using a function to resolve collisions.
-- This can only define the space of functions between union and
-- symmetric difference but, with those two, all set operations can
-- be defined (albeit inefficiently).
mergeBy :: (a -> a -> Maybe a) -> Trie a -> Trie a -> Trie a
mergeBy = error "mergeBy: unimplemented" -- TODO


mergeMaybe :: (a -> a -> Maybe a) -> Maybe a -> Maybe a -> Maybe a
{-# INLINE mergeMaybe #-}
mergeMaybe _ Nothing      Nothing  = Nothing
mergeMaybe _ Nothing mv1@(Just _)  = mv1
mergeMaybe _ mv0@(Just _) Nothing  = mv0
mergeMaybe f (Just v0)   (Just v1) = f v0 v1


{---------------------------------------------------------------
-- Priority-queue functions
---------------------------------------------------------------}

-- TODO: use a single large buffer for building up the string, since we're guaranteed not to share it.
minAssoc :: Trie a -> Maybe (ByteString, a)
minAssoc (Accept v _) = Just (BS.empty, v)
minAssoc (Reject   t) = go BS.empty t
    where
    go !_ Empty            = Nothing
    go s0 (Arc    s v  t)  = Just (BS.append s0 s, v)
    go s0 (Branch s vz tz) =
        case SA.toList vz of
        (k,v) : _ -> Just (appendSnoc s0 s k, v)
        []        ->
            -- N.B., this should only force mb if the recursion fails
            foldrWithKey
                (\k t mb -> go (appendSnoc s0 s k) t <|> mb) __impossible tz
        
    __impossible = error
        "Data.Trie.ArrayMapped.Internal.minAssoc: the impossible happened"
    (<|>) = error "minAssoc: unimplemented"


maxAssoc :: Trie a -> Maybe (ByteString, a)
maxAssoc = go BS.empty
    where
    go _ Empty                  = Nothing
    go q (Arc k (Just v) Empty) = Just (BS.append q k,v)
    go q (Arc k _        t)     = go   (BS.append q k) t
    go q (Branch _ _ _ r)       = go q r


mapView :: (Trie a -> Trie a)
        -> Maybe (ByteString, a, Trie a) -> Maybe (ByteString, a, Trie a)
mapView _ Nothing        = Nothing
mapView f (Just (k,v,t)) = Just (k,v, f t)


updateMinViewBy :: (ByteString -> a -> Maybe a)
                -> Trie a -> Maybe (ByteString, a, Trie a)
updateMinViewBy f = go BS.empty
    where
    go _ Empty              = Nothing
    go q (Arc k (Just v) t) = let q' = BS.append q k
                              in Just (q',v, arc k (f q' v) t)
    go q (Arc k Nothing  t) = mapView (arc k Nothing) (go (BS.append q k) t)
    go q (Branch p m l r)   = mapView (\l' -> branch p m l' r) (go q l)


updateMaxViewBy :: (ByteString -> a -> Maybe a)
                -> Trie a -> Maybe (ByteString, a, Trie a)
updateMaxViewBy f = go BS.empty
    where
    go _ Empty                  = Nothing
    go q (Arc k (Just v) Empty) = let q' = BS.append q k
                                  in Just (q',v, arc k (f q' v) Empty)
    go q (Arc k mv       t)     = mapView (arc k mv) (go (BS.append q k) t)
    go q (Branch p m l r)       = mapView (branch p m l) (go q r)

----------------------------------------------------------------
----------------------------------------------------------- fin.