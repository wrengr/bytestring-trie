-- To make GHC stop warning about the Prelude
{-# OPTIONS_GHC -Wall -fwarn-tabs -fno-warn-unused-imports #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- For list fusion on toListBy
{-# LANGUAGE CPP #-}

----------------------------------------------------------------
--                                                  ~ 2014.05.29
-- |
-- Module      :  Data.Trie.ArrayMapped.Internal
-- Copyright   :  Copyright (c) 2014 wren gayle romano
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
    , foldrWithKey, toListBy
    
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

import qualified Data.ByteString as S
import Data.Trie.ByteStringInternal
import Data.Trie.ArrayMapped.BitTwiddle

import Data.Binary

import Data.Monoid         (Monoid(..))
import Control.Monad       (liftM, liftM3, liftM4)
import Control.Monad       (ap)
import Control.Applicative (Applicative(..), (<$>))
import Data.Foldable       (Foldable(..))
import Data.Traversable    (Traversable(traverse))

#ifdef __GLASGOW_HASKELL__
import GHC.Exts (build)
#endif
----------------------------------------------------------------
----------------------------------------------------------------


{---------------------------------------------------------------
-- Type aliases and bit twiddling
---------------------------------------------------------------}

-- INVARIANT: this particular ByteString is not empty...
type NonEmptyByteString = ByteString 

-- Idea: bitWidth Bitmap == 2^bitsPerSubkey
type Bitmap = Word 

-- Idea: maxValue Shift == bitWidth Word
type Shift = Int 


bitsPerSubkey :: Int
bitsPerSubkey = 4


maxChildren :: Int
maxChildren = fromIntegral (1 `unsafeShiftL` bitsPerSubkey)


subkeyMask :: Bitmap
subkeyMask = 1 `unsafeShiftL` bitsPerSubkey - 1


sparseIndex :: Bitmap -> Bitmap -> Int
sparseIndex b m = popCount (b .&. (m - 1))


mask :: Word -> Shift -> Bitmap
mask w s = 1 `unsafeShiftL` index w s
{-# INLINE mask #-}


-- | Mask out the 'bitsPerSubkey' bits used for indexing at this level
-- of the tree.
index :: Hash -> Shift -> Int
index w s = fromIntegral (unsafeShiftR w s .&. subkeyMask)
{-# INLINE index #-}


-- | A bitmask with the 'bitsPerSubkey' least significant bits set.
fullNodeMask :: Bitmap
fullNodeMask = complement (complement 0 `unsafeShiftL` maxChildren)
{-# INLINE fullNodeMask #-}


{---------------------------------------------------------------
-- ByteString Array Mapped Trie datatype
---------------------------------------------------------------}

-- | A map from 'ByteString's to @a@. For all the generic functions,
-- note that tries are strict in the @Maybe@ but not in @a@.
--
-- The 'Monad' instance is strange. If a key @k1@ is a prefix of
-- other keys, then results from binding the value at @k1@ will
-- override values from longer keys when they collide. If this is
-- useful for anything, or if there's a more sensible instance, I'd
-- be curious to know.

data Trie a
    = Accept a !(Arc a)
    | Reject   !(Arc a)
    deriving (Typeable, Eq)


-- | Outgoing arcs from a node (aka 'Trie'). This data type
-- incorporates all non-root nodes. The root node must be distinguished
-- in order to accept the empty string.

data Trunk a
    = Empty
    | Arc
        {-# UNPACK #-} !NonEmptyByteString          -- Prefix
        a                                           -- Value
        !(Trunk a)                                  -- Sub-trie
    | Branch
        {-# UNPACK #-} !ByteString                  -- Prefix
        {-# UNPACK #-} !(SA.SparseArray a)          -- Values
        {-# UNPACK #-} !(SA.SparseArray (Trunk a))  -- Sub-tries
    deriving (Typeable, Eq)
    -- INVARIANT: if (Branch _ vz tz) then (vz `isSubarrayOf` ts)


-- TODO: should we break the Branch apart into the following? unordered-containers does so, but what are the tradeoffs?
--
--     | Branch !!ByteString !BranchBody
--
-- data BranchBody a
--     = Fan  (SA.SparseArray a) !(SA.SparseArray (Trunk a))
--     | Full (SA.SparseArray a) !(A.Array (Trunk a))
--
-- INVARIANT: The length of the last argument to 'Full' is 2^bitsPerSubkey


-- TODO: verify that the derived Eq instance is correct


-- TODO? add Ord instance like Data.Map?


{---------------------------------------------------------------
-- Smart constructors
---------------------------------------------------------------}

trie :: (a -> Trunk a -> b) -> (Trunk a -> b) -> Trie a -> b
{-# INLINE trie #-}
trie accept reject (Accept v t) = accept v t
trie accept reject (Reject   t) = reject   t


accept :: Maybe a -> Trunk a -> Trie a
{-# INLINE accept #-}
accept (Just v) t = Accept v t
accept Nothing  t = Reject   t


-- N.B., this does not verify the NonEmpty assertion on the argument
-- N.B., this does not clone the string to prune it; callers must do that.
arc :: NonEmptyByteString -> Maybe a -> Trunk a -> Trunk a
{-# INLINE arc #-}
arc s0 (Just v0) = Arc s0 v0
arc s0 Nothing   = prepend_ s0


-- Saves a bit on allocation/sharing; though, technically, the call to 'S.append' will re-check for empty strings.
prepend :: ByteString -> Trunk a -> Trunk a
{-# INLINE prepend #-}
prepend s0
    | S.null s0 = id
    | otherwise = prepend_ s0

prepend_ :: NonEmptyByteString -> Trunk a -> Trunk a
{-# INLINE prepend_ #-}
prepend_ s0 Empty            = Empty
prepend_ s0 (Arc    s v  t)  = Arc    (s0 `S.append` s) v t
prepend_ s0 (Branch s vz tz) = Branch (s0 `S.append` s) vz tz
    -- TODO: 'S.append' will recheck whether @s0@ is empty. We can avoid that extraneous check if we create an @unsafeAppend@...


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
            $ ("Data.Trie.fromList "++) . shows (toListBy (,) t)


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


-- TODO: is there a class for this yet?
-- | A strict version of 'fmap'
fmap' :: (a -> b) -> Trie a -> Trie b
fmap' f (Accept v t) = (Accept $! f v) (go t)
fmap' f (Reject   t) = Reject          (go t)
    where
    go Empty            = Empty
    go (Arc    s v  t)  = (Arc s $! f v) (go t)
    go (Branch s vz tz) = Branch s (f <$!> vz) (go <$> tz)
    

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
    
    Accept v t >>= f  = accept (f v `unionL` go t)
    Reject   t >>= f  = reject (go t)
        where
        go Empty                = Empty
        -- TODO: fix this so that it actually works...
        go (Arc s v t)          = arc s (f v) (go t)
        go (Branch s p q vs ts) = branch s p q (f <&> vs) (go <&> ts)
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


-- | Apply a function to all values, potentially removing them.
filterMap :: (a -> Maybe b) -> Trie a -> Trie b
filterMap f (Accept v t) = accept (f v) (go t)
filterMap f (Reject   t) = Reject       (go t)
    where
    go Empty            = Empty
    go (Arc    s v  t)  = arc s (f v) (go t)
    go (Branch s vz tz) = ... branch s (filterMap f vz) (filterMap go tz)


-- | Generic version of 'fmap'. This function is notably more
-- expensive than 'fmap' or 'filterMap' because we have to reconstruct
-- the keys.
mapBy :: (ByteString -> a -> Maybe b) -> Trie a -> Trie b
mapBy f (Accept v t) = accept (f S.empty v) (go S.empty t)
mapBy f (Reject   t) = go S.empty t
    where
    go !_ Empty            = Empty
    go s0 (Arc    s v  t)  = arc s (f s' v) (go s' t) where !s' = S.append s0 s
    go s0 (Branch s vz tz) = ... -- TODO


-- | A variant of 'fmap' which provides access to the subtrie rooted
-- at each value.
contextualMap :: (a -> Trunk a -> b) -> Trie a -> Trie b
contextualMap f (Accept v t) = Accept (f v t) (go t)
contextualMap f (Reject   t) = Reject         (go t)
    where
    go Empty            = Empty
    go (Arc    s v  t)  = Arc s (f v t) (go t)
    go (Branch s vz tz) = ... -- TODO


-- | A variant of 'contextualMap' which applies the function strictly.
contextualMap' :: (a -> Trunk a -> b) -> Trie a -> Trie b
contextualMap' f (Accept v t) = (Accept $! f v t) (go t)
contextualMap' f (Reject   t) = Reject            (go t)
    where
    go Empty            = Empty
    go (Arc    s v  t)  = (Arc s $! f v t) (go t)
    go (Branch s vz tz) = ... -- TODO


-- | A contextual variant of 'filterMap'.
contextualFilterMap :: (a -> Trunk a -> Maybe b) -> Trie a -> Trie b
contextualFilterMap f (Accept v t) = accept (f v t) (go t)
contextualFilterMap f (Reject   t) = Reject (go t)
    where
    go Empty            = Empty
    go (Arc    s v  t)  = arc s (f v t) (go t)
    go (Branch s vz tz) = ... -- TODO


-- | A contextual variant of 'mapBy'. Again note that this is
-- expensive since we must reconstruct the keys.
contextualMapBy :: (ByteString -> a -> Trunk a -> Maybe b) -> Trie a -> Trie b
contextualMapBy f (Accept v t) = accept (f S.empty v t) (go S.empty t)
contextualMapBy f (Reject   t) = go S.empty t
    where
    go !_ Empty            = Empty
    go s0 (Arc    s v  t)  = arc s (f s' v t) (go s' t) where !s' = S.append s0 s
    go s0 (Branch s vz tz) = ... -- TODO


{---------------------------------------------------------------
-- Error messages
---------------------------------------------------------------}

-- TODO: shouldn't we inline the logic and just NOINLINE the string constant? There are only three use sites, which themselves aren't inlined...
errorLogHead :: String -> ByteString -> ByteStringElem
{-# NOINLINE errorLogHead #-}
errorLogHead fn q
    | S.null q  = error $ "Data.Trie.Internal." ++ fn ++": found null subquery"
    | otherwise = S.head q


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
    | S.null s  = Accept v Empty
    | otherwise = Reject (Arc s v Empty) -- TODO: clone @s@ to trim it!


-- TODO: should we offer (\s -> maybe empty (singleton s)) ?


-- | /O(n)/, Get count of elements in trie.
size  :: Trie a -> Int
size (Accept _ t0) = go 1 t0
size (Reject   t0) = go 0 t0
    where
    go !z Empty            = z
    go  z (Arc    _ _  t)  = go (z+1) t
    go  z (Branch _ vz tz) = foldl' go (z + length vz) tz


{---------------------------------------------------------------
-- Conversion functions 
---------------------------------------------------------------}

-- | Convert a trie into a list (in key-sorted order) using a
-- function, folding the list as we go.
foldrWithKey :: (ByteString -> a -> b -> b) -> b -> Trie a -> b
foldrWithKey f = flip (start S.empty)
    where
    start !s0 (Accept v t) z = f s0 v (go s0 t z)
    start  s0 (Reject   t) z =         go s0 t z
    
    go !_ Empty            z = z
    go s0 (Arc    s v  t)  z = f s' v (go s' t z) where !s' = S.append s0 s
    go s0 (Branch s vz tz) z =
        -- BUG: we need to snoc the byte for the key before calling @start@!
        -- TODO: should we define a fused append/snoc operation? Would that help reduce copying?
        foldr' (start (S.append s0 s)) z
            (zipSubarrayWith_ Accept Reject vz tz)


-- | Convert a trie into a list (in key-sorted order) using a
-- function, folding the list as we go.
foldrWithKey' :: (ByteString -> a -> b -> b) -> b -> Trie a -> b
foldrWithKey' f = flip (start S.empty)
    where
    start !s0 (Accept v t) !z = f s0 v $! go s0 t z
    start  s0 (Reject   t)  z =           go s0 t z
    
    go !_ Empty            !z = z
    go s0 (Arc    s v  t)   z = f s' v $! go s' t z where !s' = S.append s0 s
    go s0 (Branch s vz tz)  z =
        -- BUG: we need to snoc the byte for the key before calling @start@!
        -- TODO: should we define a fused append/snoc operation? Would that help reduce copying?
        foldr' (start (S.append s0 s)) z
            (zipSubarrayWith_ Accept Reject vz tz)
    

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
-- > lookup s2 (submap s1 t) = if S.isPrefixOf s1 s2 then lookup s2 t else False
--
submap :: ByteString -> Trie a -> Trie a
submap s = lookupBy_ ((prepend s .) . Accept) (prepend s . Reject) s
-- submap s = prepend s . subtrie s
-- BUG: prepend takes a Trie not an Arc
{-# INLINE submap #-}


-- | Return the subtrie rooted at a prefix. That is, the following
-- definition is satisfied:
--
-- > lookup s2 (subtrie s1 t) = lookup (s1 `S.append` s2) t
--
subtrie :: ByteString -> Trie a -> Trie a
subtrie = lookupBy_ Accept Reject
{-# INLINE subtrie #-}
{- RULES
-- Alas, we can't define a rule for the built-in case_of_
"trie/subtrie"
    forall accept reject s t.
        trie accept reject (subtrie s t) =
            lookupBy_ accept reject s t
    -}


-- | Generic function to find a value (if it exists) and the subtrie
-- rooted at the prefix.
lookupBy :: (Maybe a -> Trunk a -> b) -> ByteString -> Trie a -> b
lookupBy f = lookupBy_ (f . Just) (f Nothing)
{-# INLINE lookupBy #-}


-- | Generic function to find a value (if it exists) and the subtrie
-- rooted at the prefix. Like 'lookupBy' but avoids constructing 'Maybe' terms
lookupBy_ :: (a -> Trunk a -> b) -> (Trunk a -> b) -> ByteString -> Trie a -> b
lookupBy_ accept reject = start
    where
    start s0
        | S.null s0 = trie accept reject
        | otherwise = go s0 . trie (flip const) id
    
    go !_ Empty       = reject Empty
    go s0 (Arc s v t) =
        let (_, s0', s') = breakMaximalPrefix s0 s in
        case (S.null s0', S.null s') of
        (True,  True)  -> accept v t
        (True,  False) -> reject (prepend_ s' t)
        (False, True)  -> go s0' t
        (False, False) -> reject Empty
    go s0 (Branch s vz tz) =
        let (_, s0', s') = breakMaximalPrefix s0 s in
        case S.uncons s0' of
        Nothing     -> reject (Branch s' vz tz)
        Just (w,ws) ->
            case (S.null ws, lookup w vz, lookup w tz) of
            (True,  Nothing, Nothing) -> reject Empty
            (True,  Nothing, Just t)  -> reject t
            (True,  Just v,  Nothing) -> __impossible
            (True,  Just v,  Just t)  -> accept v t
            (False, _,       Nothing) -> reject Empty
            (False, _,       Just t)  -> go ws t

    __impossible = error
        "Data.Trie.ArrayMapped.Internal.lookupBy_: the impossible happened"


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
    start s0
        | S.null s0 = trie accept reject
        | otherwise = go s0 . trie (flip const) id

    go !s0 Empty       = prepend s0 (reject Empty)
    go  s0 (Arc s v t) =
        let (sh, s0', s') = breakMaximalPrefix s0 s in
        case (S.null s0', S.null s') of
        (True,  True)  -> prepend_ s  (accept v t)
        (True,  False) -> prepend_ sh (reject (prepend_ s' t))
        (False, True)  -> trie __impossible (Arc s v) (go s0' t)
        (False, False) -> merge2 sh (Arc s' v t) (prepend s0' (reject Empty))
    go  s0 (Branch s vz tz) =
        let (_, s0', s') = breakMaximalPrefix s0 s in
        ... -- TODO


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
    start s0 t0
        | S.null s0 = trie (Accept . f) (const t0) t0
        | otherwise = go s0 (trie (flip const) id t0)

    go !s0 Empty       = Empty
    go  s0 (Arc s v t) =
        let (_, s0', s') = breakMaximalPrefix s0 s in
        case (S.null s0', S.null s') of
        (True,  True)  -> prepend_ s  (accept v t)
        (True,  False) -> prepend_ sh (reject (prepend_ s' t))
        (False, True)  -> trie __impossible (Arc s v) (go s0' t)
        (False, False) -> merge2 sh (Arc s' v t) (prepend s0' (reject Empty))
    go  s0 (Branch s vz tz) =
        let (_, s0', s') = breakMaximalPrefix s0 s in
        ... -- TODO


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
alterBy f = alterBy_ (\k v mv t -> (f k v mv, t))
-- TODO: use GHC's 'inline' function so that this gets specialized away.
-- TODO: benchmark to be sure that this doesn't introduce unforseen performance costs because of the uncurrying etc.


-- | A variant of 'alterBy' which also allows modifying the sub-trie. 
alterBy_ :: (ByteString -> a -> Maybe a -> Trie a -> (Maybe a, Trie a))
         -> ByteString -> a -> Trie a -> Trie a
alterBy_ f_ q_ x_
    | S.null q_ = alterEpsilon
    | otherwise = go q_
    where
    f         = f_ q_ x_
    nothing q = uncurry (arc q) (f Nothing Empty)
    
    alterEpsilon t_@Empty                    = uncurry (arc q_) (f Nothing t_)
    alterEpsilon t_@(Branch _ _ _ _)         = uncurry (arc q_) (f Nothing t_)
    alterEpsilon t_@(Arc k mv t) | S.null k  = uncurry (arc q_) (f mv      t)
                                 | otherwise = uncurry (arc q_) (f Nothing t_)
    
    
    go q Empty            = nothing q
    
    go q t@(Branch p m l r)
        | nomatch qh p m  = branchMerge p t  qh (nothing q)
        | zero qh m       = branch p m (go q l) r
        | otherwise       = branch p m l (go q r)
        where
        qh = errorLogHead "alterBy" q
    
    go q t_@(Arc k mv t) =
        let (p,k',q') = breakMaximalPrefix k q in
        case (not $ S.null k', S.null q') of
        (True,  True)  -> -- add node to middle of arc
                          uncurry (arc p) (f Nothing (Arc k' mv t))
        (True,  False) ->
            case nothing q' of
            Empty -> t_ -- Nothing to add, reuse old arc
            l     -> arc' (branchMerge (getPrefix l) l (getPrefix r) r)
                    where
                    r = Arc k' mv t
                    
                    -- inlined version of 'arc'
                    arc' | S.null p  = id
                         | otherwise = Arc p Nothing
                    
        (False, True)  -> uncurry (arc k) (f mv t)
        (False, False) -> arc k mv (go q' t)


-- | Alter the value associated with a given key. If the key is not
-- present, then the trie is returned unaltered. See 'alterBy' if
-- you are interested in inserting new keys or deleting old keys.
-- Because this function does not need to worry about changing the
-- trie structure, it is somewhat faster than 'alterBy'.
adjustBy :: (ByteString -> a -> a -> a)
         -> ByteString -> a -> Trie a -> Trie a
adjustBy f_ q_ x_
    | S.null q_ = adjustEpsilon
    | otherwise = go q_
    where
    f = f_ q_ x_
    
    adjustEpsilon (Arc k (Just v) t) | S.null k = Arc k (Just (f v)) t
    adjustEpsilon t_                            = t_
    
    go _ Empty            = Empty
    
    go q t@(Branch p m l r)
        | nomatch qh p m  = t
        | zero qh m       = Branch p m (go q l) r
        | otherwise       = Branch p m l (go q r)
        where
        qh = errorLogHead "adjustBy" q
    
    go q t_@(Arc k mv t) =
        let (_,k',q') = breakMaximalPrefix k q in
        case (not $ S.null k', S.null q') of
        (True,  True)  -> t_ -- don't break arc inline
        (True,  False) -> t_ -- don't break arc branching
        (False, True)  -> Arc k (liftM f mv) t
        (False, False) -> Arc k mv (go q' t)


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
        t1_@(Branch _ _ _ _)
        | S.null k0              = arc k0 mv0 (go t0 t1_)
    mergeBy'
        t0_@(Branch _ _ _ _)
        (Arc k1 mv1@(Just _) t1)
        | S.null k1              = arc k1 mv1 (go t1 t0_)
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
    
    -- We combine these branches of 'go' in order to clarify where the definitions of 'p0', 'p1', 'm'', 'p'' are relevant. However, this may introduce inefficiency in the pattern matching automaton...
    -- TODO: check. And get rid of 'go'' if it does.
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
                else let {-# INLINE arcMerge #-}
                         arcMerge mv' t1' t2' = arc pre mv' (go t1' t2')
                     in case (S.null k0', S.null k1') of
                         (True, True)  -> arcMerge (mergeMaybe f mv0 mv1) t0 t1
                         (True, False) -> arcMerge mv0 t0 (Arc k1' mv1 t1)
                         (False,True)  -> arcMerge mv1 (Arc k0' mv0 t0) t1
                         (False,False) -> arcMerge Nothing (Arc k0' mv0 t0)
                                                           (Arc k1' mv1 t1)
        go' (Arc _ _ _)
            (Branch _p1 m1 l r)
            | nomatch p0 p1 m1 = branchMerge p1 t1_  p0 t0_
            | zero p0 m1       = branch p1 m1 (go t0_ l) r
            | otherwise        = branch p1 m1 l (go t0_ r)
        go' (Branch _p0 m0 l r)
            (Arc _ _ _)
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


{---------------------------------------------------------------
-- Priority-queue functions
---------------------------------------------------------------}

minAssoc :: Trie a -> Maybe (ByteString, a)
minAssoc = go S.empty
    where
    go _ Empty              = Nothing
    go q (Arc k (Just v) _) = Just (S.append q k,v)
    go q (Arc k Nothing  t) = go   (S.append q k) t
    go q (Branch _ _ l _)   = go q l


maxAssoc :: Trie a -> Maybe (ByteString, a)
maxAssoc = go S.empty
    where
    go _ Empty                  = Nothing
    go q (Arc k (Just v) Empty) = Just (S.append q k,v)
    go q (Arc k _        t)     = go   (S.append q k) t
    go q (Branch _ _ _ r)       = go q r


mapView :: (Trie a -> Trie a)
        -> Maybe (ByteString, a, Trie a) -> Maybe (ByteString, a, Trie a)
mapView _ Nothing        = Nothing
mapView f (Just (k,v,t)) = Just (k,v, f t)


updateMinViewBy :: (ByteString -> a -> Maybe a)
                -> Trie a -> Maybe (ByteString, a, Trie a)
updateMinViewBy f = go S.empty
    where
    go _ Empty              = Nothing
    go q (Arc k (Just v) t) = let q' = S.append q k
                              in Just (q',v, arc k (f q' v) t)
    go q (Arc k Nothing  t) = mapView (arc k Nothing) (go (S.append q k) t)
    go q (Branch p m l r)   = mapView (\l' -> branch p m l' r) (go q l)


updateMaxViewBy :: (ByteString -> a -> Maybe a)
                -> Trie a -> Maybe (ByteString, a, Trie a)
updateMaxViewBy f = go S.empty
    where
    go _ Empty                  = Nothing
    go q (Arc k (Just v) Empty) = let q' = S.append q k
                                  in Just (q',v, arc k (f q' v) Empty)
    go q (Arc k mv       t)     = mapView (arc k mv) (go (S.append q k) t)
    go q (Branch p m l r)       = mapView (branch p m l) (go q r)

----------------------------------------------------------------
----------------------------------------------------------- fin.
