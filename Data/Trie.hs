{-# OPTIONS_GHC -Wall -fwarn-tabs #-}

----------------------------------------------------------------
--                                                  ~ 2008.12.19
-- |
-- Module      :  Data.Trie
-- Copyright   :  Copyright (c) 2008--2009 wren ng thornton
-- License     :  BSD3
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  beta
-- Portability :  portable
--
-- An efficient implementation of maps from strings to values.
--
-- The implementation is based on /big-endian patricia trees/, like
-- "Data.IntMap". We first trie on the elements of "Data.ByteString"
-- and then trie on the big-endian bit representation of those
-- elements. For further details on the latter, see
--
--    * Chris Okasaki and Andy Gill,  \"/Fast Mergeable Integer Maps/\",
--	Workshop on ML, September 1998, pages 77-86,
--	<http://www.cse.ogi.edu/~andy/pub/finite.htm>
--
--    * D.R. Morrison, \"/PATRICIA -- Practical Algorithm To Retrieve
--	Information Coded In Alphanumeric/\", Journal of the ACM, 15(4),
--	October 1968, pages 514-534.
----------------------------------------------------------------

module Data.Trie
    (
    -- * Data types
      Trie(), KeyString, KeyElem, showTrie
    
    -- * Basic functions
    , empty, null, singleton, size, toList, fromList
    
    -- * Query functions
    , lookupBy_, lookupBy, lookup, member, submap
    
    -- * Single-value modification
    , alterBy, insert, adjust, delete
    
    -- * Combining tries
    , mergeBy, unionL, unionR
    
    -- * Mapping functions
    , mapBy, filterMap
    ) where

import Prelude hiding (null, lookup)
import qualified Prelude

import qualified Data.ByteString as S
import Data.Trie.ByteStringInternal
import Data.Trie.BitTwiddle

import Data.Maybe          (isJust)
import Control.Monad       (liftM)
import Control.Applicative (Applicative(..), (<$>))
import Data.Monoid         (Monoid(..))
import Data.Foldable       (Foldable(foldMap))
import Data.Traversable    (Traversable(traverse))
----------------------------------------------------------------
----------------------------------------------------------------


{---------------------------------------------------------------
-- ByteString Big-endian Patricia Trie
---------------------------------------------------------------}
type KeyString = ByteString 
type KeyElem   = ByteStringElem 

{- Idealized:
data Node a   = Accept a (ArcSet a)
              | Reject   (Branch a)          -- Invariant: Must be Branch
data Arc a    = Arc    KeyString (Node a)    -- Invariant: never empty string
data ArcSet a = None
              | One    {KeyElem} (Arc a)
              | Branch {Prefix} {Mask} (ArcSet a) (ArcSet a)
data Trie a   = Empty
              | Start  KeyString (Node a)    -- Maybe empty string [1]

[1] If we maintain the invariants on how Nodes recurse, then we
can't simply have Start(Node a) because we may have a shared prefix
where the prefix itself is not Accept'ed.


-- Squash Arc into One:
-- (pure good)
data Node a   = Accept a (ArcSet a)
              | Reject   (Branch a)
data ArcSet a = None
              | Arc    KeyString (Node a)
              | Branch {Prefix} {Mask} (ArcSet a) (ArcSet a)
data Trie a   = Empty
              | Start  KeyString (Node a)


-- Squash Node together:
-- (most likely good)
data Node a   = Node (Maybe a) (ArcSet a)
data ArcSet a = None
              | Arc    KeyString (Node a)
              | Branch {Prefix} {Mask} (ArcSet a) (ArcSet a)
data Trie a   = Empty
              | Start  KeyString (Node a)


-- Squash Empty/None and Arc/Start together:
-- (Complicates invariants about non-empty strings and Node's recursion)
data Node a = Node (Maybe a) (ArcSet a)
data Trie a = Empty
            | Arc    KeyString (Node a)
            | Branch {Prefix} {Mask} (Trie a) (Trie a)


-- Squash Node into Arc:
-- (By this point, pure good)
-- Unseen invariants:
-- * KeyString non-empty, unless Arc is absolute root of tree
-- * If (Maybe a) is Nothing, then (Trie a) is Branch
--   * With views, we could re-expand Arc into accepting and
--     nonaccepting variants
--
-- [2] Maybe we shouldn't unpack the KeyString. We could specialize
-- or inline the splitMaximalPrefix function to prevent constructing
-- a new KeyString from the parts...
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
            | Arc    {-# UNPACK #-} !KeyString
                                    !(Maybe a)
                                    !(Trie a)
            | Branch {-# UNPACK #-} !Prefix
                     {-# UNPACK #-} !Mask
                                    !(Trie a)
                                    !(Trie a)

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


{---------------------------------------------------------------
-- Trie instances
---------------------------------------------------------------}

instance Functor Trie where
    fmap _ Empty              = Empty
    fmap f (Arc k Nothing  t) = Arc k Nothing      (fmap f t)
    fmap f (Arc k (Just v) t) = Arc k (Just (f v)) (fmap f t)
    fmap f (Branch p m l r)   = Branch p m (fmap f l) (fmap f r)

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
    return x = singleton S.empty x
    
    (>>=) Empty              _ = empty
    (>>=) (Arc k Nothing  t) f = arc k Nothing (t >>= f)
    (>>=) (Arc k (Just v) t) f = arc k Nothing (f v `unionL` (t >>= f))
    (>>=) (Branch p m l r)   f = branch p m (l >>= f) (r >>= f)


instance Monoid a => Monoid (Trie a) where
    mempty  = empty
    mappend = mergeBy $ \x y -> Just (x `mappend` y)


instance Foldable Trie where
    foldMap _ Empty              = mempty
    foldMap f (Arc _ Nothing  t) = foldMap f t
    foldMap f (Arc _ (Just v) t) = f v `mappend` foldMap f t
    foldMap f (Branch _ _ l r)   = foldMap f l `mappend` foldMap f r


instance Traversable Trie where
    traverse _ Empty              = pure Empty
    traverse f (Arc k Nothing  t) = Arc k Nothing        <$> traverse f t
    traverse f (Arc k (Just v) t) = Arc k . Just <$> f v <*> traverse f t
    traverse f (Branch p m l r)   = Branch p m <$> traverse f l <*> traverse f r

{---------------------------------------------------------------
-- Smart constructors and helper functions for building tries
---------------------------------------------------------------}

-- | Smart constructor to prune @Empty@ from @Branch@es.
branch :: Prefix -> Mask -> Trie a -> Trie a -> Trie a
branch _ _ Empty r     = r
branch _ _ l     Empty = l
branch p m l     r     = Branch p m l r


-- | Smart constructor to prune @Arc@s that lead nowhere.
arc :: KeyString -> Maybe a -> Trie a -> Trie a
arc k mv@(Just _)   t                            = Arc k mv t
arc _    Nothing    Empty                        = Empty
arc k    Nothing  t@(Branch _ _ _ _) | S.null k  = t
                                     | otherwise = Arc k Nothing t
arc k    Nothing    (Arc k' mv' t')              = Arc (S.append k k') mv' t'


-- | Smart constructor to join two tries into a @Branch@ with maximal
-- prefix sharing. Requires knowing the prefixes, but can combine
-- either @Branch@es or @Arc@s.
--
-- N.B. /do not/ use if prefixes could match entirely!
branchMerge :: Prefix -> Trie a -> Prefix -> Trie a -> Trie a
branchMerge _ Empty _ t2    = t2
branchMerge _  t1   _ Empty = t1
branchMerge p1 t1  p2 t2
    | zero p1 m = Branch p m t1 t2
    | otherwise = Branch p m t2 t1
    where
    m = branchMask p1 p2
    p = mask p1 m


-- It would be better if Arc used
-- Data.ByteString.TrieInternal.wordHead somehow, that way
-- we can see 4/8/?*Word8 at a time instead of just one.
getPrefix :: Trie a -> Prefix
getPrefix (Branch p _ _ _) = p
getPrefix (Arc k _ _)      | S.null k  = 0 -- for lack of a better
                           | otherwise = S.head k
getPrefix Empty            = error "getPrefix: no Prefix of Empty"


----------------------------------------------------------------
----------------------------------------------------------------


{---------------------------------------------------------------
-- Error messages
---------------------------------------------------------------}

-- | Once correctness is proven, these error messages could be
-- preprocessed away in order to give minor optimizations
errorInvariantBroken :: String -> String -> a
errorInvariantBroken s e =  error (s ++ ": Invariant was broken" ++ e')
    where
    e' = if Prelude.null e then e else ", found: " ++ e

errorArcAfterNothing    :: String -> a
errorArcAfterNothing   s = errorInvariantBroken s "Arc after Nothing"

errorEmptyAfterNothing  :: String -> a
errorEmptyAfterNothing s = errorInvariantBroken s "Empty after Nothing"


errorLogHead :: String -> KeyString -> KeyElem
errorLogHead s q | S.null q  = error (s ++": found null subquery")
                 | otherwise = S.head q

{---------------------------------------------------------------
-- Basic functions
---------------------------------------------------------------}

-- | /O(1)/, The empty trie.
{-# INLINE empty #-}
empty :: Trie a
empty = Empty

-- | /O(1)/, Is the trie empty?
{-# INLINE null #-}
null :: Trie a -> Bool
null Empty = True
null _     = False

-- | /O(1)/, A singleton trie.
{-# INLINE singleton #-}
singleton :: KeyString -> a -> Trie a
singleton k v = Arc k (Just v) Empty
-- For singletons, don't need to verify invariant on arc length >0

-- | /O(n)/, Get count of elements in trie.
{-# INLINE size #-}
size  :: Trie a -> Int
size t = size' t id 0

-- | /O(n)/, Internal CPS accumulator function for calculating
-- 'size'.
size' :: Trie a -> (Int -> Int) -> Int -> Int
size' Empty              f n = f n
size' (Branch _ _ l r)   f n = size' l (size' r f) n
size' (Arc _ Nothing t)  f n = size' t f n
size' (Arc _ (Just _) t) f n = size' t f $! n + 1

-- BUG: Inefficient for now
-- TODO: rewrite both of these to support list fusion
--
-- | Convert trie into association list. Keys will be in sorted order.
toList :: Trie a -> [(KeyString,a)]
toList Empty            = []
toList (Branch _ _ l r) = toList l ++ toList r
toList (Arc k mv t)     = maybe [] (\v -> [(k,v)]) mv
                          ++ map (\(q,x) -> (S.append k q, x)) (toList t)

-- | Convert association list into a trie. On key conflict, values
-- earlier in the list shadow later ones.
fromList :: [(KeyString,a)] -> Trie a
fromList = foldr (uncurry insert) empty


{---------------------------------------------------------------
-- Query functions (just recurse)
---------------------------------------------------------------}

-- | Generic function to find a value (if it exists) and the subtrie
-- rooted at the prefix. The first function argument is called if and
-- only if a node is exactly reachable by the query; if no node is
-- exactly reachable the default value is used; if the middle of
-- an arc is reached, the second function argument is used.
--
-- This function is intended for internal use.
lookupBy_ :: (Maybe a -> Trie a -> b) -> b -> (Trie a -> b)
          -> KeyString -> Trie a -> b
lookupBy_ f z a = go
    where
    go _    Empty             = z
    
    go q   (Arc k mv t)
        | not (S.null k')     = a (Arc k' mv t)
        | S.null q'           = f mv t
        | otherwise           = go q' t
        where
        (_,k',q') = splitMaximalPrefix k q
        
    go q t_@(Branch _ _ _ _) = findArc t_
        where
        qh = errorLogHead "lookupBy_" q
        
        -- | /O(min(m,W))/, where /m/ is number of @Arc@s in this
        -- branching, and /W/ is the word size of the Prefix,Mask type.
        findArc (Branch p m l r)
            | nomatch qh p m  = z
            | zero qh m       = findArc l
            | otherwise       = findArc r
        findArc t@(Arc _ _ _) = go q t
        findArc Empty         = z


-- | Generic function to find a value (if it exists) and the subtrie
-- rooted at the prefix. Intended for public consumption.
{-# INLINE lookupBy #-}
lookupBy :: (Maybe a -> Trie a -> b) -> KeyString -> Trie a -> b
lookupBy f = lookupBy_ f (f Nothing Empty) (f Nothing)

-- | Return the value associated with a query string if it exists.
{-# INLINE lookup #-}
lookup :: KeyString -> Trie a -> Maybe a
lookup = lookupBy_ const Nothing (const Nothing)

-- | Return the subtrie containing all keys beginning with a prefix.
submap :: KeyString -> Trie a -> Trie a
submap q = lookupBy_ submap' Empty (arc q Nothing) q
    where
    submap' Nothing Empty       = errorEmptyAfterNothing "submap"
    submap' Nothing (Arc _ _ _) = errorArcAfterNothing   "submap"
    submap' mx      t           = Arc q mx t

-- | Does a string have a value in the trie?
{-# INLINE member #-}
member :: KeyString -> Trie a -> Bool
member q = isJust . lookup q


{---------------------------------------------------------------
-- Single-value modification functions (recurse and clone spine)
---------------------------------------------------------------}

-- TODO: We should CPS on Empty to avoid cloning spine if no change.
-- Difficulties arise with the calls to 'branch' and 'arc'. Will
-- have to create a continuation chain, so no savings on memory
-- allocation; but would have savings on held memory, if they're
-- still holding the old one...
--
-- | Generic function to alter a trie by one element with a function
-- to resolve conflicts (or non-conflicts).
alterBy :: (KeyString -> a -> Maybe a -> Maybe a)
         -> KeyString -> a -> Trie a -> Trie a
alterBy f_ q_ x_
    | S.null q_ = mergeBy (\x y -> f_ q_ x (Just y)) (singleton q_ x_) 
    | otherwise = go q_
    where
    f         = f_ q_ x_
    nothing q = arc q (f Nothing) Empty
    
    go q Empty            = nothing q
    
    go q t@(Branch p m l r)
        | nomatch qh p m  = branchMerge p t  qh (nothing q)
        | zero qh m       = branch p m (go q l) r
        | otherwise       = branch p m l (go q r)
        where
        qh = errorLogHead "alterBy" q
    
    go q t_@(Arc k mv t)
        | not (S.null k') = case nothing q' of
                            Empty -> t_
                            l     -> let r = Arc k' mv t
                                     in (if S.null p
                                         then id
                                         else Arc p Nothing)
                                             (branchMerge (getPrefix l) l
                                                          (getPrefix r) r)
        | S.null q'       = arc k (f mv) t
        | otherwise       = arc k mv (go q' t)
        where
        (p,k',q') = splitMaximalPrefix k q


-- | Insert a new key. If the key is already present, overrides the
-- old value
{-# INLINE insert #-}
insert    :: KeyString -> a -> Trie a -> Trie a
insert     = alterBy (\_ x _ -> Just x)
                                      
-- | Apply a function to the value at a key.
{-# INLINE adjust #-}
adjust    :: (a -> a) -> KeyString -> Trie a -> Trie a
adjust f q = alterBy (\_ _ -> liftM f) q undefined

-- | Remove the value stored at a key.
{-# INLINE delete #-}
delete     :: KeyString -> Trie a -> Trie a
delete    q = alterBy (\_ _ _ -> Nothing) q undefined


{---------------------------------------------------------------
-- Trie-combining functions
---------------------------------------------------------------}

-- TEST CASES: foldr (unionL . uncurry singleton) empty t
--             foldr (uncurry insert) empty t
--    where t = map (\s -> (pk s, 0))
--                  ["heat","hello","hoi","apple","appa","hell","appb","appc"]
--
-- TODO: switch to 'go', closing over @f@.
--
-- | Combine two tries, using a function to resolve collisions.
-- This can only define the space of functions between union and
-- symmetric difference but, with those two, all set operations can
-- be defined (albeit inefficiently).
mergeBy :: (a -> a -> Maybe a) -> Trie a -> Trie a -> Trie a
mergeBy _ Empty t1    = t1
mergeBy _ t0    Empty = t0

-- /O(n+m)/ for this part where /n/ and /m/ are sizes of the branchings
mergeBy f t0@(Branch p0 m0 l0 r0) t1@(Branch p1 m1 l1 r1)
    | shorter m0 m1  = union0
    | shorter m1 m0  = union1
    | p0 == p1       = branch p0 m0 (mergeBy f l0 l1) (mergeBy f r0 r1)
    | otherwise      = branchMerge p0 t0 p1 t1
    where
    union0  | nomatch p1 p0 m0  = branchMerge p0 t0 p1 t1
            | zero p1 m0        = branch p0 m0 (mergeBy f l0 t1) r0
            | otherwise         = branch p0 m0 l0 (mergeBy f r0 t1)
    
    union1  | nomatch p0 p1 m1  = branchMerge p0 t0 p1 t1
            | zero p0 m1        = branch p1 m1 (mergeBy f t0 l1) r1
            | otherwise         = branch p1 m1 l1 (mergeBy f t0 r1)

mergeBy f t0_ t1_ =
    case (t0_,t1_) of
    (Arc k0 mv0 t0, Arc k1 mv1 t1)
        | S.null k0 -> arc k0 mv0 (mergeBy f t0 t1_)
        | S.null k1 -> arc k1 mv1 (mergeBy f t1 t0_)
        | m' == 0   ->
            let (pk,k0',k1') = splitMaximalPrefix k0 k1
            in if S.null pk
            then error "mergeBy: no mask, but no prefix string"
            else let
                 (mv',t',t'') = case (S.null k0', S.null k1') of
                     (False,False) -> ( Nothing
                                      , Arc k0' mv0 t0
                                      , Arc k1' mv1 t1
                                      )
                     (False,True)  -> ( mv1
                                      , Arc k0' mv0 t0
                                      , t1
                                      )
                     (True, False) -> ( mv0
                                      , t0
                                      , Arc k1' mv1 t1
                                      )
                     (True, True)  -> ( case (mv0,mv1) of
                                             (Nothing,Nothing) -> Nothing
                                             (Nothing,Just _)  -> mv1
                                             (Just _, Nothing) -> mv0
                                             (Just v0,Just v1) -> f v0 v1
                                      , t0
                                      , t1
                                      )
                 in arc pk mv' (mergeBy f t' t'')
    
    (Arc _ _ _, Branch _p1 m1 l r)
        | nomatch p0 p1 m1 -> branchMerge p1 t1_  p0 t0_
        | zero p0 m1       -> branch p1 m1 (mergeBy f t0_ l) r
        | otherwise        -> branch p1 m1 l (mergeBy f t0_ r)
    
    (Branch _p0 m0 l r, Arc _ _ _)
        | nomatch p1 p0 m0 -> branchMerge p0 t0_  p1 t1_
        | zero p1 m0       -> branch p0 m0 (mergeBy f t1_ l) r
        | otherwise        -> branch p0 m0 l (mergeBy f t1_ r)
    
    -- Inlined branchMerge. Both tries are disjoint @Arc@s now.
    _ | zero p0 m' -> Branch p' m' t0_ t1_
    _              -> Branch p' m' t1_ t0_
    where
    p0 = getPrefix t0_
    p1 = getPrefix t1_
    m' = branchMask p0 p1
    p' = mask p0 m'


-- | Combine two tries, resolving conflicts by choosing the value
-- from the left trie.
{-# INLINE unionL #-}
unionL :: Trie a -> Trie a -> Trie a
unionL = mergeBy (\x _ -> Just x)

-- | Combine two tries, resolving conflicts by choosing the value
-- from the right trie.
{-# INLINE unionR #-}
unionR :: Trie a -> Trie a -> Trie a
unionR = mergeBy (\_ y -> Just y)


{---------------------------------------------------------------
-- Mapping functions
---------------------------------------------------------------}

-- | Generic version of 'fmap'. This function is notably more
-- expensive than 'fmap' or 'filterMap' because we have to reconstruct
-- the keys.
mapBy :: (KeyString -> a -> Maybe b) -> Trie a -> Trie b
mapBy f = go S.empty
    where
    go _ Empty              = empty
    go q (Arc k Nothing  t) = arc k Nothing  (go q' t) where q' = S.append q k
    go q (Arc k (Just v) t) = arc k (f q' v) (go q' t) where q' = S.append q k
    go q (Branch p m l r)   = branch p m (go q l) (go q r)


-- | Apply a function to all values, potentially removing them.
filterMap :: (a -> Maybe b) -> Trie a -> Trie b
filterMap _ Empty              = empty
filterMap f (Arc k Nothing  t) = arc k Nothing (filterMap f t)
filterMap f (Arc k (Just v) t) = arc k (f v)   (filterMap f t)
filterMap f (Branch p m l r)   = branch p m (filterMap f l) (filterMap f r)

----------------------------------------------------------------
----------------------------------------------------------- fin.
