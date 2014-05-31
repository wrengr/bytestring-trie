{-# OPTIONS_GHC -fno-full-laziness -funbox-strict-fields #-}
{-# LANGUAGE BangPatterns
           , CPP
           , MagicHash
           , Rank2Types
           , UnboxedTuples
           #-}

----------------------------------------------------------------
--                                                  ~ 2014.05.32
-- |
-- Module      :  Data.Trie.ArrayMapped.SparseArray
-- Copyright   :  Copyright (c) 2014 wren gayle romano; 2010--2012 Johan Tibell
-- License     :  BSD3
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  experimental
-- Portability :  GHC only
--
-- 
----------------------------------------------------------------

module Data.Trie.ArrayMapped.SparseArray
    ( Key, SparseArray()
    , null, length, lookup, isSubarrayOf
    , singleton, doubleton
    , toList, toListBy, keys, elems
    , map, map'
    , filter, filterMap
    -- * Right-biased zipping functions.
    , rzip, rzipWith, rzipWith_
    , rzipFilter, rzipFilter_
    --
    -- MSparseArray(), trim, unsafeFreeze, run 
    ) where

import Prelude hiding (filter, foldr, length, map, read, elem)
#if defined(ASSERTS)
import qualified Prelude
#endif

import qualified Data.Traversable as Traversable
import Control.Applicative (Applicative)
import Control.DeepSeq
import Data.Bits ((.&.), (.|.), xor, unsafeShiftL, popCount, bit)
-- TODO: if the version of base is too low, use ad-hoc 'popCount' implementation

import Control.Monad.ST hiding (runST)
import Data.Trie.ArrayMapped.Unsafe (runST)
import GHC.ST (ST(..))

-- GHC 7.7 exports toList/fromList from GHC.Exts
-- In order to avoid warnings on previous GHC versions, we provide
-- an explicit import list instead of only hiding the offending symbols
import GHC.Exts (Array#, Int(..), newArray#, readArray#, writeArray#,
                 indexArray#, unsafeFreezeArray#, unsafeThawArray#,
                 MutableArray#)
#if __GLASGOW_HASKELL__ >= 702
import GHC.Exts (sizeofArray#, copyArray#, thawArray#, sizeofMutableArray#,
                 copyMutableArray#)
#endif

----------------------------------------------------------------
----------------------------------------------------------------

-- | Indices into a 'SparseArray' or 'MSparseArray'.
type Key    = Word8 -- Actually, should be a Word4 for our uses...
type Bitmap = Word  -- Actually, should be a Word16 == 2^Key
type OneBit = Bitmap 
type Mask   = Bitmap 
-- | Indicies into the underlying 'Array#' or 'MutableArray#'.
type Index  = Int# 


-- | Given a bit, return the next bit.
--
-- > bsucc (bit k) == bit (k+1)
bsucc :: OneBit -> OneBit
bsucc b = b `unsafeShiftL` 1
{-# INLINE bsucc #-}

-- | Set all bits strictly below the argument.
maskLT :: OneBit -> Mask
maskLT b = b - 1
{-# INLINE maskLT #-}

-- | Set all bits below or equal to the argument.
maskLE :: OneBit -> Mask
maskLE b = maskLT (bsucc b)
{-# INLINE maskLE #-}

-- | Get the count of set bits, as an unboxed integer.
popCount# :: Bitmap -> Int#
popCount# p = case popCount p of I# i# -> i#
{-# INLINE popCount# #-}

bit2index :: Bitmap -> OneBit -> Index
bit2index p b = popCount# (p .&. maskLT b)
{-# INLINE bit2index #-}

key2index :: Bitmap -> Key -> Index
key2index p k = bit2index p (bit k)
{-# INLINE key2index #-}

-- | Check if a bit is set in the bitmap.
elem :: OneBit -> Bitmap -> Bool
elem b p = (p .&. b /= 0)
{-# INLINE elem #-}

----------------------------------------------------------------
----------------------------------------------------------------

data SparseArray    a = SA  {-# UNPACK #-} !Bitmap !(Array# a)

data MSparseArray s a = MSA {-# UNPACK #-} !Bitmap !(MutableArray# s a)

----------------------------------------------------------------

-- | Trim and freeze the array.
trim :: MSparseArray s a -> ST s (SparseArray a)
trim (MSA p xs) = ST $ \s ->
    case freezeArray# xs 0 (popCount# p) s of
    (# s', xs' #) -> (# s', SA p xs' #)
{-# INLINE trim #-}


-- | Freeze the array in place.
unsafeFreeze :: MSparseArray s a -> ST s (SparseArray a)
unsafeFreeze (MSA p xs) = ST $ \s ->
    case unsafeFreezeArray# xs s of
    (# s', xs' #) -> (# s', SA p xs' #)
{-# INLINE unsafeFreeze #-}


run :: (forall s. ST s (MSparseArray s a)) -> SparseArray a
run act = runST (unsafeFreeze =<< act)
{-# INLINE run #-}

----------------------------------------------------------------
----------------------------------------------------------------

-- TODO: would it be better to have an invariant that SAs are non-empty, and use Maybe when we 'trim', 'filter', etc?
-- | /O(1)/. Is the array empty?
null :: SparseArray a -> Bool
null (SA 0 _) = True
null _        = False
{-# INLINE null #-}


-- | /O(1)/. Get the number of elements in the array.
length :: SparseArray a -> Int
length (SA p _) = popCount p
{-# INLINE length #-}


-- | /O(1)/. Are the first array's keysa subset of second's?
isSubarrayOf :: SparseArray a -> SparseArray b -> Bool
isSubarrayOf (SA p _) (SA q _) = (p .&. q == p)
{-# INLINE isSubarrayOf #-}


(!) :: Array# a -> Int -> a
xs ! (I# i) = index xs i
{-# INLINE (!) #-}

index :: Array# a -> Index -> a
index xs i = case indexArray# xs i of (# x #) -> x
{-# INLINE index #-}

lookup :: Key -> SparseArray a -> Maybe a
lookup k (SA p xs) =
    let !b = bit k in
    if b `elem` p
    then Just (index xs (bit2index p b))
    else Nothing
{-# INLINE lookup #-}


singleton :: Key -> a -> SparseArray a
singleton k x = run (MSA (bit k) <$> ST (newArray# 1 x))
{-# INLINE singleton #-}


doubleton :: Key -> a -> Key -> a -> SparseArray a
doubleton k x l y = run $ do
    let !bk = bit k
    let !bl = bit l
    let (lo, hi) = if bk < bl then (x,y) else (y,x)
    xs <- ST (newArray# 2 lo)
    ST (writeArray# xs 1 hi)
    return (MSA (bk .|. bl) xs)
{-# INLINE doubleton #-}

{-
-- TODO:
array :: Int -> [(Key,a)] -> SparseArray a
array n xs0 =
    CHECK_EQ("array", n, Prelude.length xs0)
        run $ do
            mary <- new_ n
            go xs0 mary 0
  where
    go [] !mary !_   = return mary
    go (x:xs) mary i = do write mary i x
                          go xs mary (i+1)

fromList :: [(Key,a)] -> SparseArray a
-- TODO: Use DynamicArray tricks to grow things as necessary.
-}

toList :: SparseArray a -> [(Key,a)]
toList = toListBy (,)
{-# INLINE toList #-}

toListBy :: (Key -> a -> b) -> SparseArray a -> [b]
toListBy f xz = build (\cons nil -> toListByFB cons nil f xz)
{-# INLINE [0] toListBy #-}
{-# RULES "toListBy const"         toListBy (\k _ -> k) = keys  #-}
{-# RULES "toListBy (flip const)"  toListBy (\_ x -> x) = elems #-}

toListByFB :: (b -> c -> c) -> c -> (Key -> a -> b) -> SparseArray a -> c
toListByFB cons nil f = \(SA p xs) -> go xs 0 p 1 1
    where
    go !xs !i !p !b !k
        | p == 0     = nil
        | b `elem` p = cons (f k (xs ! i)) $
            go xs (i+1) (p `xor` b) (bsucc b) (k+1)
        | otherwise  = go xs i p (bsucc b) (k+1)
{-# INLINE toListByFB #-}

keys :: SparseArray a -> [Key]
keys xz = build (\cons nil -> keysFB cons nil xz)
{-# INLINE keys #-}

keysFB :: (Key -> c -> c) -> c -> SparseArray a -> c
keysFB cons nil = \(SA p _) -> go p 1 1
    where
    go !p !b !k
        | p == 0     = nil
        | b `elem` p = cons k $ go (p `xor` b) (bsucc b) (k+1)
        | otherwise  =          go p           (bsucc b) (k+1)
{-# INLINE keysFB #-}

elems :: SparseArray a -> [a]
elems xz = build (\cons nil -> foldr cons nil xz)
{-# INLINE elems #-}

----------------------------------------------------------------
----------------------------------------------------------------

instance Show a => Show (SparseArray a) where
    show = show . toList


instance (Binary a) => Binary (SparseArray a) where
    put (SA p xs) = do put p; put xs
    
    get = SA <$> get <*> get


instance (NFData a) => NFData (SparseArray a) where
    {-# INLINE rnf #-}
    rnf = \(SA p xs) -> go xs (popCount p) 0
        where
        go !xs !n !i
            | i < n     = rnf (xs ! i) `seq` go xs n (i+1)
            | otherwise = ()


-- BUG: using fmap and fmap' may result in allocating the @SA@ pairs in places where it's not necessary... Does the INLINE help?
instance Functor SparseArray where
    fmap = map


map :: (a -> b) -> SparseArray a -> SparseArray b
map f = \(SA p xs) -> runST $ do
        let !n = popCount p
        ys <- new_ n
        go p xs n ys 0
    where
    go !p !xs !n !ys !i
        | i < n     = do write ys i (f (xs ! i)); go p xs n ys (i+1)
        | otherwise = unsafeFreeze (MSA p ys)
{-# INLINE [0] map #-}
{-# RULES "map id"  map id = id #-}


-- TODO: is there a class for this yet?
map' :: (a -> b) -> SparseArray a -> SparseArray b
map' f = \(SA p xs) -> runST $ do
        let !n = popCount p
        ys <- new_ n
        go p xs n ys 0
    where
    go !p !xs !n !ys !i
        | i < n     = do write ys i $! f (xs ! i); go p xs n ys (i+1)
        | otherwise = unsafeFreeze (MSA p ys)
{-# INLINE [0] map' #-}
{-# RULES "map' id"  map' id = id #-}


instance Foldable SparseArray where
    {-# INLINE fold #-}
    fold      = foldr' mappend mempty
    
    {-# INLINE foldMap #-}
    foldMap f = foldr' (mappend . f) mempty
    
    {-# INLINE foldr #-}
    foldr f z = \(SA p xs) -> go xs (popCount p) 0
        where
        go !xs !n !i
            | i < n     = f (xs ! i) (go xs n (i+1))
            | otherwise = z
    
    {-# INLINE foldr' #-}
    foldr' f = \z0 (SA p xs) -> go xs (popCount p - 1) z0
        where
        go !xs !n !z
            | n > 0     = go xs (n-1) (f (xs ! n) z)
            | otherwise = z
    
    {-# INLINE foldl #-}
    foldl f z = \(SA p xs) -> go xs (popCount p - 1)
        where
        go !xs !n
            | n > 0     = f (go xs (n-1)) (xs ! n)
            | otherwise = z
    
    {-# INLINE foldl' #-}
    foldl' f = \z0 (SA p xs) -> go xs (popCount p) 0 z0
        where
        go !xs !n !i !z
            | i < n     = go xs n (i+1) (f z (xs ! i))
            | otherwise = z

{-
-- TODO
foldrWithKey
foldrWithKey'
foldlWithKey
foldlWithKey'
-}

{-
-- BUG: does this even make sense? How can we define 'cons'?? We need to use 'singleton' instead... seems doable at least...
instance Traversable SparseArray where
    traverse f = foldr' cons_f (pure mempty)
        where
        cons_f x ys = cons <$> f x <*> ys
-}


----------------------------------------------------------------

-- | Get the first bit set in @p@, starting at @b@.
first p b = if b `elem` p then b else next p b

-- | Get the next bit set in @p@; i.e., the first set bit after @b@.
next p b = first p (bsucc b)


filterMap :: (a -> Maybe b) -> SparseArray a -> SparseArray b
filterMap f (SA p xs) =
    runST $ do
        let !n = popCount p
        ys <- new_ n
        let go !i !bi !j !q
                | i >= n    =
                    if i == j -- aka: p == q
                    then unsafeFreeze (MSA q ys)
                    else trim         (MSA q ys)
                | otherwise =  
                    case f (xs ! i) of
                    Just y  -> do
                        write ys j y
                        go (i+1) (next p bi) (j+1) (q .|. bi)
                    Nothing ->
                        go (i+1) (next p bi) j q
            --
        go 0 (first p 1) 0 0
        


filter :: (a -> Bool) -> SparseArray a -> SparseArray a
filter f (SA p xs) =
    runST $ do
        let !n = popCount p
        ys <- new_ n
        let go !i !bi !j !q
                | i >= n    =
                    if i == j -- aka: p == q
                    then unsafeFreeze (MSA q ys)
                    else trim         (MSA q ys)
                | f x       = do
                    write ys j x
                    go (i+1) (next p bi) (j+1) (q .|. bi)
                | otherwise =
                    go (i+1) (next p bi) j q
                where x = xs ! i
            --
        go 0 (first p 1) 0 0


----------------------------------------------------------------
-- All these functions assume (xz `isSubarrayOf` yz) and operate accordingly. If that's not the case, then they work as if operating on the subarray of xz such that it is the case


rzipWith_
    :: (a -> b -> c) -> (b -> c)
    -> SparseArray a -> SparseArray b -> SparseArray c
rzipWith_ f g (SA p xs) (SA q ys) =
    runST $ do
        let !n = popCount q
        zs <- new_ n
        let go !i !b !j
                | j >= n     = unsafeFreeze (MSA q zs)
                | b `elem` p = do
                    write zs j (f (xs ! i) (ys ! j))
                    go (i+1) (next q b) (j+1)
                | otherwise  = do
                    write zs j (g (ys ! j))
                    go i     (next q b) (j+1)
            --
        go 0 (first q 1) 0


rzipWith
    :: (Maybe a -> b -> c)
    -> SparseArray a -> SparseArray b -> SparseArray c
rzipWith f = rzipWith_ (f . Just) (f Nothing)


rzip :: SparseArray a -> SparseArray b -> SparseArray (Maybe a, b)
rzip = rzipWith (,)


rzipFilter_
    :: (a -> b -> Maybe c) -> (b -> Maybe c)
    -> SparseArray a -> SparseArray b -> SparseArray c
rzipFilter_ f g (SA p xs) (SA q ys) =
    runST $ do
        let !n = popCount q
        zs <- new_ n
        let go !i !b !j !k !r
                | j >= n     =
                    if j == k -- aka: q == r
                    then unsafeFreeze (MSA r zs)
                    else trim         (MSA r zs)
                | b `elem` p = 
                    case f (xs ! i) (ys ! j) of
                    Just z -> do
                        write zs k z
                        go (i+1) (next q b) (j+1) (k+1) (r .|. b)
                    Nothing ->
                        go (i+1) (next q b) (j+1) k r
                | otherwise  = 
                    case g (ys ! j) of
                    Just z -> do
                        write zs k z
                        go i (next q b) (j+1) (k+1) (r .|. b)
                    Nothing ->
                        go i (next q b) (j+1) k r
            --
        go 0 (first q 1) 0 0 0


rzipFilter
    :: (Maybe a -> b -> Maybe c)
    -> SparseArray a -> SparseArray b -> SparseArray c
rzipFilter f = rzipFilter_ (f . Just) (f Nothing)



----------------------------------------------------------------
----------------------------------------------------------- fin.
