{-# OPTIONS_GHC -fno-full-laziness -funbox-strict-fields #-}
{-# LANGUAGE BangPatterns
           , CPP
           , MagicHash
           , Rank2Types
           , UnboxedTuples
           #-}

----------------------------------------------------------------
--                                                  ~ 2014.06.01
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
    , singleton, doubleton, fromList
    , toList, toListBy, keys, elems
    , map, map'
    , filter, filterMap
    -- * Right-biased zipping functions.
    , rzip, rzipWith, rzipWith_
    , rzipFilter, rzipFilter_
    -- * Set-theoretic operations
    -- unionWith
    --
    -- MSparseArray(), new, new_, trimMSA, unsafeFreezeMSA, runMSA 
    ) where

import Prelude hiding (null, lookup, filter, foldr, foldl, length, map, read, elem, notElem)
#if defined(ASSERTS)
import qualified Prelude
#endif

import Data.Foldable hiding (elem, notElem, toList)
import qualified Data.Traversable as Traversable
import Control.Applicative (Applicative)
import Control.DeepSeq
import Data.Word
import Data.Bits ((.&.), (.|.), xor, popCount, bit)
-- TODO: if the version of base is too low, use ad-hoc 'popCount' implementation

import Data.Monoid (Monoid(..))
import Control.Applicative (Applicative(..), (<$>))
import Control.Monad ((<=<))
import Control.Monad.ST -- hiding (runST)
-- import Data.Trie.ArrayMapped.Unsafe (runST)
import GHC.ST (ST(..))

-- GHC 7.7 exports toList/fromList from GHC.Exts
-- In order to avoid warnings on previous GHC versions, we provide
-- an explicit import list instead of only hiding the offending symbols
import GHC.Exts
    ( build
    , Word(W#), Int(I#), Int#, uncheckedShiftL#
    , Array#, newArray#, readArray#, writeArray#, indexArray#, freezeArray#, unsafeFreezeArray#, unsafeThawArray#, MutableArray#)
#if __GLASGOW_HASKELL__ >= 702
import GHC.Exts
    (sizeofArray#, copyArray#, thawArray#, sizeofMutableArray#,
    copyMutableArray#)
#endif

----------------------------------------------------------------
----------------------------------------------------------------

__thisModule :: String
__thisModule = "Data.Trie.ArrayMapped.SparseArray"
{-# NOINLINE __thisModule #-}

__moduleError :: String -> a
__moduleError s = error (__thisModule ++ ": " ++ s)
{-# INLINE __moduleError #-}

__functionError :: String -> String -> a
__functionError fun s = error (__thisModule ++ "." ++ fun ++ ": " ++ s)
{-# INLINE __functionError #-}

__undefinedElem :: a
__undefinedElem = __moduleError "Undefined element"
{-# NOINLINE __undefinedElem #-}


#if defined(ASSERTS)
-- This fugly hack is brought by GHC's apparent reluctance to deal
-- with MagicHash and UnboxedTuples when inferring types. Eek!
#    define CHECK_BOUNDS(_func_,_len_,_k_) \
if (_k_) < 0 || (_k_) >= (_len_) then __functionError (_func_) ("bounds error, offset " ++ show (_k_) ++ ", length " ++ show (_len_)) else

#    define CHECK_OP(_func_,_op_,_lhs_,_rhs_) \
if not ((_lhs_) _op_ (_rhs_)) then __functionError (_func_) ("Check failed: _lhs_ _op_ _rhs_ (" ++ show (_lhs_) ++ " vs. " ++ show (_rhs_) ++ ")") else

#    define CHECK_GT(_func_,_lhs_,_rhs_) CHECK_OP(_func_,>,_lhs_,_rhs_)
#    define CHECK_LE(_func_,_lhs_,_rhs_) CHECK_OP(_func_,<=,_lhs_,_rhs_)
#    define CHECK_EQ(_func_,_lhs_,_rhs_) CHECK_OP(_func_,==,_lhs_,_rhs_)
#else
#    define CHECK_BOUNDS(_func_,_len_,_k_)
#    define CHECK_OP(_func_,_op_,_lhs_,_rhs_)
#    define CHECK_GT(_func_,_lhs_,_rhs_)
#    define CHECK_LE(_func_,_lhs_,_rhs_)
#    define CHECK_EQ(_func_,_lhs_,_rhs_)
#endif
----------------------------------------------------------------
----------------------------------------------------------------

-- | Indices into a 'SparseArray' or 'MSparseArray'.
type Key    = Word8 -- Actually, should be a Word4 for our uses...
type Bitmap = Word  -- Actually, should be a Word16 == 2^Key
type OneBit = Bitmap 
type Mask   = Bitmap 
-- | Indicies into the underlying 'Array#' or 'MutableArray#'.
type Index  = Int 
    -- HACK: actually, this should be Int#, but that causes kinding issues...


-- | Given a bit, return the next bit.
--
-- > bsucc (bit k) == bit (k+1)
bsucc :: OneBit -> OneBit
bsucc (W# b) = W# (b `uncheckedShiftL#` 1#)
{-# INLINE bsucc #-}

-- | Set all bits strictly below the argument.
maskLT :: OneBit -> Mask
maskLT b = b - 1
{-# INLINE maskLT #-}

-- | Set all bits below or equal to the argument.
maskLE :: OneBit -> Mask
maskLE b = maskLT (bsucc b)
{-# INLINE maskLE #-}

key2bit :: Key -> OneBit
-- key2bit = bit . fromIntegral
key2bit k =
    case fromIntegral k of
    I# i -> W# (1## `uncheckedShiftL#` i)
{-# INLINE key2bit #-}

bit2index :: Bitmap -> OneBit -> Index
bit2index p b = popCount (p .&. maskLT b)
{-# INLINE bit2index #-}

key2index :: Bitmap -> Key -> Index
key2index p k = bit2index p (key2bit k)
{-# INLINE key2index #-}

-- | Check if a bit is set in the bitmap.
elem :: OneBit -> Bitmap -> Bool
elem b p = (p .&. b /= 0)
{-# INLINE elem #-}

-- | Check if a bit is unset in the bitmap.
notElem :: OneBit -> Bitmap -> Bool
notElem b p = (p .&. b == 0)
{-# INLINE notElem #-}

----------------------------------------------------------------
----------------------------------------------------------------

data SparseArray    a = SA  {-# UNPACK #-} !Bitmap !(Array# a)

data MSparseArray s a = MSA {-# UNPACK #-} !Bitmap !(MutableArray# s a)

data MArray s a = MA !(MutableArray# s a)

----------------------------------------------------------------

-- We must box up the MutableArray# into something of kind * before we can return it... The other option is to CPS it, which has the same boxing-up effect
new :: Int -> a -> ST s (MArray s a)
new _n@(I# n) x =
    CHECK_GT("new", _n, (0 :: Int))
    ST $ \s ->
        case newArray# n x s of
        (# s', xs #) -> (# s', MA xs #)
{-# INLINE new #-}

new_ :: Int -> ST s (MArray s a)
new_ n = new n __undefinedElem


#if __GLASGOW_HASKELL__ >= 702
lengthMA :: MutableArray# s a -> Int
lengthMA xs = I# (sizeofMutableArray# xs)
{-# INLINE lengthMA #-}
#endif


read :: MutableArray# s a -> Index -> ST s a
read xs _i@(I# i) =
    ST $ \s ->
    CHECK_BOUNDS("read", lengthMA xs, _i)
        readArray# xs i s
{-# INLINE read #-}


write :: MutableArray# s a -> Index -> a -> ST s ()
write xs _i@(I# i) x =
    ST $ \s ->
    CHECK_BOUNDS("write", lengthMA xs, _i)
        case writeArray# xs i x s of
        s' -> (# s' , () #)
{-# INLINE write #-}


-- | Unsafely copy the elements of an array. Array bounds are not checked.
copy :: MutableArray# s e -> Index -> MutableArray# s e -> Index -> Index -> ST s ()
#if __GLASGOW_HASKELL__ >= 702
copy !src !_sidx@(I# sidx) !dst !_didx@(I# didx) _n@(I# n) =
    CHECK_BOUNDS("copy: src", lengthMA src, _sidx + _n - 1)
    CHECK_BOUNDS("copy: dst", lengthMA dst, _didx + _n - 1)
    ST $ \s ->
        case copyMutableArray# src sidx dst didx n s of
        s' -> (# s', () #)
#else
copy !src !sidx !dst !didx n =
    CHECK_BOUNDS("copy: src", lengthMA src, sidx + n - 1)
    CHECK_BOUNDS("copy: dst", lengthMA dst, didx + n - 1)
    copy_loop sidx didx 0
    where
    copy_loop !i !j !c
        | c < n     = do
            write dst j =<< read src i
            copy_loop (i+1) (j+1) (c+1)
        | otherwise = return ()
#endif


-- TODO: would it be faster just to allocate a new array and use copyMutableArray# for each half?
shiftUpOne :: MutableArray# s e -> Index -> Index -> ST s ()
shiftUpOne !xs !i !n = 
    CHECK_BOUNDS("shiftUpOne: ", lengthMA src, i + n) -- TODO: (subtract 1) ??
    go xs i (i + n)
    where
    go !xs !i !n
        | i < n     = do
            write xs n =<< read xs (n-1)
            go xs i (n-1)
        | otherwise = return ()

----------------------------------------------------------------
-- | Trim and freeze the array.
trimMSA :: MSparseArray s a -> ST s (SparseArray a)
trimMSA (MSA p xs) =
    case popCount p of
    I# n ->
        ST $ \s ->
            case freezeArray# xs 0# n s of
            (# s', xs' #) -> (# s', SA p xs' #)
{-# INLINE trimMSA #-}


-- | Freeze the array in place.
unsafeFreezeMSA :: MSparseArray s a -> ST s (SparseArray a)
unsafeFreezeMSA (MSA p xs) = ST $ \s ->
    case unsafeFreezeArray# xs s of
    (# s', xs' #) -> (# s', SA p xs' #)
{-# INLINE unsafeFreezeMSA #-}


unsafeFreezeOrTrimMSA :: Bool -> MSparseArray s a -> ST s (SparseArray a)
unsafeFreezeOrTrimMSA True  = unsafeFreezeMSA
unsafeFreezeOrTrimMSA False = trimMSA
{-# INLINE unsafeFreezeOrTrimMSA #-}


runMSA :: (forall s. ST s (MSparseArray s a)) -> SparseArray a
runMSA act = runST (unsafeFreezeMSA =<< act)
{-# INLINE runMSA #-}

----------------------------------------------------------------
----------------------------------------------------------------

empty :: SparseArray a
empty = runMSA (new_ 0 >>= \ (MA xs) -> return (MSA 0 xs))


-- TODO: would it be better to have an invariant that SAs are non-empty, and use Maybe when we 'trimMSA', 'filter', etc?
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


-- HACK: must use the prefix notation in the definition, otherwise it isn't recognized for some reason...
(!) :: Array# a -> Index -> a
(!) xs (I# i) = case indexArray# xs i of (# x #) -> x
{-# INLINE (!) #-}

lookup :: Key -> SparseArray a -> Maybe a
lookup k (SA p xs) =
    let !b = key2bit k in
    if b `elem` p
    then Just (xs ! bit2index p b)
    else Nothing
{-# INLINE lookup #-}


singleton :: Key -> a -> SparseArray a
singleton k x = runMSA $ do
    MA xs <- new 1 x
    return (MSA (key2bit k) xs)
{-# INLINE singleton #-}


doubleton :: Key -> a -> Key -> a -> SparseArray a
doubleton k x l y = runMSA $ do
    let !bk = key2bit k
    let !bl = key2bit l
    let (lo, hi) = if bk < bl then (x,y) else (y,x)
    MA xs <- new 2 lo
    write xs 1 hi
    return (MSA (bk .|. bl) xs)
{-# INLINE doubleton #-}


data DynamicMSA s a = DMSA
    { bitmapDMSA     :: {-# UNPACK #-} !Bitmap
    , maxKeyDMSA     :: {-# UNPACK #-} !Key
    , nextIndexDMSA  :: {-# UNPACK #-} !Index
    , limitIndexDMSA :: {-# UNPACK #-} !Index
    , arrayDMSA      :: !(MutableArray# s a)
    }

unsafeFreezeDMSA :: DynamicMSA s a -> ST s (SparseArray a)
unsafeFreezeDMSA (DMSA p maxK n maxN xs) =
    unsafeFreezeOrTrimMSA (n == maxN) (MSA p xs)
{-# INLINE unsafeFreezeDMSA #-}

dynamicInsert :: Key -> a -> DynamicMSA s a -> ST s (DynamicMSA s a)
dynamicInsert !k x dmsa@(DMSA p maxK n maxN xs)
    | b `elem` p = do
        write xs i x
        return dmsa
    | otherwise =
        case (n < maxN, k > maxK) of
        (True, True) -> do
            write xs n x
            return $! DMSA p' k n' maxN xs
        (True, False) -> do
            -- TODO: if shiftUpOne cannot be implemented efficiently, then maybe we should just reallocate and copy everything?
            shiftUpOne xs i (n-i)
            write xs i x
            return $! DMSA p' maxK n' maxN xs
        (False, True) -> do
            MA xs' <- new_ maxN'
            copy xs 0 xs' 0 maxN
            write xs' n x
            return $! DMSA p' k n' maxN' xs'
        (False, False) -> do
            MA xs' <- new_ maxN'
            copy xs 0 xs' 0 i
            write xs' i x
            copy xs i xs' (i+1) (maxN-i)
            return $! DMSA p' maxK n' maxN' xs'
    where
    -- TODO: verify these all get inlined/performed where they ought to
    b     = key2bit k
    i     = bit2index p b
    p'    = p .|. b
    n'    = n + 1
    maxN' = 2*maxN


-- Since we know the effective size limit for our use cases is 16(=2^Key), we just start there so that we only ever need to allocate once and then trim; thus avoiding the need to resize along the way.
-- TODO: that being the case, we should get rid of DMSA and just use MSA and insertMSA... Doing so will save us the (n < maxN) check every iteration, albeit at the cost of unsafety if we overflow...
fromList :: [(Key,a)] -> SparseArray a
fromList = fromList_ 16
{-# INLINE [0] fromList #-}
{-# RULES
"fromList/empty"
        fromList [] = empty
"fromList/singleton"
    forall k x.
        fromList [(k,x)] = singleton k x
"fromList/doubleton"
    forall k1 x1 k2 x2.
        fromList [(k1,x1),(k2,x2)] = doubleton k1 x1 k2 x2
    #-}

fromList_ :: Int -> [(Key,a)] -> SparseArray a
fromList_ !n []          = empty
fromList_  n ((k,x):kxs) = runST $ do
    MA xs <- new n x
    go kxs $! DMSA (key2bit k) k 1 n xs
    where
    go []          = unsafeFreezeDMSA
    go ((k,x):kxs) = go kxs <=< dynamicInsert k x
    -- TODO: verify that the DMSA gets unpacked everywhere
    -- TODO: do we need to eta-expand in order to tell GHC to be strict?


{-
fromAscList :: [(Key,a)] -> SparseArray a
fromAscList = fromAscList_ 1
{-# INLINE fromAscList #-}

-- TODO: clean this up based on dynamicInsert
fromAscList_ :: Int -> [(Key,a)] -> SparseArray a
fromAscList_ !n []          = empty
fromAscList_  n ((k,x):kxs) = runST $ do
    MA xs <- new n x
    go (key2bit k) xs 1 n kxs
    where
    go !p !xs !i !n !kxs =
        case kxs of
        [] -> unsafeFreezeOrTrimMSA (i == n) (MSA p xs)
        ((k,x):kxs)
            | b `elem` p -> do
                write xs (bit2index p b) x
                go p xs i n kxs
            | i < n      -> do
                write xs i x
                go (p .|. b) xs (i+1) n kxs
            | otherwise  -> do
                MA xs' <- new_ (2*n)
                copy xs 0 xs' 0 n
                write xs' i x
                go (p .|. b) xs' (i+1) (2*n) kxs
            where
            b = key2bit k
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


{-
-- TODO: how do we get/put an Array# ?
instance (Binary a) => Binary (SparseArray a) where
    put (SA p xs) = do put p; put xs
    
    get = SA <$> get <*> get
-}


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
map f = \(SA p xs) -> runMSA $ do
        let !n = popCount p
        MA ys <- new_ n
        go p xs n ys 0
    where
    go !p !xs !n !ys !i
        | i < n     = do write ys i (f (xs ! i)); go p xs n ys (i+1)
        | otherwise = return $! MSA p ys
{-# INLINE [0] map #-}
{-# RULES "map id"  map id = id #-}


-- TODO: is there a class for this yet?
map' :: (a -> b) -> SparseArray a -> SparseArray b
map' f = \(SA p xs) -> runMSA $ do
        let !n = popCount p
        MA ys <- new_ n
        go p xs n ys 0
    where
    go !p !xs !n !ys !i
        | i < n     = do write ys i $! f (xs ! i); go p xs n ys (i+1)
        | otherwise = return $! MSA p ys
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
            | n >= 0    = go xs (n-1) (f (xs ! n) z)
            | otherwise = z
    
    {-# INLINE foldl #-}
    foldl f z = \(SA p xs) -> go xs (popCount p - 1)
        where
        go !xs !n
            | n >= 0    = f (go xs (n-1)) (xs ! n)
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
first p b = if b `notElem` p then next p b else b

-- | Get the next bit set in @p@; i.e., the first set bit after @b@.
next p b = first p (bsucc b)


filterMap :: (a -> Maybe b) -> SparseArray a -> SparseArray b
filterMap f (SA p xs) =
    let !n = popCount p in
    runST $ do
        MA ys <- new_ n
        let go !i !bi !j !q
                | i >= n    =
                    unsafeFreezeOrTrimMSA (i == j {- aka: p == q -}) (MSA q ys)
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
    let !n = popCount p in
    runST $ do
        MA ys <- new_ n
        let go !i !bi !j !q
                | i >= n    =
                    unsafeFreezeOrTrimMSA (i == j {- aka: p == q -}) (MSA q ys)
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
    runMSA $ do
        let !n = popCount q
        MA zs <- new_ n
        let go !i !b !j
                | j >= n     = return $! MSA q zs
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
    let !n = popCount q in
    runST $ do
        MA zs <- new_ n
        let go !i !b !j !k !r
                | j >= n     =
                    unsafeFreezeOrTrimMSA (j == k {- aka: q == r -}) (MSA r zs)
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
