{-# OPTIONS_GHC
    -Wall -fwarn-tabs -fno-warn-name-shadowing -fno-warn-unused-binds
    -fno-full-laziness -funbox-strict-fields
    #-}
{-# LANGUAGE BangPatterns
           , CPP
           , MagicHash
           , Rank2Types
           , UnboxedTuples
           #-}

----------------------------------------------------------------
--                                                  ~ 2014.06.03
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
    , null, length, member, lookup, lookup', isSubarrayOf
    , singleton, doubleton, fromList -- fromAscList, fromDistinctAscList
    , toList, toListBy, keys, elems
    
    -- * Extra mapping functions
    , map, map'
    , filter, partition
    -- filterWithKey, partitionWithKey
    , filterMap -- mapEither
    
    -- * Extra folding functions
    , foldL, foldR
    -- foldrWithKey, foldrWithKey', foldlWithKey, foldlWithKey'
    
    -- * Extra traversal functions
    , sequenceST, traverseST
    
    -- * Set-theoretic operations
    -- ** Right-biased zipping functions
    , rzip, rzipWith, rzipWith_
    , rzipWith', rzipWith'_
    , rzipFilter, rzipFilter_
    
    -- ** TODO: left-biased zipping functions
    -- lzip, lzipWith, lzipWith_
    -- lzipWith', lzipWith'_
    -- lzipFilter, lzipFilter_
    
    -- ** Union-like operators
    , unionL, unionR, unionWith, unionWith_
    -- unionWithKey, unionWithKey_
    , unionWith', unionWith'_
    -- unionWithKey', unionWithKey'_
    , unionFilterWith, unionFilterWith_
    -- unionFilterWithKey_
    -- unionsWith_
    
    -- ** Intersection-like operators
    , intersectionL, intersectionR, intersectionWith -- intersectionWithKey
    -- intersectionWith', intersectionWithKey'
    -- intersectionFilterWith
    -- intersectionsWith
    
    -- ** Difference-like operators
    , differenceL, differenceR
    -- symdiff, symdiffWith, symdiffWith_
    
    ----
    
    -- new, new_, trim, unsafeFreeze, unsafeFreezeOrTrim
    ) where

import Prelude hiding (null, lookup, filter, foldr, foldl, length, map, read, elem, notElem)

import Data.Foldable hiding (elem, notElem, toList)
import Data.Traversable
import Control.Applicative (Applicative(..), (<$>))
import Control.Monad.ST -- hiding (runST)
-- import Data.Trie.ArrayMapped.Unsafe (runST)
import GHC.ST              (ST(..))
import Control.DeepSeq
import Data.Monoid         (Monoid(..))
import Data.Word
import Data.Bits           ((.&.), (.|.), xor, popCount)
-- TODO: if the version of base is too low, use ad-hoc 'popCount' implementation
import Data.Or             (Or(..))

-- GHC 7.7 exports toList/fromList from GHC.Exts
-- In order to avoid warnings on previous GHC versions, we provide
-- an explicit import list instead of only hiding the offending symbols
import GHC.Exts
    ( build
    , Word(W#), Int(I#), uncheckedShiftL#, not#
    , Array#, newArray#, readArray#, writeArray#, indexArray#, freezeArray#, unsafeFreezeArray#, MutableArray#
#if __GLASGOW_HASKELL__ >= 702
    , sizeofMutableArray#, copyMutableArray#
#endif
    )

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

__impossible :: String -> a
__impossible fun = __functionError fun "the impossible happened"
{-# INLINE __impossible #-}


#if defined(ASSERTS)
-- This fugly hack is brought by GHC's apparent reluctance to deal
-- with MagicHash and UnboxedTuples when inferring types. Eek!
#    define CHECK_BOUNDS(_func_,_len_,_k_) \
if (_k_) < 0 || (_k_) >= (_len_) then __functionError (_func_) ("bounds error, offset " ++ show (_k_) ++ ", length " ++ show (_len_)) else

#    define CHECK_OP(_func_,_op_,_lhs_,_rhs_) \
if not ((_lhs_) _op_ (_rhs_)) then __functionError (_func_) ("Check failed: _lhs_ _op_ _rhs_ (" ++ show (_lhs_) ++ " vs. " ++ show (_rhs_) ++ ")") else

#    define CHECK_GT(_func_,_lhs_,_rhs_) CHECK_OP(_func_,>,_lhs_,_rhs_)
#    define CHECK_GE(_func_,_lhs_,_rhs_) CHECK_OP(_func_,>=,_lhs_,_rhs_)
#    define CHECK_EQ(_func_,_lhs_,_rhs_) CHECK_OP(_func_,==,_lhs_,_rhs_)
#    define CHECK_LE(_func_,_lhs_,_rhs_) CHECK_OP(_func_,<=,_lhs_,_rhs_)
#    define CHECK_LT(_func_,_lhs_,_rhs_) CHECK_OP(_func_,<,_lhs_,_rhs_)
#else
#    define CHECK_BOUNDS(_func_,_len_,_k_)
#    define CHECK_OP(_func_,_op_,_lhs_,_rhs_)
#    define CHECK_GT(_func_,_lhs_,_rhs_)
#    define CHECK_GE(_func_,_lhs_,_rhs_)
#    define CHECK_EQ(_func_,_lhs_,_rhs_)
#    define CHECK_LE(_func_,_lhs_,_rhs_)
#    define CHECK_LT(_func_,_lhs_,_rhs_)
#endif
----------------------------------------------------------------
----------------------------------------------------------------

-- | Indices into a 'SparseArray' or 'DynamicSA'
type Key    = Word8 -- Actually, should be a Word4 for our uses...
type Bitmap = Word  -- Actually, should be a Word16 == 2^Key == 2^(2^4)
type OneBit = Bitmap 
-- | Indicies into the underlying 'Array#' or 'MutableArray#'.
type Index  = Int 
    -- HACK: actually, this should be Int#, but that causes code ugliness


-- | Given a bit, return the next bit.
--
-- > bsucc (bit k) == bit (k+1)
bsucc :: OneBit -> OneBit
bsucc (W# b) = W# (b `uncheckedShiftL#` 1#)
{-# INLINE bsucc #-}
-- TODO: use INLINEABLE instead, in order to allow duplication?


-- HACK: the definition of 'complement' in Bits is (xor maxBound) instead of not# (in order to narrow things as appropriate). But since we're only going to turn around and use (.&.) we can get rid of that and use not# directly. This does assume all the extraneous high-order bits are zero instead of garbage though.
complement :: Word -> Word
complement (W# w) = W# (not# w)
{-# INLINE complement #-}

-- N.B., via twos-complement we have that:
-- negate x == complement (x - 1) == complement x + 1


-- | Set all bits strictly below the argument.
maskLT :: OneBit -> Bitmap -> Bitmap
maskLT b p = (b - 1) .&. p
{-# INLINE maskLT #-}
-- TODO: use INLINEABLE instead, in order to allow duplication?


-- | Set all bits below or equal to the argument.
maskLE :: OneBit -> Bitmap -> Bitmap
maskLE b p = (bsucc b - 1) .&. p
{-# INLINE maskLE #-}
-- TODO: use INLINEABLE instead, in order to allow duplication?
-- N.B., this only works because @b@ has only one bit set; more generally, use (x .&. (x-1)) to clear the first bit


-- | Set all bits strictly above the argument.
maskGT :: OneBit -> Bitmap -> Bitmap
maskGT b p = negate (bsucc b) .&. p
{-# INLINE maskGT #-}


-- | Set all bits strictly above or equal to the argument.
maskGE :: OneBit -> Bitmap -> Bitmap
maskGE b p = negate b .&. p
{-# INLINE maskGE #-}


-- | Get the first bit set in @p@.
firstBit :: Bitmap -> OneBit
firstBit p = p .&. negate p
{-# INLINE firstBit #-}

-- | Get the second bit set in @p@.
secondBit :: Bitmap -> OneBit
secondBit p = firstBit (p .&. (p-1))
{-# INLINE secondBit #-}

-- | Get the next bit set in @p@; i.e., the first set bit after @b@.
nextBit :: Bitmap -> OneBit -> OneBit
nextBit p b = firstBit (maskGT b p)
{-# INLINE nextBit #-}


key2bit :: Key -> OneBit
-- key2bit = bit . fromIntegral
key2bit k =
    case fromIntegral k of
    I# i -> W# (1## `uncheckedShiftL#` i)
{-# INLINE key2bit #-}
-- TODO: use INLINEABLE instead, in order to allow duplication? This one seems the most pressing


-- TODO: we might want to inline the appropriate definition of popCount in order to avoid indirection... Not sure why it isn't resolved already; apparently the primop doesn't have architecture support for me?
-- N.B., there are also popCount primops for only looking at the lower 8, 16, or 32 bits
-- popCount (W# x#) = I# (word2Int# (popCnt# x#))
bit2index :: Bitmap -> OneBit -> Index
bit2index p b = popCount (maskLT b p)
{-# INLINE bit2index #-}


key2index :: Bitmap -> Key -> Index
key2index p k = bit2index p (key2bit k)
{-# INLINE key2index #-}


{-
-- <http://stackoverflow.com/questions/757059>
-- <http://chessprogramming.wikispaces.com/BitScan>
-- <http://graphics.stanford.edu/~seander/bithacks.html>
-- TODO: or use ffs(3) via FFI? or the x86 instruction bsf?
bit2key :: OneBit -> Key

unsigned char lowestBitTable[256];
int get_lowest_set_bit(unsigned num) {
    unsigned mask = 1;
    for (int cnt = 1; cnt <= 32; cnt++, mask <<= 1) {
        if (num & mask) return cnt;
    }
    return 0;
}
for (int i = 0; i < 256; i++) lowestBitTable[i] = get_lowest_set_bit(i);

int find_first_bits_lookup_table(unsigned int value) {
    // note that order to check indices will depend whether you are on a big or little endian machine. This is for little-endian
    unsigned char *bytes = (unsigned char *)&value;
    if (bytes[0])
        return lowestBitTable[bytes[0]];
    else if (bytes[1])
        return lowestBitTable[bytes[1]] + 8;
    else if (bytes[2])
        return lowestBitTable[bytes[2]] + 16;
    else
        return lowestBitTable[bytes[3]] + 24;
}
-}


-- | Check if a bit is set in the bitmap.
elem :: OneBit -> Bitmap -> Bool
elem b p = (p .&. b /= 0)
{-# INLINE elem #-}
-- TODO: use INLINEABLE instead, in order to allow duplication?


-- | Check if a bit is unset in the bitmap.
notElem :: OneBit -> Bitmap -> Bool
notElem b p = (p .&. b == 0)
{-# INLINE notElem #-}
-- TODO: use INLINEABLE instead, in order to allow duplication?


----------------------------------------------------------------
----------------------------------------------------------------

data SparseArray a = SA !Bitmap !(Array# a)

-- No longer actually used anywhere
-- data MSparseArray s a = MSA !Bitmap !(MutableArray# s a)

----------------------------------------------------------------

-- We must box up the MutableArray# into something of kind * before we can return it. Rather than making an explicit box just to unwrap it, we use CPS to achieve the same effect with more efficient Core code. We need to inline it in order to really do the right thing with the CPS form
new :: Int -> a -> (MutableArray# s a -> ST s r) -> ST s r
new _n@(I# n) x cont =
    CHECK_GT("new", _n, (0 :: Int))
    ST $ \s ->
        case newArray# n x s of
        (# s', xs #) ->
            case cont xs of
            ST strep -> strep s'
{-# INLINE new #-}


new_ :: Int -> (MutableArray# s a -> ST s r) -> ST s r
new_ n = new n __undefinedElem
{-# INLINE new_ #-}


#if __GLASGOW_HASKELL__ >= 702
lengthMA :: MutableArray# s a -> Int
lengthMA xs = I# (sizeofMutableArray# xs)
{-# INLINE lengthMA #-}
-- TODO: use INLINEABLE instead, in order to allow duplication?
#endif


read :: MutableArray# s a -> Index -> ST s a
read !xs !_i@(I# i) =
    ST $ \s ->
    CHECK_BOUNDS("read", lengthMA xs, _i)
        readArray# xs i s
{-# INLINE read #-}


write :: MutableArray# s a -> Index -> a -> ST s ()
write !xs !_i@(I# i) x =
    ST $ \s ->
    CHECK_BOUNDS("write", lengthMA xs, _i)
        case writeArray# xs i x s of
        s' -> (# s' , () #)
{-# INLINE write #-}


-- TODO: why was n not marked with a bang??
-- | Unsafely copy the elements of an array. Array bounds are not checked.
copy :: MutableArray# s e -> Index -> MutableArray# s e -> Index -> Index -> ST s ()
#if __GLASGOW_HASKELL__ >= 702
{-# INLINE copy #-}
copy !src !_sidx@(I# sidx) !dst !_didx@(I# didx) _n@(I# n) =
    CHECK_GE("copy: sidx", _sidx, (0 :: Int))
    CHECK_GE("copy: didx", _didx, (0 :: Int))
    CHECK_GE("copy: n", _n, (0 :: Int))
    CHECK_BOUNDS("copy: src", lengthMA src, _sidx + _n - 1)
    CHECK_BOUNDS("copy: dst", lengthMA dst, _didx + _n - 1)
    ST $ \s ->
        case copyMutableArray# src sidx dst didx n s of
        s' -> (# s', () #)
#else
copy !src !sidx !dst !didx n =
    CHECK_GE("copy: sidx", _sidx, (0 :: Int))
    CHECK_GE("copy: didx", _didx, (0 :: Int))
    CHECK_GE("copy: n", _n, (0 :: Int))
    CHECK_BOUNDS("copy: src", lengthMA src, sidx + n - 1)
    CHECK_BOUNDS("copy: dst", lengthMA dst, didx + n - 1)
    go sidx didx 0
    where
    go !i !j !c
        | c < n     = do
            write dst j =<< read src i
            go (i+1) (j+1) (c+1)
        | otherwise = return ()
#endif


-- TODO: would it be faster just to allocate a new array and use copyMutableArray# for each half?
shiftUpOne :: MutableArray# s e -> Index -> Index -> ST s ()
shiftUpOne !xs !i !n = 
    CHECK_GE("shiftUpOne: i", i, (0 :: Int))
    CHECK_GE("shiftUpOne: n", n, (0 :: Int))
    CHECK_BOUNDS("shiftUpOne: ", lengthMA src, i + n) -- TODO: (subtract 1) ??
    go xs i (i + n)
    where
    go !xs !i !n
        | i < n     = do
            write xs n =<< read xs (n-1)
            go xs i (n-1)
        | otherwise = return ()


----------------------------------------------------------------
-- | Create a 'SparseArray' by trimming a 'MutableArray#'.
trim :: Bitmap -> MutableArray# s a -> ST s (SparseArray a)
trim !p !xs =
    case popCount p of
    _n@(I# n) ->
        -- Optimally it should be LT, but LE is safe.
        CHECK_LE("trim", _n, lengthMA xs)
        ST $ \s ->
            case freezeArray# xs 0# n s of
            (# s', xs' #) -> (# s', SA p xs' #)
{-# INLINE trim #-}


-- | Create a 'SparseArray' by freezing a 'MutableArray#' in place.
unsafeFreeze :: Bitmap -> MutableArray# s a -> ST s (SparseArray a)
unsafeFreeze !p !xs =
    -- Optimally it should be EQ, but LE is safe.
    CHECK_LE("unsafeFreeze", popCount p, lengthMA xs)
    ST $ \s ->
        case unsafeFreezeArray# xs s of
        (# s', xs' #) -> (# s', SA p xs' #)
{-# INLINE unsafeFreeze #-}


unsafeFreezeOrTrim
    :: Bool -> Bitmap -> MutableArray# s a -> ST s (SparseArray a)
unsafeFreezeOrTrim True  = unsafeFreeze
unsafeFreezeOrTrim False = trim
{-# INLINE unsafeFreezeOrTrim #-}


{-
-- This has performance issues because it's not smart enough to eliminate the MSA by pushing 'unsafeFreezeMSA' to the leaves...
runMSA :: (forall s. ST s (MSparseArray s a)) -> SparseArray a
runMSA act = runST (unsafeFreezeMSA =<< act)
{-# INLINE runMSA #-}
-}


----------------------------------------------------------------
----------------------------------------------------------------

-- TODO: is there a better implementation of length-0 arrays?
empty :: SparseArray a
empty = runST (new_ 0 $ unsafeFreeze 0)


-- TODO: would it be better to have an invariant that SAs are non-empty, and use Maybe when we 'trim', 'filter', etc?
-- | /O(1)/. Is the array empty?
null :: SparseArray a -> Bool
null (SA 0 _) = True
null _        = False
{-# INLINE null #-}
-- TODO: use INLINEABLE instead, in order to allow duplication?


-- | /O(1)/. Get the number of elements in the array.
length :: SparseArray a -> Int
length (SA p _) = popCount p
{-# INLINE length #-}


-- | /O(1)/. Are the first array's keys a subset of second's?
isSubarrayOf :: SparseArray a -> SparseArray b -> Bool
isSubarrayOf (SA p _) (SA q _) = (p .&. q == p)
{-# INLINE isSubarrayOf #-}
-- TODO: use INLINEABLE instead, in order to allow duplication? 


-- HACK: must use the prefix notation in the definition, otherwise it isn't recognized for some reason...
(!) :: Array# a -> Index -> a
(!) xs (I# i) = case indexArray# xs i of (# x #) -> x
{-# INLINE (!) #-}

-- | A strict variant of '(!)' for hoisting the array lookup out of a continuation.
index :: Array# a -> Index -> (a -> r) -> r
index xs (I# i) cont = case indexArray# xs i of (# x #) -> cont x
{-# INLINE index #-}


member :: Key -> SparseArray a -> Bool
member !k (SA p _) = key2bit k `elem` p
{-# INLINE member #-}


lookup :: Key -> SparseArray a -> Maybe a
lookup !k (SA p xs) =
    let !b = key2bit k in
    if b `elem` p
    then Just (xs ! bit2index p b)
    else Nothing
{-# INLINE lookup #-}


-- This version forces the array lookup before returning
-- TODO: but in what circumstances is that preferable? Do we ever actually want the lazier version?
lookup' :: Key -> SparseArray a -> Maybe a
lookup' !k (SA p xs) =
    let !b = key2bit k in
    if b `elem` p
    then index xs (bit2index p b) Just
    else Nothing
{-# INLINE lookup' #-}


singleton :: Key -> a -> SparseArray a
singleton !k x = runST (new 1 x $ unsafeFreeze (key2bit k))
{-# INLINE singleton #-}


doubleton :: Key -> a -> Key -> a -> SparseArray a
doubleton !k x !l y = runST $
    new 2 x $ \xs -> do
    write xs (if k < l then 1 else 0) y
    unsafeFreeze (key2bit k .|. key2bit l) xs
{-# INLINE doubleton #-}


data DynamicSA s a = DSA
    !Bitmap -- The bitmap in progress
    !Key    -- The maximum key seen so far; should be the highest set bit
    !Index  -- The next free index in the array
    !Index  -- The first invalid index, aka the size of the array
    !(MutableArray# s a) -- The array in progress


unsafeFreezeDSA :: DynamicSA s a -> ST s (SparseArray a)
unsafeFreezeDSA (DSA p _maxK n maxN xs) =
    unsafeFreezeOrTrim (n == maxN) p xs
{-# INLINE unsafeFreezeDSA #-}


insertDSA :: Key -> a -> DynamicSA s a -> ST s (DynamicSA s a)
insertDSA k v dsa = insertDSA_ k v dsa return


-- We CPS it in order to avoid boxing up the DSA on returning; to really do that we must INLINE as well.
insertDSA_ :: Key -> a -> DynamicSA s a -> (DynamicSA s a -> ST s r) -> ST s r
{-# INLINE insertDSA_ #-}
insertDSA_ !k x dsa@(DSA p maxK n maxN xs) cont
    | b `elem` p = do
        write xs i x
        cont dsa
    | otherwise =
        case (n < maxN, k > maxK) of
        (True, True) -> do
            write xs n x
            cont $! DSA p' k n' maxN xs
        (True, False) -> do
            -- TODO: if shiftUpOne cannot be implemented efficiently, then maybe we should just reallocate and copy everything?
            shiftUpOne xs i (n-i)
            write xs i x
            cont $! DSA p' maxK n' maxN xs
        (False, True) ->
            new_ maxN' $ \xs' -> do
            copy xs 0 xs' 0 maxN
            write xs' n x
            cont $! DSA p' k n' maxN' xs'
        (False, False) ->
            new_ maxN' $ \xs' -> do
            copy xs 0 xs' 0 i
            write xs' i x
            copy xs i xs' (i+1) (maxN-i)
            cont $! DSA p' maxK n' maxN' xs'
    where
    b     = key2bit k
    i     = bit2index p b
    p'    = p .|. b
    n'    = n + 1
    maxN' = 2*maxN
    -- TODO: maxN' gets let-bound for sharing; we might want to add a pragma hinting that it can be duplicated relatively cheaply (or maybe the gcc/llvm backends can figure that out?)


-- Since we know the effective size limit for our use cases is 16(=2^Key), we just start there so that we only ever need to allocate once and then trim; thus avoiding the need to resize along the way.
-- TODO: that being the case, we should get rid of DSA and just use SA and insertSA... Doing so will save us the (n < maxN) check every iteration, albeit at the cost of unsafety if we overflow...
fromList :: [(Key,a)] -> SparseArray a
fromList = fromList_ 16
{-# INLINE fromList #-}


fromList_ :: Int -> [(Key,a)] -> SparseArray a
fromList_ !_ []           = empty
fromList_  n ((!k,x):kxs) = runST $
    new n x $ \xs -> do
    write xs 1 x
    go kxs $! DSA (key2bit k) k 1 n xs
    where
    -- We must use the CPS version of insertDSA in order to unpack the DSA.
    go []           dsa = unsafeFreezeDSA dsa
    go ((!k,x):kxs) dsa = insertDSA_ k x dsa (go kxs)
    {-
    -- This version is prettier, but it doesn't unpack the DSA so it's no good.
    go []           = unsafeFreezeDSA
    go ((!k,x):kxs) = go kxs <=< insertDSA k x
    -}

{-
-- The inline pragma is to avoid warnings about the rules possibly not firing; but having it means we don't get the worker/wrapper transform to unpack DSA in the loop...
{-# NOINLINE [1] fromList_ #-}
{-# RULES
"fromList_/singleton"
    forall n k x.
        fromList_ n [(k,x)] = singleton k x
"fromList_/doubleton"
    forall n k1 x1 k2 x2.
        fromList_ n [(k1,x1),(k2,x2)] = doubleton k1 x1 k2 x2
    #-}
-}

{-
fromAscList :: [(Key,a)] -> SparseArray a
fromAscList = fromAscList_ 16
{-# INLINE fromAscList #-}

-- TODO: clean this up based on insertDSA
fromAscList_ :: Int -> [(Key,a)] -> SparseArray a
fromAscList_ !_ []           = empty
fromAscList_  n ((!k,x):kxs) = ...
-}


toList :: SparseArray a -> [(Key,a)]
toList = toListBy (,)
{-# INLINE toList #-}


toListBy :: (Key -> a -> b) -> SparseArray a -> [b]
toListBy f xz = build (\cons nil -> toListByFB ((cons .) . f) nil xz)
{-# INLINE toListBy #-}

-- TODO: can we improve the asymptotics without needing bit2key?
toListByFB :: (Key -> a -> c -> c) -> c -> SparseArray a -> c
toListByFB cons_f nil = \(SA p xs) -> go xs 0 p 1 0
    where
    go !xs !i !p !b !k
        | p == 0     = nil
        | b `elem` p = cons_f k (xs ! i) $
            go xs (i+1) (p `xor` b) (bsucc b) (k+1)
        | otherwise  = go xs i p (bsucc b) (k+1)
{-# INLINE [0] toListByFB #-}

{-
-- this is silly...
{-# RULES
-- These rules are more robust, but only apply before inlining toListBy
"toListBy const"
        toListBy (\k _ -> k) = keys
"toListBy (flip const)"
        toListBy (\_ x -> x) = elems
-- These rules are very fragile, which is why we (should) wait to inline toListBy
"toListByFB const {eta}"
    forall cons.
        toListByFB (\k _ ks -> cons k ks) = keysFB cons
"toListByFB const"
    forall cons.
        toListByFB (\k _ -> cons k) = keysFB cons
"toListByFB (flip const) {eta2}"
    forall cons.
        toListByFB (\_ v vs -> cons v vs) = foldr cons
"toListByFB (flip const) {eta1}"
    forall cons.
        toListByFB (\_ v -> cons v) = foldr cons
"toListByFB (flip const)"
    forall cons.
        toListByFB (\_ -> cons) = foldr cons
    #-}
-}


keys :: SparseArray a -> [Key]
keys xz = build (\cons nil -> keysFB cons nil xz)
{-# INLINE keys #-}

-- TODO: can we improve the asymptotics without needing bit2key?
keysFB :: (Key -> c -> c) -> c -> SparseArray a -> c
keysFB cons nil = \(SA p _) -> go p 1 1
    where
    go !p !b !k
        | p == 0     = nil
        | b `elem` p = cons k $ go (p `xor` b) (bsucc b) (k+1)
        | otherwise  =          go p           (bsucc b) (k+1)
{-# INLINE [0] keysFB #-}


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
    rnf = \(SA p xs) -> go xs (popCount p) 0
        where
        go !xs !n !i
            | i < n     = rnf (xs ! i) `seq` go xs n (i+1)
            | otherwise = ()


-- BUG: using fmap and fmap' may result in allocating the @SA@ pairs in places where it's not necessary... Does the INLINE help?
instance Functor SparseArray where
    fmap = map


map :: (a -> b) -> SparseArray a -> SparseArray b
map f =
    \(SA p xs) ->
        let !n = popCount p in
        runST $
            new_ n $ \ys ->
            go p xs n ys 0
    where
    go !p !xs !n !ys !i
        | i < n     = do write ys i (f (xs ! i)); go p xs n ys (i+1)
        | otherwise = unsafeFreeze p ys
{-# INLINE [0] map #-}
{-# RULES "map id"  map id = id #-}


-- TODO: is there a class for this yet?
map' :: (a -> b) -> SparseArray a -> SparseArray b
map' f =
    \(SA p xs) ->
        let !n = popCount p in
        runST $
            new_ n $ \ys -> 
            go p xs n ys 0
    where
    go !p !xs !n !ys !i
        | i < n     = do write ys i $! f (xs ! i); go p xs n ys (i+1)
        | otherwise = unsafeFreeze p ys
{-
{-# INLINE [0] map' #-}
{-# RULES "map' id"  map' id = seqArray #-}
-}


-- N.B., trying to force the closure to be generated before passing in the extra arguments via lambda does not work. GHC floats the closure down... presumably because the arities of these methods are already specified...
-- N.B., adding INLINE pragma prevents these from generating the version which unpacks SA!
instance Foldable SparseArray where
    fold      = foldr' mappend mempty
    
    foldMap f = foldr' (mappend . f) mempty
    
    foldr f z = \ (SA p xs) -> go xs (popCount p) 0
        where
        go !xs !n !i
            | i < n     = f (xs ! i) (go xs n (i+1))
            | otherwise = z
    
    foldr' f = \ !z0 (SA p xs) -> go xs (popCount p - 1) z0
        where
        go !xs !n z
            | n >= 0    = go xs (n-1) $! f (xs ! n) z
            | otherwise = z
    
    foldl f z = \ (SA p xs) -> go xs (popCount p - 1)
        where
        go !xs !n
            | n >= 0    = f (go xs (n-1)) (xs ! n)
            | otherwise = z
    
    foldl' f = \ !z0 (SA p xs) -> go xs (popCount p) 0 z0
        where
        go !xs !n !i z
            | i < n     = go xs n (i+1) $! f z (xs ! i)
            | otherwise = z

{-
-- TODO
foldrWithKey
foldrWithKey'
foldlWithKey
foldlWithKey'
-}

-- | Short-circuiting version of foldl'. N.B., this is not necessarily
-- strict in the @b@ or @c@ values, but you can make it so by forcing
-- them before wrapping with @Left@ or @Right@.
foldL :: (b -> a -> Either b c) -> b -> SparseArray a -> Either b c
foldL f = \z0 (SA p xs) -> go xs (popCount p) 0 z0
    where
    go !xs !n !i z
        | i < n      =
            case f z (xs ! i) of
            Left  z'    -> go xs n (i+1) z'
            r@(Right _) -> r
        | otherwise     =  Left z


-- | Short-circuiting version of foldr'. N.B., this is not necessarily
-- strict in the @b@ or @c@ values, but you can make it so by forcing
-- them before wrapping with @Left@ or @Right@.
foldR :: (a -> b -> Either b c) -> b -> SparseArray a -> Either b c
foldR f = \z0 (SA p xs) -> go xs (popCount p - 1) z0
    where
    go !xs !n z
        | n >= 0        =
            case f (xs ! n) z of
            Left  z'    -> go xs (n-1) z'
            r@(Right _) -> r
        | otherwise     =  Left z


{-
-- BUG: does this even make sense?
instance Traversable SparseArray where
    traverse f = foldrWithKey' cons_f (pure empty)
        where
        cons_f k x ys = insert k <$> f x <*> ys
    
    -- TODO: can we optimize 'sequenceA' over the default?
-}


-- we can optimize Traversable for ST since we know ST computations "only contain a single value", and thus we don't need to reallocate the underlying array for each nondeterminism. We can also eliminate/fuse the runST call
sequenceST :: SparseArray (ST s a) -> ST s (SparseArray a)
sequenceST = traverseST id


traverseST :: (a -> ST s b) -> SparseArray a -> ST s (SparseArray b)
traverseST f = \ (SA p xs) ->
    let !n = popCount p in
        new_ n $ \ys ->
        go p xs n ys 0
    where
    go !p !xs !n !ys !i
        | i < n     = do write ys i =<< f (xs ! i); go p xs n ys (i+1)
        | otherwise = unsafeFreeze p ys


----------------------------------------------------------------

-- TODO: float @go@ out instead of closing over stuff?
-- AKA: mapMaybe
filterMap :: (a -> Maybe b) -> SparseArray a -> SparseArray b
filterMap f (SA p xs) =
    let !n = popCount p in
    runST $
        new_ n $ \ys -> do
        let go !i !bi !j !q
                | i >= n    =
                    unsafeFreezeOrTrim (i == j {- aka: p == q -}) q ys
                | otherwise =
                    case f (xs ! i) of
                    Just y  -> do
                        write ys j y
                        go (i+1) (nextBit p bi) (j+1) (q .|. bi)
                    Nothing ->
                        go (i+1) (nextBit p bi) j q
            --
        go 0 (firstBit p) 0 0

-- The inline pragma is to avoid warnings about the rules possibly not firing
{-# NOINLINE [1] filterMap #-}
{-# RULES "filterMap Just"  filterMap Just = id #-}


-- TODO: float @go@ out instead of closing over stuff?
filter :: (a -> Bool) -> SparseArray a -> SparseArray a
filter f xz@(SA p xs) =
    let !n = popCount p in
    runST $
        new_ n $ \ys -> do
        let go !i !bi !j !q
                | i >= n    =
                    if i == j {- aka: p == q -}
                    then return xz
                    else trim q ys
                | f x       = do
                    write ys j x
                    go (i+1) (nextBit p bi) (j+1) (q .|. bi)
                | otherwise =
                    go (i+1) (nextBit p bi) j q
                where x = xs ! i
            --
        go 0 (firstBit p) 0 0

-- The inline pragma is to avoid warnings about the rules possibly not firing
{-# NOINLINE [1] filter #-}
{-# RULES
"filter (const True)"     filter (\_ -> True)  = id
"filter (const False)"    filter (\_ -> False) = const empty
    #-}


-- TODO: float @go@ out instead of closing over stuff?
partition :: (a -> Bool) -> SparseArray a -> (SparseArray a, SparseArray a)
partition f xz@(SA p xs) =
    let !n = popCount p in
    runST $
        new_ n $ \ys ->
        new_ n $ \zs -> do
        let go !i !bi !j !q !k !r
                | i >= n    =
                    if i == j {- aka: p == q; aka: r == 0 -}
                    then return (xz, empty)
                    else
                        if i == k {- aka: p == r; aka q == 0 -}
                        then return (empty, xz)
                        else do
                            yz <- trim q ys
                            zz <- trim r zs
                            return (yz, zz)
                | f x       = do
                    write ys j x
                    go (i+1) (nextBit p bi) (j+1) (q .|. bi) k r
                | otherwise = do
                    write zs k x
                    go (i+1) (nextBit p bi) j q (k+1) (r .|. bi)
                where x = xs ! i
            --
        go 0 (firstBit p) 0 0 0 0

-- The inline pragma is to avoid warnings about the rules possibly not firing
{-# NOINLINE [1] partition #-}
{-# RULES
"partition (const True)"     partition (\_ -> True)  = \xz -> (xz, empty)
"partition (const False)"    partition (\_ -> False) = \xz -> (empty, xz)
    #-}



----------------------------------------------------------------
-- All these functions assume (xz `isSubarrayOf` yz) and operate accordingly. If that's not the case, then they work as if operating on the subarray of xz such that it is the case



-- TODO: float @go@ out instead of closing over stuff?
rzipWith'_
    :: (a -> b -> c) -> (b -> c)
    -> SparseArray a -> SparseArray b -> SparseArray c
rzipWith'_ f g (SA p xs) (SA q ys) =
    let !n = popCount q in
    runST $
        new_ n $ \zs -> do
        let go !i !b !j
                | j >= n     = unsafeFreeze q zs
                | b `elem` p = do
                    write zs j $! f (xs ! i) (ys ! j)
                    go (i+1) (nextBit q b) (j+1)
                | otherwise  = do
                    write zs j $! g (ys ! j)
                    go i     (nextBit q b) (j+1)
            --
        go 0 (firstBit q) 0

rzipWith'
    :: (Maybe a -> b -> c)
    -> SparseArray a -> SparseArray b -> SparseArray c
rzipWith' f = rzipWith'_ (f . Just) (f Nothing)
{-# INLINE rzipWith' #-}


-- TODO: float @go@ out instead of closing over stuff?
rzipWith_
    :: (a -> b -> c) -> (b -> c)
    -> SparseArray a -> SparseArray b -> SparseArray c
rzipWith_ f g (SA p xs) (SA q ys) =
    let !n = popCount q in
    runST $
        new_ n $ \zs -> do
        let go !i !b !j
                | j >= n     = unsafeFreeze q zs
                | b `elem` p = do
                    write zs j (f (xs ! i) (ys ! j))
                    go (i+1) (nextBit q b) (j+1)
                | otherwise  = do
                    write zs j (g (ys ! j))
                    go i     (nextBit q b) (j+1)
            --
        go 0 (firstBit q) 0


rzipWith
    :: (Maybe a -> b -> c)
    -> SparseArray a -> SparseArray b -> SparseArray c
rzipWith f = rzipWith_ (f . Just) (f Nothing)
{-# INLINE rzipWith #-}


rzip :: SparseArray a -> SparseArray b -> SparseArray (Maybe a, b)
rzip = rzipWith (,)


-- TODO: float @go@ out instead of closing over stuff?
rzipFilter_
    :: (a -> b -> Maybe c) -> (b -> Maybe c)
    -> SparseArray a -> SparseArray b -> SparseArray c
rzipFilter_ f g (SA p xs) (SA q ys) =
    let !n = popCount q in
    runST $
        new_ n $ \zs -> do
        let go !i !b !j !k !r
                | j >= n     =
                    unsafeFreezeOrTrim (j == k {- aka: q == r -}) r zs
                | b `elem` p =
                    case f (xs ! i) (ys ! j) of
                    Just z -> do
                        write zs k z
                        go (i+1) (nextBit q b) (j+1) (k+1) (r .|. b)
                    Nothing ->
                        go (i+1) (nextBit q b) (j+1) k r
                | otherwise  =
                    case g (ys ! j) of
                    Just z -> do
                        write zs k z
                        go i (nextBit q b) (j+1) (k+1) (r .|. b)
                    Nothing ->
                        go i (nextBit q b) (j+1) k r
            --
        go 0 (firstBit q) 0 0 0


rzipFilter
    :: (Maybe a -> b -> Maybe c)
    -> SparseArray a -> SparseArray b -> SparseArray c
rzipFilter f = rzipFilter_ (f . Just) (f Nothing)
{-# INLINE rzipFilter #-}


----------------------------------------------------------------
----------------------------------------------------------------
-- TODO: is there a way to unify unionWith_ and unionWith'_ by abstracting over ($) vs ($!) such that those applicators get inlined appropriately so as to have no overhead for this abstraction?

-- | Left-biased union.
unionL :: SparseArray a -> SparseArray a -> SparseArray a
unionL = unionWith_ id const id


-- | Right-biased union.
unionR :: SparseArray a -> SparseArray a -> SparseArray a
unionR = unionWith_ id (flip const) id


unionWith :: (Or a b -> c) -> SparseArray a -> SparseArray b -> SparseArray c
unionWith f = unionWith_ (f . Fst) ((f .) . Both) (f . Snd)
{-# INLINE unionWith #-}


unionWith_
    :: (a -> c) -> (a -> b -> c) -> (b -> c)
    -> SparseArray a -> SparseArray b -> SparseArray c
unionWith_ f g h (SA p xs) (SA q ys) =
    let !r = p .|. q
        !n = popCount r
    in runST $
        new_ n $ \zs -> do
        let go !b !i !j !k
                | k >= n    = unsafeFreeze r zs
                | otherwise =
                    case (b `elem` p, b `elem` q) of
                    (True, False) -> do
                        write zs k (f (xs ! i))
                        go (nextBit r b) (i+1) j (k+1)
                    (True, True) -> do
                        write zs k (g (xs ! i) (ys ! j))
                        go (nextBit r b) (i+1) (j+1) (k+1)
                    (False, True) -> do
                        write zs k (h (ys ! j))
                        go (nextBit r b) i (j+1) (k+1)
                    (False, False) -> __impossible "unionWith_"
            --
        go (firstBit r) 0 0 0


unionWith' :: (Or a b -> c) -> SparseArray a -> SparseArray b -> SparseArray c
unionWith' f = unionWith'_ (f . Fst) ((f .) . Both) (f . Snd)
{-# INLINE unionWith' #-}

unionWith'_
    :: (a -> c) -> (a -> b -> c) -> (b -> c)
    -> SparseArray a -> SparseArray b -> SparseArray c
unionWith'_ f g h (SA p xs) (SA q ys) =
    let !r = p .|. q
        !n = popCount r
    in runST $
        new_ n $ \zs -> do
        let go !b !i !j !k
                | k >= n    = unsafeFreeze r zs
                | otherwise =
                    case (b `elem` p, b `elem` q) of
                    (True, False) -> do
                        write zs k $! f (xs ! i)
                        go (nextBit r b) (i+1) j (k+1)
                    (True, True) -> do
                        write zs k $! g (xs ! i) (ys ! j)
                        go (nextBit r b) (i+1) (j+1) (k+1)
                    (False, True) -> do
                        write zs k $! h (ys ! j)
                        go (nextBit r b) i (j+1) (k+1)
                    (False, False) -> __impossible "unionWith'_"
            --
        go (firstBit r) 0 0 0


unionFilterWith
    :: (Or a b -> Maybe c)
    -> SparseArray a -> SparseArray b -> SparseArray c
unionFilterWith f = unionFilterWith_ (f . Fst) ((f .) . Both) (f . Snd)
{-# INLINE unionFilterWith #-}


-- TODO: can we get rid of xor to improve the asymptotics without needing bit2key?
unionFilterWith_
    :: (a -> Maybe c) -> (a -> b -> Maybe c) -> (b -> Maybe c)
    -> SparseArray a -> SparseArray b -> SparseArray c
unionFilterWith_ f g h (SA p xs) (SA q ys) =
    let !r0 = p .|. q
        !n  = popCount r0
    in runST $
        new_ n $ \zs -> do
        let go !r0 !b !i !j !k !r
                | r0 == 0   = unsafeFreezeOrTrim (k >= n) r zs
                | otherwise =
                    case (b `elem` p, b `elem` q) of
                    (True, False) ->
                        case f (xs ! i) of
                        Just z -> do
                            write zs k z
                            go (r0 `xor` b) (nextBit r0 b) (i+1) j (k+1) (r .|. b)
                        Nothing ->
                            go (r0 `xor` b) (nextBit r0 b) (i+1) j k r
                    (True, True) ->
                        case g (xs ! i) (ys ! j) of
                        Just z -> do
                            write zs k z
                            go (r0 `xor` b) (nextBit r0 b) (i+1) (j+1) (k+1) (r .|. b)
                        Nothing ->
                            go (r0 `xor` b) (nextBit r0 b) (i+1) (j+1) k r
                    (False, True) ->
                        case h (ys ! j) of
                        Just z -> do
                            write zs k z
                            go (r0 `xor` b) (nextBit r0 b) i (j+1) (k+1) (r .|. b)
                        Nothing ->
                            go (r0 `xor` b) (nextBit r0 b) i (j+1) k r
                    (False, False) -> __impossible "unionFilterWith_"
            --
        go r0 (firstBit r0) 0 0 0 0


{-
unionFilterWithKey
    :: (Key -> Or a b -> Maybe c)
    -> SparseArray a -> SparseArray b -> SparseArray c
unionFilterWithKey f =
    unionFilterWithKey_
        (\k x   -> f k (Fst x))
        (\k x y -> f k (Both x y))
        (\k   y -> f k (Snd y))
{-# INLINE unionFilterWithKey #-}


-- | This is the most powerful binary merging function for 'SparseArray', and consequently the most expensive. If you can get away with using one of the simpler functions, you should.
unionFilterWithKey_
    :: (Key -> a -> Maybe c)
    -> (Key -> a -> b -> Maybe c)
    -> (Key -> b -> Maybe c)
    -> SparseArray a -> SparseArray b -> SparseArray c
-}

---------------------------------------------------------------- 
-- | Left-biased intersection.
intersectionL :: SparseArray a -> SparseArray b -> SparseArray a
intersectionL = intersectionWith const


-- | Right-biased intersection.
intersectionR :: SparseArray a -> SparseArray b -> SparseArray b
intersectionR = intersectionWith (flip const)


intersectionWith
    :: (a -> b -> c)
    -> SparseArray a -> SparseArray b -> SparseArray c
intersectionWith f =
    \(SA p xs) (SA q ys) ->
        let !r = p .&. q
            !n = popCount r
        in runST $
            new_ n $ \zs ->
            go n p xs q ys r zs (firstBit r) 0
    where
    go !n !p !xs !q !ys !r !zs !b !k
        | k >= n    = unsafeFreeze r zs
        | otherwise = do
            write zs k (f (xs ! bit2index p b) (ys ! bit2index q b))
            go n p xs q ys r zs (nextBit r b) (k+1)

intersectionWith'
    :: (a -> b -> c)
    -> SparseArray a -> SparseArray b -> SparseArray c
intersectionWith' f =
    \(SA p xs) (SA q ys) ->
        let !r = p .&. q
            !n = popCount r
        in runST $
            new_ n $ \zs ->
            go n p xs q ys r zs (firstBit r) 0
    where
    go !n !p !xs !q !ys !r !zs !b !k
        | k >= n    = unsafeFreeze r zs
        | otherwise = do
            write zs k $! f (xs ! bit2index p b) (ys ! bit2index q b)
            go n p xs q ys r zs (nextBit r b) (k+1)

{-
intersectionWithKey
    :: (Key -> a -> b -> c)
    -> SparseArray a -> SparseArray b -> SparseArray c
-}

---------------------------------------------------------------- 
    -- differenceL, differenceR, symdiff, symdiffWith, symdiffWith_

differenceL
    :: SparseArray a -> SparseArray b -> SparseArray a
differenceL = \(SA p xs) (SA q _) ->
        let !r = p .&. complement q
            !n = popCount r
        in runST $
            new_ n $ \zs -> do
            go p xs r zs n 0 (firstBit r)
    where
    go !p !xs !r !zs !n !k !b
        | k >= n    = unsafeFreeze r zs
        | otherwise =
            -- We inline (!) in order to hoist it out and avoid thunks in the new array.
            index xs (bit2index p b) $ \x -> do
                write zs k x
                go p xs r zs n (k+1) (nextBit r b)


differenceR
    :: SparseArray a -> SparseArray b -> SparseArray b
differenceR = flip differenceL

----------------------------------------------------------------
----------------------------------------------------------- fin.
