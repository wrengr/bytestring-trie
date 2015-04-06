-- The -fno-full-laziness required by Data.Trie.ArrayMapped.UnsafeST
{-# OPTIONS_GHC
    -Wall -fwarn-tabs -fno-warn-name-shadowing -fno-warn-unused-binds
    -fno-full-laziness -funbox-strict-fields
    #-}

{-# LANGUAGE BangPatterns
           , CPP
           , ForeignFunctionInterface
           , MagicHash
           , Rank2Types
           , UnboxedTuples
           , Trustworthy
           #-}

#if __GLASGOW_HASKELL__ >= 710
{-# LANGUAGE RoleAnnotations #-}
#endif

-- TODO: cf <http://hackage.haskell.org/package/base-4.8.0.0/docs/src/GHC-Arr.html> for more implementation hacks

-- TODO: for our purposes, should we reimplement everything using SmallArray# and SmallMutableArray# ? cf., ghc-prim-0.4.0.0

----------------------------------------------------------------
--                                                  ~ 2015.04.05
-- |
-- Module      :  Data.Trie.ArrayMapped.SparseArray
-- Copyright   :  Copyright (c) 2014--2015 wren gayle romano; 2010--2012 Johan Tibell
-- License     :  BSD3
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  experimental
-- Portability :  GHC only
--
-- For the asymptotic performance numbers: /O(FI)/ is the asymptotic
-- performance of @fromIntegral :: Key -> Int@, which is almost
-- surely /O(1)/; /O(PC)/ is the asymptotic performance of @'popCount'
-- :: Word -> Int@, which may be /O(1)/ but may also be /O(n)/; and
-- /O(BK)/ is the asymptotic performance of 'bit2key' which is
-- currently the same as POSIX's @ffs(3)@.
----------------------------------------------------------------

module Data.Trie.ArrayMapped.SparseArray
    (
    -- * Sparse arrays
      Key, SparseArray()
    -- ** Constructors
    , empty, singleton, doubleton
    -- ** Predicates
    , null, length, member, isSubarrayOf
    -- ** Single-element operations
    , lookup_, lookup'
    , insert, remove
    -- ** Conversion to\/from lists
    , fromList -- fromAscList, fromDistinctAscList
    -- Can't use the name toList because of conflict with Foldable
    , assocs, assocsBy, keys, elems
    
    -- * Views
    , SubsingletonView(..), viewSubsingleton
    
    -- * Map-like functions
    -- ** Mapping
    , map, map'
    , mapWithKey, mapWithKey'
    -- ** Filtering
    , filter,    filterWithKey
    , filterMap, filterMapWithKey -- aka mapMaybe{,WithKey}
    -- ** Partitioning
    , partition,    partitionWithKey
    , partitionMap, partitionMapWithKey -- aka mapEither{,WithKey}
    
    -- * Extra folding functions
    , foldL, foldR
    , foldrWithKey, foldrWithKey'
    , foldlWithKey, foldlWithKey'
    
    -- * Extra traversal functions
    , sequenceST, traverseST
    
    -- * Set-theoretic operations
    -- ** Right-biased zipping functions
    , rzip, rzipWith, rzipWith_
    , rzipWith', rzipWith'_
    , rzipWithKey, rzipWithKey_
    , rzipWithKey', rzipWithKey'_
    , rzipFilter, rzipFilter_
    
    -- ** TODO: left-biased zipping functions
    -- lzip, lzipWith, lzipWith_
    -- lzipWithKey, lzipWithKey_
    -- lzipWith', lzipWith'_
    -- lzipWithKey', lzipWithKey'_
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
    , intersectionWith' -- intersectionWithKey'
    -- intersectionFilterWith
    -- intersectionsWith
    
    -- ** Difference-like operators
    , differenceL, differenceR
    -- symdiff, symdiffWith, symdiffWith_
    
    ----
    
    -- new, new_, trim, unsafeFreeze, unsafeFreezeOrTrim
    ) where

import Prelude hiding
    ( null
    , lookup
    , filter
    , foldr
    , foldl
    , length
    , map
    , read
    , elem
    , notElem
#if __GLASGOW_HASKELL__ >= 710
    , traverse
#endif
    )

import qualified Data.Foldable as F
{-
    hiding
    ( elem
    , notElem
    , toList
#if __GLASGOW_HASKELL__ >= 710
    , null
    , length
#endif
    )
-}
import Data.Traversable               (Traversable(traverse))
-- import Control.Applicative         (Applicative(..))
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative            ((<$>))
#endif
import Control.Monad                  (replicateM)
import Control.Monad.ST               -- hiding (runST)
-- import Data.Trie.ArrayMapped.UnsafeST (runST)
import GHC.ST                         (ST(..))
import Control.DeepSeq                (NFData(rnf))
import Data.Binary
#if __GLASGOW_HASKELL__ < 710
import Data.Monoid                    (Monoid(..))
import Data.Word
#endif
import Data.Bits                      ((.&.), (.|.), xor)
-- The 'popCount' import requires base >= 4.5.0 or GHC >= 7.4
-- On older compilers we can use "Data.Trie.ArrayMapped.PopCount" instead
import Data.Bits                      (popCount)
import Data.Or                        (Or(..))

-- GHC 7.7 exports toList/fromList from GHC.Exts
-- In order to avoid warnings on previous GHC versions, we provide
-- an explicit import list instead of only hiding the offending symbols
import GHC.Exts
    ( build
    , Word(W#), Int(I#), uncheckedShiftL#, not#, and#, neWord#
    , Array#, newArray#, readArray#, writeArray#, indexArray#, freezeArray#, unsafeFreezeArray#, MutableArray#
#if __GLASGOW_HASKELL__ >= 702
    , sizeofMutableArray#, copyMutableArray#
    -- TODO: Which version added these?
    , copyArray#
    , isTrue#, sameMutableArray#
#endif
    )
-- for c_ffs, used by bit2key, used in turn by viewSubsingleton
import Foreign.C (CInt(..))

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
-- Section: Bit bashing

-- | Indices into a 'SparseArray' or 'DynamicSA'.
type Key    = Word8 -- Actually, should be a Word4 for our uses...
-- | A set of inhabited positions in a 'SparseArray' or 'DynamicSA'.
type Bitmap = Word  -- Actually, should be a 2^Key == 2^(2^4) == Word16
-- | A 'Bitmap' with exactly one set bit.
type OneBit = Bitmap 
-- | Indicies into the underlying 'Array#' or 'MutableArray#'.
type Index  = Int 
    -- HACK: actually, this should be Int#, but that causes code ugliness


-- | /O(1)/. Given a bit, return the next bit.
--
-- > bsucc (bit k) == bit (k+1)
bsucc :: OneBit -> OneBit
bsucc (W# b) = W# (b `uncheckedShiftL#` 1#)
{-# INLINE bsucc #-}
-- TODO: use INLINEABLE instead, in order to allow duplication?
-- TODO: use Data.Trie.ArrayMapped.UnsafeShift.unsafeShiftL instead?


-- HACK: the definition of 'complement' in Bits is (xor maxBound)
-- instead of not# (in order to narrow things as appropriate). But
-- since we're only going to turn around and use (.&.) we can get
-- rid of that and use not# directly. This does assume all the
-- extraneous high-order bits are zero instead of garbage though.
complement :: Word -> Word
complement (W# w) = W# (not# w)
{-# INLINE complement #-}

-- N.B., via twos-complement we have that:
-- negate x == complement (x - 1) == complement x + 1


-- | /O(1)/. Set all bits strictly below the argument.
maskLT :: Bitmap -> OneBit -> Bitmap
maskLT p b = p .&. (b - 1)
{-# INLINE maskLT #-}


-- | /O(1)/. Set all bits below or equal to the argument.
maskLE :: Bitmap -> OneBit -> Bitmap
maskLE p b = p .&. (bsucc b - 1)
{-# INLINE maskLE #-}


-- | /O(1)/. Set all bits strictly above the argument.
maskGT :: Bitmap -> OneBit -> Bitmap
maskGT p b = p .&. negate (bsucc b)
{-# INLINE maskGT #-}


-- | /O(1)/. Set all bits strictly above or equal to the argument.
maskGE :: Bitmap -> OneBit -> Bitmap
maskGE p b = p .&. negate b
{-# INLINE maskGE #-}


-- | /O(1)/. Get the first set bit in @p@.
getFirstBit :: Bitmap -> OneBit
getFirstBit p = p .&. negate p
{-# INLINE getFirstBit #-}


-- | /O(1)/. Clear the first set bit in @p@ (without knowing what
-- it is beforehand). If you already know what it is, you can use
-- 'xor' instead.
clearFirstBit :: Bitmap -> OneBit
clearFirstBit p = p .&. (p-1)
{-# INLINE clearFirstBit #-}


-- | /O(1)/. Get the second set bit in @p@.
getSecondBit :: Bitmap -> OneBit
getSecondBit p = getFirstBit (clearFirstBit p)
{-# INLINE getSecondBit #-}


-- | /O(1)/. Get the next set bit in @p@; i.e., the first set bit after @b@.
getNextBit :: Bitmap -> OneBit -> OneBit
getNextBit p b = getFirstBit (p `maskGT` b)
{-# INLINE getNextBit #-}


-- | /O(FI)/.
key2bit :: Key -> OneBit
-- key2bit = bit . fromIntegral
key2bit k =
    case fromIntegral k of
    I# i -> W# (1## `uncheckedShiftL#` i)
{-# INLINE key2bit #-}
-- TODO: use INLINEABLE instead, in order to allow duplication?
-- TODO: use Data.Trie.ArrayMapped.UnsafeShift.unsafeShiftL instead?


-- TODO: we might want to inline the appropriate definition of popCount in order to avoid indirection... Not sure why it isn't resolved already; apparently the primop doesn't have architecture support for me?
-- N.B., there are also popCount primops for only looking at the lower 8, 16, or 32 bits of a Word#
-- > popCount (W# x#) = I# (word2Int# (popCnt# x#))
-- | /O(PC)/.
bit2index :: Bitmap -> OneBit -> Index
bit2index p b = popCount (p `maskLT` b)
{-# INLINE bit2index #-}


-- | /O(FI + PC)/.
key2index :: Bitmap -> Key -> Index
key2index p k = bit2index p (key2bit k)
{-# INLINE key2index #-}


{-
-- <http://stackoverflow.com/questions/757059>
-- <http://chessprogramming.wikispaces.com/BitScan>
-- <http://graphics.stanford.edu/~seander/bithacks.html>
-- TODO: can we use the x86 instruction bsf in lieu of POSIX's ffs(3)?
-- TODO: can we use GCC's __builtin_ffs in lieu of POSIX's ffs(3)?
-- TODO: add a new GHC primop for this...?
-}
-- | /O(BK)/.
bit2key :: OneBit -> Key
bit2key b = fromIntegral (c_ffs (fromIntegral b))
{-# INLINE bit2key #-}
-- | Return the position of the lowest set bit
foreign import ccall unsafe "strings.h ffs" c_ffs :: CInt -> CInt


-- | /O(1)/. Check if a bit is set in the bitmap.
elem :: OneBit -> Bitmap -> Bool
elem b p = (p .&. b /= 0)
{-# INLINE elem #-}


-- BUG: it looks like neWord# isn't actually branch-free. So this hack doesn't actually help anything...
-- TODO: benchmark performance differences anyways
-- TODO: see whether they generate the same code under GHC 7.10
elem# :: OneBit -> Bitmap -> Index
elem# (W# b) (W# p) = I# (neWord# (and# p b) 0##)
{-# INLINE elem# #-}


-- | /O(1)/. Check if a bit is unset in the bitmap.
notElem :: OneBit -> Bitmap -> Bool
notElem b p = (p .&. b == 0)
{-# INLINE notElem #-}


----------------------------------------------------------------
----------------------------------------------------------------
-- Section: Wrappers for working with MutableArray#

-- We must box up the MutableArray# into something of kind * before
-- we can return it. Rather than making an explicit box just to
-- unwrap it, we use CPS to achieve the same effect with more
-- efficient Core code. We need to inline it in order to really do
-- the right thing with the CPS form
new :: Int -> a -> (MutableArray# s a -> ST s r) -> ST s r
new _n@(I# n) x continue =
    CHECK_GT("new", _n, (0 :: Int))
    ST $ \s ->
        case newArray# n x s of
        (# s', xs #) ->
            case continue xs of
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
-- We say "some" to constrast against cloneMutableArray#
-- | Unsafely copy some elements of an array. Array bounds are not
-- checked.
copyMA :: MutableArray# s e -> Index -> MutableArray# s e -> Index -> Index -> ST s ()
#if __GLASGOW_HASKELL__ >= 702
{-# INLINE copyMA #-}
copyMA !src !_sidx@(I# sidx) !dst !_didx@(I# didx) _n@(I# n) =
    CHECK_GE("copyMA: sidx", _sidx, (0 :: Int))
    CHECK_GE("copyMA: didx", _didx, (0 :: Int))
    CHECK_GE("copyMA: n", _n, (0 :: Int))
    CHECK_BOUNDS("copyMA: src", lengthMA src, _sidx + _n - 1)
    CHECK_BOUNDS("copyMA: dst", lengthMA dst, _didx + _n - 1)
    ST $ \s ->
        case copyMutableArray# src sidx dst didx n s of
        s' -> (# s', () #)
#else
copyMA !src !sidx !dst !didx n =
    CHECK_GE("copyMA: sidx", _sidx, (0 :: Int))
    CHECK_GE("copyMA: didx", _didx, (0 :: Int))
    CHECK_GE("copyMA: n", _n, (0 :: Int))
    CHECK_BOUNDS("copyMA: src", lengthMA src, sidx + n - 1)
    CHECK_BOUNDS("copyMA: dst", lengthMA dst, didx + n - 1)
    go sidx didx 0
    where
    go !i !j !c
        | c < n     = do
            write dst j =<< read src i
            go (i+1) (j+1) (c+1)
        | otherwise = return ()
#endif


-- TODO: would it be faster just to allocate a new array and use copyMutableArray# for each half?
-- | Shift the elements between @i@ and @n-1@ to @i+1@ and @n@.
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
----------------------------------------------------------------
-- Section: Wrappers for working with Array#


-- HACK: must use the prefix notation in the definition, otherwise
-- it isn't recognized for some reason...
(!) :: Array# a -> Index -> a
(!) xs (I# i) = case indexArray# xs i of (# x #) -> x
{-# INLINE (!) #-}


-- | A CPS'ed variant of '(!)' for hoisting the array lookup out
-- of a continuation. Mostly useful for avoiding thunks without
-- forcing the array values.
index :: Array# a -> Index -> (a -> r) -> r
index xs (I# i) continue = case indexArray# xs i of (# x #) -> continue x
{-# INLINE index #-}


-- We say "some" to constrast against cloneMutableArray#
-- | Unsafely copy some elements of an array. Array bounds are not
-- checked.
copy :: Array# e -> Index -> MutableArray# s e -> Index -> Index -> ST s ()
#if __GLASGOW_HASKELL__ >= 702
{-# INLINE copy #-}
copy !src !_sidx@(I# sidx) !dst !_didx@(I# didx) _n@(I# n) =
    CHECK_GE("copy: sidx", _sidx, (0 :: Int))
    CHECK_GE("copy: didx", _didx, (0 :: Int))
    CHECK_GE("copy: n", _n, (0 :: Int))
    CHECK_BOUNDS("copy: src", length   src, _sidx + _n - 1)
    CHECK_BOUNDS("copy: dst", lengthMA dst, _didx + _n - 1)
    ST $ \s ->
        case copyArray# src sidx dst didx n s of
        s' -> (# s', () #)
#else
copy !src !sidx !dst !didx n =
    CHECK_GE("copy: sidx", _sidx, (0 :: Int))
    CHECK_GE("copy: didx", _didx, (0 :: Int))
    CHECK_GE("copy: n", _n, (0 :: Int))
    CHECK_BOUNDS("copy: src", length   src, sidx + n - 1)
    CHECK_BOUNDS("copy: dst", lengthMA dst, didx + n - 1)
    go sidx didx 0
    where
    go !i !j !c
        | c < n     = do
            index src i (write dst j)
            go (i+1) (j+1) (c+1)
        | otherwise = return ()
#endif


----------------------------------------------------------------
----------------------------------------------------------------
-- Section: Defining SparseArray

-- | A sparse array, mapping 'Key's to values.
data SparseArray a = SA !Bitmap !(Array# a)

#if __GLASGOW_HASKELL__ >= 710
type role SparseArray representational
#endif


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


----------------------------------------------------------------

-- TODO: is there a better implementation of length-0 arrays?
-- | /O(1)/. The empty array.
empty :: SparseArray a
empty = runST (new_ 0 $ unsafeFreeze 0)
{-
empty =
    runST . ST $ \s0 ->
        case newArray# 0# __undefinedElem s0 of
        (# s1, xs1 #) ->
            case unsafeFreezeArray# xs1 s1 of
            (# s2, xs2 #) -> (# s2, SA 0 xs2 #)
-}

-- | /O(1)/. An array mapping a single key to a value.
singleton :: Key -> a -> SparseArray a
singleton !k x = runST (new 1 x $ unsafeFreeze (key2bit k))
{-# INLINE singleton #-}


-- | /O(1)/. An array mapping two keys to values.
doubleton :: Key -> a -> Key -> a -> SparseArray a
doubleton !k x !l y = runST $
    new 2 x $ \xs -> do
    write xs (if k < l then 1 else 0) y
    unsafeFreeze (key2bit k .|. key2bit l) xs
{-# INLINE doubleton #-}


-- TODO: would it be better to have an invariant that SAs are non-empty, and use Maybe when we 'trim', 'filter', etc?
-- | /O(1)/. Is the array empty?
null :: SparseArray a -> Bool
null = __null
{-# INLINE null #-}

-- HACK: to avoid cyclic definition in the Foldable instance
__null :: SparseArray a -> Bool
__null (SA 0 _) = True
__null _        = False
{-# INLINE __null #-}
-- TODO: use INLINEABLE instead, in order to allow duplication?


-- | /O(PC)/. Get the number of elements in the array.
length :: SparseArray a -> Int
length = __length
{-# INLINE length #-}

-- HACK: to avoid cyclic definition in the Foldable instance
__length :: SparseArray a -> Int
__length (SA p _) = popCount p
{-# INLINE __length #-}
-- TODO: use INLINEABLE instead, in order to allow duplication?


-- | /O(1)/. Are the first array's keys a subset of second's?
isSubarrayOf :: SparseArray a -> SparseArray b -> Bool
isSubarrayOf (SA p _) (SA q _) = (p .&. q == p)
{-# INLINE isSubarrayOf #-}


-- | /O(1)/. Is the key associated with some value?
member :: Key -> SparseArray a -> Bool
member k (SA p _) = key2bit k `elem` p
{-# INLINE member #-}


-- TODO: would we *ever* want to postpone computing @bit2index p b@ too??
-- TODO: do we ever actually want/need this function?
--
-- | /O(FI + PC)/.
-- Lazily look up an element in an array. That is, the actual memory
-- access associated with the array lookup is postponed until after
-- returning the data constructor for 'Maybe'. This could be
-- beneficial for delaying or reducing memory traffic; but it's not
-- entirely clear whether we actually /need/ that anywhere. Other
-- than the laziness of the array lookup itself, we perform as much
-- work as possible before returning the 'Maybe' data constructor.
lookup_ :: Key -> SparseArray a -> Maybe a
lookup_ k (SA p xs) =
    let b = key2bit k in
    if  b `elem` p
    then let i = bit2index p b in i `seq` Just (xs ! i)
    else Nothing
{-# INLINE lookup_ #-}


-- TODO: is there any way we can convince GHC to treat this as an
-- intro rule, thus allowing case-of-constructor to avoid constructing
-- the 'Maybe'? If not, we should create an explicit CPSed version.
--
-- | /O(FI + PC)/.
-- Eagerly look up an element in an array. That is, we do all
-- possible work before returning the data constructor for 'Maybe'.
-- Unlike 'lookup_', we avoid constructing any thunks here.
lookup' :: Key -> SparseArray a -> Maybe a
lookup' k (SA p xs) =
    let b = key2bit k in
    if  b `elem` p
    then index xs (bit2index p b) Just
    else Nothing
{-# INLINE lookup' #-}


-- | /O(n)/. Create a copy of the array, inserting (or overwriting)
-- a new value.
insert :: Key -> a -> SparseArray a -> SparseArray a
insert !k x sa@(SA p xs)
    | b `elem` p =
        let maxN = length sa in
        runST $
            new_ maxN $ \xs' -> do
            copy xs 0 xs' 0 maxN
            write xs' i x
            unsafeFreeze p xs'
    | otherwise =
        let maxN = length sa + 1 in
        runST $
            new_ maxN $ \xs' -> do
            copy xs 0 xs' 0 i
            write xs' i x
            copy xs i xs' (i+1) (maxN-i)
            unsafeFreeze (p .|. b) xs'
    where
    b = key2bit k
    i = bit2index p b


-- | /O(n)/. Create a copy of the array, removing a key. If the key
-- is not present, then returns the same array.
remove :: Key -> SparseArray a -> SparseArray a
remove !k sa@(SA p xs)
    | b `elem` p =
        let maxN = length sa - 1 in
        runST $
            new_ maxN $ \xs' -> do
            copy xs 0 xs' 0 i
            copy xs (i+1) xs' i (maxN-i)
            unsafeFreeze (p `xor` b) xs'
    | otherwise = sa
    where
    b = key2bit k
    i = bit2index p b

----------------------------------------------------------------
data SubsingletonView a
    = IsEmpty
    | IsSingleton !Key a
    | IsNotSubsingleton
    deriving (Eq, Show)

#if __GLASGOW_HASKELL__ >= 710
type role SubsingletonView representational
#endif

-- | /O(PC+BK)/.
viewSubsingleton :: SparseArray a -> SubsingletonView a
viewSubsingleton xz@(SA p xs) =
    case length xz of
    0 -> IsEmpty
    1 -> index xs 0 (IsSingleton (bit2key p))
    _ -> IsNotSubsingleton
{-# INLINE viewSubsingleton #-}


----------------------------------------------------------------
----------------------------------------------------------------
-- Section: Defining DynamicSA
-- TODO: This isn't *really* used much; so inline/remove it?

-- | A mutable variant of 'SparseArray', for internal use.
data DynamicSA s a = DSA
    !Bitmap              -- The bitmap in progress
    !Key                 -- The maximum key seen so far == the highest set bit
    !Index               -- The next free index in the array
    !Index               -- The first invalid index == the size of the array
    !(MutableArray# s a) -- The array in progress

#if __GLASGOW_HASKELL__ >= 710
type role DynamicSA nominal representational
#endif

-- Just pointer equality on mutable arrays:
instance Eq (DynamicSA s e) where
    DSA _ _ _ _ xs == DSA _ _ _ _ ys =
        isTrue# (sameMutableArray# xs ys)


unsafeFreezeDSA :: DynamicSA s a -> ST s (SparseArray a)
unsafeFreezeDSA (DSA p _maxK n maxN xs) =
    unsafeFreezeOrTrim (n == maxN) p xs
{-# INLINE unsafeFreezeDSA #-}


insertDSA :: Key -> a -> DynamicSA s a -> ST s (DynamicSA s a)
insertDSA k v dsa = insertDSA_ k v dsa return


-- We CPS it in order to avoid boxing up the DSA on returning; to
-- really do that we must INLINE as well.
insertDSA_ :: Key -> a -> DynamicSA s a -> (DynamicSA s a -> ST s r) -> ST s r
{-# INLINE insertDSA_ #-}
insertDSA_ !k x dsa@(DSA p maxK n maxN xs) continue
    | b `elem` p = do
        write xs i x
        continue dsa
    | otherwise =
        case (n < maxN, k > maxK) of
        (True, True) -> do
            write xs n x
            continue $! DSA p' k n' maxN xs
        (True, False) -> do
            -- TODO: if shiftUpOne cannot be implemented efficiently, then maybe we should just reallocate and copy everything?
            shiftUpOne xs i (n-i)
            write xs i x
            continue $! DSA p' maxK n' maxN xs
        (False, True) ->
            new_ maxN' $ \xs' -> do
            copyMA xs 0 xs' 0 maxN
            write xs' n x
            continue $! DSA p' k n' maxN' xs'
        (False, False) ->
            new_ maxN' $ \xs' -> do
            copyMA xs 0 xs' 0 i
            write xs' i x
            copyMA xs i xs' (i+1) (maxN-i)
            continue $! DSA p' maxK n' maxN' xs'
    where
    b     = key2bit k
    i     = bit2index p b
    p'    = p .|. b
    n'    = n + 1
    maxN' = 2*maxN
    -- TODO: maxN' gets let-bound for sharing; we might want to add a pragma hinting that it can be duplicated relatively cheaply (or maybe the gcc/llvm backends can figure that out?)


-- Since we know the effective size limit for our use cases is
-- 16(=2^Key), we just start there so that we only ever need to
-- allocate once and then trim; thus avoiding the need to resize
-- along the way.
--
-- TODO: that being the case, we should get rid of DSA and just use
-- SA and insertSA... Doing so will save us the (n < maxN) check
-- every iteration, albeit at the cost of unsafety if we overflow...
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
-- The inline pragma is to avoid warnings about the rules possibly
-- not firing; but having it means we don't get the worker/wrapper
-- transform to unpack DSA in the loop...
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


assocs :: SparseArray a -> [(Key,a)]
assocs = assocsBy (,)
{-# INLINE assocs #-}


assocsBy :: (Key -> a -> b) -> SparseArray a -> [b]
assocsBy f xz = build (\cons nil -> assocsByFB ((cons .) . f) nil xz)
{-# INLINE assocsBy #-}

-- BUG: can we improve the asymptotics without needing bit2key for incrementing k?
-- TODO: (?) given b0, k += popCount (not 1 `maskLE` getNextBit b0 `maskGT` b0)
assocsByFB :: (Key -> a -> c -> c) -> c -> SparseArray a -> c
assocsByFB cons_f nil = \(SA p xs) -> go xs 0 p 1 0
    where
    go !xs !i !p !b !k
        | p == 0     = nil
        | b `elem` p = cons_f k (xs ! i) $
            go xs (i+1) (p `xor` b) (bsucc b) (k+1)
        | otherwise  = go xs i p (bsucc b) (k+1)
{-# INLINE [0] assocsByFB #-}

{-
-- this is silly...
{-# RULES
-- These rules are more robust, but only apply before inlining assocsBy
"assocsBy const"
        assocsBy (\k _ -> k) = keys
"assocsBy (flip const)"
        assocsBy (\_ x -> x) = elems
-- These rules are very fragile, which is why we (should) wait to inline assocsBy
"assocsByFB const {eta}"
    forall cons.
        assocsByFB (\k _ ks -> cons k ks) = keysFB cons
"assocsByFB const"
    forall cons.
        assocsByFB (\k _ -> cons k) = keysFB cons
"assocsByFB (flip const) {eta2}"
    forall cons.
        assocsByFB (\_ v vs -> cons v vs) = foldr cons
"assocsByFB (flip const) {eta1}"
    forall cons.
        assocsByFB (\_ v -> cons v) = foldr cons
"assocsByFB (flip const)"
    forall cons.
        assocsByFB (\_ -> cons) = foldr cons
    #-}
-}


keys :: SparseArray a -> [Key]
keys xz = build (\cons nil -> keysFB cons nil xz)
{-# INLINE keys #-}

-- BUG: can we improve the asymptotics without needing bit2key to increment k?
keysFB :: (Key -> c -> c) -> c -> SparseArray a -> c
keysFB cons nil = \(SA p _) -> go p 1 1
    where
    go !p !b !k
        | p == 0     = nil
        | b `elem` p = cons k $ go (p `xor` b) (bsucc b) (k+1)
        | otherwise  =          go p           (bsucc b) (k+1)
{-# INLINE [0] keysFB #-}


elems :: SparseArray a -> [a]
elems xz = build (\cons nil -> F.foldr cons nil xz)
{-# INLINE elems #-}


----------------------------------------------------------------
----------------------------------------------------------------
-- Section: instances, maps, folds, set-theoretic ops, etc


instance Show a => Show (SparseArray a) where
    showsPrec p sa =
        showParen (p > 10)
            $ showString "fromList "
            . showsPrec 11 (assocs sa)

    -- show = show . assocs


----------------------------------------------------------------
-- TODO: cf @GHC.Arr.eqArray@
instance Eq a => Eq (SparseArray a) where
    SA p xs == SA p' xs' =
        (p == p') && eqArray# xs xs' (popCount p) 0
        where
        eqArray# !xs !xs' !n !i
            | i < n     =
                if (xs ! i) == (xs' ! i)
                then eqArray# xs xs' n (i+1)
                else False
            | otherwise = True


----------------------------------------------------------------
-- Inefficient, but implemented a la @GHC.Arr.cmpArray@
instance Ord a => Ord (SparseArray a) where
    compare sa sa'= compare (assocs sa) (assocs sa')


----------------------------------------------------------------
-- TODO: is there a more efficient way to get/put an Array# ?
instance (Binary a) => Binary (SparseArray a) where
    put (SA p xs) = do
        put p
        putArray# xs (popCount p) 0
      where
        putArray# !xs !n !i
            | i < n     = do index xs i put; putArray# xs n (i+1)
            | otherwise = return ()
    
    get = do
        p <- get
        fromElems p <$> replicateM (popCount p) get


fromElems :: Bitmap -> [a] -> SparseArray a
fromElems p xs =
    let n = popCount p in
    runST (new_ n $ \dst -> go p dst n 0 xs)
    where
    go !p !dst !_n !_i [] =
        CHECK_EQ("fromElems", _i, _n)
        unsafeFreeze p dst
    go  p  dst  n  i (x:xs) =
        CHECK_LT("fromElems", i, n)
        do write dst i x; go p dst n (i+1) xs


----------------------------------------------------------------
instance (NFData a) => NFData (SparseArray a) where
    rnf = \(SA p xs) -> go xs (popCount p) 0
        where
        go !xs !n !i
            | i < n     = rnf (xs ! i) `seq` go xs n (i+1)
            | otherwise = ()


----------------------------------------------------------------
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
    -- If @f@ ignores it's argument, then this performs extraneous memory lookups compared to using @f(xs!i)@ instead...
    go !p !xs !n !ys !i
        | i < n     = do write ys i (index xs i f); go p xs n ys (i+1)
        | otherwise = unsafeFreeze p ys


-- TODO: is there a class for map' yet?
map' :: (a -> b) -> SparseArray a -> SparseArray b
map' f =
    \(SA p xs) ->
        let !n = popCount p in
        runST $
            new_ n $ \ys ->
            go p xs n ys 0
    where
    -- TODO: benchmark vs hoisting the 'index' up over ST's (>>)
    go !p !xs !n !ys !i
        | i < n     = do index xs i (\x -> write ys i $! f x)
                         go p xs n ys (i+1)
        | otherwise = unsafeFreeze p ys


-- The inline pragma is to avoid warnings about the rules possibly
-- not firing
{-# NOINLINE [1] map  #-}
{-# NOINLINE [1] map' #-}
{-# RULES
"map . map"    forall f g xs.  map  f (map  g xs) = map  (\x -> f (g x)) xs
"map' . map"   forall f g xs.  map' f (map  g xs) = map' (\x -> f (g x)) xs
"map . map'"   forall f g xs.  map  f (map' g xs) = map  (\x -> f $! g x) xs
"map' . map'"  forall f g xs.  map' f (map' g xs) = map' (\x -> f $! g x) xs
    #-}


-- TODO: how expensive is using bit2key? can we avoid/amortize that cost?
-- TODO: benchmark alongside map and map'
mapWithKey :: (Key -> a -> b) -> SparseArray a -> SparseArray b
mapWithKey f = 
    \(SA p xs) ->
        let !n = popCount p in
        runST $
            new_ n $ \ys ->
            go p xs n ys 0 (getFirstBit p)
    where
    go !p !xs !n !ys !i !b
        | i < n     = do
            write ys i (index xs i (f (bit2key b)))
            go p xs n ys (i+1) (getNextBit p b)
        | otherwise = unsafeFreeze p ys


mapWithKey' :: (Key -> a -> b) -> SparseArray a -> SparseArray b
mapWithKey' f = 
    \(SA p xs) ->
        let !n = popCount p in
        runST $
            new_ n $ \ys ->
            go p xs n ys 0 (getFirstBit p)
    where
    go !p !xs !n !ys !i !b
        | i < n     = do
            index xs i (\x -> write ys i $! f (bit2key b) x)
            go p xs n ys (i+1) (getNextBit p b)
        | otherwise = unsafeFreeze p ys


----------------------------------------------------------------
-- N.B., trying to force the closure to be generated before passing
-- in the extra arguments via lambda does not work. GHC floats the
-- closure down... presumably because the arities of these methods
-- are already specified...
--
-- N.B., adding INLINE pragma prevents the worker/wrapper transform
--
-- The instance for @array:Data.Array.Array@ is in @base:Data.Foldable@
-- TODO: compare our instance to that one
instance Foldable SparseArray where
    fold      = F.foldr' mappend mempty
    
    foldMap f = F.foldr' (mappend . f) mempty
    
    foldr f z = \ (SA p xs) -> go xs (popCount p) 0
        where
        go !xs !n !i
            | i < n     = f (xs ! i) (go xs n (i+1))
            | otherwise = z
    
#if (MIN_VERSION_base(4,6,0))
    foldr' f = \ !z0 (SA p xs) -> go xs (popCount p - 1) z0
        where
        go !xs !n z
            | n >= 0    = go xs (n-1) $! f (xs ! n) z
            | otherwise = z
#endif
    
    foldl f z = \ (SA p xs) -> go xs (popCount p - 1)
        where
        go !xs !n
            | n >= 0    = f (go xs (n-1)) (xs ! n)
            | otherwise = z
    
#if (MIN_VERSION_base(4,6,0))
    foldl' f = \ !z0 (SA p xs) -> go xs (popCount p) 0 z0
        where
        go !xs !n !i z
            | i < n     = go xs n (i+1) $! f z (xs ! i)
            | otherwise = z
#endif

    {-
    -- as far back as MIN_VERSION_base(4,0,0)) at least
    foldl1 = foldl1Elems
    foldr1 = foldr1Elems
    
        -- | A left fold over the elements with no starting value
        {-# INLINABLE foldl1Elems #-}
        foldl1Elems :: Ix i => (a -> a -> a) -> Array i a -> a
        foldl1Elems f = \ arr@(Array _ _ n _) ->
          let
            go i | i == 0    = unsafeAt arr 0
                 | otherwise = f (go (i-1)) (unsafeAt arr i)
          in
            if n == 0 then error "foldl1: empty Array" else go (n-1)
        
        -- | A right fold over the elements with no starting value
        {-# INLINABLE foldr1Elems #-}
        foldr1Elems :: Ix i => (a -> a -> a) -> Array i a -> a
        foldr1Elems f = \ arr@(Array _ _ n _) ->
          let
            go i | i == n-1  = unsafeAt arr i
                 | otherwise = f (unsafeAt arr i) (go (i + 1))
          in
            if n == 0 then error "foldr1: empty Array" else go 0
    -}

-- aka GHC >= 7.10
#if (MIN_VERSION_base(4,8,0))
    toList = elems    -- this is identical to the default implementation
    length = __length -- HACK: to avoid cyclic definition
    null   = __null   -- HACK: to avoid cyclic definition
    
    {- TODO
    elem :: Eq a => a -> t a -> Bool
    -- N.B., the default implementations for these use the (#.) hack to avoid efficiency issues with eta; cf., <http://hackage.haskell.org/package/base-4.8.0.0/docs/src/Data-Foldable.html>
    maximum :: Ord a => t a -> a
    minimum :: Ord a => t a -> a
    sum :: Num a => t a -> a
    product :: Num a => t a -> a
    -}
#endif
    


-- BUG: can we improve the asymptotics without needing bit2key for incrementing k?
-- TEST: foldrWithKey (\k v xs -> (k,v):xs) [] == assocs
foldrWithKey :: (Key -> a -> b -> b) -> b -> SparseArray a -> b
foldrWithKey f z = \ (SA p xs) -> go xs 0 p 1 0
    where
    go !xs !i !p !b !k
        | p == 0     = z
        | b `elem` p = f k (xs ! i) $
            go xs (i+1) (p `xor` b) (bsucc b) (k+1)
        | otherwise  = go xs i p (bsucc b) (k+1)

foldrWithKey' :: (Key -> a -> b -> b) -> b -> SparseArray a -> b
foldrWithKey' f z = \ (SA p xs) -> go xs 0 p 1 0
    where
    go !xs !i !p !b !k
        | p == 0     = z
        | b `elem` p = f k (xs ! i) $!
            go xs (i+1) (p `xor` b) (bsucc b) (k+1)
        | otherwise  = go xs i p (bsucc b) (k+1)

foldlWithKey :: (b -> Key -> a -> b) -> b -> SparseArray a -> b
foldlWithKey f = \ !z0 (SA p xs) -> go xs 0 p 1 0 z0
    where
    go !xs !i !p !b !k z
        | p == 0     = z
        | b `elem` p = go xs (i+1) (p `xor` b) (bsucc b) (k+1) $
            f z k (xs ! i)
        | otherwise  = go xs i p (bsucc b) (k+1) z

foldlWithKey' :: (b -> Key -> a -> b) -> b -> SparseArray a -> b
foldlWithKey' f = \ !z0 (SA p xs) -> go xs 0 p 1 0 z0
    where
    go !xs !i !p !b !k !z
        | p == 0     = z
        | b `elem` p = go xs (i+1) (p `xor` b) (bsucc b) (k+1) $!
            f z k (xs ! i)
        | otherwise  = go xs i p (bsucc b) (k+1) z


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


----------------------------------------------------------------
-- This implementation a la the one for @array:Data.Array.Array@ is in @base:Data.Traversable@
instance Traversable SparseArray where
    traverse f sa@(SA p _) = fromElems p <$> traverse f (elems sa)
    
    -- TODO: can we optimize 'sequenceA' over the default?


-- we can optimize Traversable for ST since we know ST computations
-- "only contain a single value", and thus we don't need to reallocate
-- the underlying array for each nondeterminism. We can also
-- eliminate/fuse the runST call
sequenceST :: SparseArray (ST s a) -> ST s (SparseArray a)
sequenceST = traverseST id


traverseST :: (a -> ST s b) -> SparseArray a -> ST s (SparseArray b)
traverseST f = \ (SA p xs) ->
    let !n = popCount p in
        new_ n $ \ys ->
        go p xs n ys 0
    where
    -- TODO: use 'index' in lieu of (!)?
    go !p !xs !n !ys !i
        | i < n     = do write ys i =<< f (xs ! i); go p xs n ys (i+1)
        | otherwise = unsafeFreeze p ys

-- TODO: traverseST fusion rules


----------------------------------------------------------------

-- TODO: float @go@ out instead of closing over stuff?
-- AKA: mapMaybe
-- | Apply a function to all values, potentially removing them.
filterMap :: (a -> Maybe b) -> SparseArray a -> SparseArray b
filterMap f (SA p xs) =
    let !n = popCount p in
    runST $
        new_ n $ \ys ->
        let go !i !bi !j !q
                | i >= n    =
                    unsafeFreezeOrTrim (i == j {- aka: p == q -}) q ys
                | otherwise =
                    case f (xs ! i) of
                    Just y  -> do
                        write ys j y
                        go (i+1) (getNextBit p bi) (j+1) (q .|. bi)
                    Nothing ->
                        go (i+1) (getNextBit p bi) j q
            --
        in go 0 (getFirstBit p) 0 0

-- The inline pragma is to avoid warnings about the rules possibly
-- not firing
{-# NOINLINE [1] filterMap #-}
{-# RULES
-- These ones are probably useless...
"filterMap Just"
        filterMap Just = id
"filterMap (Just . f)"
    forall f.
        filterMap (\x -> Just (f x)) = map f
"filterMap (const Nothing)"
        filterMap (\_ -> Nothing) = const empty

-- These ones are probably more worthwhile...
"filterMap . filterMap"
    forall f g xs.
        filterMap f (filterMap g xs) = filterMap (\x -> f =<< g x) xs
"filterMap . map"
    forall f g xs.
        filterMap f (map g xs) = filterMap (\x -> f (g x)) xs
"filterMap . map'"
    forall f g xs.
        filterMap f (map' g xs) = filterMap (\x -> f $! g x) xs
"map . filterMap"
    forall f g xs.
        map f (filterMap g xs) = filterMap (\x -> f <$> g x) xs
"map' . filterMap"
    forall f g xs.
        map' f (filterMap g xs) =
            filterMap (\x -> case g x of {Just y -> Just $! f y; _ -> Nothing}) xs
    #-}

-- | Apply a function to all values, potentially removing them.
filterMapWithKey :: (Key -> a -> Maybe b) -> SparseArray a -> SparseArray b
filterMapWithKey f (SA p xs) =
    let !n = popCount p in
    runST $
        new_ n $ \ys ->
        let go !i !bi !j !q
                | i >= n    =
                    unsafeFreezeOrTrim (i == j {- aka: p == q -}) q ys
                | otherwise =
                    case f (bit2key bi) (xs ! i) of
                    Just y  -> do
                        write ys j y
                        go (i+1) (getNextBit p bi) (j+1) (q .|. bi)
                    Nothing ->
                        go (i+1) (getNextBit p bi) j q
            --
        in go 0 (getFirstBit p) 0 0



-- TODO: float @go@ out instead of closing over stuff?
filter :: (a -> Bool) -> SparseArray a -> SparseArray a
filter f xz@(SA p xs) =
    let !n = popCount p in
    runST $
        new_ n $ \ys ->
        let go !i !bi !j !q
                | i >= n    =
                    if i == j {- aka: p == q -}
                    then return xz
                    else trim q ys
                | f x       = do
                    write ys j x
                    go (i+1) (getNextBit p bi) (j+1) (q .|. bi)
                | otherwise =
                    go (i+1) (getNextBit p bi) j q
                where x = xs ! i
            --
        in go 0 (getFirstBit p) 0 0

-- The inline pragma is to avoid warnings about the rules possibly
-- not firing
{-# NOINLINE [1] filter #-}
{-# RULES
-- These ones are probably useless...
"filter (const True)"     filter (\_ -> True)  = id
"filter (const False)"    filter (\_ -> False) = const empty

-- These ones are probably more worthwhile...
"filter . filter"
    forall f g xs.
        filter f (filter g xs) = filter (\x -> g x && f x) xs
"filter . map"
    forall f g xs.
        filter f (map g xs) =
            filterMap (\x -> let y = g x in if f y then Just y else Nothing) xs
"filter . map'"
    forall f g xs.
        filter f (map' g xs) =
            filterMap (\x -> let y = g x in if f $! y then Just y else Nothing) xs
"map . filter"
    forall f g xs.
        map f (filter g xs) =
            filterMap (\x -> if g x then Just (f x) else Nothing) xs
"map' . filter"
    forall f g xs.
        map' f (filter g xs) =
            filterMap (\x -> if g x then Just $! f x else Nothing) xs
"filterMap . filter"
    forall f g xs.
        filterMap f (filter g xs) =
            filterMap (\x -> if g x then f x else Nothing) xs
"filter . filterMap"
    forall f g xs.
        filter f (filterMap g xs) =
            filterMap (\x -> case g x of { my@(Just y) | f y -> my; _ -> Nothing }) xs
    #-}
    
filterWithKey :: (Key -> a -> Bool) -> SparseArray a -> SparseArray a
filterWithKey f xz@(SA p xs) =
    let !n = popCount p in
    runST $
        new_ n $ \ys ->
        let go !i !bi !j !q
                | i >= n    =
                    if i == j {- aka: p == q -}
                    then return xz
                    else trim q ys
                | f (bit2key bi) x = do
                    write ys j x
                    go (i+1) (getNextBit p bi) (j+1) (q .|. bi)
                | otherwise =
                    go (i+1) (getNextBit p bi) j q
                where
                x = xs ! i
            --
        in go 0 (getFirstBit p) 0 0


-- TODO: float @go@ out instead of closing over stuff?
partition :: (a -> Bool) -> SparseArray a -> (SparseArray a, SparseArray a)
partition f xz@(SA p xs) =
    let !n = popCount p in
    runST $
        new_ n $ \ys ->
        new_ n $ \zs ->
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
                    go (i+1) (getNextBit p bi) (j+1) (q .|. bi) k r
                | otherwise = do
                    write zs k x
                    go (i+1) (getNextBit p bi) j q (k+1) (r .|. bi)
                where x = xs ! i
            --
        in go 0 (getFirstBit p) 0 0 0 0

-- The inline pragma is to avoid warnings about the rules possibly
-- not firing
{-# NOINLINE [1] partition #-}
{-# RULES
-- These ones are probably useless...
"partition (const True)"     partition (\_ -> True)  = \xz -> (xz, empty)
"partition (const False)"    partition (\_ -> False) = \xz -> (empty, xz)

-- TODO: the more worthwhile interaction rules
    #-}

partitionWithKey
    :: (Key -> a -> Bool) -> SparseArray a -> (SparseArray a, SparseArray a)
partitionWithKey f xz@(SA p xs) =
    let !n = popCount p in
    runST $
        new_ n $ \ys ->
        new_ n $ \zs ->
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
                | f (bit2key bi) x = do
                    write ys j x
                    go (i+1) (getNextBit p bi) (j+1) (q .|. bi) k r
                | otherwise = do
                    write zs k x
                    go (i+1) (getNextBit p bi) j q (k+1) (r .|. bi)
                where x = xs ! i
            --
        in go 0 (getFirstBit p) 0 0 0 0


-- AKA: mapEither
-- TODO: float @go@ out instead of closing over stuff?
partitionMap :: (a -> Either b c) -> SparseArray a -> (SparseArray b, SparseArray c)
partitionMap f (SA p xs) =
    let !n = popCount p in
    runST $
        new_ n $ \ys ->
        new_ n $ \zs ->
        let go !i !bi !j !q !k !r
                | i >= n = do
                    yz <- trim q ys
                    zz <- trim r zs
                    return (yz, zz)
                | otherwise =
                    case f (xs ! i) of
                    Left  y -> do
                        write ys j y
                        go (i+1) (getNextBit p bi) (j+1) (q .|. bi) k r
                    Right z -> do
                        write zs k z
                        go (i+1) (getNextBit p bi) j q (k+1) (r .|. bi)
            --
        in go 0 (getFirstBit p) 0 0 0 0

-- The inline pragma is to avoid warnings about the rules possibly
-- not firing
{-# NOINLINE [1] partitionMap #-}
{-# RULES
-- These ones are probably useless...
"partitionMap Left"     partitionMap Left  = \xz -> (xz, empty)
"partitionMap Right"    partitionMap Right = \xz -> (empty, xz)
"partitionMap (Left . f)"
    forall f.
        partitionMap (\x -> Left (f x)) = \xz -> (map f xz, empty)
"partitionMap (Right . f)"
    forall f.
        partitionMap (\x -> Right (f x)) = \xz -> (empty, map f xz)

-- TODO: the more worthwhile interaction rules
    #-}

partitionMapWithKey
    :: (Key -> a -> Either b c)
    -> SparseArray a -> (SparseArray b, SparseArray c)
partitionMapWithKey f (SA p xs) =
    let !n = popCount p in
    runST $
        new_ n $ \ys ->
        new_ n $ \zs ->
        let go !i !bi !j !q !k !r
                | i >= n = do
                    yz <- trim q ys
                    zz <- trim r zs
                    return (yz, zz)
                | otherwise =
                    case f (bit2key bi) (xs ! i) of
                    Left  y -> do
                        write ys j y
                        go (i+1) (getNextBit p bi) (j+1) (q .|. bi) k r
                    Right z -> do
                        write zs k z
                        go (i+1) (getNextBit p bi) j q (k+1) (r .|. bi)
            --
        in go 0 (getFirstBit p) 0 0 0 0


----------------------------------------------------------------
-- All these functions assume (xz `isSubarrayOf` yz) and operate
-- accordingly. If that's not the case, then they work as if operating
-- on the subarray of xz such that it is the case


rzipWith_, rzipWith'_
    :: (a -> b -> c) -> (b -> c)
    -> SparseArray a -> SparseArray b -> SparseArray c
rzipWith_  f g = __rzipWith ($)  f g
rzipWith'_ f g = __rzipWith ($!) f g
-- HACK: to avoid repeating ourselves
{-# INLINE __rzipWith #-}
__rzipWith
    :: (forall a b. (a -> b) -> a -> b)
    -> (a -> b -> c) -> (b -> c)
    -> SparseArray a -> SparseArray b -> SparseArray c
-- TODO: float @go@ out instead of closing over stuff?
__rzipWith ($?) f g =
    \ (SA p xs) (SA q ys) ->
        let !n = popCount q in
        runST $
            new_ n $ \zs ->
            let go !i !b !j
                    | j >= n     = unsafeFreeze q zs
                    | b `elem` p = do
                        write zs j $? f (xs ! i) (ys ! j)
                        go (i+1) (getNextBit q b) (j+1)
                    | otherwise  = do
                        write zs j $? g (ys ! j)
                        go i     (getNextBit q b) (j+1)
                --
            in go 0 (getFirstBit q) 0


rzipWith'
    :: (Maybe a -> b -> c)
    -> SparseArray a -> SparseArray b -> SparseArray c
rzipWith' f = rzipWith'_ (f . Just) (f Nothing)
{-# INLINE rzipWith' #-}


rzipWith
    :: (Maybe a -> b -> c)
    -> SparseArray a -> SparseArray b -> SparseArray c
rzipWith f = rzipWith_ (f . Just) (f Nothing)
{-# INLINE rzipWith #-}


rzip :: SparseArray a -> SparseArray b -> SparseArray (Maybe a, b)
rzip = rzipWith (,)


rzipWithKey_, rzipWithKey'_
    :: (Key -> a -> b -> c) -> (Key -> b -> c)
    -> SparseArray a -> SparseArray b -> SparseArray c
rzipWithKey_  f g = __rzipWithKey ($)  f g
rzipWithKey'_ f g = __rzipWithKey ($!) f g
-- HACK: to avoid repeating ourselves
{-# INLINE __rzipWithKey #-}
__rzipWithKey
    :: (forall a b. (a -> b) -> a -> b)
    -> (Key -> a -> b -> c) -> (Key -> b -> c)
    -> SparseArray a -> SparseArray b -> SparseArray c
-- TODO: float @go@ out instead of closing over stuff?
-- TODO: how expensive is it to use bit2key; can we avoid/amortize that?
__rzipWithKey ($?) f g =
    \ (SA p xs) (SA q ys) ->
        let !n = popCount q in
        runST $
            new_ n $ \zs ->
            let go !i !b !j
                    | j >= n     = unsafeFreeze q zs
                    | b `elem` p = do
                        write zs j $? f (bit2key b) (xs ! i) (ys ! j)
                        go (i+1) (getNextBit q b) (j+1)
                    | otherwise  = do
                        write zs j $? g (bit2key b) (ys ! j)
                        go i     (getNextBit q b) (j+1)
                --
            in go 0 (getFirstBit q) 0


rzipWithKey'
    :: (Key -> Maybe a -> b -> c)
    -> SparseArray a -> SparseArray b -> SparseArray c
rzipWithKey' f = rzipWithKey'_ (\k -> f k . Just) (\k -> f k Nothing)
{-# INLINE rzipWithKey' #-}


rzipWithKey
    :: (Key -> Maybe a -> b -> c)
    -> SparseArray a -> SparseArray b -> SparseArray c
rzipWithKey f = rzipWithKey_ (\k -> f k . Just) (\k -> f k Nothing)
{-# INLINE rzipWithKey #-}


-- TODO: float @go@ out instead of closing over stuff?
rzipFilter_
    :: (a -> b -> Maybe c) -> (b -> Maybe c)
    -> SparseArray a -> SparseArray b -> SparseArray c
rzipFilter_ f g (SA p xs) (SA q ys) =
    let !n = popCount q in
    runST $
        new_ n $ \zs ->
        let go !i !b !j !k !r
                | j >= n     =
                    unsafeFreezeOrTrim (j == k {- aka: q == r -}) r zs
                | b `elem` p =
                    case f (xs ! i) (ys ! j) of
                    Just z -> do
                        write zs k z
                        go (i+1) (getNextBit q b) (j+1) (k+1) (r .|. b)
                    Nothing ->
                        go (i+1) (getNextBit q b) (j+1) k r
                | otherwise  =
                    case g (ys ! j) of
                    Just z -> do
                        write zs k z
                        go i (getNextBit q b) (j+1) (k+1) (r .|. b)
                    Nothing ->
                        go i (getNextBit q b) (j+1) k r
            --
        in go 0 (getFirstBit q) 0 0 0


rzipFilter
    :: (Maybe a -> b -> Maybe c)
    -> SparseArray a -> SparseArray b -> SparseArray c
rzipFilter f = rzipFilter_ (f . Just) (f Nothing)
{-# INLINE rzipFilter #-}


----------------------------------------------------------------
----------------------------------------------------------------

-- | Left-biased union.
unionL :: SparseArray a -> SparseArray a -> SparseArray a
unionL (SA p xs) (SA q ys) =
    let !r = p .|. q
        !n = popCount r
    in runST $
        new_ n $ \zs ->
        let go !b !i !j !k
                | k >= n     = unsafeFreeze r zs
                | b `elem` p =
                    index xs i $ \x -> do
                    write zs k x
                    go (getNextBit r b) (i+1) (j + elem# b q) (k+1)
                | otherwise =
                    index ys j $ \y -> do
                    write zs k y
                    go (getNextBit r b) i (j+1) (k+1)
            --
        in go (getFirstBit r) 0 0 0


-- | Right-biased union.
unionR :: SparseArray a -> SparseArray a -> SparseArray a
unionR = flip unionL


unionWith
    :: (Or a b -> c)
    -> SparseArray a -> SparseArray b -> SparseArray c
unionWith f = unionWith_ (f . Fst) ((f .) . Both) (f . Snd)
{-# INLINE unionWith #-}


unionWith'
    :: (Or a b -> c)
    -> SparseArray a -> SparseArray b -> SparseArray c
unionWith' f = unionWith'_ (f . Fst) ((f .) . Both) (f . Snd)
{-# INLINE unionWith' #-}


unionWith_, unionWith'_
    :: (a -> c) -> (a -> b -> c) -> (b -> c)
    -> SparseArray a -> SparseArray b -> SparseArray c
unionWith_  f g h = __unionWith ($)  f g h
unionWith'_ f g h = __unionWith ($!) f g h
-- HACK: to avoid repeating ourselves
-- TODO: inline this so we can play around with using 'index' in lieu of (!) as we did for map/map'?
{-# INLINE __unionWith #-}
__unionWith
    :: (forall a b. (a -> b) -> a -> b)
    -> (a -> c) -> (a -> b -> c) -> (b -> c)
    -> SparseArray a -> SparseArray b -> SparseArray c
-- TODO: float @go@ out instead of closing over stuff?
__unionWith ($?) f g h (SA p xs) (SA q ys) =
    let !r = p .|. q
        !n = popCount r
    in runST $
        new_ n $ \zs ->
        let go !b !i !j !k
                | k >= n    = unsafeFreeze r zs
                | otherwise =
                    case (b `elem` p, b `elem` q) of
                    (True, False) -> do
                        write zs k $? f (xs ! i)
                        go (getNextBit r b) (i+1) j (k+1)
                    (True, True) -> do
                        write zs k $? g (xs ! i) (ys ! j)
                        go (getNextBit r b) (i+1) (j+1) (k+1)
                    (False, True) -> do
                        write zs k $? h (ys ! j)
                        go (getNextBit r b) i (j+1) (k+1)
                    (False, False) -> __impossible "__unionWith"
            --
        in go (getFirstBit r) 0 0 0


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
        new_ n $ \zs ->
        let go !r0 !b !i !j !k !r
                | r0 == 0   = unsafeFreezeOrTrim (k >= n) r zs
                | otherwise =
                    case (b `elem` p, b `elem` q) of
                    (True, False) ->
                        case f (xs ! i) of
                        Just z -> do
                            write zs k z
                            go (r0 `xor` b) (getNextBit r0 b) (i+1) j (k+1) (r .|. b)
                        Nothing ->
                            go (r0 `xor` b) (getNextBit r0 b) (i+1) j k r
                    (True, True) ->
                        case g (xs ! i) (ys ! j) of
                        Just z -> do
                            write zs k z
                            go (r0 `xor` b) (getNextBit r0 b) (i+1) (j+1) (k+1) (r .|. b)
                        Nothing ->
                            go (r0 `xor` b) (getNextBit r0 b) (i+1) (j+1) k r
                    (False, True) ->
                        case h (ys ! j) of
                        Just z -> do
                            write zs k z
                            go (r0 `xor` b) (getNextBit r0 b) i (j+1) (k+1) (r .|. b)
                        Nothing ->
                            go (r0 `xor` b) (getNextBit r0 b) i (j+1) k r
                    (False, False) -> __impossible "unionFilterWith_"
            --
        in go r0 (getFirstBit r0) 0 0 0 0


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


-- | This is the most powerful binary merging function for
-- 'SparseArray', and consequently the most expensive. If you can
-- get away with using one of the simpler functions, you should.
unionFilterWithKey_
    :: (Key -> a -> Maybe c)
    -> (Key -> a -> b -> Maybe c)
    -> (Key -> b -> Maybe c)
    -> SparseArray a -> SparseArray b -> SparseArray c
-}

----------------------------------------------------------------
-- | Left-biased intersection.
intersectionL :: SparseArray a -> SparseArray b -> SparseArray a
intersectionL =
    \(SA p xs) (SA q _) ->
        let !r = p .&. q
            !n = popCount r
        in runST $
            new_ n $ \zs ->
            go n p xs r zs (getFirstBit r) 0
    where
    go !n !p !xs !r !zs !b !k
        | k >= n    = unsafeFreeze r zs
        | otherwise =
            -- We use 'index' instead of '(!)' to avoid thunks in the new array
            index xs (bit2index p b) $ \x -> do
            write zs k x
            go n p xs r zs (getNextBit r b) (k+1)


-- | Right-biased intersection.
intersectionR :: SparseArray a -> SparseArray b -> SparseArray b
intersectionR = flip intersectionL


intersectionWith, intersectionWith'
    :: (a -> b -> c)
    -> SparseArray a -> SparseArray b -> SparseArray c
intersectionWith  f = __intersectionWith ($)  f
intersectionWith' f = __intersectionWith ($!) f
-- HACK: to avoid repeating ourselves
-- TODO: inline this so we can play around with using 'index' in lieu of (!) as we did for map/map'?
{-# INLINE __intersectionWith #-}
__intersectionWith
    :: (forall a b. (a -> b) -> a -> b)
    -> (a -> b -> c)
    -> SparseArray a -> SparseArray b -> SparseArray c
__intersectionWith ($?) f =
    \(SA p xs) (SA q ys) ->
        let !r = p .&. q
            !n = popCount r
        in runST $
            new_ n $ \zs ->
            go n p xs q ys r zs (getFirstBit r) 0
    where
    go !n !p !xs !q !ys !r !zs !b !k
        | k >= n    = unsafeFreeze r zs
        | otherwise = do
            write zs k $? f (xs ! bit2index p b) (ys ! bit2index q b)
            go n p xs q ys r zs (getNextBit r b) (k+1)

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
            go p xs r zs n 0 (getFirstBit r)
    where
    go !p !xs !r !zs !n !k !b
        | k >= n    = unsafeFreeze r zs
        | otherwise =
            -- We use 'index' to avoid thunks in the new array.
            index xs (bit2index p b) $ \x -> do
                write zs k x
                go p xs r zs n (k+1) (getNextBit r b)


differenceR
    :: SparseArray a -> SparseArray b -> SparseArray b
differenceR = flip differenceL

----------------------------------------------------------------
----------------------------------------------------------- fin.
