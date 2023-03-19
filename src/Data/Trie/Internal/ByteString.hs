{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE CPP, BangPatterns #-}
#if __GLASGOW_HASKELL__ >= 701
-- Neither 'unsafeDupablePerformIO' nor 'Data.ByteString.Internal' is safe.
{-# LANGUAGE Trustworthy #-}
#endif
------------------------------------------------------------
--                                              ~ 2022.02.27
-- |
-- Module      :  Data.Trie.Internal.ByteString
-- Copyright   :  2008--2023 wren romano
-- License     :  BSD-3-Clause
-- Maintainer  :  wren@cpan.org
-- Stability   :  stable
-- Portability :  GHC-only
--
-- Helper functions on 'ByteString's for "Data.Trie.Internal".
------------------------------------------------------------

module Data.Trie.Internal.ByteString
    ( ByteString, ByteStringElem
    , breakMaximalPrefix
    , RevLazyByteString(..), (+>!), (+>?), fromStrict, toStrict
        -- TODO: we want to export the 'Nil' constructor; but
        -- do we want to export the patterns?
    ) where

import qualified Data.ByteString          as S
import qualified Data.ByteString.Internal as S
import Data.ByteString.Internal (ByteString(PS))
import Data.Word
import Foreign.ForeignPtr       (ForeignPtr)
#if MIN_VERSION_base(4,15,0)
-- [aka GHC 9.0.1]:
import GHC.ForeignPtr           (unsafeWithForeignPtr)
#else
import Foreign.ForeignPtr       (withForeignPtr)
#endif
import Foreign.Ptr              (Ptr, plusPtr)
import Foreign.Storable         (Storable(..))
-- This module name is since @__GLASGOW_HASKELL__ >= 611@.
import GHC.IO                   (unsafeDupablePerformIO)

------------------------------------------------------------
#if !(MIN_VERSION_base(4,15,0))
-- bytestring-0.10.12.1 and 0.11.1.0 use and export this definition;
-- however neither 0.10.12.0 nor 0.11.0.0 define nor use it.So,
-- rather than dealing with all that nonsense, we'll just do it
-- ourselves.
unsafeWithForeignPtr :: ForeignPtr a -> (Ptr a -> IO b) -> IO b
unsafeWithForeignPtr = withForeignPtr
#endif


------------------------------------------------------------
------------------------------------------------------------
-- | Associated type of 'ByteString'
type ByteStringElem = Word8


------------------------------------------------------------
-- The @since annotation is for when this got re-exported from
-- "Data.Trie.Internal".
--
-- | Returns the longest shared prefix and the two remaining suffixes
-- for a pair of strings.  This function performs no allocation\/copying,
-- it simply returns slices\/views of the arguments.
--
-- * @s ≡ (\\(pre,s',z') -> pre '<>' s') ('breakMaximalPrefix' s z)@
-- * @z ≡ (\\(pre,s',z') -> pre '<>' z') ('breakMaximalPrefix' s z)@
--
-- @since 0.2.2
breakMaximalPrefix
    :: ByteString
    -> ByteString
    -> (ByteString, ByteString, ByteString)
--
-- [Implementation Notes]
--
-- * We've had to define 'strictTriple' and use BangPatterns to
--   keep GHC from wrapping all the returned triples in
--   ghc-prim:'GHC.Magic.lazy'.  Not sure how much this actually
--   helps performance, but it's a stepping stone towards defining
--   a custom result type which unpacks the three ByteStrings.  And
--   given that GHC's worker-wrapper transform generates a worker
--   that returns an unboxed tuple and yet internally does construct
--   the tuple, this suggests that using a custom return type should
--   help performance.
--
-- * TODO: the result of the inlined 'indexOfDifference' is still
--   being wrapped in ghc-prim:'GHC.Magic.lazy'; but nothing I can
--   do seems to change that.  Is it something about the
--   'unsafeDupablePerformIO' or what?  Would changing it even help
--   performance?
--
-- * The first two cases can safely be allowed to fall through to
--   the @i <= 0@ case. After inlining, there shouldn't be any
--   function-call overhead for letting 'goByte' do the comparison
--   instead.  The only difference is that the @i <= 0@ case will
--   hold onto @s0@/@s1@ rather than replacing them by 'S.empty'.
--   Unfortunately, that difference in liveness seems to result in
--   slightly worse performance.
--   TODO: a better benchmark than just running the test suite.
--
-- * The 'unsafeWithForeignPtr' allows for more aggressive optimization
--   than 'withForeignPtr', since it encodes the knowledge that the
--   continuation cannot diverge (loop, or throw exceptions).  In
--   particular, without this, the call to 'min' will get hoisted
--   above the inner 'withForeignPtr' and the call to 'indexOfDifference'
--   will be duplicated in both branches of the 'min'; and since
--   'indexOfDifference' will get inlined (recursive 'goBytes' and
--   all), that's a lot of code duplication.  However, for whatever
--   reason the 'unsafeWithForeignPtr' version actually seems to
--   result in slightly worse performance (0.2~2% on the test suite).
--   TODO: a better benchmark than just running the test suite.
--   TODO: if that hoisting actually does help, then perhaps manually
--     lift the 'max' above both 'withForeignPtr' and manually
--     express the branch duplication.
--   TODO: Also consider whether this might be relevant:
--     <https://gitlab.haskell.org/ghc/ghc/-/issues/16556>
--
-- * TODO: should we yield to the accursed call of
--     'Data.ByteString.Internal.accursedUnutterablePerformIO'?
--     Recent versions of bytestring export it, so we wouldn't
--     even need to copy the accursed incantation itself.  Regarding
--     correctness, probably the closest thing to compare against
--     are these bugs against 'S.elemIndices':
--     <https://gitlab.haskell.org/ghc/ghc/-/issues/3487>
--     <https://gitlab.haskell.org/ghc/ghc/-/issues/3486>
--
-- * TODO: re-investigate performance of lifting the non-IO stuff
--     out of the scope of the 'unsafeDupablePerformIO', vs leaving
--     it within that scope.
--
breakMaximalPrefix
    s0@(PS fp0 off0 len0)
    s1@(PS fp1 off1 len1)
    | len0 <= 0 = strictTriple S.empty S.empty s1
    | len1 <= 0 = strictTriple S.empty s0      S.empty
    | otherwise =
        let i = unsafeDupablePerformIO $
                unsafeWithForeignPtr fp0 $ \p0 ->
                unsafeWithForeignPtr fp1 $ \p1 ->
                indexOfDifference
                    (p0 `ptrElemOff` off0)
                    (p1 `ptrElemOff` off1)
                    (len0 `min` len1)
        in  if i <= 0 -- can only be equal, but for paranoia's sake.
            then strictTriple S.empty s0 s1
            else strictTriple
                    (if off0 + len0 < off1 + len1  -- share the smaller one
                        then PS fp0 off0 i  -- TODO: assert(i<=len0) for paranoia?
                        else PS fp1 off1 i) -- TODO: assert(i<=len1) for paranoia?
                    (dropPS i fp0 off0 len0)
                    (dropPS i fp1 off1 len1)

-- | Construct a triple, strict in all arguments.  This helps improve
-- code generation over our previous approach.  Making our own
-- datatype for this result or CPSing 'breakMaximalPrefix' may still
-- improve things further.
strictTriple :: ByteString -> ByteString -> ByteString
             -> (ByteString,  ByteString,   ByteString)
strictTriple !p !s !z = (p,s,z)
{-# INLINE strictTriple #-}

-- | Get the 'sizeOf' type @a@, without requiring @-XScopedTypeVariables@
-- nor making a spurious call to 'System.IO.Unsafe.unsafePerformIO' or similar.
sizeOfElem :: Storable a => Ptr a -> Int
sizeOfElem = sizeOf . (undefined :: Ptr a -> a)
{-# INLINE sizeOfElem #-}

-- | C-style pointer addition, without the excessively liberal type
-- of 'plusPtr'.
ptrElemOff :: Storable a => Ptr a -> Int -> Ptr a
ptrElemOff p i = p `plusPtr` (i * sizeOfElem p)
{-# INLINE [0] ptrElemOff #-}
-- This rewrite rule helps ensure that on bytestring>=0.11 we don't
-- incur any additional cost for using the 'PS' pattern synonym.
{-# RULES
"Data.Trie.ByteStringInternal ptrElemOff/0"
    forall p . ptrElemOff p 0 = p
 #-}

-- For bytestring>=0.11, there's no way to improve over the 'PS'
-- constructor synonym here.  After inlining, the @off=0@ from the
-- 'PS' pattern synonym will constant-propogate away, so all we'll
-- be left with is @BS (plusForeignPtr fp n) (len - n)@; which is
-- the same thing we would've written by hand.  Plus, bytestring>=0.11
-- will already define the compatibility definition of 'plusForeignPtr'
-- for use with base<4.10.
--
-- | Unpacked version of 'S.drop', for use as a smart-constructor.
-- N.B., this assumes the @n <= 0@ case has already been handled
-- (otherwise you might as well just say @drop n (PS fp off len)@
-- and let the compiler remove the intermediate 'PS').
dropPS :: Int -> ForeignPtr ByteStringElem -> Int -> Int -> ByteString
dropPS !n !fp !off !len
    | n >= len  = S.empty
    | otherwise = PS fp (off + n) (len - n)
{-# INLINE dropPS #-}


------------------------------------------------------------
-- This naive algorithm doesn't depend on architecture details.  We
-- could speed things up (in theory) by checking a natural word at
-- a time and then falling back to checking each byte once the
-- mismatched word is found.  But in practice that doesn't seem to
-- actually speed things up.
--
-- TODO: that's probably because of alignment issues, or because
-- we should really vectorize by the largest single load on an
-- architecture rather than by the natural word size.  For more
-- details on how to do it right, see GNU glibc's implementation
-- of @memcmp@.  We should be able to do a simple twist on that
-- algorithm to return the index of difference rather than the
-- ordering.  That would mean requiring GPL, but unfortunately every
-- other implementations of @memcmp@ I've found (FreeBSD libc, GCC's
-- builtin,...) just uses the same naive algorithm I have below.
-- I suppose we could always fork that algorithm off into a separate
-- optional dependency of this library; where we fallback to this
-- implementation if the user doesn't want the GPL burden.
--
-- | Calculates the first index where values differ.
indexOfDifference
    :: Ptr ByteStringElem
    -> Ptr ByteStringElem
    -> Int
    -> IO Int
indexOfDifference !p1 !p2 !limit = goByte 0
    where
    goByte n
        | n >= limit = return limit
        | otherwise  = do
            c1 <- peekElemOff p1 n
            c2 <- peekElemOff p2 n
            if c1 == c2
                then goByte (n+1)
                else return n

-- TODO: why does bytestring-0.11 use 'peekByteOff' in lieu of
-- 'peekElemOff'?  Given the definitions, the latter is more
-- direct/simpler: using @readWord8OffAddr# p# n# s@ instead of
-- @readWord8OffAddr# (plusAddr# p# n# ) 0# s@, though surely GHC
-- will optimize those to generate the same assembly.

------------------------------------------------------------
------------------------------------------------------------

-- | A \"reversed\" variant of lazy bytestrings; i.e., a snoc-list
-- of strict bytestrings.
data RevLazyByteString
    = RevLazyByteString :+> {-# UNPACK #-} !S.ByteString
    -- Invariant: every 'S.ByteString' is non-null.
    | Nil

-- TODO: should we add an 'assert' even though we don't check in general?
-- | \(\mathcal{O}(1)\). Unsafely\/uncheckedly append a BS to the
-- RLBS.  It is up to the caller to maintain the invariant that
-- 'S.ByteString' is indeed non-null.
(+>!) :: RevLazyByteString -> S.ByteString -> RevLazyByteString
xs +>! x = xs :+> x
{-# INLINE (+>!) #-}

-- | \(\mathcal{O}(1)\). Safely append a BS to the RLBS, maintaining
-- the invariant.
(+>?) :: RevLazyByteString -> S.ByteString -> RevLazyByteString
xs +>? PS _ _ 0 = xs
xs +>? x        = xs :+> x
{-# INLINE (+>?) #-}

-- | \(\mathcal{O}(1)\). Safely convert a strict BS to RLBS,
-- maintaining the invariant.
fromStrict :: S.ByteString -> RevLazyByteString
fromStrict = (Nil +>?)
{-# INLINE fromStrict #-}

-- HACK: bytestring-0.10.8.1 (GHC 8.0.2) used 'S.checkedSum' (and
-- a simpler algorithm), whereas bytestring-0.10.8.2 (GHC 8.2.1)
-- introduced 'S.checkedAdd' instead; alas, those version numbers
-- cannot be differentiated by the MIN_VERSION_bytestring macro.
-- Thus, we'll simply define it ourselves.
-- TODO: since we built the trie from bytestrings that were short
-- enough to have a valid length, do we actually need to perform
-- this check at all?
--
-- | Add two non-negative numbers. Errors out on overflow.
(+?) :: Int -> Int -> Int
x +? y
  | r >= 0    = r
  | otherwise = error overflowError
  where r = x + y
{-# INLINE (+?) #-}

overflowError :: String
overflowError = "Data.Trie.ByteStringInternal.toStrict: size overflow"
{-# NOINLINE overflowError #-}

-- See commentary at LazyByteString's version of @toStrict@.  This
-- implementation is from Git SHA 688f3c0887f2ca0623f2f54f78e8f675f92e31bf,
-- modulo the necessary changes for using a snoc-list in lieu of a
-- cons-list.
-- | \(\mathcal{O}(n)\). Convert the RLBS to a strict BS, by copying it.
toStrict :: RevLazyByteString -> S.ByteString
toStrict = \cs0 -> goLen0 cs0 cs0
    where
    -- It's still possible that the result is empty.
    goLen0 _               Nil                = S.empty
    goLen0 cs0             (cs :+> PS _ _ 0)  = goLen0 cs0 cs
    goLen0 cs0             (cs :+> c)         = goLen1 cs0 c cs
    -- It's still possible that the result is a single chunk.
    goLen1 _   b           Nil                = b
    goLen1 cs0 b           (cs :+> PS _ _ 0)  = goLen1 cs0 b cs
    goLen1 cs0 (PS _ _ bl) (cs :+> PS _ _ cl) = goLen  cs0 (bl +? cl) cs
    -- General case, just find the total length we'll need.
    goLen  cs0 !total      (cs :+> PS _ _ cl) = goLen  cs0 (total +? cl) cs
    goLen  cs0  total      Nil                =
        S.unsafeCreate total $ \ptr ->
            -- TODO: this gives the correct behavior (re off-by-one
            -- concerns); however, it is bad praxis to use a pointer
            -- to something outside the allocated region; even if
            -- it is just pointing to the first invalid byte after
            -- the allocated region.
            goCopy cs0 (ptr `ptrElemOff` total)
    -- Copy the data
    goCopy Nil                    !_   = return ()
    goCopy (cs :+> PS _  _   0  ) !ptr = goCopy cs ptr
    goCopy (cs :+> PS fp off len) !ptr =
        unsafeWithForeignPtr fp $ \p -> do
            let ptr' = ptr `ptrElemOff` negate len
            S.memcpy ptr' (p `ptrElemOff` off) len
            goCopy cs ptr'

------------------------------------------------------------
------------------------------------------------------- fin.
