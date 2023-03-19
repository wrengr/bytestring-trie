{-# OPTIONS_GHC -Wall -fwarn-tabs #-}

----------------------------------------------------------------
--                                                  ~ 2021.12.14
-- |
-- Module      :  Test.Validity
-- Copyright   :  2021--2023 wren romano, 2004--2021 libraries@haskell.org
-- License     :  BSD-3-Clause
-- Maintainer  :  wren@cpan.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Verify the invariants of 'Trie', a~la
-- <https://github.com/haskell/containers/blob/master/containers-tests/tests/IntMapValidity.hs>
----------------------------------------------------------------
module Test.Validity
    ( BrokenInvariant(..), validate
    , valid
    ) where

-- BUG: requires exporting the constructors!
-- TODO: probably easiers to just move this into "Data.Trie.Internal"
-- itself; like we did for 'showTrie'
import           Data.Trie.Internal           (Trie(..))
import qualified Data.Trie.BitTwiddle   as BT (zero)
import qualified Data.ByteString        as S  (null)
import           Data.Bits                    ((.&.), popCount)
import qualified Test.QuickCheck        as QC (Property, counterexample, (.&&.))

----------------------------------------------------------------
----------------------------------------------------------------

data BrokenInvariant
    = EmptyAfterReject  -- ^ Forbidden: @Arc _ Nothing Empty@.
    | EmptyAfterBranch  -- ^ Forbidden: @Branch _ _ Empty _@ and @Branch _ _ _ Empty@.
    | ArcAfterReject    -- ^ Forbidden: @Arc _ Nothing (Arc _ _ _)@.
    | NonRootEpsilon    -- ^ Forbidden: @Arc S.empty (Just _) _@ except when at the root.
    | EpsilonReject     -- ^ Forbidden: @Arc S.empty Nothing _@.
    | MaskNotPowerOfTwo -- ^ 'Mask' must have exactly one bit set.
    | PrefixNotMatched  -- ^ All keys after a branch must start with the 'Prefix'.
    | MaskBitNotRespected
        -- ^ The 'Mask' bit is 'BT.zero' for all keys in the left
        -- subtrie, and not for any keys in the right subtrie.
    deriving (Eq, Show)

et :: Maybe e -> Maybe e -> Maybe e
et m@(Just _) _ = m
et Nothing    n = n
{-# INLINE et #-}

-- This combines all the invariant checks into a single pass.
-- However, the improved performance may not be worth the added
-- code complexity, since now it's no longer trivial to see that
-- this does indeed check the invariants.
-- TODO: we may want to try doing back to CPS/Codensity stuff to
-- avoid the case analysis of 'et' in the two places it's needed.
validate :: Trie a -> Maybe BrokenInvariant
validate = start
    where
    -- | Handle epsilon values at the root.
    start (Arc _ (Just _) t)    = checkNNAccept t -- Skip the non-null check at root.
    start t                     = checkNNAccept t -- Otherwise same as usual.
    -- | Validate properties of an 'Arc' with non-null bytestring.
    checkNNArc (Just _) t       = checkNNAccept t
    checkNNArc Nothing  t       = checkNNReject t
    -- | Validate properties of accept-'Arc' with non-null bytestring.
    -- These are also the properties of the root just after any epsilon
    -- value.
    checkNNAccept Empty         = Nothing
    checkNNAccept (Arc k mv t)
        | S.null k              = Just (epsilonError mv)
        | otherwise             = checkNNArc mv t
    checkNNAccept b@(Branch{})  = checkBranch b (const Nothing)
    -- | Validate properties of reject-'Arc' with non-null bytestring.
    checkNNReject Empty         = Just EmptyAfterReject
    checkNNReject (Arc{})       = Just ArcAfterReject
    checkNNReject b@(Branch{})  = checkBranch b (const Nothing)
    -- | Validate 'Branch' properties.
    -- The code complexity is to avoid O(n^2) traversal of branch
    -- collections, which would happen if we checked @all _ (keyHeads _)@
    -- in the more straightforward manner.
    -- TODO: see how big of a branch collection it takes before
    -- this complexity is actually worth it.
    checkBranch Empty            _  = Just EmptyAfterBranch
    checkBranch (Arc k mv t)     pred
        | S.null k                  = Just (epsilonError mv)
        | otherwise                 = pred (S.unsafeHead k) `et` checkNNArc mv t
    checkBranch (Branch p m l r) pred
        | popCount m /= 1           = Just MaskNotPowerOfTwo
        | otherwise                 =
            checkBranch l (\x ->
                if x .&. p /= p      then Just PrefixNotMatched    else
                if not (BT.zero x m) then Just MaskBitNotRespected else pred x)
            `et` checkBranch r (\x ->
                if x .&. p /= p     then Just PrefixNotMatched    else
                if BT.zero x m      then Just MaskBitNotRespected else pred x)

    epsilonError (Just _) = NonRootEpsilon
    epsilonError Nothing  = EpsilonReject

----------------------------------------------------------------
----------------------------------------------------------------
-- TODO: would be faster to combine all these invariant predicates
-- into a single recursion; just the issue of specifying the failure
-- mode, and keeping the logic clean\/obvious.
--
-- TODO: generalize the conjunction type so we can use smallcheck too.
--
-- | Returns true iff the internal structure of the 'Trie' is valid.
valid :: Trie a -> QC.Property
valid t = QC.counterexample "validShape"     (validShape     t)
  QC..&&. QC.counterexample "validEpsilon"   (validEpsilon   t)
  QC..&&. QC.counterexample "maskPowerOfTwo" (maskPowerOfTwo t)
  QC..&&. QC.counterexample "commonPrefix"   (commonPrefix   t)
  QC..&&. QC.counterexample "maskRespected"  (maskRespected  t)

-- | Invariants:
-- * 'Empty' may only occur at root and after an accept-'Arc'.
-- * 'Branch' must not have 'Empty' as child.
-- * Reject-'Arc' must have 'Branch' as child.
validShape :: Trie a -> Bool
validShape = go True -- allow empty at root.
    where
    go allowEmpty Empty     = allowEmpty
    go _ (Arc _ Nothing  t) = isBranch t && go False t -- forbid empty after reject
    go _ (Arc _ (Just _) t) =               go True  t -- allow  empty after accept
    go _ (Branch _ _ l r)   = go False l && go False r -- forbid empty after branch

    isBranch (Branch{}) = True
    isBranch _          = False

-- | Invariants:
-- * Reject-'Arc' must not have null bytestring.
-- * Accept-'Arc' may only have null bytestring at the root.
validEpsilon :: Trie a -> Bool
validEpsilon = start
    where
    start (Arc _ (Just _) t) = go t -- Skip the non-null check at root.
    start t                  = go t -- Otherwise same as usual.
    go    Empty              = True
    go    (Arc k _ t)        = not (S.null k) && go t
    go    (Branch _ _ l r)   = go l && go r

-- | Invariant: The 'Mask' is a power of 2.
-- TODO: validate that it is the largest bit position at which two
-- keys of the trie differ.
maskPowerOfTwo :: Trie a -> Bool
maskPowerOfTwo = go
    where
    go Empty            = True
    go (Arc _ _ t)      = go t
    go (Branch _ m l r) = popCount (fromIntegral m) == 1 && go l && go r

keyHeads :: Trie a -> [ByteStringElem]
keyHeads = \t -> go t id []
    where
    go Empty            xs = xs
    go (Arc k _ _)      xs = (S.head k :) . xs
    go (Branch _ _ l r) xs = go l . go r . xs

-- | Invariant: 'Prefix' is the common high-order bits that all
-- elements share to the left of the 'Mask' bit.
commonPrefix :: Trie a -> Bool
commonPrefix = go
    where
    go Empty              = True
    go (Arc _ _ t)        = go t
    go b@(Branch p _ l r) = all (sharedPrefix p) (keyHeads b) && go l && go r
        -- TODO: fix this O(n^2) traversal of the collection of branches.

    sharedPrefix :: Prefix -> ByteStringElem -> Bool
    sharedPrefix p a = p == p .&. a

-- | Invariant: In 'Branch', left consists of the elements that
-- don't have the 'Mask' bit set; right is all the elements that
-- do.
maskRespected :: Trie a -> Bool
maskRespected = go
    where
    go Empty            = True
    go (Arc _ _ t)      = go t
    go (Branch _ m l r) = and
        -- TODO: fix this O(n^2) traversal of the collection of branches.
        [ all (\x ->      BT.zero x m)  (keyHeads l)
        , all (\x -> not (BT.zero x m)) (keyHeads r)
        , go l
        , go r
        ]

----------------------------------------------------------------
----------------------------------------------------------- fin.
