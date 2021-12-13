{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
{-# LANGUAGE CPP
           , MultiParamTypeClasses
           , FlexibleInstances
           , FlexibleContexts
           #-}

----------------------------------------------------------------
--                                                  ~ 2021.12.12
-- |
-- Module      :  test/Utils.hs
-- Copyright   :  2008--2021 wren gayle romano
-- License     :  BSD3
-- Maintainer  :  wren@cpan.org
-- Stability   :  provisional
-- Portability :  semi-portable (MPTC,...)
--
-- Utilities for testing 'Trie's.
----------------------------------------------------------------
module Utils
    ( packC2W, vocab2trie
    , localQuickCheckOptions
    , testEqual
    , W(..), everyW
    , WS(..), packWS, unpackWS
    , WTrie(..)
    , CheckGuard(..), (.==>.), (.==.)
    ) where

import qualified Data.Trie                as T
import           Data.Word                (Word8)
import qualified Data.ByteString          as S
import qualified Data.ByteString.Internal as S (c2w, w2c)
import           Data.ByteString.Internal (ByteString(PS))
import           Control.Monad            ((<=<))

-- N.B., "Test.Tasty.HUnit" does not in fact depend on "Test.HUnit";
-- hence using the longer alias.
import qualified Test.Tasty             as Tasty
import qualified Test.Tasty.HUnit       as TastyHU
import qualified Test.Tasty.QuickCheck  as TastyQC
import qualified Test.QuickCheck        as QC
import qualified Test.SmallCheck        as SC
import qualified Test.SmallCheck.Series as SC
-- import qualified Test.LazySmallCheck as LSC
-- import qualified Test.SparseCheck    as PC

----------------------------------------------------------------
----------------------------------------------------------------

-- TODO: apparently this is exported as 'Data.ByteString.Internal.packChars'
-- | Construct a bytestring from the first byte of each 'Char'.
packC2W :: String -> S.ByteString
packC2W  = S.pack . map S.c2w

-- | Construct a trie via 'packC2W' giving each key a unique value
-- (namely its position in the list).
vocab2trie :: [String] -> T.Trie Int
vocab2trie  = T.fromList . flip zip [0..] . map packC2W

----------------------------------------------------------------
-- QuickCheck >=2.1.0 && <2.5.0 used 'maxDiscard' instead, which
-- has a different semantics and which we set to @max 1000 (10*n)@.
-- But since the cabal file lists QuickCheck-2.10 as the minimum
-- version, must switch to the new 'maxDiscardRatio' instead.

-- | Convert most of 'QC.Args' into Tasty.  There are a few QuickCheck
-- args which are not handled:
--
--  * 'QC.maxShrinks' if tasty-quickcheck<0.10.2, because
--    'TastyQC.QuickCheckMaxShrinks' is not exported.
--    <https://github.com/UnkindPartition/tasty/issues/316>
--  * 'QC.chatty' because Tasty always ignores this setting.
--  * 'QC.replay' because of technical difficulty with inverting
--    @Test.QuickCheck.Random.mkQCGen :: Int -> QCGen@
--
-- Conversely, there are two TastyQC options which have no equivalent
-- in 'QC.Args': 'TastyQC.QuickCheckVerbose' and 'TastyQC.QuickCheckShowReplay'.
localQuickCheckOptions :: QC.Args -> Tasty.TestTree -> Tasty.TestTree
localQuickCheckOptions args
    = Tasty.localOption (TastyQC.QuickCheckTests      $ QC.maxSuccess      args)
    . Tasty.localOption (TastyQC.QuickCheckMaxSize    $ QC.maxSize         args)
    . Tasty.localOption (TastyQC.QuickCheckMaxRatio   $ QC.maxDiscardRatio args)
#if MIN_VERSION_tasty_quickcheck(0,10,2)
    . Tasty.localOption (TastyQC.QuickCheckMaxShrinks $ QC.maxShrinks      args)
#endif
{-
Tasty lacks some options that QC.Args has:
    * (QC.chatty Bool{default=True}), though 'TastyQC.optionSetToArgs'
      always sets this to False.
Tasty has some additional options that QC.Args lacks:
    * (TastyQC.QuickCheckVerbose Bool{default=False})
      chooses between QC.verboseCheckWithResult vs QC.quickCheckWithResult.
      Where,
        QC.verboseCheckWithResult a p = QC.quickCheckWithResult a (QC.verbose p)
    * (TastyQC.QuickCheckShowReplay Bool{default=False})
      says whether to print the replay seed even on successful tests
      (it's always printed on unsuccessful tests).
Tasty has some discrepancy with QC.Args:
    * (TastyQC.QuickCheckReplay (Maybe Int){default=Nothing})
      vs (QC.replay (Maybe (QCGen, Int)){default=Nothing})

      The Int of QC.replay is the value returned by QC.getSize,
      which can be ignored for the purposes of TastyQC.QuickCheckReplay
      since QC.verboseCheckWithResult doesn't use it for random
      seed stuff. (QC.verboseCheckWithResult does use it to define
      the case for QC.State.computeSize applied to (0,0) however.)

      However, there's no good way I can think to invert
        Test.QuickCheck.Random.mkQCGen :: Int -> QCGen
      Which is just a wrapper around
        System.Random.mkStdGen :: Int -> StdGen
      or
        System.Random.SplitMix.mkSMGen :: Word64 -> SMGen
      depending on, if impl(hugs): cpp-options: -DNO_SPLITMIX

      Partly because it depends on whether the @random@ library
      version is >=1.2 vs <1.2, since they use different internals
      which the QCGen type is expressly designed to paper over.
      But mainly because there is no inverse function already given.
      We could use the Read and Show instances to recover the components
      of the QCGen, however it's less clear how to put them back
      together into an Int.
-}


-- TODO: come up with a thing that pretty-prints the diff, instead
-- of just showing the expected\/actual.
testEqual :: (Show a, Eq a) => String -> a -> a -> Tasty.TestTree
testEqual name expected actual =
    TastyHU.testCase name (TastyHU.assertEqual "" expected actual)

----------------------------------------------------------------
-- | A small subset of 'Word8', so that 'WS' is more likely to have
-- shared prefixes.  The 'Show' instance shows it as a 'Char', for
-- better legibility and for consistency with the 'Show' instance
-- of 'WS'.
newtype W = W { unW :: Word8 }
    deriving (Eq, Ord)

instance Show W where
    showsPrec p = showsPrec p . S.w2c . unW

-- TODO: ensure that these have good bit-patterns for covering corner cases.
-- | All the possible 'W' values; or rather, all the ones generated
-- by the 'QC.Arbitrary' and 'SC.Serial' instances.
everyW :: [W]
everyW = (W . S.c2w) <$> ['a'..'m']

-- TODO: if we define (Enum W) then we could use 'QC.chooseEnum'
-- which is much faster than 'QC.elements'.  Alternatively we might
-- consider using 'QC.growingElements' if we want something more
-- like what the SC.Serial case does.
instance QC.Arbitrary W where
    arbitrary = QC.elements everyW
    shrink w  = takeWhile (w /=) everyW

instance QC.CoArbitrary W where
    coarbitrary = QC.coarbitrary . unW

-- We take @(d+1)@ to match the instances for 'Char', (SC.N a), etc
instance Monad m => SC.Serial m W where
    series = SC.generate (\d -> take (d+1) everyW)

instance Monad m => SC.CoSerial m W where
    coseries = fmap (. unW) . SC.coseries

----------------------------------------------------------------
-- TODO: we need a better instance of Arbitrary for lists to make
-- them longer than our smallcheck depth.
--
-- | A subset of 'S.ByteString' produced by 'packWS'.
-- This newtype is to ensure that generated bytestrings are more
-- likely to have shared prefixes (and thus non-trivial tries).
newtype WS = WS { unWS :: S.ByteString }
    deriving (Eq, Ord)

instance Show WS where
    showsPrec p = showsPrec p . unWS

packWS :: [W] -> WS
packWS = WS . S.pack . map unW

unpackWS :: WS -> [W]
unpackWS = map W . S.unpack . unWS

-- | Like 'S.inits' but each step keeps half more, rather than just one more.
prefixes :: WS -> [WS]
prefixes (WS (PS x s l)) =
    [WS (PS x s (l - k)) | k <- takeWhile (> 0) (iterate (`div` 2) l)]

instance QC.Arbitrary WS where
    arbitrary = QC.sized $ \n -> do
        k  <- QC.chooseInt (0,n)
        xs <- QC.vector k
        return $ packWS xs
    shrink = QC.shrinkMap packWS unpackWS <=< prefixes

instance QC.CoArbitrary WS where
    coarbitrary = QC.coarbitrary . unpackWS

instance Monad m => SC.Serial m WS where
    series = packWS <$> SC.series

-- TODO: While this is a perfectly valid instance, is it really the
-- most efficient one for our needs?
instance Monad m => SC.CoSerial m WS where
    coseries rs =
        SC.alts0 rs SC.>>- \z ->
        SC.alts2 rs SC.>>- \f ->
        return $ \(WS xs) ->
            if S.null xs
            then z
            else f (W $ S.head xs) (WS $ S.tail xs)

----------------------------------------------------------------
-- | A subset of 'T.Trie' where all the keys are 'WS'.  This newtype
-- is mainly just to avoid orphan instances.
newtype WTrie a = WT { unWT :: T.Trie a }
    deriving (Eq)

instance Show a => Show (WTrie a) where
    showsPrec p = showsPrec p . unWT

first :: (b -> c) -> (b,d) -> (c,d)
first f (x,y) = (f x, y)

-- TODO: maybe we ought to define @T.fromListBy@ for better fusion?
fromListWT :: [(WS,a)] -> WTrie a
fromListWT = WT . T.fromList . map (first unWS)

-- We can use 'T.toListBy' to manually fuse with the map
toListWT :: WTrie a -> [(WS,a)]
toListWT = map (first WS) . T.toList . unWT

instance (QC.Arbitrary a) => QC.Arbitrary (WTrie a) where
    arbitrary = QC.sized $ \n -> do
        k      <- QC.chooseInt (0,n)
        labels <- QC.vector k
        elems  <- QC.vector k
        return . fromListWT $ zip labels elems
    -- Extremely inefficient, but should be effective at least.
    shrink = QC.shrinkMap fromListWT toListWT

-- TODO: instance QC.CoArbitrary (WTrie a)

-- TODO: This instance really needs some work. The smart constructures
-- ensure only valid values are generated, but there are redundancies
-- and inefficiencies.
instance (Monad m, SC.Serial m a) => SC.Serial m (WTrie a) where
    series =   SC.cons0 (WT T.empty)
        SC.\/  SC.cons3 arcHACK
        SC.\/  SC.cons2 branch
        where
        arcHACK (WS k) mv (WT t) =
            case mv of
            Nothing -> WT (T.singleton k () >> t)
            Just v  -> WT (T.singleton k v >>= T.unionR t . T.singleton S.empty)

        branch (WT t0) (WT t1) = WT (t0 `T.unionR` t1)

-- TODO: instance Monad m => SC.CoSerial m (WTrie a)

----------------------------------------------------------------
----------------------------------------------------------------

infixr 0 ==>, .==>.
infix  4 .==.

{-
-- TODO: clean up something like this:
class ForAll src p q where
    forAll :: forall a. (Show a) => src a -> (a -> p) -> q
instance (QC.Testable p) => ForAll QC.Gen p QC.Property where
    forAll gen pf = QC.forAllShrink gen QC.shrink pf
instance (SC.Testable m p) => ForAll (SC.Series m) p (SC.Property m) where
    forAll srs pf = SC.forAll (SC.over srs pf)

class SuchThat src where
    suchThat :: forall a. src a -> (a -> Bool) -> src a
instance SuchThat QC.Gen where
    suchThat = QC.suchThat
instance SuchThat (SC.Series m) where
    suchThat = flip Control.Monad.mfilter

class Generable src a where
    generate :: src a
instance (QC.Arbitrary a) => Generable QC.Gen a where
    generate = QC.arbitrary
instance (SC.Serial m a) => Generable (SC.Series m) a where
    generate = SC.series

forEach :: (Forall src p q, SuchThat src, Generable src a, Show a) => (a -> Bool) -> (a -> p) -> q
forEach = forAll . suchThat generate
-}


-- | Deal with QC\/SC polymorphism issues because of @(==>)@.
-- Fundeps would be nice here, but @|b->a@ is undecidable, and @|a->b@ is wrong.
class CheckGuard p q where
    (==>) :: Bool -> p -> q

instance (QC.Testable p) => CheckGuard p QC.Property where
    (==>) = (QC.==>)
    -- TODO: might should also use 'QC.cover' with this.
    -- TODO: or we may prefer to rephrase things to use 'QC.suchThat' instead (should be sufficient for our particular use case, if we can find a smallcheck analogue (probably 'SC.over'))

instance (Monad m, SC.Testable m p) => CheckGuard p (SC.Property m) where
    (==>) = (SC.==>)

-- | Lifted implication.
(.==>.) :: CheckGuard testable prop => (a -> Bool) -> (a -> testable) -> (a -> prop)
(.==>.) p q x = p x ==> q x

-- | Function equality / lifted equality.
(.==.) :: (Eq b) => (a -> b) -> (a -> b) -> (a -> Bool)
(.==.) f g x = f x == g x
    -- TODO: should use (QC.===) or diy with QC.counterexample; assuming we can overload that for smallcheck equivalent (or for smallcheck to ignore and fall back to (==))

----------------------------------------------------------------
----------------------------------------------------------- fin.
