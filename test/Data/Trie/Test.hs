{-# OPTIONS_GHC -Wall -fwarn-tabs -fno-warn-orphans #-}
{-# LANGUAGE CPP
           , MultiParamTypeClasses
           , FlexibleInstances
           , FlexibleContexts
           , TypeSynonymInstances
           #-}
{-# LANGUAGE StandaloneDeriving, DeriveGeneric #-}

----------------------------------------------------------------
--                                                  ~ 2011.02.12
-- |
-- Module      :  Data.Trie.Test
-- Copyright   :  Copyright (c) 2008--2011 wren gayle romano
-- License     :  BSD3
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  provisional
-- Portability :  semi-portable (MPTC,...)
--
-- Testing 'Trie's.
----------------------------------------------------------------
module Data.Trie.Test (packC2W, test) where

import qualified Data.Trie                as Tr
-- import qualified Data.Trie.Internal       as TrI
import qualified Data.Trie.Convenience    as TC
import qualified Data.ByteString          as S
import qualified Data.ByteString.Internal as S (c2w) --, w2c)
-- import qualified Data.Text                as T
-- import qualified Data.Text.Internal.Fusion as TIF

import qualified Test.HUnit          as HU
import qualified Test.QuickCheck     as QC
-- import qualified Test.QuickCheck.Arbitrary as QCA
import qualified Test.SmallCheck     as SC
import qualified Test.SmallCheck.Series     as SCS
-- import qualified Test.LazySmallCheck as LSC
-- import qualified Test.SparseCheck    as PC

-- import Data.Monoid
-- import Control.Monad (liftM)
-- import Data.Function
import Data.List     (nubBy, sortBy)
import Data.Ord      (comparing)
-- import GHC.Generics
----------------------------------------------------------------
----------------------------------------------------------------

packC2W :: String -> S.ByteString
packC2W  = S.pack . map S.c2w

vocab2trie :: [String] -> Tr.Trie Int
vocab2trie  = Tr.fromList . flip zip [0..] . map packC2W

----------------------------------------------------------------
test :: IO ()
test  = do
    putStrLn ""
    putStrLn (replicate 80 '~')

    putStrLn "hunit:"
    _ <- HU.runTestTT $ HU.TestList
                 [ test_Union
                 , test_Submap
                 , test_Insert
                 , test_Delete
                 ]
    putStrLn ""

    putStrLn "quickcheck @ Int:"
    putStrLn "prop_insert"
    checkQuick 500  (prop_insert        :: Str -> Int -> Tr.Trie Int -> Bool)
    putStrLn "prop_singleton"
    checkQuick 5000 (prop_singleton     :: Str -> Int -> Bool)
    putStrLn "prop_size_insert"
    checkQuick 500  (prop_size_insert   :: Str -> Int -> Tr.Trie Int -> QC.Property)
    putStrLn "prop_size_delete"
    checkQuick 500  (prop_size_delete   :: Str -> Int -> Tr.Trie Int -> QC.Property)
    putStrLn "prop_insert_delete"
    checkQuick 500  (prop_insert_delete :: Str -> Int -> Tr.Trie Int -> QC.Property)
    putStrLn "prop_delete_lookup"
    checkQuick 500  (prop_delete_lookup :: Str -> Tr.Trie Int -> QC.Property)
    putStrLn "prop_submap1"
    checkQuick 500  (prop_submap1       :: Str -> Tr.Trie Int -> Bool)
    putStrLn "prop_submap2"
    checkQuick 500  (prop_submap2       :: Str -> Tr.Trie Int -> Bool)
    putStrLn "prop_submap3"
    checkQuick 500  (prop_submap3       :: Str -> Tr.Trie Int -> Bool)
    putStrLn "prop_toList"
    checkQuick 500  (prop_toList        :: Tr.Trie Int -> Bool)
    putStrLn "prop_fromList_takes_first"
    checkQuick 500  (prop_fromList_takes_first :: [(Str, Int)] -> QC.Property)
    putStrLn "prop_fromListR_takes_first"
    checkQuick 500  (prop_fromListR_takes_first :: [(Str, Int)] -> QC.Property)
    putStrLn "prop_fromListL_takes_first"
    checkQuick 500  (prop_fromListL_takes_first :: [(Str, Int)] -> QC.Property)
    putStrLn "prop_fromListS_takes_first"
    checkQuick 500  (prop_fromListS_takes_first :: [(Str, Int)] -> QC.Property)
    putStrLn "prop_fromListWithConst_takes_first"
    checkQuick 500  (prop_fromListWithConst_takes_first :: [(Str, Int)] -> QC.Property)
    putStrLn "prop_fromListWithLConst_takes_first"
    checkQuick 500  (prop_fromListWithLConst_takes_first :: [(Str, Int)] -> QC.Property)
    putStrLn ""

    putStrLn "smallcheck @ ():" -- Beware the exponential!
    putStrLn "prop_insert"
    checkSmall 3 (prop_insert        :: Str -> () -> Tr.Trie () -> Bool)
    putStrLn "prop_singleton"
    checkSmall 7 (prop_singleton     :: Str -> () -> Bool)
    putStrLn "prop_size_insert"
    checkSmall 3 (prop_size_insert   :: Str -> () -> Tr.Trie () -> SC.Property IO)
    putStrLn "prop_size_delete"
    checkSmall 3 (prop_size_delete   :: Str -> () -> Tr.Trie () -> SC.Property IO)
    putStrLn "prop_insert_delete"
    checkSmall 3 (prop_insert_delete :: Str -> () -> Tr.Trie () -> SC.Property IO)
    putStrLn "prop_delete_lookup"
    checkSmall 3 (prop_delete_lookup :: Str -> Tr.Trie () -> SC.Property IO)
    putStrLn "prop_submap1"
    checkSmall 3 (prop_submap1       :: Str -> Tr.Trie () -> Bool)
    putStrLn "prop_submap2"
    checkSmall 3 (prop_submap2       :: Str -> Tr.Trie () -> Bool)
    -- checkSmall 3 (prop_submap3 :: Str -> Tr.Trie () -> Bool)
    {- -- BUG: Needs both instances of Monoid and SC.Serial...
    putStrLn "smallcheck @ Letter:"
    checkSmall 4 (prop_toList        :: Tr.Trie Letter -> Bool)
    checkSmall 5 (prop_fromList_takes_first :: [(Str, Letter)] -> Bool)
    checkSmall 5 (prop_fromListR_takes_first :: [(Str, Letter)] -> Bool)
    checkSmall 5 (prop_fromListL_takes_first :: [(Str, Letter)] -> Bool)
    checkSmall 5 (prop_fromListS_takes_first :: [(Str, Letter)] -> Bool)
    checkSmall 5 (prop_fromListWithConst_takes_first :: [(Str, Letter)] -> Bool)
    checkSmall 5 (prop_fromListWithLConst_takes_first :: [(Str, Letter)] -> Bool)
    -}
    putStrLn ""

    where
#ifdef __USE_QUICKCHECK_1__
    checkQuick n =
        QC.check (QC.defaultConfig
            { QC.configMaxTest = n
            , QC.configMaxFail = 1000 `max` 10*n
            })
#else
    checkQuick n =
        QC.quickCheckWith (QC.stdArgs
            { QC.maxSize    = n
            , QC.maxSuccess = n
            , QC.maxDiscardRatio = 1000 `max` 10*n
            })
#endif
    checkSmall d f = SC.smallCheck d f >> putStrLn ""

-- deriving instance Generic Int
--
-- testText :: IO ()
-- testText  = do
--     putStrLn "quickcheck @ Int (Text):"
--     putStrLn "prop_insertText"
--     checkQuick 500  (prop_insertText        :: StrText -> Int -> Tr.TrieText Int -> Bool)
--     putStrLn "prop_singletonText"
--     checkQuick 5000 (prop_singletonText     :: StrText -> Int -> Bool)
--     putStrLn "prop_size_insertText"
--     checkQuick 500  (prop_size_insertText   :: StrText -> Int -> Tr.TrieText Int -> QC.Property)
--     putStrLn "prop_size_deleteText"
--     checkQuick 500  (prop_size_deleteText   :: StrText -> Int -> Tr.TrieText Int -> QC.Property)
--     putStrLn "prop_insert_deleteText"
--     checkQuick 500  (prop_insert_deleteText :: StrText -> Int -> Tr.TrieText Int -> QC.Property)
--     putStrLn "prop_delete_lookupText"
--     checkQuick 500  (prop_delete_lookupText :: StrText -> Tr.TrieText Int -> QC.Property)
--     putStrLn "prop_submap1Text"
--     checkQuick 500  (prop_submap1Text       :: StrText -> Tr.TrieText Int -> Bool)
--     putStrLn "prop_submap2Text"
--     checkQuick 500  (prop_submap2Text       :: StrText -> Tr.TrieText Int -> Bool)
--     putStrLn "prop_submap3Text"
--     checkQuick 500  (prop_submap3Text       :: StrText -> Tr.TrieText Int -> Bool)

--     putStrLn "prop_toListText"
--     checkQuick 500  (prop_toListText        :: Tr.TrieText Int -> QC.Property)
--     putStrLn "prop_fromList_takes_firstText"
--     checkQuick 500  (prop_fromList_takes_firstText  :: [(StrText, Int)] -> QC.Property)
--     putStrLn "prop_fromListR_takes_firstText"
--     checkQuick 500  (prop_fromListR_takes_firstText :: [(StrText, Int)] -> QC.Property)
--     putStrLn "prop_fromListL_takes_firstText"
--     checkQuick 500  (prop_fromListL_takes_firstText :: [(StrText, Int)] -> QC.Property)
--     putStrLn "prop_fromListS_takes_firstText"
--     checkQuick 500  (prop_fromListS_takes_firstText :: [(StrText, Int)] -> QC.Property)
--     putStrLn "prop_fromListWithConst_takes_firstText"
--     checkQuick 500  (prop_fromListWithConst_takes_firstText  :: [(StrText, Int)] -> QC.Property)
--     putStrLn "prop_fromListWithLConst_takes_firstText"
--     checkQuick 500  (prop_fromListWithLConst_takes_firstText :: [(StrText, Int)] -> QC.Property)
--     putStrLn ""

--     putStrLn "smallcheck @ () (Text):" -- Beware the exponential!
--     putStrLn "prop_insertText"
--     checkSmall 3 (prop_insertText        :: StrText -> () -> Tr.TrieText () -> Bool)
--     putStrLn "prop_singletonText"
--     checkSmall 7 (prop_singletonText     :: StrText -> () -> Bool)
--     putStrLn "prop_size_insertText"
--     checkSmall 3 (prop_size_insertText   :: StrText -> () -> Tr.TrieText () -> SC.Property IO)
--     putStrLn "prop_size_deleteText"
--     checkSmall 3 (prop_size_deleteText   :: StrText -> () -> Tr.TrieText () -> SC.Property IO)
--     putStrLn "prop_insert_deleteText"
--     checkSmall 3 (prop_insert_deleteText :: StrText -> () -> Tr.TrieText () -> SC.Property IO)
--     putStrLn "prop_delete_lookupText"
--     checkSmall 3 (prop_delete_lookupText :: StrText -> Tr.TrieText () -> SC.Property IO)
--     putStrLn "prop_submap1Text"
--     checkSmall 3 (prop_submap1Text       :: StrText -> Tr.TrieText () -> Bool)
--     putStrLn "prop_submap2Text"
--     checkSmall 3 (prop_submap2Text       :: StrText -> Tr.TrieText () -> Bool)
--     putStrLn ""

--     where
-- #ifdef __USE_QUICKCHECK_1__
--     checkQuick n =
--         QC.check (QC.defaultConfig
--             { QC.configMaxTest = n
--             , QC.configMaxFail = 1000 `max` 10*n
--             })
-- #else
--     checkQuick n =
--         QC.quickCheckWith (QC.stdArgs
--             { QC.maxSize    = n
--             , QC.maxSuccess = n
--             , QC.maxDiscardRatio = 1000 `max` 10*n
--             })
-- #endif
--     checkSmall d f = SC.smallCheck d f >> putStrLn ""


testEqual ::  (Show a, Eq a) => String -> a -> a -> HU.Test
testEqual s a b =
    HU.TestLabel s $ HU.TestCase $ HU.assertEqual "" a b

----------------------------------------------------------------
-- Because we avoid epsilons everywhere else, need to make sure 'mergeBy' gets it right
test_Union :: HU.Test
test_Union = HU.TestLabel "epsilon union"
    $ HU.TestList
    [ testEqual "left"  (e1 `Tr.unionL` e2) e1
    , testEqual "right" (e1 `Tr.unionR` e2) e2 -- meh, why not
    , testEqual "unionR regression" (tLeft `Tr.unionR` tRight) tRightResult
    , testEqual "unionL regression" (tLeft `Tr.unionL` tRight) tLeftResult
    ]
    where
    e1 = Tr.singleton S.empty (4::Int)
    e2 = Tr.singleton S.empty (2::Int)

    -- Regression test against bug filed by Gregory Crosswhite on 2010.06.10 against version 0.2.1.1.
    a, b :: S.ByteString
    a = read "\"\231^\179\160Y\134Gr\158<)&\222\217#\156\""
    b = read "\"\172\193\GSp\222\174GE\186\151\DC1#P\213\147\SI\""
    tLeft   = Tr.fromList [(a,1::Int),(b,0::Int)]
    tRight  = Tr.fromList [(a,2::Int)]
    tRightResult = Tr.fromList [(a,2::Int),(b,0::Int)]
    tLeftResult  = Tr.fromList [(a,1::Int),(b,0::Int)]


----------------------------------------------------------------
test_Submap :: HU.Test
test_Submap = HU.TestLabel "submap"
    $ HU.TestList
    [ nullSubmap "split on arc fails"    fi   True
    , nullSubmap "prefix of arc matches" fo   False
    , nullSubmap "suffix of empty fails" food True
    , nullSubmap "missing branch fails"  bag  True
    , nullSubmap "at a branch matches"   ba   False
    ]
    where
    t    = vocab2trie ["foo", "bar", "baz"]
    fi   = packC2W "fi"
    fo   = packC2W "fo"
    food = packC2W "food"
    ba   = packC2W "ba"
    bag  = packC2W "bag"

    nullSubmap s q b = testEqual s (Tr.null $ Tr.submap q t) b

----------------------------------------------------------------
-- requires Eq (Trie a) and, in case it fails, Show (Trie a)
test_Insert :: HU.Test
test_Insert = HU.TestLabel "insert"
    $ HU.TestList
    [ testEqual "insertion is commutative for prefix/superfix"
        (Tr.insert aba o $ Tr.insert abaissed i $ Tr.empty)
        (Tr.insert abaissed i $ Tr.insert aba o $ Tr.empty)
    ]
    where
    aba      = packC2W "aba"
    abaissed = packC2W "abaissed"

    o = 0::Int
    i = 1::Int


test_Delete :: HU.Test
test_Delete = HU.TestLabel "delete"
    $ HU.TestList
    [ testEqual "deleting epsilon from empty trie is empty"
        (Tr.delete epsilon Tr.empty) (Tr.empty :: Tr.Trie Int)
    ]
    where
    epsilon = packC2W ""

----------------------------------------------------------------
-- TODO: we need a better instance of Arbitrary for lists to make them longer than our smallcheck depth.
--
-- I use strings with characters picked from a very restricted subset
-- in order to have more labels with shared prefixes.
newtype Letter = Letter { unLetter :: Char }
    deriving (Eq, Ord, Show)
letters :: [Char]
letters = ['a'..'m']

-- newtype LetterText = LetterText { unLetterText :: Char }
--     deriving (Eq, Ord, Show)
--
-- lettersText :: [Char]
-- lettersText =
--   concat
--     [ ['\t' .. '\n']
--     , [' ']
--     , ['"' .. '#']
--     , ['(' .. ')']
--     , ['+' .. ';']
--     , ['=', '?']
--     , ['A' .. 'Z']
--     , ['_']
--     , ['a' .. 'z']
--     , [ '\170'
--       , '\220'
--       , '\223'
--       , '\233'
--       , '\241'
--       , '\261'
--       , '\338'
--       , '\12354'
--       , '\12509'
--       , '\13312'
--       , '\19970'
--       , '\34920'
--       , '\36877'
--       , '\40407'
--       , '\65314'
--       , '\131072'
--       ]
--     ]

instance QC.Arbitrary Letter where
    arbitrary = Letter `fmap` QC.elements letters
    -- coarbitrary -- used in QCv1, separated in QCv2

-- instance QCA.Arbitrary LetterText where
--     arbitrary = LetterText `fmap` QC.elements lettersText
--
--     shrink = fmap LetterText . QCA.shrink . unLetterText

newtype Str = Str { unStr :: S.ByteString }
    deriving (Eq, Ord)

instance Show Str where
    show (Str s) = "Str {unStr = packC2W " ++ show s ++ " }"

instance QC.Arbitrary Str where
    arbitrary = QC.sized $ \n -> do
        k <- QC.choose (0,n)
        s <- QC.vector k
        c <- QC.arbitrary -- We only want non-empty strings.
        return . Str . packC2W $ map unLetter (c:s)
    -- coarbitrary -- used in QCv1, separated in QCv2


-- newtype StrText = StrText { unStrText :: T.Text }
--     deriving (Eq, Ord, Show)
--
-- instance QCA.Arbitrary StrText where
--     arbitrary = QC.sized $ \n -> do
--         k <- QC.choose (0,n)
--         s <- QC.vector k
--         c <- QC.arbitrary -- We only want non-empty strings.
--         return . StrText . T.pack $ map unLetterText (c:s)
--
--     shrink = fmap StrText . QCA.shrink . unStrText


instance (QC.Arbitrary a) => QC.Arbitrary (Tr.Trie a) where
    arbitrary = QC.sized $ \n -> do
        k      <- QC.choose (0,n)
        labels <- map unStr `fmap` QC.vector k
        elems  <- QC.vector k
        return . Tr.fromList $ zip labels elems
    -- coarbitrary -- used in QCv1, separated in QCv2


-- instance QCA.Arbitrary T.Text where
--   arbitrary = T.pack <$> QCA.arbitrary
--
--   shrink = fmap T.pack . QCA.shrink . T.unpack
--
-- instance (QCA.Arbitrary a, Generic a) => QCA.Arbitrary (Tr.TrieText a) where
--     arbitrary = QC.sized $ \n -> do
--         k      <- QC.choose (0,n)
--         labels <- map unStrText `fmap` QC.vector k
--         elems  <- QC.vector k
--         return . Tr.fromListText $ zip labels elems
--
--     shrink = QCA.genericShrink

----------------------------------------------------------------
-- cf <http://www.cs.york.ac.uk/fp/darcs/smallcheck/README>
-- type Series a = Int -> [a]

instance Monad m => SCS.Serial m Letter where
  series = SCS.generate $ \d -> take (d+1) $ map Letter letters
--     coseries rs d = do f <- SC.coseries rs d
--                        return $ \c -> f (fromEnum (unLetter c) - fromEnum 'a')

-- instance Monad m => SCS.Serial m LetterText where
--   series = SCS.generate $ \d -> take (d+1) $ map LetterText lettersText


instance Monad m => SCS.Serial m Str where
    series = Str . packC2W . map unLetter <$> SCS.series
--
--     coseries rs d = do y <- SC.alts0 rs d
--                        f <- SC.alts2 rs d
--                        return $ \(Str xs) ->
--                            if S.null xs
--                            then y
--                            else f (Letter . S.w2c $ S.head xs)
--                                   (Str $ S.tail xs)

-- instance Monad m => SCS.Serial m StrText where
--     series = StrText . T.pack . map unLetterText <$> SCS.series

-- -- TODO: This instance really needs some work. The smart constructures ensure only valid values are generated, but there are redundancies and inefficiencies.
instance (Monoid a, SCS.Serial m a) => SCS.Serial m (Tr.Trie a) where
    series =      SCS.cons0 Tr.empty
           SCS.\/  SCS.cons3 arcHACK
           SCS.\/  SCS.cons2 mappend
           where
           arcHACK (Str k) Nothing  t = Tr.singleton k () >> t
           arcHACK (Str k) (Just v) t = Tr.singleton k v
                                         >>= Tr.unionR t . Tr.singleton S.empty
--
--     -- coseries :: Series b -> Series (Trie a -> b)
--     coseries = error "coseries@Trie: not implemented"

-- instance (Monoid a, SCS.Serial m a) => SCS.Serial m (Tr.TrieText a) where
--     series =      SCS.cons0 Tr.emptyText
--            SCS.\/  SCS.cons3 arcHACK
--            SCS.\/  SCS.cons2 mappend
--            where
--            arcHACK (StrText k) Nothing  t = Tr.singletonText k () >> t
--            arcHACK (StrText k) (Just v) t = Tr.singletonText k v
--                                             >>= Tr.unionRText t . Tr.singletonText T.empty


----------------------------------------------------------------
----------------------------------------------------------------
-- | If you insert a value, you can look it up
prop_insert :: (Eq a) => Str -> a -> Tr.Trie a -> Bool
prop_insert (Str k) v t =
    (Tr.lookup k . Tr.insert k v $ t) == Just v

-- prop_insertText :: (Eq a) => StrText -> a -> Tr.TrieText a -> Bool
-- prop_insertText (StrText k) v t =
--     (Tr.lookupText k . Tr.insertText k v $ t) == Just v


-- | A singleton, is.
prop_singleton :: (Eq a) => Str -> a -> Bool
prop_singleton (Str k) v =
    Tr.insert k v Tr.empty == Tr.singleton k v

-- prop_singletonText :: (Eq a) => StrText -> a -> Bool
-- prop_singletonText (StrText k) v =
--     Tr.insertText k v Tr.emptyText == Tr.singletonText k v


-- | Deal with QC/SC polymorphism issues because of (==>)
-- Fundeps would be nice here, but |b->a is undecidable, and |a->b is wrong
class CheckGuard a b where
    (==>) :: Bool -> a -> b

instance (QC.Testable a) => CheckGuard a QC.Property where
    (==>) = (QC.==>)

instance SC.Testable IO QC.Property where
  test prop = SC.monadic $ do
    result <- checkQuick 500 prop
    if QC.isSuccess result
       then return True
       else do
         print result
         return False
    where
#ifdef __USE_QUICKCHECK_1__
    checkQuick n =
        QC.checkResult (QC.defaultConfig
            { QC.configMaxTest = n
            , QC.configMaxFail = 1000 `max` 10*n
            })
#else
    checkQuick n =
        QC.quickCheckWithResult (QC.stdArgs
            { QC.maxSize    = n
            , QC.maxSuccess = n
            , QC.maxDiscardRatio = 1000 `max` 10*n
            })
#endif


instance (Monad m, SC.Testable m a) => CheckGuard a (SC.Property m) where
    (==>) = (SC.==>)

prop_size_insert :: (Eq a, Show a, CheckGuard QC.Property b) => Str -> a -> Tr.Trie a -> b
prop_size_insert (Str k) v t = not (k `Tr.member` t) ==> (
    (Tr.size . Tr.insert k v) === ((1+) . Tr.size)
    $ t)

-- prop_size_insertText :: (Eq a, Show a, CheckGuard QC.Property b) => StrText -> a -> Tr.TrieText a -> b
-- prop_size_insertText (StrText k) v t = not (k `Tr.memberText` t) ==> (
--     (Tr.sizeText . Tr.insertText k v) === ((1+) . Tr.sizeText)
--     $ t)


prop_size_delete :: (Eq a, Show a, CheckGuard QC.Property b) => Str -> a -> Tr.Trie a -> b
prop_size_delete (Str k) v t = not (k `Tr.member` t) ==> (
    (Tr.size . Tr.delete k . Tr.insert k v) === Tr.size
    $ t)

-- prop_size_deleteText :: (Eq a, Show a, CheckGuard QC.Property b) => StrText -> a -> Tr.TrieText a -> b
-- prop_size_deleteText (StrText k) v t = not (k `Tr.memberText` t) ==> (
--     (Tr.sizeText . Tr.deleteText k . Tr.insertText k v) === Tr.sizeText
--     $ t)


prop_insert_delete :: (Eq a, Show a, CheckGuard QC.Property b) => Str -> a -> Tr.Trie a -> b
prop_insert_delete (Str k) v t = not (k `Tr.member` t) ==> (
    (Tr.delete k . Tr.insert k v) === id
    $ t)

-- prop_insert_deleteText :: (Eq a, Show a, CheckGuard QC.Property b) => StrText -> a -> Tr.TrieText a -> b
-- prop_insert_deleteText (StrText k) v t = not (k `Tr.memberText` t) ==> (
--     (Tr.deleteText k . Tr.insertText k v) === id
--     $ t)


prop_delete_lookup :: (Eq a, Show a, CheckGuard QC.Property b) => Str -> Tr.Trie a -> b
prop_delete_lookup (Str k) t = not (k `Tr.member` t) ==> (
    (Tr.lookup k . Tr.delete k) === const Nothing
    $ t)

-- prop_delete_lookupText :: (Eq a, Show a, CheckGuard QC.Property b) => StrText -> Tr.TrieText a -> b
-- prop_delete_lookupText (StrText k) t = not (k `Tr.memberText` t) ==> (
--     (Tr.lookupText k . Tr.deleteText k) === const Nothing
--     $ t)


-- | All keys in a submap are keys in the supermap
prop_submap1 :: Str -> Tr.Trie a -> Bool
prop_submap1 (Str k) t =
    all (`Tr.member` t) . Tr.keys . Tr.submap k $ t

-- prop_submap1Text :: StrText -> Tr.TrieText a -> Bool
-- prop_submap1Text (StrText k) t =
--     all (`Tr.memberText` t) . Tr.keysText . Tr.submapText k $ t


-- | All keys in a submap have the query as a prefix
prop_submap2 :: Str -> Tr.Trie a -> Bool
prop_submap2 (Str k) t =
    all (S.isPrefixOf k) . Tr.keys . Tr.submap k $ t

-- prop_submap2Text :: StrText -> Tr.TrieText a -> Bool
-- prop_submap2Text (StrText k) t =
--     all (T.isPrefixOf k) . Tr.keysText . Tr.submapText k $ t


-- | All values in a submap are the same in the supermap
prop_submap3 :: (Eq a) => Str -> Tr.Trie a -> Bool
prop_submap3 (Str k) t =
    (\q -> Tr.lookup q t' == Tr.lookup q t) `all` Tr.keys t'
    where t' = Tr.submap k t

-- prop_submap3Text :: (Eq a) => StrText -> Tr.TrieText a -> Bool
-- prop_submap3Text (StrText k) t =
--     (\q -> Tr.lookupText q t' == Tr.lookupText q t) `all` Tr.keysText t'
--     where t' = Tr.submapText k t


-- | Keys are ordered when converting to a list
prop_toList :: Tr.Trie a -> Bool
prop_toList t = ordered (Tr.keys t)
    where ordered xs = and (zipWith (<=) xs (drop 1 xs))

-- infix 4 <==
-- (<==) :: (Ord a, Show a) => a -> a -> QC.Property
-- x <== y =
--   QC.counterexample (show x ++ interpret res ++ show y) (res == LT || res == EQ)
--   where
--     res = x `compare` y
--     interpret LT = " <  "
--     interpret EQ = " == "
--     interpret GT = " >  "

-- instance Show (TIF.Stream Char) where
--   show = show . TIF.unstream

-- prop_toListText :: Tr.TrieText a -> QC.Property
-- prop_toListText t = ordered (TrI.toList16 <$> Tr.keysText t)
--     where ordered xs = QC.conjoin (zipWith (<==) xs (drop 1 xs))


_takes_first :: (Eq c, Show c) => ([(S.ByteString, c)] -> Tr.Trie c) -> [(Str, c)] -> QC.Property
_takes_first f assocs =
    (Tr.toList . f) === (nubBy (apFst (==)) . sortBy (comparing fst))
    $ map (first unStr) assocs

-- _takes_firstText :: (Eq c, Show c) => ([(T.Text, c)] -> Tr.TrieText c) -> [(StrText, c)] -> QC.Property
-- _takes_firstText f assocs =
--     (Tr.toListText . f) === (nubBy (apFst ((==) `on` TrI.toList16)) . sortBy (comparing (TrI.toList16 . fst)))
--     $ map (first unStrText) assocs


-- | 'fromList' takes the first value for a given key
prop_fromList_takes_first :: (Eq a, Show a) => [(Str, a)] -> QC.Property
prop_fromList_takes_first = _takes_first Tr.fromList

-- prop_fromList_takes_firstText :: (Eq a, Show a) => [(StrText, a)] -> QC.Property
-- prop_fromList_takes_firstText = _takes_firstText Tr.fromListText


-- | 'fromListR' takes the first value for a given key
prop_fromListR_takes_first :: (Eq a, Show a) => [(Str, a)] -> QC.Property
prop_fromListR_takes_first = _takes_first TC.fromListR

-- prop_fromListR_takes_firstText :: (Eq a, Show a) => [(StrText, a)] -> QC.Property
-- prop_fromListR_takes_firstText = _takes_firstText TC.fromListRText


-- | 'fromListL' takes the first value for a given key
prop_fromListL_takes_first :: (Eq a, Show a) => [(Str, a)] -> QC.Property
prop_fromListL_takes_first = _takes_first TC.fromListL

-- prop_fromListL_takes_firstText :: (Eq a, Show a) => [(StrText, a)] -> QC.Property
-- prop_fromListL_takes_firstText = _takes_firstText TC.fromListLText


-- | 'fromListS' takes the first value for a given key
prop_fromListS_takes_first :: (Eq a, Show a) => [(Str, a)] -> QC.Property
prop_fromListS_takes_first = _takes_first TC.fromListS

-- prop_fromListS_takes_firstText :: (Eq a, Show a) => [(StrText, a)] -> QC.Property
-- prop_fromListS_takes_firstText = _takes_firstText TC.fromListSText


-- | @('fromListWith' const)@ takes the first value for a given key
prop_fromListWithConst_takes_first :: (Eq a, Show a) => [(Str, a)] -> QC.Property
prop_fromListWithConst_takes_first = _takes_first (TC.fromListWith const)

-- prop_fromListWithConst_takes_firstText :: (Eq a, Show a) => [(StrText, a)] -> QC.Property
-- prop_fromListWithConst_takes_firstText = _takes_firstText (TC.fromListWithText const)


-- | @('fromListWithL' const)@ takes the first value for a given key
prop_fromListWithLConst_takes_first :: (Eq a, Show a) => [(Str, a)] -> QC.Property
prop_fromListWithLConst_takes_first = _takes_first (TC.fromListWithL const)

-- prop_fromListWithLConst_takes_firstText :: (Eq a, Show a) => [(StrText, a)] -> QC.Property
-- prop_fromListWithLConst_takes_firstText = _takes_firstText (TC.fromListWithLText const)


----------------------------------------------------------------
-- | Lift a function to apply to the first of pairs, retaining the second.
first :: (a -> b) -> (a,c) -> (b,c)
first f (x,y) = (f x, y)

-- | Lift a binary function to apply to the first of pairs, discarding seconds.
apFst :: (a -> b -> c) -> ((a,d) -> (b,e) -> c)
apFst f (x,_) (y,_) = f x y

-- | Function equality
(===) :: (Eq b, Show b) => (a -> b) -> (a -> b) -> (a -> QC.Property)
(===) f g x = (QC.===) (f x) (g x)
----------------------------------------------------------------
----------------------------------------------------------- fin.
