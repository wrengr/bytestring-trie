{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

----------------------------------------------------------------
--                                                  ~ 2015.10.22
-- |
-- Module      :  Data.Trie.TestIsect
-- Copyright   :  Copyright (c) 2008--2011 wren gayle romano
-- License     :  BSD3
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  provisional
-- Portability :  semi-portable (MPTC,...)
--
-- Testing intersection of 'Trie's.
----------------------------------------------------------------
module Data.Trie.Test (main) where

import qualified Data.ByteString          as S
import qualified Data.ByteString.Internal as S (c2w)
import qualified Data.Trie                as T
import qualified Data.Trie.Convenience    as TC
import qualified Data.Trie.Internal       as TI

import qualified Test.QuickCheck          as QC

----------------------------------------------------------------
----------------------------------------------------------------

packC2W :: String -> S.ByteString
packC2W  = S.pack . map S.c2w

----------------------------------------------------------------
main :: IO ()
main  = do
    putStrLn ""
    putStrLn (replicate 80 '~')

    putStrLn ""

    putStrLn "quickcheck @ Int:"
    checkQuick 500  (prop_isect        :: T.Trie Int -> T.Trie Int -> Bool)
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
            })
#endif


prop_isect :: (Eq a) => T.Trie a -> T.Trie a -> Bool
prop_isect t0 t1 =
  TI.intersectBy (\a _ -> Just a) t0 t1 == TC.disunion (T.unionL t0 t1) (TC.disunion t0 t1)


----------------------------------------------------------------
-- TODO: we need a better instance of Arbitrary for lists to make them longer than our smallcheck depth.
--
-- I use strings with characters picked from a very restricted subset
-- in order to have more labels with shared prefixes.
newtype Letter = Letter { unLetter :: Char }
    deriving (Eq, Ord, Show)
letters :: [Char]
letters = ['a'..'m']

instance QC.Arbitrary Letter where
    arbitrary = Letter `fmap` QC.elements letters
    -- coarbitrary -- used in QCv1, separated in QCv2

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

instance (QC.Arbitrary a) => QC.Arbitrary (T.Trie a) where
    arbitrary = QC.sized $ \n -> do
        k      <- QC.choose (0,n)
        labels <- map unStr `fmap` QC.vector k
        elems  <- QC.vector k
        return . T.fromList $ zip labels elems
    -- coarbitrary -- used in QCv1, separated in QCv2
