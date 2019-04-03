{-# OPTIONS_GHC -Wall -fwarn-tabs #-}

----------------------------------------------------------------
--                                                  ~ 2009.02.06
-- |
-- Module      :  Data.Trie.TextInternal.Test
-- Copyright   :  Copyright (c) 2019 Michael J. Klein
-- License     :  BSD3
-- Maintainer  :  lambdamichael@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Testing helper functions on 'Text'.
----------------------------------------------------------------

module Data.Trie.TextInternal.Test where

import qualified Data.Text as T
import Data.Trie.TextInternal

import Data.List (unfoldr)
----------------------------------------------------------------

-- | For debugging. [] is the infinite bit, head is the little end
showBits :: (Integral a) => a -> String
showBits = unfoldr getBit
    where
    getBit 0             = Nothing
    getBit i | odd i     = Just ('I', (i-1)`div`2)
             | otherwise = Just ('O', i`div`2)

-- TODO: make this into an HUnit test
test :: IO ()
test  = do
    cmp' helloText
    cmp' $ T.pack "hi"
    cmp' $ T.pack "heat"
    cmp' $ T.pack "held"
    cmp' $ T.pack "hell"
    cmp' $ T.pack "hello"
    cmp' $ T.pack "jello"

    where
    cmp' y = do putStrLn . show . breakMaximalPrefixText helloText $ y
                putStrLn . show . (\(a,b,c) -> (a,c,b)) . breakMaximalPrefixText y $ helloText
                putStrLn "\n"

    helloText = T.pack "hello"

----------------------------------------------------------------
----------------------------------------------------------- fin.

