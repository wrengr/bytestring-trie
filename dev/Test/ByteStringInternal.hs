{-# OPTIONS_GHC -Wall -fwarn-tabs #-}

----------------------------------------------------------------
--                                                  ~ 2021.12.13
-- |
-- Module      :  Test.ByteStringInternal
-- Copyright   :  2008--2021 wren gayle romano
-- License     :  BSD3
-- Maintainer  :  wren@cpan.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Testing helper functions on 'ByteString's.
----------------------------------------------------------------

module Test.ByteStringInternal where

import Test.Utils (packC2W)
import Data.Trie.Internal.ByteString

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
    cmp hello
    cmp $ packC2W "hi"
    cmp $ packC2W "heat"
    cmp $ packC2W "held"
    cmp $ packC2W "hell"
    cmp $ packC2W "hello"
    cmp $ packC2W "jello"
    where
    cmp y = do putStrLn . show . breakMaximalPrefix hello $ y
               putStrLn . show . (\(a,b,c) -> (a,c,b)) . breakMaximalPrefix y $ hello
               putStrLn "\n"
    hello = packC2W "hello"
----------------------------------------------------------------
----------------------------------------------------------- fin.
