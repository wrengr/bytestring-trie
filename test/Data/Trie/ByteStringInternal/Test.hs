{-# OPTIONS_GHC -Wall -fwarn-tabs #-}

----------------------------------------------------------------
--                                                  ~ 2009.01.04
-- |
-- Module      :  Data.Trie.ByteStringInternal.Test
-- Copyright   :  Copyright (c) 2008--2009 wren ng thornton
-- License     :  BSD3
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Testing helper functions on 'ByteString's.
----------------------------------------------------------------


module Data.Trie.ByteStringInternal.Test where
import Data.Trie.ByteStringInternal
import Data.Trie.Test (packC2W)

----------------------------------------------------------------

-- TODO: make this into an HUnit test
test :: IO ()
test  = do
    cmp hello
    cmp $ packC2W "hi"
    cmp $ packC2W "heat"
    cmp $ packC2W "hell" -- This is the tricky one
    cmp $ packC2W "jello"
    where
    cmp y = do putStrLn . show . splitMaximalPrefix hello $ y
               putStrLn . show . (\(a,b,c) -> (a,c,b)) . splitMaximalPrefix y $ hello
               putStrLn "\n"
    hello = packC2W "hello"
----------------------------------------------------------------
----------------------------------------------------------- fin.