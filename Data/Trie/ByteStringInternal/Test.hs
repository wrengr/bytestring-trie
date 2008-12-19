{-# OPTIONS_GHC -Wall -fwarn-tabs #-}

----------------------------------------------------------------
--                                                  ~ 2008.12.12
-- |
-- Module      :  Data.Trie.ByteStringInternal.Test
-- Copyright   :  Copyright (c) 2008--2009 wren ng thornton
-- License     :  BSD3
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Helper functions on 'ByteString's for "Data.ByteStringTrie".
----------------------------------------------------------------


module Data.Trie.ByteStringInternal.Test where
import Data.Trie.ByteStringInternal

import qualified Data.ByteString as S
import Data.ByteString.Internal (c2w)
----------------------------------------------------------------

pk :: String -> S.ByteString
pk = S.pack . map c2w


test :: IO ()
test  = do
    cmp hello
    cmp $ pk "hi"
    cmp $ pk "heat"
    cmp $ pk "hell" -- This is the tricky one
    cmp $ pk "jello"
    where
    cmp y = do putStrLn . show . splitMaximalPrefix hello $ y
               putStrLn . show . (\(a,b,c) -> (a,c,b)) . splitMaximalPrefix y $ hello
               putStrLn "\n"
    hello = pk "hello"
----------------------------------------------------------------
----------------------------------------------------------- fin.