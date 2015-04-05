{-# OPTIONS_GHC -Wall -fwarn-tabs #-}

----------------------------------------------------------------
--                                                  ~ 2014.10.08
-- |
-- Module      :  Data.Trie.BigEndianPatricia.Errors
-- Copyright   :  Copyright (c) 2008--2015 wren gayle romano
-- License     :  BSD3
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Internal convenience functions for giving error messages.
----------------------------------------------------------------

module Data.Trie.BigEndianPatricia.Errors
    ( impossible
    ) where

----------------------------------------------------------------
----------------------------------------------------------------

-- | The impossible happened. Use this instead of 'undefined' just in case.
impossible :: String -> a
{-# NOINLINE impossible #-}
impossible fn =
    error $ "Data.Trie.BigEndianPatricia." ++ fn ++ ": the impossible happened. This is a bug, please report it to the maintainer."

----------------------------------------------------------------
----------------------------------------------------------- fin.