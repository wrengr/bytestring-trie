{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Safe #-}
#endif
----------------------------------------------------------------
--                                                  ~ 2021.10.17
-- |
-- Module      :  Data.Trie.Errors
-- Copyright   :  Copyright (c) 2008--2021 wren gayle romano
-- License     :  BSD3
-- Maintainer  :  wren@cpan.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Internal convenience functions for giving error messages.
----------------------------------------------------------------

module Data.Trie.Errors
    ( impossible
    ) where

----------------------------------------------------------------
----------------------------------------------------------------

-- | The impossible happened. Use this instead of 'undefined' just in case.
impossible :: String -> a
{-# NOINLINE impossible #-}
impossible fn =
    error $ "Data.Trie." ++ fn ++ ": the impossible happened. This is a bug, please report it to the maintainer."

----------------------------------------------------------------
----------------------------------------------------------- fin.
