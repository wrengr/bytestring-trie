{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Safe #-}
#endif
----------------------------------------------------------------
--                                                  ~ 2021.12.07
-- |
-- Module      :  Data.Trie.Errors
-- Copyright   :  2008--2021 wren romano
-- License     :  BSD3
-- Maintainer  :  wren@cpan.org
-- Stability   :  internal
-- Portability :  portable
--
-- Internal convenience functions for giving error messages.
----------------------------------------------------------------

module Data.Trie.Errors (impossible) where

----------------------------------------------------------------
----------------------------------------------------------------

-- | The impossible happened. Use this instead of 'undefined'
-- whenever there's an unreachable case, an argument that shouldn't
-- ever get touched, etc.
impossible :: String -> a
impossible fn = error (formatMessage fn)
{-# INLINE impossible #-}
-- Inline the 'error' call itself, just not the string literals in the message.

formatMessage :: String -> String
formatMessage fn
    = "Data.Trie." ++ fn ++ ": The impossible happened."
    ++ "\nThis is a bug, please report it to the maintainer.\n"
{-# NOINLINE formatMessage #-}

-- N.B., at some point GHC adjusted 'error' to throw a
-- 'GHC.Exception.ErrorCall' which contains both the original message
-- and the location info.  So we shouldn't have to resort to tricks
-- like *loch* or *placeholders* use in order to get the exact
-- location of the errors.
--
-- For older versions of GHC, see this post for how
-- 'Control.Exception.assert' gets turned into
-- 'GHC.IO.Exception.assertError': <https://stackoverflow.com/a/22997767>

----------------------------------------------------------------
----------------------------------------------------------- fin.
