-- To make GHC stop warning about the Prelude
{-# OPTIONS_GHC -Wall -fwarn-tabs -fno-warn-unused-imports #-}
{-# LANGUAGE NoImplicitPrelude #-}
----------------------------------------------------------------
--                                                  ~ 2014.05.29
-- |
-- Module      :  Data.Trie.ArrayMapped.Base
-- Copyright   :  Copyright (c) 2014--2015 wren gayle romano
-- License     :  BSD3
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- An efficient implementation of finite maps from strings to values.
-- The implementation is based on /array mapped tries/, like
-- "Data.HashMap". For further details on the latter, see
--
--    * Phil Bagwell,  \"/Fast And Space Efficient Trie Searches/\",
--    $CONFERENCE, $DATE, pages $PAGES,
--    <$URL>
--
-- This module aims to provide an austere interface, while being
-- detailed enough for most users. For an extended interface with
-- many additional functions, see "Data.Trie.Convenience". For
-- functions that give more detailed (potentially abstraction-breaking)
-- access to the data strucuture, or for experimental functions
-- which aren't quite ready for the public API, see "Data.Trie.Internal".
----------------------------------------------------------------

module Data.Trie.ArrayMapped.Base
    (
    -- * Data type
      Trie()
    
    -- * Basic functions
    , empty, null, singleton, size
    
    -- * Conversion functions
    , fromList, toListBy, toList, keys, elems
    
    -- * Query functions
    , lookupBy, lookup, member, submap
    
    -- * Single-value modification
    , alterBy, insert, adjust, delete
    
    -- * Combining tries
    , mergeBy, unionL, unionR
    
    -- * Mapping functions
    , mapBy, filterMap
    ) where

import Prelude hiding    (null, lookup)
import qualified Prelude (null, lookup)

import Data.Trie.ArrayMapped.Internal
import Data.Trie.ArrayMapped.Errors     (impossible)
import Data.ByteString                  (ByteString)
import Data.Maybe                       (isJust)
import Control.Monad                    (liftM)
----------------------------------------------------------------
----------------------------------------------------------------


{---------------------------------------------------------------
-- Conversion functions 
---------------------------------------------------------------}

-- | Convert association list into a trie. On key conflict, values
-- earlier in the list shadow later ones.
fromList :: [(ByteString,a)] -> Trie a
{-# INLINE fromList #-}
fromList = foldr (uncurry insert) empty
    -- TODO: this can be *greatly* improved by using DynamicArrays


-- | Convert trie into association list. Keys will be in sorted order.
toList :: Trie a -> [(ByteString,a)]
{-# INLINE toList #-}
toList  = toListBy (,)


-- FIX? should 'keys' and 'elems' move to Data.Trie.Convenience instead?

-- | Return all keys in the trie, in sorted order.
keys :: Trie a -> [ByteString]
{-# INLINE keys #-}
keys  = toListBy const


-- | Return all values in the trie, in sorted order according to the keys.
elems :: Trie a -> [a]
{-# INLINE elems #-}
elems  = toListBy (flip const)


{---------------------------------------------------------------
-- Query functions (just recurse)
---------------------------------------------------------------}


-- | Does a string have a value in the trie?
member :: ByteString -> Trie a -> Bool
member  = (isJust .) . lookup
{-# INLINE member #-}


-- | Return the value associated with a query string if it exists.
lookup :: ByteString -> Trie a -> Maybe a
lookup  = lookupBy const
{-# INLINE lookup #-}


-- | Return the subtrie containing all keys beginning with a prefix.
submap :: ByteString -> Trie a -> Trie a
submap  = lookupBy accept
{-# INLINE submap #-}



{---------------------------------------------------------------
-- Single-value modification functions (recurse and clone spine)
---------------------------------------------------------------}

-- | Insert a new key. If the key is already present, overrides the
-- old value
insert :: ByteString -> a -> Trie a -> Trie a
{-# INLINE insert #-}
insert = alterBy (\_ x _ -> Just x)


-- | Apply a function to the value at a key.
adjust :: (a -> a) -> ByteString -> Trie a -> Trie a
{-# INLINE adjust #-}
adjust f q = adjustBy (\_ _ -> f) q (impossible "adjust")
-- TODO: benchmark vs the definition with alterBy/liftM


-- | Remove the value stored at a key.
delete :: ByteString -> Trie a -> Trie a
{-# INLINE delete #-}
delete q = alterBy (\_ _ _ -> Nothing) q (impossible "delete")


{---------------------------------------------------------------
-- Trie-combining functions
---------------------------------------------------------------}

-- | Combine two tries, resolving conflicts by choosing the value
-- from the left trie.
unionL :: Trie a -> Trie a -> Trie a
{-# INLINE unionL #-}
unionL = mergeBy (\x _ -> Just x)


-- | Combine two tries, resolving conflicts by choosing the value
-- from the right trie.
unionR :: Trie a -> Trie a -> Trie a
{-# INLINE unionR #-}
unionR = mergeBy (\_ y -> Just y)

----------------------------------------------------------------
----------------------------------------------------------- fin.