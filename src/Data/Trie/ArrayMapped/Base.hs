-- To make GHC stop warning about the Prelude
{-# OPTIONS_GHC -Wall -fwarn-tabs -fno-warn-unused-imports #-}
{-# LANGUAGE NoImplicitPrelude #-}
----------------------------------------------------------------
--                                                  ~ 2016.04.10
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
    , I.empty, I.null, I.singleton, I.size
    
    -- * Conversion functions
    , fromList, toListBy, toList, keys, elems
    
    -- * Query functions
    , I.lookupBy, lookup, member, I.submap
    
    -- * Single-value modification
    , I.alterBy, insert, I.adjust, delete
    
    -- * Combining tries
    , I.mergeBy, unionL, unionR
    
    -- * Mapping functions
    , I.mapBy, I.filterMap
    ) where

import Prelude hiding    (null, lookup)
import qualified Prelude (null, lookup)

import Data.Trie.ArrayMapped.Internal (Trie())
import qualified Data.Trie.ArrayMapped.Internal as I
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
fromList = foldr (uncurry insert) I.empty
    -- TODO: this can be *greatly* improved by using DynamicArrays


toListBy :: (ByteString -> a -> b) -> Trie a -> [b]
{-# INLINE toListBy #-}
toListBy = error "TODO: toListBy"

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
member  = I.lookupBy_ (\_ _ -> True) (\_ -> False)
{-# INLINE member #-}


-- | Return the value associated with a query string if it exists.
lookup :: ByteString -> Trie a -> Maybe a
lookup  = I.lookupBy_ (\v _ -> Just v) (\_ -> Nothing)
{-# INLINE lookup #-}



{---------------------------------------------------------------
-- Single-value modification functions (recurse and clone spine)
---------------------------------------------------------------}

-- | Insert a new key. If the key is already present, overrides the
-- old value
insert :: ByteString -> a -> Trie a -> Trie a
{-# INLINE insert #-}
insert k v = I.alter (\_ -> Just v) k
-- TODO: can we use 'I.alterBy_' directly?


-- | Remove the value stored at a key.
delete :: ByteString -> Trie a -> Trie a
{-# INLINE delete #-}
delete = I.alter (\_ -> Nothing)
-- TODO: can we use 'I.alterBy_' directly?


{---------------------------------------------------------------
-- Trie-combining functions
---------------------------------------------------------------}

-- | Combine two tries, resolving conflicts by choosing the value
-- from the left trie.
unionL :: Trie a -> Trie a -> Trie a
{-# INLINE unionL #-}
unionL = I.mergeBy (\x _ -> Just x)


-- | Combine two tries, resolving conflicts by choosing the value
-- from the right trie.
unionR :: Trie a -> Trie a -> Trie a
{-# INLINE unionR #-}
unionR = I.mergeBy (\_ y -> Just y)

----------------------------------------------------------------
----------------------------------------------------------- fin.