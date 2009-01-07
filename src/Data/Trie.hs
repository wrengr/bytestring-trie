{-# OPTIONS_GHC -Wall -fwarn-tabs #-}

----------------------------------------------------------------
--                                                  ~ 2009.01.05
-- |
-- Module      :  Data.Trie
-- Copyright   :  Copyright (c) 2008--2009 wren ng thornton
-- License     :  BSD3
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  beta
-- Portability :  portable
--
-- An efficient implementation of finite maps from strings to values.
--
-- The implementation is based on /big-endian patricia trees/, like
-- "Data.IntMap". We first trie on the elements of "Data.ByteString"
-- and then trie on the big-endian bit representation of those
-- elements. For further details on the latter, see
--
--    * Chris Okasaki and Andy Gill,  \"/Fast Mergeable Integer Maps/\",
--	Workshop on ML, September 1998, pages 77-86,
--	<http://www.cse.ogi.edu/~andy/pub/finite.htm>
--
--    * D.R. Morrison, \"/PATRICIA -- Practical Algorithm To Retrieve/
--	/Information Coded In Alphanumeric/\", Journal of the ACM, 15(4),
--	October 1968, pages 514-534.
----------------------------------------------------------------

module Data.Trie
    (
    -- * Data types
      Trie(), KeyString, KeyElem
    
    -- * Basic functions
    , empty, null, singleton, size
    
    -- * Conversion functions
    , fromList, toListBy, toList, keys
    
    -- * Query functions
    , lookupBy, lookup, member, submap
    
    -- * Single-value modification
    , alterBy, insert, adjust, delete
    
    -- * Combining tries
    , mergeBy, unionL, unionR
    
    -- * Mapping functions
    , mapBy, filterMap
    ) where

import Prelude hiding (null, lookup)
import qualified Prelude

import Data.Trie.Internal

import Data.Maybe    (isJust)
import Control.Monad (liftM)
----------------------------------------------------------------
----------------------------------------------------------------


{---------------------------------------------------------------
-- Conversion functions 
---------------------------------------------------------------}

-- | Convert association list into a trie. On key conflict, values
-- earlier in the list shadow later ones.
{-# INLINE fromList #-}
fromList :: [(KeyString,a)] -> Trie a
fromList = foldr (uncurry insert) empty

-- | Convert trie into association list. Keys will be in sorted order.
toList :: Trie a -> [(KeyString,a)]
toList  = toListBy (,)

-- | Return all keys in the trie, in sorted order.
keys :: Trie a -> [KeyString]
keys  = toListBy const


{---------------------------------------------------------------
-- Query functions (just recurse)
---------------------------------------------------------------}

-- | Generic function to find a value (if it exists) and the subtrie
-- rooted at the prefix.
{-# INLINE lookupBy #-}
lookupBy :: (Maybe a -> Trie a -> b) -> KeyString -> Trie a -> b
lookupBy f = lookupBy_ f (f Nothing empty) (f Nothing)

-- | Return the value associated with a query string if it exists.
{-# INLINE lookup #-}
lookup :: KeyString -> Trie a -> Maybe a
lookup = lookupBy_ const Nothing (const Nothing)

-- TODO? move to "Data.Trie.Conventience"?
-- | Does a string have a value in the trie?
{-# INLINE member #-}
member :: KeyString -> Trie a -> Bool
member q = isJust . lookup q


{---------------------------------------------------------------
-- Single-value modification functions (recurse and clone spine)
---------------------------------------------------------------}

-- | Insert a new key. If the key is already present, overrides the
-- old value
{-# INLINE insert #-}
insert    :: KeyString -> a -> Trie a -> Trie a
insert     = alterBy (\_ x _ -> Just x)
                                      
-- | Apply a function to the value at a key.
{-# INLINE adjust #-}
adjust    :: (a -> a) -> KeyString -> Trie a -> Trie a
adjust f q = alterBy (\_ _ -> liftM f) q undefined

-- | Remove the value stored at a key.
{-# INLINE delete #-}
delete     :: KeyString -> Trie a -> Trie a
delete    q = alterBy (\_ _ _ -> Nothing) q undefined


{---------------------------------------------------------------
-- Trie-combining functions
---------------------------------------------------------------}

-- | Combine two tries, resolving conflicts by choosing the value
-- from the left trie.
{-# INLINE unionL #-}
unionL :: Trie a -> Trie a -> Trie a
unionL = mergeBy (\x _ -> Just x)

-- | Combine two tries, resolving conflicts by choosing the value
-- from the right trie.
{-# INLINE unionR #-}
unionR :: Trie a -> Trie a -> Trie a
unionR = mergeBy (\_ y -> Just y)

----------------------------------------------------------------
----------------------------------------------------------- fin.
