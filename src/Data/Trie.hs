{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
{-# LANGUAGE NoImplicitPrelude, CPP #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Safe #-}
#endif
----------------------------------------------------------------
--                                                  ~ 2021.12.05
-- |
-- Module      :  Data.Trie
-- Copyright   :  Copyright (c) 2008--2021 wren romano
-- License     :  BSD3
-- Maintainer  :  wren@cpan.org
-- Stability   :  provisional
-- Portability :  portable
--
-- An efficient implementation of finite maps from strings to values.
-- The implementation is based on /big-endian patricia trees/, like
-- "Data.IntMap". We first trie on the elements of "Data.ByteString"
-- and then trie on the big-endian bit representation of those
-- elements. For further details on the latter, see
--
--    * Chris Okasaki and Andy Gill,  \"/Fast Mergeable Integer Maps/\",
--    Workshop on ML, September 1998, pages 77-86,
--    <http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.37.5452>
--
--    * D.R. Morrison, \"/PATRICIA -- Practical Algorithm To Retrieve/
--    /Information Coded In Alphanumeric/\", Journal of the ACM, 15(4),
--    October 1968, pages 514-534.
--
-- This module aims to provide an austere interface, while being
-- detailed enough for most users. For an extended interface with
-- many additional functions, see "Data.Trie.Convenience". For
-- functions that give more detailed (potentially abstraction-breaking)
-- access to the data strucuture, or for experimental functions
-- which aren't quite ready for the public API, see "Data.Trie.Internal".
----------------------------------------------------------------

module Data.Trie
    (
    -- * Data type
      Trie()

    -- * Basic functions
    , empty, null, singleton, size

    -- * Conversion functions
    , fromList, toListBy, toList, keys, elems

    -- * Query functions
    , lookupBy, lookup, member, submap, match, minMatch, matches

    -- * Simple modification
    , insert, adjust, adjustBy, alterBy, delete, deleteSubmap

    -- * Combining tries
    , mergeBy, unionL, unionR
    , intersectBy, intersectL, intersectR

    -- * Mapping functions
    , mapBy, filterMap
    ) where

import Prelude hiding     (null, lookup)

import Data.Trie.Internal
import Data.ByteString    (ByteString)
import qualified Data.ByteString as S
import Data.Maybe         (isJust)
----------------------------------------------------------------
----------------------------------------------------------------


{---------------------------------------------------------------
-- Conversion functions
---------------------------------------------------------------}

-- | Return all keys in the trie, in sorted order.
keys :: Trie a -> [ByteString]
{-# INLINE keys #-}
keys  = toListBy const



{---------------------------------------------------------------
-- Query functions (just recurse)
---------------------------------------------------------------}

-- | Generic function to find a value (if it exists) and the subtrie
-- rooted at the prefix.
lookupBy :: (Maybe a -> Trie a -> b) -> ByteString -> Trie a -> b
{-# INLINE lookupBy #-}
lookupBy f = lookupBy_ f (f Nothing empty) (f Nothing)

-- | Return the value associated with a query string if it exists.
lookup :: ByteString -> Trie a -> Maybe a
{-# INLINE lookup #-}
lookup = lookupBy const

-- | Does a string have a value in the trie?
member :: ByteString -> Trie a -> Bool
{-# INLINE member #-}
member q = isJust . lookup q

getMatch :: ByteString -> (Int, a) -> (ByteString, a, ByteString)
{-# INLINE getMatch #-}
getMatch q (n,x) =
    case S.splitAt n q of
    (p,q') -> (p, x, q')

-- | Given a query, find the longest prefix with an associated value
-- in the trie, and return that prefix, it's value, and the remainder
-- of the query.
--
-- @since 0.2.4
match :: Trie a -> ByteString -> Maybe (ByteString, a, ByteString)
{-# INLINE match #-}
match t q = getMatch q <$> match_ t q

-- | Given a query, find the shortest prefix with an associated value
-- in the trie, and return that prefix, it's value, and the remainder
-- of the query.
--
-- @since 0.2.6
minMatch :: Trie a -> ByteString -> Maybe (ByteString, a, ByteString)
{-# INLINE minMatch #-}
minMatch t q =
    case matches t q of
    []  -> Nothing
    x:_ -> Just x

-- | Given a query, find all prefixes with associated values in the
-- trie, and return their (prefix, value, remainder) triples in
-- order from shortest prefix to longest.  This function is a good
-- producer for list fusion.
--
-- @since 0.2.4
matches :: Trie a -> ByteString -> [(ByteString, a, ByteString)]
{-# INLINE matches #-}
matches t q = getMatch q <$> matches_ t q


{---------------------------------------------------------------
-- Simple modification functions (recurse and clone spine)
---------------------------------------------------------------}

-- | Insert a new key. If the key is already present, overrides the
-- old value
insert :: ByteString -> a -> Trie a -> Trie a
{-# INLINE insert #-}
insert = alterBy (\_ x _ -> Just x)

-- | Alter the value associated with a given key. If the key is not
-- present, then the trie is returned unaltered. See 'alterBy' if
-- you are interested in inserting new keys or deleting old keys.
-- Because this function does not need to worry about changing the
-- trie structure, it is somewhat faster than 'alterBy'.
--
-- @since 0.2.6
-- __NOTE__: Prior to version 0.2.6 this function was exported from
-- "Data.Trie.Internal" instead.
adjustBy :: (ByteString -> a -> a -> a)
         -> ByteString -> a -> Trie a -> Trie a
{-# INLINE adjustBy #-}
adjustBy f q x = adjust (f q x) q

-- | Remove the value stored at a key.
delete :: ByteString -> Trie a -> Trie a
{-# INLINE delete #-}
delete = alterBy_ (\_ t -> (Nothing, t))

-- | Remove all keys beginning with a prefix.
--
-- @since 0.2.6
deleteSubmap :: ByteString -> Trie a -> Trie a
{-# INLINE deleteSubmap #-}
deleteSubmap = alterBy_ (\_ _ -> (Nothing, empty))


{---------------------------------------------------------------
-- Trie-combining functions
---------------------------------------------------------------}

-- | Take the union of two tries, resolving conflicts by choosing
-- the value from the left trie.
unionL :: Trie a -> Trie a -> Trie a
{-# INLINE unionL #-}
unionL = mergeBy (\x _ -> Just x)

-- | Take the union of two tries, resolving conflicts by choosing
-- the value from the right trie.
unionR :: Trie a -> Trie a -> Trie a
{-# INLINE unionR #-}
unionR = mergeBy (\_ y -> Just y)

-- | Take the intersection of two tries, with values from the left trie.
--
-- @since 0.2.6
intersectL :: Trie a -> Trie b -> Trie a
{-# INLINE intersectL #-}
intersectL = intersectBy (\x _ -> Just x)

-- | Take the intersection of two tries, with values from the right trie.
--
-- @since 0.2.6
intersectR :: Trie a -> Trie b -> Trie b
{-# INLINE intersectR #-}
intersectR = intersectBy (\_ y -> Just y)

----------------------------------------------------------------
----------------------------------------------------------- fin.
