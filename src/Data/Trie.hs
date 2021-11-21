-- To make GHC stop warning about the Prelude
{-# OPTIONS_GHC -Wall -fwarn-tabs -fno-warn-unused-imports #-}
{-# LANGUAGE NoImplicitPrelude #-}
----------------------------------------------------------------
--                                                  ~ 2021.11.20
-- |
-- Module      :  Data.Trie
-- Copyright   :  Copyright (c) 2008--2021 wren gayle romano
-- License     :  BSD3
-- Maintainer  :  wren@cpan.org
-- Stability   :  experimental
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
import qualified Prelude  (null, lookup)

import Data.Trie.Internal
import Data.Trie.Errors   (impossible)
import Data.ByteString    (ByteString)
import qualified Data.ByteString as S
import Data.Maybe         (isJust)
import Control.Monad      (liftM)
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

-- | Generic function to find a value (if it exists) and the subtrie
-- rooted at the prefix.
lookupBy :: (Maybe a -> Trie a -> b) -> ByteString -> Trie a -> b
{-# INLINE lookupBy #-}
lookupBy f = lookupBy_ f (f Nothing empty) (f Nothing)

-- | Return the value associated with a query string if it exists.
lookup :: ByteString -> Trie a -> Maybe a
{-# INLINE lookup #-}
lookup = lookupBy_ const Nothing (const Nothing)

-- TODO? move to "Data.Trie.Convenience"?
-- | Does a string have a value in the trie?
member :: ByteString -> Trie a -> Bool
{-# INLINE member #-}
member q = isJust . lookup q


-- | Given a query, find the longest prefix with an associated value
-- in the trie, and return that prefix, it's value, and the remainder
-- of the query.
match :: Trie a -> ByteString -> Maybe (ByteString, a, ByteString)
match t q =
    case match_ t q of
    Nothing    -> Nothing
    Just (n,x) ->
        case S.splitAt n q of
        (p,q') -> Just (p, x, q')

-- | Given a query, find the shortest prefix with an associated value
-- in the trie, and return that prefix, it's value, and the remainder
-- of the query.
--
-- /Since: 0.2.6/
minMatch :: Trie a -> ByteString -> Maybe (ByteString, a, ByteString)
minMatch t q =
    case matches t q of
    []  -> Nothing
    x:_ -> Just x

-- | Given a query, find all prefixes with associated values in the
-- trie, and return their (prefix, value, remainder) triples in
-- order from shortest prefix to longest.  This function is a good
-- producer for list fusion.
matches :: Trie a -> ByteString -> [(ByteString, a, ByteString)]
{-# INLINE matches #-}
matches t q = map f (matches_ t q)
    where
    f (n,x) =
        case S.splitAt n q of
        (p,q') -> (p, x, q')


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
-- /Since: 0.2.6/ for being exported from "Data.Trie".  Before then
-- it was only exported from "Data.Trie.Internal".
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
-- /Since: 0.2.6/
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
-- /Since: 0.2.6/
intersectL :: Trie a -> Trie b -> Trie a
{-# INLINE intersectL #-}
intersectL = intersectBy (\x _ -> Just x)

-- | Take the intersection of two tries, with values from the right trie.
--
-- /Since: 0.2.6/
intersectR :: Trie a -> Trie b -> Trie b
{-# INLINE intersectR #-}
intersectR = intersectBy (\_ y -> Just y)

----------------------------------------------------------------
----------------------------------------------------------- fin.
