{-# OPTIONS_GHC -Wall -fwarn-tabs #-}

----------------------------------------------------------------
--                                                  ~ 2016.04.10
-- |
-- Module      :  Data.Trie.ArrayMapped.Convenience
-- Copyright   :  Copyright (c) 2008--2016 wren gayle romano
-- License     :  BSD3
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Additional convenience functions. In order to keep "Data.Trie"
-- concise, non-essential and uncommonly used functions have been
-- moved here. Most of these functions simplify the generic functions
-- from "Data.Trie", following after the interface for "Data.Map"
-- and "Data.IntMap".
----------------------------------------------------------------

module Data.Trie.ArrayMapped.Convenience
    (
    -- * Conversion functions ('fromList' variants)
    -- $fromList
      fromListL, fromListR, fromListS
    , fromListWith,  fromListWith'
    , fromListWithL, fromListWithL'
    
    -- * Query functions ('lookupBy' variants)
    , lookupWithDefault
    
    -- * Inserting values ('alterBy' variants)
    , insertIfAbsent
    , insertWith,    insertWith'
    , insertWithKey, insertWithKey'
    
    -- * Updating and adjusting values ('alterBy' and 'adjust' variants)
    , adjustWithKey
    , update, updateWithKey
    
    -- * Combining tries ('mergeBy' variants)
    , disunion
    , unionWith, unionWith'
    ) where

import Data.Trie.ArrayMapped.Base
import Data.Trie.ArrayMapped.Internal (lookupBy_, alter)
import Data.ByteString                (ByteString)
import Data.List                      (foldl', sortBy)
import Data.Ord                       (comparing)

----------------------------------------------------------------
----------------------------------------------------------------
-- $fromList
-- Just like 'fromList' all of these functions convert an association
-- list into a trie, with earlier values shadowing later ones when
-- keys conflict. Depending on the order of keys in the list, there
-- can be as much as 5x speed difference between the left and right
-- variants. Yet, performance is about the same when matching
-- best-case to best-case and worst-case to worst-case (which is
-- which is swapped when reversing the list or changing which
-- function is used).


-- | A left-fold version of 'fromList'. If you run into issues with
-- stack overflows when using 'fromList' or 'fromListR', then you
-- should use this function instead.
fromListL :: [(ByteString,a)] -> Trie a
{-# INLINE fromListL #-}
fromListL = foldl' (flip . uncurry $ insertIfAbsent) empty


-- | An explicitly right-fold variant of 'fromList'. It is a good
-- consumer for list fusion. Worst-case behavior is somewhat worse
-- than worst-case for 'fromListL'. The 'fromList' function is
-- currently just an alias for 'fromListR'.
fromListR :: [(ByteString,a)] -> Trie a
{-# INLINE fromListR #-}
fromListR = fromList -- == foldr (uncurry insert) empty


-- TODO: compare performance against a fromListL variant, adjusting the sort appropriately
--
-- | This variant sorts the list before folding over it. This adds
-- /O(n log n)/ overhead and requires the whole list be in memory
-- at once, but it ensures that the list is in best-case order. The
-- benefits generally outweigh the costs.
fromListS :: [(ByteString,a)] -> Trie a
{-# INLINE fromListS #-}
fromListS = fromListR . sortBy (comparing fst)


-- | A variant of 'fromListR' that takes a function for combining
-- values on conflict. The first argument to the combining function
-- is the ``new'' value from the initial portion of the list; the
-- second argument is the value that has been accumulated into the
-- trie from the tail of the list (just like the first argument to
-- 'foldr'). Thus, @fromList = fromListWith const@.
fromListWith :: (a -> a -> a) -> [(ByteString,a)] -> Trie a
{-# INLINE fromListWith #-}
fromListWith f = foldr step empty
    where
    step (k,v) = alter (Just . maybe v (f v)) k


-- | A variant of 'fromListWith' which applies the combining
-- function strictly. This function is a good consumer for list
-- fusion. If you need list fusion and are running into stack
-- overflow problems with 'fromListWith', then this function may
-- solve the problem.
fromListWith' :: (a -> a -> a) -> [(ByteString,a)] -> Trie a
{-# INLINE fromListWith' #-}
fromListWith' f = foldr step empty
    where
    step (k,v) = alter (\m -> Just $! maybe v (f v) m) k


-- | A left-fold variant of 'fromListWith'. Note that the arguments
-- to the combining function are swapped: the first is the value
-- in the trie which has been accumulated from the initial part of
-- the list; the second argument is the ``new'' value from the
-- remaining tail of the list (just like the first argument to
-- 'foldl'). Thus, @fromListL = fromListWithL const@.
fromListWithL :: (a -> a -> a) -> [(ByteString,a)] -> Trie a
{-# INLINE fromListWithL #-}
fromListWithL f = foldl' step empty
    where
    step t (k,v) = alter (Just . maybe v (\w -> f w v)) k t


-- | A variant of 'fromListWithL' which applies the combining
-- function strictly.
fromListWithL' :: (a -> a -> a) -> [(ByteString,a)] -> Trie a
{-# INLINE fromListWithL' #-}
fromListWithL' f = foldl' step empty
    where
    step t (k,v) = alter (\m -> Just $! maybe v (\w -> f w v) m) k t


----------------------------------------------------------------
-- | Lookup a key, returning a default value if it's not found.
lookupWithDefault :: a -> ByteString -> Trie a -> a
lookupWithDefault def =
    lookupBy_ const (const def)


----------------------------------------------------------------
-- | Insert a new key, retaining old value on conflict.
insertIfAbsent :: ByteString -> a -> Trie a -> Trie a
insertIfAbsent k v = alter (Just . maybe v id) k

-- | Insert a new key, with a function to resolve conflicts.
insertWith :: (a -> a -> a) -> ByteString -> a -> Trie a -> Trie a
insertWith f k v = alter (Just . maybe v (\w -> f w v)) k

-- | A variant of 'insertWith' which applies the combining function
-- strictly.
insertWith' :: (a -> a -> a) -> ByteString -> a -> Trie a -> Trie a
insertWith' f k v = alter (\m -> Just $! maybe v (\w -> f w v) m) k

-- | A variant of 'insertWith' which also provides the key to the
-- combining function.
insertWithKey
    :: (ByteString -> a -> a -> a) -> ByteString -> a -> Trie a -> Trie a
insertWithKey f k v = alter (Just . maybe v (\w -> f k w v)) k

-- | A variant of 'insertWithKey' which applies the combining
-- function strictly.
insertWithKey'
    :: (ByteString -> a -> a -> a) -> ByteString -> a -> Trie a -> Trie a
insertWithKey' f k v = alter (\m -> Just $! maybe v (\w -> f k w v) m) k

{- This is a tricky one...
insertLookupWithKey :: (ByteString -> a -> a -> a) -> ByteString -> a -> Trie a -> (Maybe a, Trie a)
-}

----------------------------------------------------------------
-- | Apply a function to change the value at a key.
adjustWithKey :: (ByteString -> a -> a) -> ByteString -> Trie a -> Trie a
adjustWithKey f q = adjust (f q) q

-- | Apply a function to the value at a key, possibly removing it.
update :: (a -> Maybe a) -> ByteString -> Trie a -> Trie a
update f = alter (maybe Nothing f)

-- | A variant of 'update' which also provides the key to the function.
updateWithKey
    :: (ByteString -> a -> Maybe a) -> ByteString -> Trie a -> Trie a
updateWithKey f q = alter (maybe Nothing (f q)) q

{-
updateLookupWithKey :: (ByteString -> a -> Maybe a) -> ByteString -> Trie a -> (Maybe a, Trie a)
-- Also tricky
-}

----------------------------------------------------------------

-- | Combine two tries, a la symmetric difference. If they define
-- the same key, it is removed; otherwise it is retained with the
-- value it has in whichever trie.
disunion :: Trie a -> Trie a -> Trie a
disunion = mergeBy (\_ _ -> Nothing)

-- | Combine two tries, using a function to resolve conflicts.
unionWith :: (a -> a -> a) -> Trie a -> Trie a -> Trie a
unionWith f = mergeBy (\x y -> Just (f x y))

-- | A variant of 'unionWith' which applies the combining function
-- strictly.
unionWith' :: (a -> a -> a) -> Trie a -> Trie a -> Trie a
unionWith' f = mergeBy (\x y -> Just $! f x y)

{- TODO: (efficiently)
difference, intersection
-}

----------------------------------------------------------------
----------------------------------------------------------- fin.