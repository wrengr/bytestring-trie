{-# OPTIONS_GHC -Wall -fwarn-tabs #-}

----------------------------------------------------------------
--                                                  ~ 2009.01.20
-- |
-- Module      :  Data.Trie.Convenience
-- Copyright   :  Copyright (c) 2008--2011 wren ng thornton
-- License     :  BSD3
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Additional convenience functions. In order to keep "Data.Trie"
-- concise, non-essential and uncommonly used functions have been
-- moved here. Most of these functions simplify the generic functions
-- from "Data.Trie", following after the interface for "Data.Map"
-- and "Data.IntMap".
----------------------------------------------------------------

module Data.Trie.Convenience
    (
    -- * Conversion functions
    -- $fromList
      fromListL, fromListR, fromListS, fromListWith
    
    -- * 'lookupBy' variants
    , lookupWithDefault
    
    -- * 'alterBy' variants
    , insertIfAbsent, insertWith, insertWithKey
    , adjustWithKey
    , update, updateWithKey
    
    -- * 'mergeBy' variants
    , disunion, unionWith
    ) where

import Data.Trie
import Data.Trie.Internal (lookupBy_)
import Data.ByteString    (ByteString)
import Data.List          (foldl', sortBy)
import Data.Ord           (comparing)
import Control.Monad      (liftM)

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

-- | This version is just an alias for 'fromList'. It is a good
-- consumer for list fusion. Worst-case behavior is somewhat worse
-- than worst-case for 'fromListL'.
fromListR :: [(ByteString,a)] -> Trie a
{-# INLINE fromListR #-}
fromListR = fromList

-- TODO: compare performance against a fromListL definition, adjusting the sort
--
-- | This version sorts the list before folding over it. This adds
-- /O(n log n)/ overhead and requires the whole list be in memory
-- at once, but it ensures that the list is in best-case order. The
-- benefits generally outweigh the costs.
fromListS :: [(ByteString,a)] -> Trie a
{-# INLINE fromListS #-}
fromListS = fromListR . sortBy (comparing fst)

-- | A variant of 'fromListR' that takes a function for combining
-- values on conflict.
fromListWith :: (a -> a -> a) -> [(ByteString,a)] -> Trie a
{-# INLINE fromListWith #-}
fromListWith f = foldr (uncurry $ alterBy g) empty
    where
    g _ v Nothing  = Just v
    g _ v (Just w) = Just (f v w)

----------------------------------------------------------------
-- | Lookup a key, returning a default value if it's not found.
lookupWithDefault :: a -> ByteString -> Trie a -> a
lookupWithDefault x = lookupBy_ (\mv _ -> case mv of
                                          Nothing -> x
                                          Just v  -> v) x (const x)

----------------------------------------------------------------

-- | Insert a new key, retaining old value on conflict.
insertIfAbsent :: ByteString -> a -> Trie a -> Trie a
insertIfAbsent = alterBy $ \_ x mv -> case mv of
                                      Nothing -> Just x
                                      Just _  -> mv

-- | Insert a new key, with a function to resolve conflicts.
insertWith :: (a -> a -> a) -> ByteString -> a -> Trie a -> Trie a
insertWith f = alterBy $ \_ x mv -> case mv of
                                    Nothing -> Just x
                                    Just v  -> Just (f x v)

insertWithKey :: (ByteString -> a -> a -> a) -> ByteString -> a -> Trie a -> Trie a
insertWithKey f = alterBy $ \k x mv -> case mv of
                                    Nothing -> Just x
                                    Just v  -> Just (f k x v)

{- This is a tricky one...
insertLookupWithKey :: (ByteString -> a -> a -> a) -> ByteString -> a -> Trie a -> (Maybe a, Trie a)
-}

-- | Apply a function to change the value at a key.
adjustWithKey  :: (ByteString -> a -> a) -> ByteString -> Trie a -> Trie a
adjustWithKey f q = alterBy (\k _ -> liftM (f k)) q undefined

-- | Apply a function to the value at a key, possibly removing it.
update :: (a -> Maybe a) -> ByteString -> Trie a -> Trie a
update        f q = alterBy (\_ _ mx -> mx >>= f) q undefined

updateWithKey :: (ByteString -> a -> Maybe a) -> ByteString -> Trie a -> Trie a
updateWithKey f q = alterBy (\k _ mx -> mx >>= f k) q undefined

{-
updateLookupWithKey :: (ByteString -> a -> Maybe a) -> ByteString -> Trie a -> (Maybe a, Trie a)
-- Also tricky
-}

----------------------------------------------------------------

-- | Combine two tries. If they define the same key, it is removed.
disunion :: Trie a -> Trie a -> Trie a
disunion = mergeBy (\_ _ -> Nothing)

-- | Combine two tries, using a function to resolve conflicts.
unionWith :: (a -> a -> a) -> Trie a -> Trie a -> Trie a
unionWith f = mergeBy (\x y -> Just (f x y))

{- TODO: (efficiently)
difference, intersection
-}

----------------------------------------------------------------
----------------------------------------------------------- fin.
