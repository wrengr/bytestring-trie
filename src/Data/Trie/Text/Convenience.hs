{-# OPTIONS_GHC -Wall -fwarn-tabs #-}

----------------------------------------------------------------
--                                                  ~ 2011.02.12
-- |
-- Module      :  Data.Trie.Convenience
-- Copyright   :  Copyright (c) 2008--2015 wren gayle romano
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

module Data.Trie.Text.Convenience
    (
    -- * Conversion functions ('fromList' variants)
    -- $fromList
      fromListLText, fromListRText, fromListSText
    , fromListWithText,  fromListWithText'
    , fromListWithLText, fromListWithLText'

    -- * Query functions ('lookupBy' variants)
    , lookupWithDefaultText

    -- * Inserting values ('alterBy' variants)
    , insertIfAbsentText
    , insertWithText,    insertWithText'
    , insertWithKeyText, insertWithKeyText'

    -- * Updating and adjusting values ('alterBy' and 'adjustBy' variants)
    , adjustWithKeyText
    , updateText, updateWithKeyText

    -- * Combining tries ('mergeBy' variants)
    , disunionText
    , unionWithText, unionWithText'
    ) where

import Data.Trie.Text
import Data.Trie.Text.Internal (lookupByText_, adjustByText)
import Data.Trie.Errors   (impossible)
import Data.Text          (Text)
import Data.List          (foldl', sortBy)
import Data.Ord           (comparing)

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
fromListLText :: [(Text,a)] -> TrieText a
{-# INLINE fromListLText #-}
fromListLText = foldl' (flip . uncurry $ insertIfAbsentText) emptyText

-- | An explicitly right-fold variant of 'fromList'. It is a good
-- consumer for list fusion. Worst-case behavior is somewhat worse
-- than worst-case for 'fromListL'. The 'fromList' function is
-- currently just an alias for 'fromListR'.
fromListRText :: [(Text,a)] -> TrieText a
{-# INLINE fromListRText #-}
fromListRText = fromListText -- == foldr (uncurry insert) empty


-- TODO: compare performance against a fromListL variant, adjusting the sort appropriately
--
-- | This variant sorts the list before folding over it. This adds
-- /O(n log n)/ overhead and requires the whole list be in memory
-- at once, but it ensures that the list is in best-case order. The
-- benefits generally outweigh the costs.
fromListSText :: [(Text,a)] -> TrieText a
{-# INLINE fromListSText #-}
fromListSText = fromListRText . sortBy (comparing fst)

-- | A variant of 'fromListR' that takes a function for combining
-- values on conflict. The first argument to the combining function
-- is the ``new'' value from the initial portion of the list; the
-- second argument is the value that has been accumulated into the
-- trie from the tail of the list (just like the first argument to
-- 'foldr'). Thus, @fromList = fromListWith const@.
fromListWithText :: (a -> a -> a) -> [(Text,a)] -> TrieText a
{-# INLINE fromListWithText #-}
fromListWithText f = foldr (uncurry $ alterByText g) emptyText
    where
    g _ v Nothing  = Just v
    g _ v (Just w) = Just (f v w)

-- | A variant of 'fromListWith' which applies the combining
-- function strictly. This function is a good consumer for list
-- fusion. If you need list fusion and are running into stack
-- overflow problems with 'fromListWith', then this function may
-- solve the problem.
fromListWithText' :: (a -> a -> a) -> [(Text,a)] -> TrieText a
{-# INLINE fromListWithText' #-}
fromListWithText' f = foldr (uncurry $ alterByText g') emptyText
    where
    g' _ v Nothing  = Just v
    g' _ v (Just w) = Just $! f v w

-- | A left-fold variant of 'fromListWith'. Note that the arguments
-- to the combining function are swapped: the first is the value
-- in the trie which has been accumulated from the initial part of
-- the list; the second argument is the ``new'' value from the
-- remaining tail of the list (just like the first argument to
-- 'foldl'). Thus, @fromListL = fromListWithL const@.
fromListWithLText :: (a -> a -> a) -> [(Text,a)] -> TrieText a
{-# INLINE fromListWithLText #-}
fromListWithLText f = foldl' (flip . uncurry $ alterByText flipG) emptyText
    where
    flipG _ v Nothing  = Just v
    flipG _ v (Just w) = Just (f w v)


-- | A variant of 'fromListWithL' which applies the combining
-- function strictly.
fromListWithLText' :: (a -> a -> a) -> [(Text,a)] -> TrieText a
{-# INLINE fromListWithLText' #-}
fromListWithLText' f = foldl' (flip . uncurry $ alterByText flipG') emptyText
    where
    flipG' _ v Nothing  = Just v
    flipG' _ v (Just w) = Just $! f w v


----------------------------------------------------------------
-- | Lookup a key, returning a default value if it's not found.
lookupWithDefaultText :: a -> Text -> TrieText a -> a
lookupWithDefaultText def = lookupByText_ f def (const def)
    where
    f Nothing  _ = def
    f (Just v) _ = v


----------------------------------------------------------------

-- | Insert a new key, retaining old value on conflict.
insertIfAbsentText :: Text -> a -> TrieText a -> TrieText a
insertIfAbsentText =
    alterByText $ \_ x mv ->
        case mv of
        Nothing -> Just x
        Just _  -> mv


-- | Insert a new key, with a function to resolve conflicts.
insertWithText :: (a -> a -> a) -> Text -> a -> TrieText a -> TrieText a
insertWithText f =
    alterByText $ \_ x mv ->
        case mv of
        Nothing -> Just x
        Just v  -> Just (f x v)


-- | A variant of 'insertWith' which applies the combining function
-- strictly.
insertWithText' :: (a -> a -> a) -> Text -> a -> TrieText a -> TrieText a
insertWithText' f =
    alterByText $ \_ x mv ->
        case mv of
        Nothing -> Just x
        Just v  -> Just $! f x v


-- | A variant of 'insertWith' which also provides the key to the
-- combining function.
insertWithKeyText :: (Text -> a -> a -> a) -> Text -> a -> TrieText a -> TrieText a
insertWithKeyText f =
    alterByText $ \k x mv ->
        case mv of
        Nothing -> Just x
        Just v  -> Just (f k x v)


-- | A variant of 'insertWithKey' which applies the combining
-- function strictly.
insertWithKeyText' :: (Text -> a -> a -> a) -> Text -> a -> TrieText a -> TrieText a
insertWithKeyText' f =
    alterByText $ \k x mv ->
        case mv of
        Nothing -> Just x
        Just v  -> Just $! f k x v


{- This is a tricky one...
insertLookupWithKey :: (ByteString -> a -> a -> a) -> ByteString -> a -> Trie a -> (Maybe a, Trie a)
-}

----------------------------------------------------------------
-- | Apply a function to change the value at a key.
adjustWithKeyText :: (Text -> a -> a) -> Text -> TrieText a -> TrieText a
adjustWithKeyText f q =
    adjustByText (\k _ -> f k) q (impossible "Convenience.adjustWithKeyText")
-- TODO: benchmark vs the definition with alterBy/liftM


-- | Apply a function to the value at a key, possibly removing it.
updateText :: (a -> Maybe a) -> Text -> TrieText a -> TrieText a
updateText f q =
    alterByText (\_ _ mx -> mx >>= f) q (impossible "Convenience.updateText")


-- | A variant of 'update' which also provides the key to the function.
updateWithKeyText :: (Text -> a -> Maybe a) -> Text -> TrieText a -> TrieText a
updateWithKeyText f q =
    alterByText (\k _ mx -> mx >>= f k) q (impossible "Convenience.updateWithKeyText")


{-
updateLookupWithKey :: (ByteString -> a -> Maybe a) -> ByteString -> Trie a -> (Maybe a, Trie a)
-- Also tricky
-}

----------------------------------------------------------------

-- | Combine two tries, a la symmetric difference. If they define
-- the same key, it is removed; otherwise it is retained with the
-- value it has in whichever trie.
disunionText :: TrieText a -> TrieText a -> TrieText a
disunionText = mergeByText (\_ _ -> Nothing)


-- | Combine two tries, using a function to resolve conflicts.
unionWithText :: (a -> a -> a) -> TrieText a -> TrieText a -> TrieText a
unionWithText f = mergeByText (\x y -> Just (f x y))


-- | A variant of 'unionWith' which applies the combining function
-- strictly.
unionWithText' :: (a -> a -> a) -> TrieText a -> TrieText a -> TrieText a
unionWithText' f = mergeByText (\x y -> Just $! f x y)


{- TODO: (efficiently)
difference, intersection
-}

----------------------------------------------------------------
----------------------------------------------------------- fin.

