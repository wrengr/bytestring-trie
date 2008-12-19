{-# OPTIONS_GHC -Wall -fwarn-tabs #-}

----------------------------------------------------------------
--                                                  ~ 2008.12.19
-- |
-- Module      :  Data.Trie.Convenience
-- Copyright   :  Copyright (c) 2008--2009 wren ng thornton
-- License     :  BSD3
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Additional convenience versions of the generic functions.
----------------------------------------------------------------

module Data.Trie.Convenience
    (
    -- * 'lookupBy' variants
      lookupWithDefault
    
    -- * 'alterBy' variants
    , insertIfAbsent, insertWith, insertWithKey
    , adjustWithKey
    , update, updateWithKey
    
    -- * 'mergeBy' variants
    , disunion, unionWith
    ) where

import Data.Trie
import Control.Monad (liftM)

----------------------------------------------------------------
----------------------------------------------------------------

-- | Lookup a key, returning a default value if it's not found.
lookupWithDefault :: a -> KeyString -> Trie a -> a
lookupWithDefault x = lookupBy_ (\mv _ -> case mv of
                                          Nothing -> x
                                          Just v  -> v) x (const x)

----------------------------------------------------------------

-- | Insert a new key, retaining old value on conflict.
insertIfAbsent :: KeyString -> a -> Trie a -> Trie a
insertIfAbsent = alterBy $ \_ x mv -> case mv of
                                      Nothing -> Just x
                                      Just _  -> mv

-- | Insert a new key, with a function to resolve conflicts.
insertWith :: (a -> a -> a) -> KeyString -> a -> Trie a -> Trie a
insertWith f = alterBy $ \_ x mv -> case mv of
                                    Nothing -> Just x
                                    Just v  -> Just (f x v)

insertWithKey :: (KeyString -> a -> a -> a) -> KeyString -> a -> Trie a -> Trie a
insertWithKey f = alterBy $ \k x mv -> case mv of
                                    Nothing -> Just x
                                    Just v  -> Just (f k x v)

{- This is a tricky one...
insertLookupWithKey :: (KeyString -> a -> a -> a) -> KeyString -> a -> Trie a -> (Maybe a, Trie a)
-}

-- | Apply a function to change the value at a key.
adjustWithKey  :: (KeyString -> a -> a) -> KeyString -> Trie a -> Trie a
adjustWithKey f q = alterBy (\k _ -> liftM (f k)) q undefined

-- | Apply a function to the value at a key, possibly removing it.
update :: (a -> Maybe a) -> KeyString -> Trie a -> Trie a
update        f q = alterBy (\_ _ mx -> mx >>= f) q undefined

updateWithKey :: (KeyString -> a -> Maybe a) -> KeyString -> Trie a -> Trie a
updateWithKey f q = alterBy (\k _ mx -> mx >>= f k) q undefined

{-
updateLookupWithKey :: (Key -> a -> Maybe a) -> Key -> ByteStringTrie a -> (Maybe a, ByteStringTrie a)
-- Also tricky
-}

----------------------------------------------------------------

-- | Combine two tries. If they define the same key, it is removed.
disunion :: Trie a -> Trie a -> Trie a
disunion = mergeBy (\_ _ -> Nothing)

-- | Combine two tries, using a function to resolve conflicts
unionWith :: (a -> a -> a) -> Trie a -> Trie a -> Trie a
unionWith f = mergeBy (\x y -> Just (f x y))

{- TODO:
difference, intersection
-}

----------------------------------------------------------------
----------------------------------------------------------- fin.