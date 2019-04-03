{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
------------------------------------------------------------
--                                              ~ 2019.04.03
-- |
-- Module      :  Data.Trie.TextInternal
-- Copyright   :  Copyright (c) 2008--2015 wren gayle romano, 2019 michael j. klein
-- License     :  BSD3
-- Maintainer  :  lambdamichael@gmail.com
-- Stability   :  experimental
--
-- Helper functions on 'ByteString's for "Data.Trie.Internal".
------------------------------------------------------------

module Data.Trie.TextInternal
    ( Text, TextElem
    , breakMaximalPrefix
    ) where

import Data.Text (Text)
import qualified Data.Text as T

import Data.Word

------------------------------------------------------------
-- | Associated type of 'Text'
type TextElem = Word16

-- | `T.commonPrefixes` unless `Nothing`,
-- in which case @(`T.empty`, x, y)@ is returned
breakMaximalPrefix
    :: Text
    -> Text
    -> (Text, Text, Text)
breakMaximalPrefix x y =
  maybe (T.empty, x, y) id $
    T.commonPrefixes x y

------------------------------------------------------------
------------------------------------------------------- fin.
