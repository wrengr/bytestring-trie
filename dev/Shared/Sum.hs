{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
{-# LANGUAGE CPP #-}

----------------------------------------------------------------
--                                                  ~ 2021.12.13
-- |
-- Module      :  Shared.Sum
-- Copyright   :  2008--2021 wren gayle romano
-- License     :  BSD3
-- Maintainer  :  wren@cpan.org
-- Stability   :  provisional
-- Portability :  portable (CPP)
--
-- Define the 'Sum' newtype when @base < 4.9@.
----------------------------------------------------------------
module Shared.Sum (Sum(..)) where

-- N.B., for base>=4.13/GHC>=8.8.1, this module needs to explicitly
-- import 'Sum' in order to re-export it.  However, users of this
-- module will get a warning about unused imports for those versions,
-- even though the Prelude doesn't actually re-export 'Sata.Semigroup.Sum'.
-- TODO: figure out what the heck is going on there.
#if MIN_VERSION_base(4,9,0)
-- [aka GHC 8.0.1]: "Data.Semigroup" added to base.
import Data.Semigroup (Sum(..))
#elif MIN_VERSION_base(4,7,0)
-- [aka GHC 7.8.1]: "Data.Coerce" added to base.
import Data.Coerce (coerce)
#endif

----------------------------------------------------------------

#if !(MIN_VERSION_base(4,9,0))
data Sum a = Sum { getSum :: a }
    deriving (Eq, Ord, Read, Show, Bounded, Num)

instance Num a => Monoid (Sum a) where
    mempty = Sum 0
#   if MIN_VERSION_base(4,7,0)
    mappend = coerce (+)
#   else
    mappend (Sum x) (Sum y) = Sum (x + y)
#   endif
#endif

----------------------------------------------------------------
----------------------------------------------------------- fin.
