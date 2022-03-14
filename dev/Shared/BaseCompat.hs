{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
{-# LANGUAGE CPP #-}

----------------------------------------------------------------
--                                                  ~ 2022.03.12
-- |
-- Module      :  Shared.BaseCompat
-- Copyright   :  2008--2022 wren romano
-- License     :  BSD-3-Clause
-- Maintainer  :  wren@cpan.org
-- Stability   :  provisional
-- Portability :  portable (CPP)
--
-- Compatibility shim for various versions of @base@.
--
-- TODO: depending how much more we need in here, consider instead
-- just using the @base-compat-batteries@ package instead.
----------------------------------------------------------------
module Shared.BaseCompat
    (
    -- * "Data.Semigroup"
      (<>)
    , Sum(..)
    , Product(..)
    -- TODO: (?) 'Min', 'Max', 'Endo', 'Data.Monoid.Dual'
    -- * "Data.Functor"
    , (<&>)
    -- * "Data.Functor.Compose"
    , Compose(..)
    -- * "Data.Functor.Identity"
    , Identity(..)
    ) where

----------------------------------------------------------------
#if MIN_VERSION_base(4,13,0)
-- [GHC 8.8.1]: @(<&>)@ added to "Data.Functor".
import Data.Functor ((<&>))
#endif

----------------------------------------------------------------
#if MIN_VERSION_base(4,13,0)
-- [GHC 8.8.1]: Prelude re-exports 'Semigroup' and @('<>')@, but not 'stimes'.
#elif MIN_VERSION_base(4,9,0)
-- [GHC 8.0.1]: "Data.Semigroup" added to base.
import Data.Semigroup   (Semigroup((<>)))
#elif MIN_VERSION_base(4,5,0)
-- [GHC 7.4.1]: @(<>)@ added to "Data.Monoid".
import Data.Monoid      ((<>))
#endif

----------------------------------------------------------------
-- For [base 4.13 / GHC 8.8.1], this module needs to explicitly
-- import 'Sum' in order to re-export it.  However, for some reason
-- those versions will also cause users of this module to get an
-- \"unused import\" warning, even though the Prelude doesn't export
-- 'Data.Semigroup.Sum'.
-- TODO: figure out what the heck is going on re that warning...
-- FIXME: now getting that issue for GHC 8.0--8.6 too...
#if MIN_VERSION_base(4,9,0)
-- [GHC 8.0.1]
import Data.Semigroup       (Sum(..), Product(..))
import Data.Functor.Compose (Compose(..))
#endif

#if MIN_VERSION_base(4,8,0)
-- [GHC 7.10.1]
import Data.Functor.Identity (Identity(..))
#endif

#if MIN_VERSION_base(4,7,0) && !(MIN_VERSION_base(4,9,0))
-- [GHC 7.8.1]: "Data.Coerce" added to base.
import Data.Coerce (coerce)
#endif

----------------------------------------------------------------
----------------------------------------------------------------
#if !(MIN_VERSION_base(4,13,0))
(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap
{-# INLINE (<&>) #-}
#endif

----------------------------------------------------------------
#if !(MIN_VERSION_base(4,9,0))
-- | Same as the derived instance, if the type was declared without
-- the record\/field syntax.
newtypeShowsPrec :: Show a => String -> (b -> a) -> Int -> b -> ShowS
newtypeShowsPrec name oldtype d x =
    showParen (d > 10)
        $ showString name
        . showChar ' '
        . showsPrec 11 (oldtype x)
-- aka:
-- > newtypeShowsPrec name oldtype =
-- >     Data.Functor.Classes.showsUnaryWith (\d -> showsPrec d . oldtype) name
-- But that function was also introduced in base-4.9.0.0
#endif

----------------------------------------------------------------
#if !(MIN_VERSION_base(4,9,0))
newtype Sum a = Sum { getSum :: a }
    deriving (Eq, Ord, Bounded, Num)
    -- We're not deriving the 'Show' instance, even though base does.
    -- Also forgoing the 'Read' instance.

instance (Show a) => Show (Sum a) where
    showsPrec = newtypeShowsPrec "Sum" getSum

instance Num a => Monoid (Sum a) where
    mempty = Sum 0
#   if MIN_VERSION_base(4,7,0)
    mappend = coerce ((+) :: a -> a -> a)
#   else
    mappend (Sum x) (Sum y) = Sum (x + y)
#   endif

-- TODO: if needed, there are also instances for: Functor, Applicative, Monad.
#endif

----------------------------------------------------------------
#if !(MIN_VERSION_base(4,9,0))
newtype Product a = Product { getProduct :: a }
    deriving (Eq, Ord, Bounded, Num)
    -- We're not deriving the 'Show' instances, even though base does.
    -- Also forgoing the 'Read' instance.

instance (Show a) => Show (Product a) where
    showsPrec = newtypeShowsPrec "Product" getProduct

instance Num a => Monoid (Product a) where
    mempty = Product 1
#   if MIN_VERSION_base(4,7,0)
    mappend = coerce ((*) :: a -> a -> a)
#   else
    mappend (Product x) (Product y) = Product (x * y)
#   endif

-- TODO: if needed, there are also instances for: Functor, Applicative, Monad.
#endif

----------------------------------------------------------------
#if !(MIN_VERSION_base(4,9,0))
newtype Compose f g a = Compose { getCompose :: f (g a) }
    -- Ignoring the other instances since we don't need them.

-- BUG: can't define 'Show' instance without 'Show1' for @f,g@; but
-- 'Show1' wasn't introduced until base-4.9.0.0. Ditto for 'Eq',
-- 'Ord'.

instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap f (Compose x) = Compose (fmap (fmap f) x)
#   if __GLASGOW_HASKELL__
    a <$ Compose x = Compose (fmap (a <$) x)
#   endif

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
    foldMap f (Compose t) = foldMap (foldMap f) t

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
    traverse f (Compose t) = Compose <$> traverse (traverse f) t

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    pure x = Compose (pure (pure x))
    Compose f <*> Compose x = Compose (liftA2 (<*>) f x)
#endif

----------------------------------------------------------------
#if !(MIN_VERSION_base(4,8,0))
newtype Identity a = Identity { runIdentity :: a }
    deriving (Eq, Ord, Bounded, Num)
    -- Ignoring the other instances since we don't need them.

instance (Show a) => Show (Identity a) where
    showsPrec = newtypeShowsPrec "Identity" runIdentity

instance Functor Identity where
#   if MIN_VERSION_base(4,7,0)
    fmap = coerce
#   else
    fmap f (Identity x) = Identity (f x)
#   endif

instance Applicative Identity where
    pure = Identity
#   if MIN_VERSION_base(4,7,0)
    (<*>) = coerce
#   else
    Identity f <*> Identity x = Identity (f x)
#   endif

-- Using 'runIdentity' here instead of pattern matching, because
-- that's what base-4.16.0.0 does.
instance Monad Identity where
    m >>= k = k (runIdentity m)

-- TODO: if needed, there's also a Foldable instance.
#endif

----------------------------------------------------------------
----------------------------------------------------------- fin.
