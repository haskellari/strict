#if __GLASGOW_HASKELL__ >= 608
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
#endif
#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE DeriveGeneric      #-}
#endif
#ifndef __HADDOCK__
#ifdef __GLASGOW_HASKELL__
{-# LANGUAGE TypeOperators      #-}
#endif
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Strict.Tuple
-- Copyright   :  (c) 2006-2007 Roman Leshchinskiy
--                (c) 2013 Simon Meier
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Simon Meier <iridcode@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- The strict variant of the standard Haskell pairs and the corresponding
-- variants of the functions from "Data.Tuple".
--
-- Note that unlike regular Haskell pairs, @(x :*: _|_) = (_|_ :*: y) = _|_@
--
-----------------------------------------------------------------------------

module Data.Strict.Tuple (
    Pair(..)
  , toStrict, toLazy
#ifndef __HADDOCK__
#ifdef __GLASGOW_HASKELL__
  , (:!:)
#endif
#endif
  , fst
  , snd
  , curry
  , uncurry
  , swap
  , zip
  , unzip
) where

import           Prelude             hiding (curry, fst, snd, uncurry, unzip,
                                      zip)

import           Control.DeepSeq     (NFData (..))
import           Data.Bifoldable     (Bifoldable (..))
import           Data.Bifunctor      (Bifunctor (..))
import           Data.Bitraversable  (Bitraversable (..))
import           Data.Binary         (Binary (..))
#if MIN_VERSION_base(4,7,0)
import           Data.Data           (Data (..), Typeable)
#else
import           Data.Data           (Data (..), Typeable2 (..))
#endif
import           Data.Ix             (Ix (..))
#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative (Applicative ((<*>)), (<$>))
import           Data.Foldable       (Foldable (..))
import           Data.Traversable    (Traversable (..))
import           Data.Monoid         (Monoid (..))
#endif
#if __GLASGOW_HASKELL__ >= 706
import           GHC.Generics        (Generic (..))
#endif
import           Data.Hashable       (Hashable(..))
import           Data.Semigroup      (Semigroup (..))

#if __HADDOCK__
import Data.Tuple ()
#endif

infixl 2 :!:

-- | The type of strict pairs.
data Pair a b = !a :!: !b deriving (Eq, Ord, Show, Read, Bounded, Ix)

#ifndef __HADDOCK__
#ifdef __GLASGOW_HASKELL__
-- This gives a nicer syntax for the type but only works in GHC for now.
type (:!:) = Pair
#endif
#endif

toStrict :: (a, b) -> Pair a b
toStrict (a, b) = a :!: b

toLazy :: Pair a b -> (a, b)
toLazy (a :!: b) = (a, b)

-- | Extract the first component of a strict pair.
fst :: Pair a b -> a
fst (x :!: _) = x

-- | Extract the second component of a strict pair.
snd :: Pair a b -> b
snd (_ :!: y) = y

-- | Curry a function on strict pairs.
curry :: (Pair a b -> c) -> a -> b -> c
curry f x y = f (x :!: y)

-- | Convert a curried function to a function on strict pairs.
uncurry :: (a -> b -> c) -> Pair a b -> c
uncurry f (x :!: y) = f x y

-- | Analagous to 'L.swap' from "Data.Tuple"
swap :: Pair a b -> Pair b a
swap (a :!: b) = b :!: a

-- | Zip for strict pairs (defined with zipWith).
zip :: [a] -> [b] -> [Pair a b]
zip x y = zipWith (:!:) x y

-- | Unzip for stict pairs into a (lazy) pair of lists.
unzip :: [Pair a b] -> ([a], [b])
unzip x = ( map fst x
          , map snd x
          )

-- Instances
------------

#if __GLASGOW_HASKELL__ >= 608
deriving instance (Data a, Data b) => Data (Pair a b)
#endif
#if MIN_VERSION_base(4,7,0)
deriving instance Typeable Pair
#else
deriving instance Typeable2 Pair
#endif

-- fails with compiler panic on GHC 7.4.2
#if __GLASGOW_HASKELL__ >= 706
deriving instance Generic  (Pair a b)
#endif

instance Functor (Pair e) where
    fmap f = toStrict . fmap f . toLazy

instance Foldable (Pair e) where
  foldMap f (_ :!: x) = f x

instance Traversable (Pair e) where
  traverse f (e :!: x) = (:!:) e <$> f x

instance (Semigroup a, Semigroup b) => Semigroup (Pair a b) where
  (x1 :!: y1) <> (x2 :!: y2) = (x1 <> x2) :!: (y1 <> y2)

instance (Monoid a, Monoid b) => Monoid (Pair a b) where
  mempty                            = mempty :!: mempty
  (x1 :!: y1) `mappend` (x2 :!: y2) = (x1 `mappend` x2) :!: (y1 `mappend` y2)

-- deepseq
instance (NFData a, NFData b) => NFData (Pair a b) where
  rnf = rnf . toLazy

-- binary
instance (Binary a, Binary b) => Binary (Pair a b) where
  put = put . toLazy
  get = toStrict <$> get

-- bifunctors
instance Bifunctor Pair where
  bimap f g (a :!: b) = f a :!: g b
  first f (a :!: b) = f a :!: b
  second g (a :!: b) = a :!: g b

instance Bifoldable Pair where
  bifold (a :!: b) = a `mappend` b
  bifoldMap f g (a :!: b) = f a `mappend` g b
  bifoldr f g c (a :!: b) = g b (f a c)
  bifoldl f g c (a :!: b) = g (f c a) b

instance Bitraversable Pair where
  bitraverse f g (a :!: b) = (:!:) <$> f a <*> g b
#if !MIN_VERSION_bifunctors(5,1,0)
  bisequenceA (a :!: b) = (:!:) <$> a <*> b
#endif

-- hashable
instance (Hashable a, Hashable b) => Hashable (Pair a b) where
  hashWithSalt salt = hashWithSalt salt . toLazy
