{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE DeriveGeneric      #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Strict.Either
-- Copyright   :  (c) 2006-2007 Roman Leshchinskiy
--                (c) 2013 Simon Meier
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Simon Meier <iridcode@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- The strict variant of the standard Haskell 'L.Either' type and the
-- corresponding variants of the functions from "Data.Either".
--
-- Note that the strict 'Either' type is not an applicative functor, and
-- therefore also no monad. The reasons are the same as the ones for the
-- strict @Maybe@ type, which are explained in "Data.Maybe.Strict".
--
-----------------------------------------------------------------------------

module Data.Strict.Either (
    Either(..)
  , either
  , isLeft, isRight
  , fromLeft, fromRight
  , lefts, rights
  , partitionEithers
) where

import           Prelude             hiding (Either (..), either)
import qualified Prelude             as L

import           Control.DeepSeq     (NFData (..))
import           Data.Bifoldable     (Bifoldable (..))
import           Data.Bifunctor      (Bifunctor (..))
import           Data.Binary         (Binary (..))
import           Data.Bitraversable  (Bitraversable (..))
#if MIN_VERSION_base(4,7,0)
import           Data.Data           (Data (..), Typeable)
#else
import           Data.Data           (Data (..), Typeable2 (..))
#endif
#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative (pure, (<$>))
import           Data.Foldable       (Foldable (..))
import           Data.Traversable    (Traversable (..))
import           Data.Monoid         (Monoid (..))
#endif
#if __GLASGOW_HASKELL__ >= 706
import           GHC.Generics        (Generic (..))
#endif
import           Data.Hashable       (Hashable(..))


-- | The strict choice type.
data Either a b = Left !a | Right !b deriving(Eq, Ord, Read, Show)

toStrict :: L.Either a b -> Either a b
toStrict (L.Left x)  = Left x
toStrict (L.Right y) = Right y

toLazy :: Either a b -> L.Either a b
toLazy (Left x)  = L.Left x
toLazy (Right y) = L.Right y

-- | Case analysis: if the value is @'Left' a@, apply the first function to @a@;
-- if it is @'Right' b@, apply the second function to @b@.
either :: (a -> c) -> (b -> c) -> Either a b -> c
either f _ (Left  x) = f x
either _ g (Right y) = g y

-- | Yields 'True' iff the argument is of the form @Left _@.
--
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

-- | Yields 'True' iff the argument is of the form @Right _@.
--
isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False

-- | Extracts the element out of a 'Left' and throws an error if the argument
-- is a 'Right'.
fromLeft :: Either a b -> a
fromLeft (Left x) = x
fromLeft _        = error "Data.Strict.Either.fromLeft: Right"

-- | Extracts the element out of a 'Right' and throws an error if the argument
-- is a 'Left'.
fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight _         = error "Data.Strict.Either.fromRight: Left"

-- | Analogous to 'L.lefts' in "Data.Either".
lefts   :: [Either a b] -> [a]
lefts x = [a | Left a <- x]

-- | Analogous to 'L.rights' in "Data.Either".
rights   :: [Either a b] -> [b]
rights x = [a | Right a <- x]

-- | Analogous to 'L.partitionEithers' in "Data.Either".
partitionEithers :: [Either a b] -> ([a],[b])
partitionEithers =
    Prelude.foldr (either left right) ([],[])
  where
    left  a ~(l, r) = (a:l, r)
    right a ~(l, r) = (l, a:r)

-- Instances
------------

#if __GLASGOW_HASKELL__ >= 608
deriving instance (Data a, Data b) => Data (Either a b)
#endif
#if MIN_VERSION_base(4,7,0)
deriving instance Typeable Either
#else
deriving instance Typeable2 Either
#endif

#if __GLASGOW_HASKELL__ >= 706
deriving instance Generic  (Either a b)
#endif

instance Functor (Either a) where
  fmap _ (Left  x) = Left x
  fmap f (Right y) = Right (f y)

instance Foldable (Either e) where
  foldr _ y (Left _)  = y
  foldr f y (Right x) = f x y

  foldl _ y (Left _)  = y
  foldl f y (Right x) = f y x

instance Traversable (Either e) where
  traverse _ (Left x)  = pure (Left x)
  traverse f (Right x) = Right <$> f x

-- deepseq
instance (NFData a, NFData b) => NFData (Either a b) where
  rnf = rnf . toLazy

-- binary
instance (Binary a, Binary b) => Binary (Either a b) where
  put = put . toLazy
  get = toStrict <$> get

-- bifunctors
instance Bifunctor Either where
  bimap f _ (Left a) = Left (f a)
  bimap _ g (Right a) = Right (g a)
  first f = either (Left . f) Right
  second g = either Left (Right . g)

instance Bifoldable Either where
  bifold (Left a) = a
  bifold (Right b) = b
  bifoldMap = either
  bifoldr f _ c (Left a) = f a c
  bifoldr _ g c (Right b) = g b c
  bifoldl f _ c (Left a) = f c a
  bifoldl _ g c (Right b) = g c b

instance Bitraversable Either where
  bitraverse f _ (Left a) = fmap Left (f a)
  bitraverse _ g (Right b) = fmap Right (g b)

-- hashable
instance (Hashable a, Hashable b) => Hashable (Either a b) where
  hashWithSalt salt = hashWithSalt salt . toLazy
