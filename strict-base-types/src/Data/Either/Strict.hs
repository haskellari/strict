{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-----------------------------------------------------------------------------
-- | Copyright :  (c) 2006-2007 Roman Leshchinskiy
--                (c) 2013 Simon Meier
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Simon Meier <iridcode@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC
--
-- The strict variant of the standard Haskell 'L.Either' type and the
-- corresponding variants of the functions from "Data.Either".
--
-- Note that the strict 'Either' type is not an applicative functor, and
-- therefore also no monad. The reasons are the same as the ones for the
-- strict @Maybe@ type, which are explained in "Data.Maybe.Strict".
--
-----------------------------------------------------------------------------
module Data.Either.Strict (
    Either(Left, Right)
  , isRight
  , isLeft
  , either
  , lefts
  , rights
  , partitionEithers
  , _Left
  , _Right
) where

import           Data.Strict.Either  (Either (Left, Right), either, isLeft,
                                      isRight)
import           Prelude             hiding (Either (..), either)
import qualified Prelude             as L

import           Control.DeepSeq     (NFData (..))
import           Control.Lens.Iso    (Strict (..), Swapped (..), iso)
import           Control.Lens.Prism  (Prism, prism)
import           Data.Aeson          (FromJSON (..), ToJSON (..))
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
import           Test.QuickCheck     (Arbitrary (..))
import           Data.Hashable       (Hashable(..))


-- Utilities
------------
toStrict :: L.Either a b -> Either a b
toStrict (L.Left x)  = Left x
toStrict (L.Right y) = Right y

toLazy :: Either a b -> L.Either a b
toLazy (Left x)  = L.Left x
toLazy (Right y) = L.Right y


-- missing instances
--------------------

deriving instance (Data a, Data b) => Data     (Either a b)
#if MIN_VERSION_base(4,7,0)
deriving instance Typeable Either
#else
deriving instance Typeable2 Either
#endif

#if __GLASGOW_HASKELL__ >= 706
deriving instance Generic  (Either a b)
#endif

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

-- aeson
instance (ToJSON a, ToJSON b) => ToJSON (Either a b) where
  toJSON = toJSON . toLazy

instance (FromJSON a, FromJSON b) => FromJSON (Either a b) where
  parseJSON val = toStrict <$> parseJSON val

-- quickcheck
instance (Arbitrary a, Arbitrary b) => Arbitrary (Either a b) where
  arbitrary = toStrict <$> arbitrary
  shrink    = map toStrict . shrink . toLazy

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
#if !MIN_VERSION_bifunctors(5,1,0)
  bisequenceA = either (fmap Left) (fmap Right)
#endif

-- lens
instance Strict (L.Either a b) (Either a b) where
  strict = iso toStrict toLazy

instance Swapped Either where
  swapped = either Right Left `iso` either Right Left

-- hashable
instance (Hashable a, Hashable b) => Hashable (Either a b) where
  hashWithSalt salt = hashWithSalt salt . toLazy

-- missing functions
--------------------

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

-- | Analogous to 'Control.Lens.Prism._Left' in "Control.Lens.Prism".
_Left :: Prism (Either a c) (Either b c) a b
_Left = prism Left $ either L.Right (L.Left . Right)

-- | Analogous to 'Control.Lens.Prism._Right' in "Control.Lens.Prism".
_Right :: Prism (Either c a) (Either c b) a b
_Right = prism Right $ either (L.Left . Left) L.Right

------------------------------------------------------------------------------
-- Code required to make this module independent of the 'strict' package
------------------------------------------------------------------------------

{-
-- | The strict choice type.
--
-- Note that this type is not an applicative functor, and therefore also no
-- monad. The reasons are the same as the ones explained in the documentation
-- of the strict 'Data.Strict.Maybe.Maybe' type.
data Either a b = Left !a | Right !b
    deriving(Eq, Ord, Read, Show)
-}

{-
instance Functor (Either a) where
  fmap f  = toStrict . fmap f . toLazy

-- | Analogous to 'L.either' in "Data.Either".
either :: (a -> c) -> (b -> c) -> Either a b -> c
either f g = L.either f g . toLazy
-}

{-
-- | Analogous to 'L.isLeft' in "Data.Either", which will be included in base
-- \> 4.6.
isLeft :: Either a b -> Bool
isLeft (Left  _) = True
isLeft (Right _) = False

-- | Analogous to 'L.isRight' in "Data.Either", which will be included in base
-- \> 4.6.
isRight :: Either a b -> Bool
isRight (Left  _) = False
isRight (Right _) = True
-}
