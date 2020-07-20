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
-- The strict variant of the standard Haskell 'L.Maybe' type and the
-- corresponding variants of the functions from "Data.Maybe".
--
-- Note that in contrast to the standard lazy 'L.Maybe' type, the strict
-- 'Maybe' type is not an applicative functor, and therefore also not a monad.
-- The problem is the /homomorphism/ law, which states that
--
--      @'pure' f '<*>' 'pure' x = 'pure' (f x)  -- must hold for all f@
--
-- This law does not hold for the expected applicative functor instance of
-- 'Maybe', as this instance does not satisfy @pure f \<*\> pure _|_ = pure (f
-- _|_)@ for @f = const@.
--
-----------------------------------------------------------------------------

module Data.Maybe.Strict (
     Maybe(Nothing,Just)
   , maybe

   , isJust
   , isNothing
   , fromJust
   , fromMaybe
   , listToMaybe
   , maybeToList
   , catMaybes
   , mapMaybe
   , _Just
   , _Nothing
) where

import           Data.Strict.Maybe   (Maybe (Nothing, Just), fromJust,
                                      fromMaybe, isJust, isNothing, maybe)
import           Prelude             hiding (Maybe (..), maybe)
import qualified Prelude             as L

import           Control.DeepSeq     (NFData (..))
import           Control.Lens.Iso    (Strict (..), iso)
import           Control.Lens.Prism  (Prism, Prism', prism, prism')
import           Data.Aeson          (FromJSON (..), ToJSON (..))
import           Data.Binary         (Binary (..))
#if MIN_VERSION_base(4,7,0)
import           Data.Data           (Data (..), Typeable)
#else
import           Data.Data           (Data (..), Typeable1 (..))
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
import           Data.Semigroup      (Semigroup)
import qualified Data.Semigroup      as Semigroup


-- utilities
------------

toStrict :: L.Maybe a -> Maybe a
toStrict L.Nothing  = Nothing
toStrict (L.Just x) = Just x

toLazy :: Maybe a -> L.Maybe a
toLazy Nothing  = L.Nothing
toLazy (Just x) = L.Just x

deriving instance Data a => Data (Maybe a)
#if MIN_VERSION_base(4,7,0)
deriving instance Typeable Maybe
#else
deriving instance Typeable1 Maybe
#endif

#if __GLASGOW_HASKELL__ >= 706
deriving instance Generic  (Maybe a)
#endif

instance Semigroup a => Semigroup (Maybe a) where
  Nothing <> m       = m
  m       <> Nothing = m
  Just x1 <> Just x2 = Just (x1 Semigroup.<> x2)

#if MIN_VERSION_base(4,11,0)
instance Semigroup a => Monoid (Maybe a) where
  mempty = Nothing
#else
instance Monoid a => Monoid (Maybe a) where
  mempty = Nothing

  Nothing `mappend` m       = m
  m       `mappend` Nothing = m
  Just x1 `mappend` Just x2 = Just (x1 `mappend` x2)
#endif

-- foldable
instance Foldable Maybe where
    foldMap _ Nothing  = mempty
    foldMap f (Just x) = f x

-- traversable
instance Traversable Maybe where
    traverse _ Nothing  = pure Nothing
    traverse f (Just x) = Just <$> f x

-- deepseq
instance NFData a => NFData (Maybe a) where
  rnf = rnf . toLazy

-- binary
instance Binary a => Binary (Maybe a) where
  put = put . toLazy
  get = toStrict <$> get

-- aeson
instance ToJSON a => ToJSON (Maybe a) where
  toJSON = toJSON . toLazy

instance FromJSON a => FromJSON (Maybe a) where
  parseJSON val = toStrict <$> parseJSON val

-- quickcheck
instance Arbitrary a => Arbitrary (Maybe a) where
  arbitrary = toStrict <$> arbitrary
  shrink    = map toStrict . shrink . toLazy

-- lens
instance Strict (L.Maybe a) (Maybe a) where
  strict = iso toStrict toLazy

-- hashable
instance Hashable a => Hashable (Maybe a) where
  hashWithSalt salt = hashWithSalt salt . toLazy

-- | Analogous to 'L.listToMaybe' in "Data.Maybe".
listToMaybe :: [a] -> Maybe a
listToMaybe []        =  Nothing
listToMaybe (a:_)     =  Just a

-- | Analogous to 'L.maybeToList' in "Data.Maybe".
maybeToList :: Maybe a -> [a]
maybeToList  Nothing   = []
maybeToList  (Just x)  = [x]

-- | Analogous to 'L.catMaybes' in "Data.Maybe".
catMaybes :: [Maybe a] -> [a]
catMaybes ls = [x | Just x <- ls]

-- | Analogous to 'L.mapMaybe' in "Data.Maybe".
mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ []     = []
mapMaybe f (x:xs) = case f x of
    Nothing -> rs
    Just r  -> r:rs
  where
    rs = mapMaybe f xs

-- | Analogous to 'Control.Lens.Prism._Just' in "Control.Lens.Prism"
_Just :: Prism (Maybe a) (Maybe b) a b
_Just = prism Just $ maybe (Left Nothing) Right

-- | Analogous to 'Control.Lens.Prism._Nothing' in "Control.Lens.Prism"
_Nothing :: Prism' (Maybe a) ()
_Nothing = prism' (const Nothing) $ maybe (L.Just ()) (const L.Nothing)

------------------------------------------------------------------------------
-- Code required to make this module independent of the 'strict' package
------------------------------------------------------------------------------

{-
-- | The type of strict optional values.
--
-- In contrast to the standard lazy 'L.Maybe' type, this type is not an
-- applicative functor, and therefore also not a monad. The problem is the
-- /homomorphism/ law, which states that
--
--      @'pure' f '<*>' 'pure' x = 'pure' (f x)@
--
-- must hold for all @f@. This law does not hold for the expected applicative
-- functor instance of 'Maybe', as this instance does not satisfy @pure f
-- \<*\> pure _|_ = pure (f _|_)@ for @f = const@.
data Maybe a = Nothing | Just !a
    deriving(Eq, Ord, Show, Read, Data, Typeable, Generic)
-}

-- instances
------------

{-
instance StrictType (Maybe a) where
  type LazyVariant (Maybe a) = L.Maybe a

  toStrict L.Nothing  = Nothing
  toStrict (L.Just x) = Just x

  toLazy Nothing  = L.Nothing
  toLazy (Just x) = L.Just x

instance Functor Maybe where
  fmap f = toStrict . fmap f . toLazy

instance Foldable Maybe where
  foldr f y  = Foldable.foldr f y . toLazy
  foldl f y  = Foldable.foldl f y . toLazy

instance Traversable Maybe where
  traverse _ Nothing  = pure Nothing
  traverse f (Just x) = Just <$> f x
-}

{-
-- | Analogous to 'L.isJust' in "Data.Maybe".
isJust :: Maybe a -> Bool
isJust = L.isJust . toLazy

-- | Analogous to 'L.isNothing' in "Data.Maybe".
isNothing :: Maybe a -> Bool
isNothing = L.isNothing . toLazy

-- | Analogous to 'L.fromJust' in "Data.Maybe".
fromJust :: Maybe a -> a
fromJust Nothing  = error "Data.Strict.Maybe.fromJust: Nothing"
fromJust (Just x) = x

-- | Analogous to 'L.fromMaybe' in "Data.Maybe".
fromMaybe :: a -> Maybe a -> a
fromMaybe x = L.fromMaybe x . toLazy

-- | Analogous to 'L.maybe' in "Data.Maybe".
maybe :: b -> (a -> b) -> Maybe a -> b
maybe x f = L.maybe x f . toLazy
-}
