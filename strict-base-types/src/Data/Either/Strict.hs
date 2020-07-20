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

import           Data.Strict.Classes (toStrict, toLazy)
import           Data.Strict.Either  (Either (Left, Right), either, isLeft,
                                      isRight, lefts, rights, partitionEithers)
import           Prelude             hiding (Either (..), either)
import qualified Prelude             as L

import           Control.Lens.Iso    (Strict (..), Swapped (..), iso)
import           Control.Lens.Prism  (Prism, prism)
import           Data.Aeson          (FromJSON (..), ToJSON (..))
import           Test.QuickCheck     (Arbitrary (..))

-- missing instances
--------------------

-- aeson
instance (ToJSON a, ToJSON b) => ToJSON (Either a b) where
  toJSON = toJSON . toLazy

instance (FromJSON a, FromJSON b) => FromJSON (Either a b) where
  parseJSON val = fmap toStrict (parseJSON val)

-- quickcheck
instance (Arbitrary a, Arbitrary b) => Arbitrary (Either a b) where
  arbitrary = fmap toStrict arbitrary
  shrink    = map toStrict . shrink . toLazy

-- lens
instance Strict (L.Either a b) (Either a b) where
  strict = iso toStrict toLazy

instance Swapped Either where
  swapped = either Right Left `iso` either Right Left

-- TODO: Each (Either a)

-- | Analogous to 'Control.Lens.Prism._Left' in "Control.Lens.Prism".
_Left :: Prism (Either a c) (Either b c) a b
_Left = prism Left $ either L.Right (L.Left . Right)

-- | Analogous to 'Control.Lens.Prism._Right' in "Control.Lens.Prism".
_Right :: Prism (Either c a) (Either c b) a b
_Right = prism Right $ either (L.Left . Left) L.Right
