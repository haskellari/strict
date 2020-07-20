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

import           Data.Strict.Classes (toStrict, toLazy)
import           Data.Strict.Maybe   (Maybe (Nothing, Just), fromJust,
                                      fromMaybe, isJust, isNothing, maybe,
                                      listToMaybe, maybeToList, mapMaybe, catMaybes)
import           Prelude             hiding (Maybe (..), maybe)
import qualified Prelude             as L

import           Control.Lens.Iso    (Strict (..), iso)
import           Control.Lens.Prism  (Prism, Prism', prism, prism')
import           Data.Aeson          (FromJSON (..), ToJSON (..))
import           Test.QuickCheck     (Arbitrary (..))


-- missing instances
--------------------

-- aeson
instance ToJSON a => ToJSON (Maybe a) where
  toJSON = toJSON . toLazy

instance FromJSON a => FromJSON (Maybe a) where
  parseJSON val = fmap toStrict (parseJSON val)

-- quickcheck
instance Arbitrary a => Arbitrary (Maybe a) where
  arbitrary = fmap toStrict arbitrary
  shrink    = map toStrict . shrink . toLazy

-- lens
instance Strict (L.Maybe a) (Maybe a) where
  strict = iso toStrict toLazy

-- TODO: Each Maybe

-- | Analogous to 'Control.Lens.Prism._Just' in "Control.Lens.Prism"
_Just :: Prism (Maybe a) (Maybe b) a b
_Just = prism Just $ maybe (Left Nothing) Right

-- | Analogous to 'Control.Lens.Prism._Nothing' in "Control.Lens.Prism"
_Nothing :: Prism' (Maybe a) ()
_Nothing = prism' (const Nothing) $ maybe (L.Just ()) (const L.Nothing)
