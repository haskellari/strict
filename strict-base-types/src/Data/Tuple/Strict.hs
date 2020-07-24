{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

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
-- The strict variant of the standard Haskell pairs and the corresponding
-- variants of the functions from "Data.Tuple".
--
-----------------------------------------------------------------------------


module Data.Tuple.Strict (
    Pair(..)
  , fst
  , snd
  , curry
  , uncurry
  , swap
  , zip
  , unzip
) where

import           Data.Strict.Classes (toStrict, toLazy)
import           Data.Strict.Tuple   (Pair (..), curry, fst, snd, uncurry,
                                      swap, unzip, zip)
import           Prelude             hiding (curry, fst, snd, uncurry, unzip,
                                      zip)

import           Data.Aeson          (FromJSON (..), ToJSON (..))
#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative ((<$>))
#endif

#if __HADDOCK__
import Data.Tuple ()
#endif

import Data.Strict.Lens ()
import Test.QuickCheck.Instances.Strict ()

-- missing instances
--------------------

-- aeson
instance (ToJSON a, ToJSON b) => ToJSON (Pair a b) where
  toJSON = toJSON . toLazy

instance (FromJSON a, FromJSON b) => FromJSON (Pair a b) where
  parseJSON val = toStrict <$> parseJSON val
