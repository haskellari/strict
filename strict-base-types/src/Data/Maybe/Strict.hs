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
-- 'Maybe' type does not have lawful functor, applicative, or monad instances.
-- However, we provide instances for these type classes (and related others)
-- since they are only partial due to the below example.
--
-- The problem is the /homomorphism/ law, which states that
--
--      @'pure' f '<*>' 'pure' x = 'pure' (f x)  -- must hold for all f@
--
-- This law does not hold for the expected applicative functor instance of
-- 'Maybe', as this instance does not satisfy @pure f \<*\> pure _|_ = pure (f
-- _|_)@ for @f = const@.
--
-- Many consider this hole in the lawfulness to be unuseful, however, as _|_ is
-- rarely used and if one is using _|_ with strict types you'd expect the program
-- to error immediately.
--
-----------------------------------------------------------------------------

module Data.Maybe.Strict (
     Maybe(..)
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

import Data.Aeson ()
import Data.Strict.Lens (_Just, _Nothing)
import Data.Strict.Maybe   (Maybe (..), fromJust, fromMaybe, isJust, isNothing, maybe, listToMaybe, maybeToList, mapMaybe, catMaybes)
import Prelude ()
import Test.QuickCheck.Instances.Strict ()
