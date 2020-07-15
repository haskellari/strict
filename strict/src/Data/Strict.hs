{-# LANGUAGE Safe #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Strict
-- Copyright   :  (c) 2006-2007 Roman Leshchinskiy
--                (c) 2013 Simon Meier
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   :  experimental
-- Portability :  portable
--
-- Strict versions of some standard Haskell types.
--
-----------------------------------------------------------------------------

module Data.Strict (
    module Data.Strict.Classes
  , module Data.Strict.These
  , module Data.Strict.Tuple
  , module Data.Strict.Maybe
  , module Data.Strict.Either
) where

import Data.Strict.Classes
import Data.Strict.These
import Data.Strict.Tuple
import Data.Strict.Maybe
import Data.Strict.Either
