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
    module Data.Strict.Tuple
  , module Data.Strict.Maybe
  , module Data.Strict.Either
) where

import Data.Strict.Tuple hiding (toStrict, toLazy)
import Data.Strict.Maybe hiding (toStrict, toLazy)
import Data.Strict.Either hiding (toStrict, toLazy)
