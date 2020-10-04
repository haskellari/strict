{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Strict.Optics (
    -- * Tuple
    -- | See instances, in particular for 'Field1' and 'Field2' type classes.
    -- * Either
    _Left, _Right,
    -- * Maybe
    _Just, _Nothing,
    -- * These
    here, there,
    _This, _That, _These,
    -- * Combinators
    strict, lazy,
    ) where


import           Control.Applicative (pure, (<$>), (<*>))
import           Prelude             (Int, flip, ($), (.))

-- Lazy variants
import qualified Prelude             as L

import           Optics.Core         (Each (..), Field1 (..), Field2 (..),
                                      Index, Iso', Prism, Prism', Swapped (..),
                                      Traversal, iso, itraversalVL, lensVL,
                                      prism, prism', traversalVL, (<&>))

import           Data.Strict         (Either (..), Maybe (..), Pair (..),
                                      Strict (..), These (..), either, maybe,
                                      swap, these)

-------------------------------------------------------------------------------
-- Tuple
-------------------------------------------------------------------------------

instance Field1 (Pair a b) (Pair a' b) a a' where
  _1 = lensVL $ \k (a :!: b) -> k a <&> \a' -> (a' :!: b)

instance Field2 (Pair a b) (Pair a b') b b' where
  _2 = lensVL $ \k (a :!: b) -> k b <&> \b' -> (a :!: b')

instance Swapped Pair where
  swapped = iso swap swap

type instance Index (Pair a b) = Int
instance (a~a', b~b') => Each Int (Pair a a') (Pair b b') a b where
  each = itraversalVL $ \f ~(a :!: b) -> (:!:) <$> f 0 a <*> f 1 b
  {-# INLINE each #-}

-------------------------------------------------------------------------------
-- Either
-------------------------------------------------------------------------------

instance Swapped Either where
  swapped = either Right Left `iso` either Right Left

instance (a ~ a', b ~ b') => Each (Either () ()) (Either a a') (Either b b') a b where
  each = itraversalVL aux where
    aux f (Left x)  = Left <$> f (Left ()) x
    aux f (Right x) = Right <$> f (Right ()) x

-- | Analogous to 'Control.Lens.Prism._Left' in "Control.Lens.Prism".
_Left :: Prism (Either a c) (Either b c) a b
_Left = prism Left $ either L.Right (L.Left . Right)

-- | Analogous to 'Control.Lens.Prism._Right' in "Control.Lens.Prism".
_Right :: Prism (Either c a) (Either c b) a b
_Right = prism Right $ either (L.Left . Left) L.Right

-------------------------------------------------------------------------------
-- Maybe
-------------------------------------------------------------------------------

instance Each () (Maybe a) (Maybe b) a b where
    each = itraversalVL aux where
        aux _ Nothing  = pure Nothing
        aux f (Just x) = Just <$> f () x

-- | Analogous to 'Control.Lens.Prism._Just' in "Control.Lens.Prism"
_Just :: Prism (Maybe a) (Maybe b) a b
_Just = prism Just $ maybe (L.Left Nothing) L.Right

-- | Analogous to 'Control.Lens.Prism._Nothing' in "Control.Lens.Prism"
_Nothing :: Prism' (Maybe a) ()
_Nothing = prism' (L.const Nothing) $ maybe (L.Just ()) (L.const L.Nothing)

-------------------------------------------------------------------------------
-- These
-------------------------------------------------------------------------------

instance Swapped These where
    swapped = iso swapThese swapThese

instance (a ~ a', b ~ b') => Each (Either () ()) (These a a') (These b b') a b where
    each = itraversalVL aux where
        aux f (This a)    = This <$> f (Left ()) a
        aux f (That b)    = That <$> f (Right ()) b
        aux f (These a b) = These <$> f (Left ()) a <*> f (Right ()) b

-- | A 'Control.Lens.Traversal' of the first half of a 'These', suitable for use with "Control.Lens".
--
-- >>> over here show (That 1)
-- That 1
--
-- >>> over here show (These 'a' 2)
-- These "'a'" 2
--
here :: Traversal (These a c) (These b c) a b
here = traversalVL aux where
    aux f (This x)    = This <$> f x
    aux f (These x y) = flip These y <$> f x
    aux _ (That x)    = pure (That x)

-- | A 'Control.Lens.Traversal' of the second half of a 'These', suitable for use with "Control.Lens".
--
-- @
-- 'there' :: 'Control.Lens.Traversal' ('These' t b) ('These' t b) a b
-- @
--
-- >>> over there show (That 1)
-- That "1"
--
-- >>> over there show (These 'a' 2)
-- These 'a' "2"
--
there :: Traversal (These c a) (These c b) a b
there = traversalVL aux where
    aux _ (This x)    = pure (This x)
    aux f (These x y) = These x <$> f y
    aux f (That x)    = That <$> f x

-- | A 'Control.Lens.Prism'' selecting the 'This' constructor.
--
-- /Note:/ cannot change type.
_This :: Prism' (These a b) a
_This = prism This (these L.Right (L.Left . That) (\x y -> L.Left $ These x y))

-- | A 'Control.Lens.Prism'' selecting the 'That' constructor.
--
-- /Note:/ cannot change type.
_That :: Prism' (These a b) b
_That = prism That (these (L.Left . This) L.Right (\x y -> L.Left $ These x y))

-- | A 'Control.Lens.Prism'' selecting the 'These' constructor. 'These' names are ridiculous!
--
-- /Note:/ cannot change type.
_These :: Prism' (These a b) (a, b)
_These = prism (\(a,b) -> These a b) (these (L.Left . This) (L.Left . That) (\x y -> L.Right (x, y)))

swapThese :: These a b -> These b a
swapThese (This a)    = That a
swapThese (That b)    = This b
swapThese (These a b) = These b a

-------------------------------------------------------------------------------
-- Combinators
-------------------------------------------------------------------------------

-- | Ad hoc conversion between "strict" and "lazy" versions of a structure,
-- using 'Strict' class.
strict :: Strict lazy strict => Iso' lazy strict
strict = iso toStrict toLazy

-- | An 'Iso'' between the strict variant of a structure and its lazy counterpart.
--
-- @
-- lazy = re strict
-- @
--
lazy :: Strict lazy strict => Iso' strict lazy
lazy = iso toLazy toStrict
