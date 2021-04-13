{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Strict.Map.Internal where

import Data.Map.Lazy                  as L
import Data.Strict.Map.Autogen.Strict as S

import Data.Strict.Classes

instance (Eq k, Ord k) => Strict (L.Map k v) (S.Map k v) where
  toStrict = S.fromList . L.toList
  toLazy = L.fromList . S.toList
