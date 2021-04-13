{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Strict.HashMap.Internal where

import Data.HashMap.Lazy                  as L
import Data.Strict.HashMap.Autogen.Strict as S

import Data.Hashable
import Data.Strict.Classes

instance (Eq k, Hashable k) => Strict (L.HashMap k v) (S.HashMap k v) where
  toStrict = S.fromList . L.toList
  toLazy = L.fromList . S.toList
