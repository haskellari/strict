{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Strict.Sequence.Internal where

import Data.Sequence                as L
import Data.Strict.Sequence.Autogen as S

import Data.Strict.Classes
import Data.Foldable

instance Strict (L.Seq k) (S.Seq k) where
  toStrict = S.fromList . toList
  toLazy = L.fromList . toList
