module Data.UniformPair.OrphanInstances where

import Data.Monoid ((<>))
import Data.UniformPair

instance Eq a => Eq (Pair a) where
  (a :# b) == (a' :# b')
    = a == a' && b == b'

instance Ord a => Ord (Pair a) where
  (a :# b) `compare` (a' :# b')
    = a `compare` a' <> b `compare` b'

