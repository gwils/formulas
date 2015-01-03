{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.UniformPair.Lens
  ( module Data.UniformPair )
  where

import Control.Lens (lens, Field1(_1), Field2(_2))
import Data.UniformPair

instance Field1 (Pair a) (Pair a) a a where
  _1 = lens fstP (\(_ :# b) a -> a :# b)

instance Field2 (Pair a) (Pair a) a a where
  _2 = lens sndP (\(a :# _) b -> a :# b)

