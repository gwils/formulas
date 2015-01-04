{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

module Math.Logic.Formula where

import Control.Applicative
import Control.Lens
import Data.Set (Set)
import Data.Foldable (Foldable(..))
import Data.UniformPair.Lens

type Form = Formula Pair Int

data Formula f literal = Lit literal
                       | Neg (Formula f literal)
                       | Con (f (Formula f literal))
                       | Dis (f (Formula f literal))

deriving instance Eq lit   => Eq (Formula Pair lit)
deriving instance Eq lit   => Eq (Formula Set lit)
deriving instance Eq lit   => Eq (Formula [] lit)
deriving instance Ord lit  => Ord (Formula Pair lit)
deriving instance Ord lit  => Ord (Formula Set lit)
deriving instance Ord lit  => Ord (Formula [] lit)
deriving instance Show lit => Show (Formula Pair lit)
deriving instance Show lit => Show (Formula Set lit)
deriving instance Show lit => Show (Formula [] lit)


instance Functor f => Functor (Formula f) where
  fmap f form = case form of
                  Lit l     -> Lit (f l)
                  Neg form' -> Neg (fmap f form')
                  Con ff    -> Con (fmap (fmap f) ff)
                  Dis ff    -> Dis (fmap (fmap f) ff)

instance Foldable f => Foldable (Formula f) where
  foldMap f form = case form of
                     Lit l     -> f l
                     Neg form' -> foldMap f form'
                     Con ff    -> foldMap (foldMap f) ff
                     Dis ff    -> foldMap (foldMap f) ff

instance Traversable f => Traversable (Formula f) where
  traverse = lits

foldForm :: Foldable f => Fold (Formula f a) a
foldForm = folded

lits :: Traversable f => Traversal (Formula f a) (Formula f b) a b
lits = litsOf traverse

litsOf :: (forall c d. Traversal (f c) (f d) c d) -> Traversal (Formula f a) (Formula f b) a b
litsOf _ f (Lit l)    = Lit <$> f l
litsOf t f (Neg form) = Neg <$> litsOf t f form
litsOf t f (Con ff)   = Con <$> traverseOf t (litsOf t f) ff
litsOf t f (Dis ff)   = Dis <$> traverseOf t (litsOf t f) ff

