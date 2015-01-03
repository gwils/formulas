{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

module Math.Logic.Formula where

import Control.Applicative
import Control.Lens
import Data.Data
import Data.Typeable
import Data.Foldable
import Data.Monoid
import Data.Traversable
import Data.UniformPair.Lens
import Data.UniformPair.OrphanInstances

type Form = Formula Pair Int

data Formula f literal = Lit literal
                       | Neg (Formula f literal)
                       | Con (f (Formula f literal))
                       | Dis (f (Formula f literal))
                       deriving (Functor, Foldable, Traversable, Typeable)

deriving instance Eq lit   => Eq (Formula Pair lit)
deriving instance Ord lit  => Ord (Formula Pair lit)
deriving instance Show lit => Show (Formula Pair lit)

{-
instance Functor f => Functor (Formula f) where
  fmap f form = case form of
                  Lit l     -> Lit (f l)
                  Neg form' -> Neg (fmap f form')
                  Con ff    -> Con (fmap (fmap f) ff)
                  Dis ff    -> Dis (fmap (fmap f) ff)
-}

{-
lits :: Traversable f => Traversal (Formula f l) (Formula f l') l l'
lits f (Lit l)    = Lit <$> f l
lits f (Neg form) = Neg <$> lits f form
lits f (Con ff)   = Con <$> traverse (lits f) ff
lits f (Dis ff)   = Dis <$> traverse (lits f) ff
-}

lits :: Traversable f => Traversal (Formula f a) (Formula f b) a b
lits = litsOf traverse


litsOf :: (forall a b. Traversal (f a) (f b) a b) -> Traversal (Formula f a) (Formula f b) a b
litsOf _ f (Lit l)    = Lit <$> f l
litsOf t f (Neg form) = Neg <$> litsOf t f form
litsOf t f (Con ff)   = Con <$> traverseOf t (litsOf t f) ff
litsOf t f (Dis ff)   = Dis <$> traverseOf t (litsOf t f) ff



