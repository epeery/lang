{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Dual where

import Data.Functor.Identity (Identity (..))
import Positionable

class Dual f g | f -> g, g -> f where
  zap :: (a -> b -> c) -> f a -> g b -> c

(>$<) :: Dual f g => f (a -> b) -> g a -> b
(>$<) = zap id

instance Dual Identity Identity where
  zap f (Identity a) (Identity b) = f a b

data (f :+: g) a = Inl (f a) | Inr (g a) deriving (Show, Functor)

data (f :*: g) a = Prod (f a) (g a) deriving (Show, Functor)

instance (Dual f f', Dual g g') => Dual (f :+: g) (f' :*: g') where
  zap op (Inl f) (Prod a _) = zap op f a
  zap op (Inr f) (Prod _ b) = zap op f b

instance (Dual f f', Dual g g') => Dual (f :*: g) (f' :+: g') where
  zap op (Prod f _) (Inl a) = zap op f a
  zap op (Prod _ g) (Inr b) = zap op g b

class BiDual p q | p -> q, q -> p where
  biZap :: (a -> c -> e) -> (b -> d -> e) -> p a b -> q c d -> e

(>>$<<) :: BiDual p q => p (a -> c) (b -> c) -> q a b -> c
(>>$<<) = biZap id id

instance BiDual (,) Either where
  biZap l _ (f, _) (Left a) = l f a
  biZap _ r (_, g) (Right b) = r g b

instance BiDual Either (,) where
  biZap l _ (Left f) (a, _) = l f a
  biZap _ r (Right g) (_, b) = r g b
