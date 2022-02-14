{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Vector where

data Vector (attrs :: VAttrs) a where
  VNil :: Vector 'Empty a
  VStart :: a -> Vector 'Start a
  VEnd :: a -> Vector 'End a
  VMiddle :: a -> Vector 'Middle a
  VSize :: a -> Vector 'Size a
  Vector ::
    { _start :: a,
      _end :: a,
      _middle :: a,
      _size :: a
    } ->
    Vector 'Constrained a

deriving instance Show a => Show (Vector attrs a)

instance (Ord a, Fractional a) => Semigroup (Vector 'Start a) where
  VStart s1 <> VStart s2 = VStart (min s1 s2)

instance (Ord a, Fractional a) => Semigroup (Vector 'End a) where
  VEnd e1 <> VEnd e2 = VEnd (max e1 e2)

instance Fractional a => Semigroup (Vector 'Middle a) where
  VMiddle m1 <> VMiddle m2 = VMiddle ((m1 + m2) / 2)

instance Fractional a => Semigroup (Vector 'Size a) where
  VSize s1 <> VSize s2 = VSize (s1 + s2)

instance Fractional a => Monoid (Vector 'Size a) where
  mempty = VSize 0

instance (Ord a, Fractional a) => Semigroup (Vector 'Constrained a) where
  Vector s1 e1 _ _ <> Vector s2 e2 _ _ = Vector s e m l
    where
      s = min s1 s2
      e = max e1 e2
      m = s + l / 2
      l = e - s

instance (Bounded a, Ord a, Fractional a) => Monoid (Vector 'Constrained a) where
  mempty = Vector s e m l
    where
      s = maxBound
      e = undefined
      m = undefined
      l = undefined

data VAttrs = Empty | Start | End | Middle | Size | Constrained

vector :: Vector 'Empty n
vector = VNil

class CanMiddle (as :: VAttrs) where
  type AddMiddle as :: VAttrs
  middle :: Fractional a => a -> Vector as a -> Vector (AddMiddle as) a

instance CanMiddle 'Empty where
  type AddMiddle 'Empty = 'Middle
  middle n _ = VMiddle n

instance CanMiddle 'Start where
  type AddMiddle 'Start = 'Constrained
  middle n (VStart s) = Vector s e n m
    where
      m = (n - s) * 2
      e = s + m

instance CanMiddle 'End where
  type AddMiddle 'End = 'Constrained
  middle n (VEnd e) = Vector s e n m
    where
      s = e - m
      m = (e - n) * 2

instance CanMiddle 'Size where
  type AddMiddle 'Size = 'Constrained
  middle n (VSize m) = Vector s e n m
    where
      s = n - (m / 2)
      e = s + m

instance CanMiddle 'Middle where
  type AddMiddle 'Middle = 'Middle
  middle n _ = VMiddle n

instance CanMiddle 'Constrained where
  type AddMiddle 'Constrained = 'Constrained
  middle n (Vector _ _ _ l) = Vector (n - (l / 2)) (n + (l / 2)) n l

class CanStart (as :: VAttrs) where
  type AddStart as :: VAttrs
  start :: Fractional n => n -> Vector as n -> Vector (AddStart as) n

instance CanStart 'Empty where
  type AddStart 'Empty = 'Start
  start n _ = VStart n

instance CanStart 'End where
  type AddStart 'End = 'Constrained
  start n (VEnd e) = Vector n e c m
    where
      m = e - n
      c = m / 2

instance CanStart 'Size where
  type AddStart 'Size = 'Constrained
  start n (VSize m) = Vector n e c m
    where
      e = n + m
      c = n + (m / 2)

instance CanStart 'Middle where
  type AddStart 'Middle = 'Constrained
  start n (VMiddle c) = Vector n e c m
    where
      m = (c - n) * 2
      e = n + m

instance CanStart 'Start where
  type AddStart 'Start = 'Start
  start n _ = VStart n

instance CanStart 'Constrained where
  type AddStart 'Constrained = 'Constrained
  start n (Vector _ _ _ l) = Vector n (n + l) (n + (l / 2)) l

class CanEnd (as :: VAttrs) where
  type AddEnd as :: VAttrs
  end :: Fractional a => a -> Vector as a -> Vector (AddEnd as) a

instance CanEnd 'Empty where
  type AddEnd 'Empty = 'End
  end n _ = VEnd n

instance CanEnd 'Start where
  type AddEnd 'Start = 'Constrained
  end n (VStart s) = Vector s n c m
    where
      m = n - s
      c = s + (m / 2)

instance CanEnd 'Middle where
  type AddEnd 'Middle = 'Constrained
  end n (VMiddle c) = Vector s n c m
    where
      m = (n - c) * 2
      s = n - m

instance CanEnd 'Size where
  type AddEnd 'Size = 'Constrained
  end n (VSize m) = Vector s n c m
    where
      s = n - m
      c = s + (m / 2)

instance CanEnd 'End where
  type AddEnd 'End = 'End
  end n _ = VEnd n

instance CanEnd 'Constrained where
  type AddEnd 'Constrained = 'Constrained
  end n (Vector _ _ _ l) = Vector (n - l) n (n - (l / 2)) l

class CanSize (as :: VAttrs) where
  type AddSize as :: VAttrs
  size :: Fractional a => a -> Vector as a -> Vector (AddSize as) a

instance CanSize 'Empty where
  type AddSize 'Empty = 'Size
  size n _ = VSize n

instance CanSize 'Start where
  type AddSize 'Start = 'Constrained
  size n (VStart s) = Vector s e c n
    where
      e = s + n
      c = s + (n / 2)

instance CanSize 'End where
  type AddSize 'End = 'Constrained
  size n (VEnd e) = Vector s e c n
    where
      s = e - n
      c = s + (n / 2)

instance CanSize 'Middle where
  type AddSize 'Middle = 'Constrained
  size n (VMiddle c) = Vector s e c n
    where
      e = c + (n / 2)
      s = c - (n / 2)

instance CanSize 'Size where
  type AddSize 'Size = 'Size
  size n _ = VSize n

instance CanSize 'Constrained where
  type AddSize 'Constrained = 'Constrained
  size n (Vector s _ _ _) = Vector s (s + n) (s + (n / 2)) n

class HasStart (as :: VAttrs) where
  getStart :: Fractional a => Vector as a -> a

instance HasStart 'Start where
  getStart (VStart n) = n

instance HasStart 'Constrained where
  getStart = _start

class HasMiddle (as :: VAttrs) where
  getMiddle :: Fractional a => Vector as a -> a

instance HasMiddle 'Middle where
  getMiddle (VMiddle n) = n

instance HasMiddle 'Constrained where
  getMiddle = _middle

class HasEnd (as :: VAttrs) where
  getEnd :: Fractional a => Vector as a -> a

instance HasEnd 'End where
  getEnd (VEnd n) = n

instance HasEnd 'Constrained where
  getEnd = _end

class HasSize (as :: VAttrs) where
  getSize :: Fractional a => Vector as a -> a

instance HasSize 'Size where
  getSize (VSize n) = n

instance HasSize 'Constrained where
  getSize = _size
