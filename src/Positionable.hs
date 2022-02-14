{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies #-}

module Positionable where

import Control.Applicative (liftA, liftA2)
import Data.Functor.Identity (Identity (..))

type Positioned = (Constrained, Constrained)

type Ctx = (Positioned, Positioned)

type PartialLayout c a = c Ctx a

type Layout c = PartialLayout c Positioned

data Empty = Empty deriving (Show, Eq, Ord)

newtype Start = Start Double deriving (Show, Eq, Ord, Num, Fractional)

newtype Middle = Middle Double deriving (Show, Eq, Ord, Num, Fractional)

newtype End = End Double deriving (Show, Eq, Ord, Num, Fractional)

newtype Size = Size Double deriving (Show, Eq, Ord, Num, Fractional)

data Constrained = Constrained Start Middle End Size deriving (Show, Eq, Ord)

class Bounds a where
  getBounds :: [a] -> a

instance Bounds Empty where
  getBounds _ = Empty

instance Bounds Start where
  getBounds ss = minimum ss

instance Bounds End where
  getBounds ss = maximum ss

instance Bounds Middle where
  getBounds ss = (minimum ss + maximum ss) / 2

instance Bounds Size where
  getBounds ss = maximum ss

instance Bounds Constrained where
  getBounds ss = Constrained (Start s) (Middle $ (s + e) / 2) (End e) (Size $ e - s)
    where
      points = foldr (\(Constrained (Start a) _ (End b) _) acc -> s : b : acc) [] ss
      s = minimum points
      e = maximum points

bounds :: (Applicative f, Bounds h, Bounds v) => f [(h, v)] -> (f h, f v)
bounds xs = ((getBounds <$>) $ horizontal <$> xs, (getBounds <$>) $ vertical <$> xs)

class Containable f where
  container :: f Ctx Positioned
  screen :: f Ctx Positioned

class (forall r. Fractional (f r Double)) => Positionable f where
  val :: Double -> f r Double
  default val :: Applicative (f r) => Double -> f r Double
  val = pure

  box :: f r (Empty, Empty)
  default box :: Applicative (f r) => f r (Empty, Empty)
  box = pure (Empty, Empty)

  left :: CanStart h => f r Double -> f r (h, v) -> f r ((Add Start h), v)
  default left :: (Applicative (f r), CanStart h) => f r Double -> f r (h, v) -> f r ((Add Start h), v)
  left = onHorizontal start

  getLeft :: HasStart h => f r (h, v) -> f r Double
  default getLeft :: (Functor (f r), HasStart h) => f r (h, v) -> f r Double
  getLeft = getHorizontal getStart

  right :: CanEnd h => f r Double -> f r (h, v) -> f r ((Add End h), v)
  default right :: (Applicative (f r), CanEnd h) => f r Double -> f r (h, v) -> f r ((Add End h), v)
  right = onHorizontal end

  getRight :: HasEnd h => f r (h, v) -> f r Double
  default getRight :: (Functor (f r), HasEnd h) => f r (h, v) -> f r Double
  getRight = getHorizontal getEnd

  width :: CanSize h => f r Double -> f r (h, v) -> f r ((Add Size h), v)
  default width :: (Applicative (f r), CanSize h) => f r Double -> f r (h, v) -> f r ((Add Size h), v)
  width = onHorizontal size

  getWidth :: HasSize h => f r (h, v) -> f r Double
  default getWidth :: (Functor (f r), HasSize h) => f r (h, v) -> f r Double
  getWidth = getHorizontal getSize

  centerX :: CanMiddle h => f r Double -> f r (h, v) -> f r ((Add Middle h), v)
  default centerX :: (Applicative (f r), CanMiddle h) => f r Double -> f r (h, v) -> f r ((Add Middle h), v)
  centerX = onHorizontal middle

  getCenterX :: HasMiddle h => f r (h, v) -> f r Double
  default getCenterX :: (Functor (f r), HasMiddle h) => f r (h, v) -> f r Double
  getCenterX = getHorizontal getMiddle

  top :: CanStart v => f r Double -> f r (h, v) -> f r (h, (Add Start v))
  default top :: (Applicative (f r), CanStart v) => f r Double -> f r (h, v) -> f r (h, (Add Start v))
  top = onVertical start

  getTop :: HasStart v => f r (h, v) -> f r Double
  default getTop :: (Functor (f r), HasStart v) => f r (h, v) -> f r Double
  getTop = getVertical getStart

  bottom :: CanEnd v => f r Double -> f r (h, v) -> f r (h, (Add End v))
  default bottom :: (Applicative (f r), CanEnd v) => f r Double -> f r (h, v) -> f r (h, (Add End v))
  bottom = onVertical end

  getBottom :: HasEnd v => f r (h, v) -> f r Double
  default getBottom :: (Functor (f r), HasEnd v) => f r (h, v) -> f r Double
  getBottom = getVertical getEnd

  height :: CanSize v => f r Double -> f r (h, v) -> f r (h, (Add Size v))
  default height :: (Applicative (f r), CanSize v) => f r Double -> f r (h, v) -> f r (h, (Add Size v))
  height = onVertical size

  getHeight :: HasSize v => f r (h, v) -> f r Double
  default getHeight :: (Functor (f r), HasSize v) => f r (h, v) -> f r Double
  getHeight = getVertical getSize

  centerY :: CanMiddle v => f r Double -> f r (h, v) -> f r (h, (Add Middle v))
  default centerY :: (Applicative (f r), CanMiddle v) => f r Double -> f r (h, v) -> f r (h, (Add Middle v))
  centerY = onVertical middle

  getCenterY :: HasMiddle v => f r (h, v) -> f r Double
  default getCenterY :: (Functor (f r), HasMiddle v) => f r (h, v) -> f r Double
  getCenterY = getVertical getMiddle

type family Add a b where
  Add a Empty = a
  Add Empty b = b
  Add a a = a
  Add (f a) (f b) = f (Add a b)
  Add (f a) b = f (Add a b)
  Add a (f b) = f (Add a b)
  Add a b = Constrained

class CanStart v where
  start :: Applicative f => f Double -> f v -> f (Add Start v)

class HasStart v where
  getStart :: Functor f => f v -> f Double

class CanEnd v where
  end :: Applicative f => f Double -> f v -> f (Add End v)

class HasEnd v where
  getEnd :: Functor f => f v -> f Double

class CanMiddle v where
  middle :: Applicative f => f Double -> f v -> f (Add Middle v)

class HasMiddle v where
  getMiddle :: Functor f => f v -> f Double

class CanSize v where
  size :: Applicative f => f Double -> f v -> f (Add Size v)

class HasSize v where
  getSize :: Functor f => f v -> f Double

instance CanStart Empty where
  start fs _ = Start <$> fs

instance (Applicative f, CanStart a) => CanStart (f a) where
  start fd f = (\x d -> start (pure d) x) <$> f <*> fd

instance CanStart Start where
  start fs _ = Start <$> fs

instance CanStart Middle where
  start fs fm = (\s (Middle m) -> Constrained (Start s) (Middle m) (End $ m + (m - s)) (Size $ (m - s) * 2)) <$> fs <*> fm

instance CanStart End where
  start fs fe = (\s (End e) -> Constrained (Start s) (Middle $ (s + e) / 2) (End e) (Size $ e - s)) <$> fs <*> fe

instance CanStart Size where
  start fs fl = (\s (Size l) -> Constrained (Start s) (Middle $ s + (l / 2)) (End $ s + l) (Size l)) <$> fs <*> fl

instance CanStart Constrained where
  start fs fc = (\s (Constrained _ _ _ (Size l)) -> Constrained (Start s) (Middle $ s + (l / 2)) (End $ s + l) (Size l)) <$> fs <*> fc

instance HasStart Start where
  getStart = ((\(Start s) -> s) <$>)

instance HasStart Constrained where
  getStart = ((\(Constrained (Start s) _ _ _) -> s) <$>)

instance CanMiddle Empty where
  middle fm _ = Middle <$> fm

instance CanMiddle Start where
  middle fm fs = (\m (Start s) -> Constrained (Start s) (Middle m) (End $ m + (m - s)) (Size $ (m - s) * 2)) <$> fm <*> fs

instance CanMiddle Middle where
  middle fm _ = Middle <$> fm

instance CanMiddle End where
  middle fm fe = (\m (End e) -> Constrained (Start $ m - (e - m)) (Middle m) (End e) (Size $ (e - m) * 2)) <$> fm <*> fe

instance CanMiddle Size where
  middle fm fl = (\m (Size l) -> Constrained (Start $ m - (l / 2)) (Middle m) (End $ m + (l / 2)) (Size l)) <$> fm <*> fl

instance CanMiddle Constrained where
  middle fm fc = (\m (Constrained _ _ _ (Size l)) -> Constrained (Start $ m - (l / 2)) (Middle m) (End $ m + (l / 2)) (Size l)) <$> fm <*> fc

instance HasMiddle Middle where
  getMiddle = ((\(Middle m) -> m) <$>)

instance HasMiddle Constrained where
  getMiddle = ((\(Constrained _ (Middle m) _ _) -> m) <$>)

instance CanEnd Empty where
  end fe _ = End <$> fe

instance CanEnd Start where
  end fe fs = (\e (Start s) -> Constrained (Start s) (Middle $ (s + e) / 2) (End e) (Size $ e - s)) <$> fe <*> fs

instance CanEnd Middle where
  end fe fm = (\e (Middle m) -> Constrained (Start $ m - (e - m)) (Middle m) (End e) (Size $ (e - m) * 2)) <$> fe <*> fm

instance CanEnd End where
  end fe _ = End <$> fe

instance CanEnd Size where
  end fe fl = (\e (Size l) -> Constrained (Start $ e - l) (Middle $ e - (l / 2)) (End e) (Size l)) <$> fe <*> fl

instance CanEnd Constrained where
  end fe fl = (\e (Constrained _ _ _ (Size l)) -> Constrained (Start $ e - l) (Middle $ e - (l / 2)) (End e) (Size l)) <$> fe <*> fl

instance HasEnd End where
  getEnd = ((\(End e) -> e) <$>)

instance HasEnd Constrained where
  getEnd = ((\(Constrained _ _ (End e) _) -> e) <$>)

instance CanSize Empty where
  size fl _ = Size <$> fl

instance CanSize Start where
  size fl fs = (\l (Start s) -> Constrained (Start s) (Middle $ s + (l / 2)) (End $ s + l) (Size l)) <$> fl <*> fs

instance CanSize Middle where
  size fl fm = (\l (Middle m) -> Constrained (Start $ m - (l / 2)) (Middle m) (End $ m + (l / 2)) (Size l)) <$> fl <*> fm

instance CanSize End where
  size fl fe = (\l (End e) -> Constrained (Start $ e - l) (Middle $ e - (l / 2)) (End e) (Size l)) <$> fl <*> fe

instance CanSize Size where
  size fl _ = Size <$> fl

instance CanSize Constrained where
  size fl fc = (\l (Constrained _ (Middle m) _ _) -> Constrained (Start $ m - (l / 2)) (Middle m) (End $ m + (l / 2)) (Size l)) <$> fl <*> fc

instance HasSize Size where
  getSize = ((\(Size l) -> l) <$>)

instance HasSize Constrained where
  getSize = ((\(Constrained _ _ _ (Size l)) -> l) <$>)

horizontal ::
  (Functor f2) =>
  f2 (h, v) ->
  f2 h
horizontal fhv = (\(h, _) -> h) <$> fhv

vertical ::
  (Functor f2) =>
  f2 (h, v) ->
  f2 v
vertical fhv = (\(_, v) -> v) <$> fhv

getHorizontal ::
  (Applicative f1, Functor f2) =>
  (f1 a -> Identity b1) ->
  f2 (a, b2) ->
  f2 b1
getHorizontal f fhv = (\(h, _) -> runIdentity $ f (pure h)) <$> fhv

getVertical ::
  (Applicative f1, Functor f2) =>
  (f1 a1 -> Identity b) ->
  f2 (a2, a1) ->
  f2 b
getVertical f fhv = (\(_, v) -> runIdentity $ f (pure v)) <$> fhv

onHorizontal ::
  (Applicative f1, Applicative f2, Applicative f3) =>
  (f2 a1 -> f3 a2 -> Identity a3) ->
  f1 a1 ->
  f1 (a2, b) ->
  f1 (a3, b)
onHorizontal f fx fhv = (\(h, v) x -> (runIdentity $ f (pure x) (pure h), v)) <$> fhv <*> fx

onVertical ::
  (Applicative f1, Applicative f2, Applicative f3) =>
  (f2 a1 -> f3 a2 -> Identity b) ->
  f1 a1 ->
  f1 (a3, a2) ->
  f1 (a3, b)
onVertical f fx fhv = (\(h, v) x -> (h, runIdentity $ f (pure x) (pure v))) <$> fhv <*> fx
