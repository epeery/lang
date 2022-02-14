{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module B where

import Text.Pretty.Simple (pPrintString)
import V

eval :: Box (Vector a, Vector b) -> (Vector a, Vector b)
eval = unBox

view_ :: S (Vector a, Vector b) -> String
view_ = unS

view :: S (Vector a, Vector b) -> IO ()
view = pPrintString . view_

class B repr where
  box :: V f => repr (f Empty, f Empty)

class CanLeft repr where
  left :: CanStart f h => repr (f Double) -> repr (f h, f v) -> repr (f (Add Start h), f v)

class HasLeft repr where
  getLeft :: HasStart f h => repr (f h, f v) -> repr (f Double)

class CanRight repr where
  right :: CanEnd f h => repr (f Double) -> repr (f h, f v) -> repr (f (Add End h), f v)

class HasRight repr where
  getRight :: HasEnd f h => repr (f h, f v) -> repr (f Double)

class CanTop repr where
  top :: CanStart f v => repr (f Double) -> repr (f h, f v) -> repr (f h, f (Add Start v))

class HasTop repr where
  getTop :: HasStart f v => repr (f h, f v) -> repr (f Double)

class CanBottom repr where
  bottom :: CanEnd f v => repr (f Double) -> repr (f h, f v) -> repr (f h, f (Add End v))

class HasBottom repr where
  getBottom :: HasEnd f v => repr (f h, f v) -> repr (f Double)

class CanWidth repr where
  width :: CanSize f h => repr (f Double) -> repr (f h, f v) -> repr (f (Add Size h), f v)

class HasWidth repr where
  getWidth :: HasSize f h => repr (f h, f v) -> repr (f Double)

class CanHeight repr where
  height :: CanSize f v => repr (f Double) -> repr (f h, f v) -> repr (f h, f (Add Size v))

class HasHeight repr where
  getHeight :: HasSize f v => repr (f h, f v) -> repr (f Double)

class CanCenterX repr where
  centerX :: CanMiddle f h => repr (f Double) -> repr (f h, f v) -> repr (f (Add Middle h), f v)

class HasCenterX repr where
  getCenterX :: HasMiddle f h => repr (f h, f v) -> repr (f Double)

class CanCenterY repr where
  centerY :: CanMiddle f v => repr (f Double) -> repr (f h, f v) -> repr (f h, f (Add Middle v))

class HasCenterY repr where
  getCenterY :: HasMiddle f v => repr (f h, f v) -> repr (f Double)

newtype Box a = Box {unBox :: a}
  deriving (Show, Num, Fractional, Functor)

instance Applicative Box where
  pure = Box
  Box f <*> Box a = Box $ f a

instance Monad Box where
  return = pure
  Box a >>= f = f a

instance V Box where
  val = Box
  nil = Box Empty

instance B Box where
  box = Box (nil, nil)

instance CanLeft Box where
  left (Box n) (Box (h, v)) = Box (start n h, v)

instance HasLeft Box where
  getLeft (Box (h, _)) = Box $ getStart h

instance CanRight Box where
  right (Box n) (Box (h, v)) = Box (end n h, v)

instance HasRight Box where
  getRight (Box (h, _)) = Box $ getEnd h

instance CanTop Box where
  top (Box n) (Box (h, v)) = Box (h, start n v)

instance HasTop Box where
  getTop (Box (_, v)) = Box $ getStart v

instance CanBottom Box where
  bottom (Box n) (Box (h, v)) = Box (h, end n v)

instance HasBottom Box where
  getBottom (Box (_, v)) = Box $ getEnd v

instance CanWidth Box where
  width (Box n) (Box (h, v)) = Box (size n h, v)

instance HasWidth Box where
  getWidth (Box (h, _)) = Box $ getSize h

instance CanHeight Box where
  height (Box n) (Box (h, v)) = Box (h, size n v)

instance HasHeight Box where
  getHeight (Box (_, v)) = Box $ getSize v

instance CanCenterX Box where
  centerX (Box n) (Box (h, v)) = Box (middle n h, v)

instance HasCenterX Box where
  getCenterX (Box (h, _)) = Box $ getMiddle h

instance CanCenterY Box where
  centerY (Box n) (Box (h, v)) = Box (h, middle n v)

instance HasCenterY Box where
  getCenterY (Box (_, v)) = Box $ getMiddle v

instance B S where
  box = S "Box"

instance CanLeft S where
  left (S n) (S x) = S $ "SetLeft " <> n <> " (" <> x <> ")"

instance HasLeft S where
  getLeft (S n) = S $ "GetLeft (" <> n <> ")"

instance CanRight S where
  right (S n) (S x) = S $ "SetRight " <> n <> " (" <> x <> ")"

instance HasRight S where
  getRight (S n) = S $ "GetRight (" <> n <> ")"

instance CanTop S where
  top (S n) (S x) = S $ "SetTop " <> n <> " (" <> x <> ")"

instance HasTop S where
  getTop (S n) = S $ "GetTop (" <> n <> ")"

instance CanBottom S where
  bottom (S n) (S x) = S $ "SetBottom " <> n <> " (" <> x <> ")"

instance HasBottom S where
  getBottom (S n) = S $ "GetBottom (" <> n <> ")"

instance CanWidth S where
  width (S n) (S x) = S $ "SetWidth " <> n <> " (" <> x <> ")"

instance HasWidth S where
  getWidth (S n) = S $ "GetWidth (" <> n <> ")"

instance CanHeight S where
  height (S n) (S x) = S $ "SetHeight " <> n <> " (" <> x <> ")"

instance HasHeight S where
  getHeight (S n) = S $ "GetHeight (" <> n <> ")"

instance CanCenterX S where
  centerX (S n) (S x) = S $ "SetCenterX " <> n <> " (" <> x <> ")"

instance HasCenterX S where
  getCenterX (S n) = S $ "GetCenterX (" <> n <> ")"

instance CanCenterY S where
  centerY (S n) (S x) = S $ "SetCenterY " <> n <> " (" <> x <> ")"

instance HasCenterY S where
  getCenterY (S n) = S $ "GetCenterY (" <> n <> ")"

spaceBetweenH ::
  ( Foldable t,
    Functor t,
    HasWidth repr,
    Fractional (repr (f Double)),
    HasSize f h1,
    HasSize f h2,
    CanLeft repr,
    CanStart f h1,
    HasLeft repr,
    HasStart f h2
  ) =>
  t (repr (f h1, f v1)) ->
  repr (f h2, f v2) ->
  [repr (f (Add Start h1), f v1)]
spaceBetweenH cs c = snd $ foldr (\v (offset, acc) -> (offset + margin + getWidth v, left offset v : acc)) (getLeft c, []) cs
  where
    contentSize = sum (getWidth <$> cs)
    margin = (getWidth c - contentSize) / (fromIntegral $ length cs - 1)

spaceBetweenV ::
  ( Foldable t,
    Functor t,
    HasHeight repr,
    Fractional (repr (f Double)),
    HasSize f v1,
    HasSize f v2,
    CanTop repr,
    CanStart f v1,
    HasTop repr,
    HasStart f v2
  ) =>
  t (repr (f h1, f v1)) ->
  repr (f h2, f v2) ->
  [repr (f h1, f (Add Start v1))]
spaceBetweenV cs c = snd $ foldr (\v (offset, acc) -> (offset + margin + getHeight v, top offset v : acc)) (getTop c, []) cs
  where
    contentSize = sum (getHeight <$> cs)
    margin = (getHeight c - contentSize) / (fromIntegral $ length cs - 1)
